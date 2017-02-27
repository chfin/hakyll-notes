--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Foldable (concatMap)
import           Hakyll as H
import           Control.Monad

import System.FilePath (replaceExtension)
import Data.List.Split
import Data.List (find)
import Data.Either (rights)

import Text.CSL.Input.Bibtex (readBibtex)
import qualified Text.CSL.Reference as Ref
import Text.CSL.Parser (readCSLFile)
import Text.CSL.Proc (citeproc, procOpts)
import Text.CSL.Output.Pandoc (renderPandoc')
import Text.CSL.Style ( Locale(..), styleDefaultLocale, styleLocale
                      , BiblioData(..), Style, Formatted
                      , citeId, emptyCite)

import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Options
import Text.Pandoc (readOrg)

--------------------------------------------------------------------------------
bibFile :: FilePath
bibFile = "notes/bibliography.bib"

cslFile :: FilePath
cslFile = "csl/chicago-author-date.csl"

paperNotesFile :: FilePath
paperNotesFile = (replaceExtension bibFile "org")

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler
    match "js/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "notes/**.bib" $ version "bib" $ compile biblioCompiler
    match "csl/*.csl" $ compile cslCompiler

    match "notes/**.bib" $ version "html" $ do
      route $ setExtension "bib.html"
      compile $ compileBib cslFile
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "notes/**.org" $ do
      route $ setExtension "html"
      compile $ do
        let noteCtx = constField "isNote" "true" `mappend` defaultContext
        compileNote bibFile cslFile
          >>= loadAndApplyTemplate "templates/default.html" noteCtx
          >>= relativizeUrls
        
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- loadAll "notes/*.org"
        bibs <- loadAll ("notes/*.bib" .&&. hasVersion "html")
        let indexCtx =
              listField "posts" defaultContext (return posts) `mappend`
              listField "bibs" defaultContext (return bibs) `mappend`
              constField "title" "Index" `mappend`
              defaultContext
        
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

processBib :: Style -> [Ref.Reference] -> BiblioData
processBib s rs = citeproc procOpts s rs [map (\r -> emptyCite { citeId = Ref.unLiteral $ Ref.refId r}) rs]

paperNotesLink = replaceExtension paperNotesFile "html"

matchNoteEntry :: String -> [Block] -> Bool
matchNoteEntry ref ((Header _ (id, _, _) _):_)
  | ref == id = True
matchNoteEntry _ _ = False

classAttr :: String -> Attr
classAttr c = ("", [c], [])

renderRef :: Style -> [[Block]] -> (Formatted, String) -> [Block]
renderRef style nEntries (ref, rId) = toList $ extend block
  where block = renderPandoc' style (ref, rId)
        extend (Div a bs) = divWith a ((B.fromList bs) <> linkPara <> noteDiv)
        extend blk = divWith nullAttr (singleton blk) <> linkPara <> noteDiv
        linkPara = if null nEntry then
                     mempty
                   else 
                     divWith pAttr (para $ showNotes <> space <> noteLink)
        noteLink = link ("/" ++ paperNotesLink ++ "#" ++ rId) "Go to paper notes" "go to notes →"
        showNotes = singleton $
          Link (classAttr "reference-toggle-notes") [Str "show / hide notes"] ("#","")
        pAttr = classAttr "reference-links"
        nEntry = maybe [] tail (find (matchNoteEntry rId) nEntries)
        noteDiv = singleton $ Div nAttr nEntry
        nAttr = classAttr "reference-notes"

isH2 :: Block -> Bool
isH2 (Header 2 _ _) = True
isH2 _              = False

splitNotes :: Pandoc -> [[Block]]
splitNotes notes = (split . dropInitBlank . keepDelimsL .whenElt)
                   isH2 (blocks notes)
  where blocks (Pandoc _ b) = b

compileBib :: FilePath -> Compiler (Item String)
compileBib styleFp = do
  -- bibI <- load (fromFilePath bibfn)
  bibFp <- getUnderlying
  style <- unsafeCompiler $ readCSLFile Nothing styleFp
  refs  <- unsafeCompiler $ readBibtex False True (toFilePath bibFp)
  bibI  <- load $ setVersion (Just "bib") bibFp
  cslI  <- load $ fromFilePath styleFp
  notes <- (unsafeCompiler $ readFile paperNotesFile)
           >>= return . Item (fromFilePath paperNotesFile)
           >>= readPandocBiblio defaultHakyllReaderOptions cslI bibI
           >>= return . walk (linkCitations bibI)
  let bibData  = processBib style refs
      bib      = bibliography bibData
      nEntries = splitNotes (itemBody notes)
      blocks   = (zip bib (citationIds bibData)) >>= (renderRef style nEntries)
      res      = doc (B.fromList blocks)
  return $ writePandoc (Item bibFp res)

asideTemplate = "<aside id=\"toc\" class=\"container\"><div>$toc$</div></aside>\
                \<main class=\"container\">\
                \  <div id=\"layout-body\">$body$</div>\
                \</main>"

compileNote :: FilePath -> FilePath -> Compiler (Item String)
compileNote bibFp styleFp = do
  bib <- load $ setVersion (Just "bib") (fromFilePath bibFp)
  csl <- load $ fromFilePath styleFp
  --liftM writePandoc
  getResourceBody
    >>= readPandocBiblio readerOpts csl bib
    >>= return . walk (linkCitations bib)
    >>= return . walk incHeader
    >>= return . writePandocWith writerOpts
  where readerOpts = defaultHakyllReaderOptions
        writerOpts = defaultHakyllWriterOptions {
          writerTableOfContents = True,
          writerTemplate = Just asideTemplate
          }

linkCitations :: Item Biblio -> Inline -> Inline
linkCitations bib (Cite cts txts) = Span nullAttr (zipWith mkLink cts txts) 
  where mkLink c t = Link ("",["citation-link"],[]) [Cite [c] [t]] (mkTarget c)
        mkTarget c = ("/" ++ path ++ "#ref-" ++ citationId c, "")
        path       = replaceExtension (toFilePath (itemIdentifier bib)) "bib.html"
linkCitations _ i = i

incHeader :: Block -> Block
incHeader (Header depth attr cont) = Header (depth + 1) attr cont
incHeader block = block
