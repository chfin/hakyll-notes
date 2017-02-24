--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Foldable (concatMap)
import           Hakyll as H
import           Control.Monad
import Text.CSL.Input.Bibtex (readBibtex)
import qualified Text.CSL.Reference as Ref
import Text.CSL.Parser (readCSLFile)
import Text.CSL.Proc (citeproc, procOpts)
import Text.CSL.Output.Pandoc (renderPandoc')
import Text.CSL.Style ( Locale(..), styleDefaultLocale, styleLocale
                      , BiblioData(..), Style, Formatted
                      , citeId, emptyCite)
import System.FilePath (replaceExtension)

import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Options

--------------------------------------------------------------------------------
bibFile :: FilePath
bibFile = "notes/notes-bib.bib"

cslFile :: FilePath
cslFile = "csl/chicago-author-date.csl"

paperNotesFile :: FilePath
paperNotesFile = "paper_notes.org"

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "notes/*.bib" $ version "bib" $ compile biblioCompiler
    match "csl/*.csl" $ compile cslCompiler

    match "notes/*.bib" $ version "html" $ do
      route $ setExtension "html"
      compile $ compileBib cslFile
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "notes/*.org" $ do
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

renderRef :: Style -> (Formatted, String) -> [Block]
renderRef s (ref, id) = toList $ extend block
  where block = renderPandoc' s (ref, id)
        extend (Div a bs) = divWith a ((B.fromList bs) <> linkPara)
        extend blk = divWith nullAttr (singleton blk) <> linkPara
        linkPara = divWith pAttr (para $ link noteLink "Go to paper notes" "Notes")
        noteLink = paperNotesLink ++ "#" ++ id
        pAttr = ("", ["reference-links"], [])

compileBib :: FilePath -> Compiler (Item String)
compileBib stylefn = do
  -- bibI <- load (fromFilePath bibfn)
  bibfn <- getUnderlying
  style <- unsafeCompiler $ readCSLFile Nothing stylefn
  refs <- unsafeCompiler $ readBibtex False True (toFilePath bibfn)
  let bibData = processBib style refs
      bib     = bibliography bibData
      blocks  = (zip bib (citationIds bibData)) >>= (renderRef style)
      res     = doc (B.fromList blocks)
  return $ writePandoc (Item bibfn res)

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
        path       = replaceExtension (toFilePath (itemIdentifier bib)) "html"
linkCitations _ i = i

incHeader :: Block -> Block
incHeader (Header depth attr cont) = Header (depth + 1) attr cont
incHeader block = block
