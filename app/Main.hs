--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Ast
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
import Prs (pDocument)
import Text.Megaparsec (errorBundlePretty, parse)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith (defaultConfiguration {previewHost = "0.0.0.0"}) $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*.prs" $ do
    route $ setExtension "html"
    compile $ do
      body <- getResourceBody
      path <- getResourceFilePath
      cached "prs" $
        traverse
          ( \t -> case parse pDocument path t of
              Left bundle -> fail (errorBundlePretty bundle)
              Right ast -> return . show $ ast
          )
          (T.pack <$> body)
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "posts/**/*.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
-- postCtx =
--   dateField "date" "%B %e, %Y"
--     `mappend` defaultContext
postCtx = defaultContext
