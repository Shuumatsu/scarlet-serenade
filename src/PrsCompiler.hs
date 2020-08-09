{-# LANGUAGE OverloadedStrings #-}
module PrsCompiler where


import Ast
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
import Hakyll.Core.Compiler (Compiler)
import Prs (pDocument)
import Text.Megaparsec (errorBundlePretty, parse)

prsCompiler :: Compiler (Item String)
prsCompiler = do
  path <- getResourceFilePath
  src <- getResourceBody
  readPrs path (T.pack <$> src)

readPrs :: String -> Item Text -> Compiler (Item String)
readPrs path src = traverse h src
  where
    h src = case parse pDocument path src of
      Left bundle -> fail (errorBundlePretty bundle)
      Right ast -> return . show $ ast