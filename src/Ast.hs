{-# LANGUAGE OverloadedStrings #-}

module Ast where

import Data.List (intercalate)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Natural
import Text.Blaze (text)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html (toHtml)
import Text.Printf (printf)

-- |
newtype Attrs = Attrs [(Text, Text)]

instance Semigroup Attrs where
  (Attrs xs) <> (Attrs ys) = Attrs (xs <> ys)

instance Monoid Attrs where
  mempty = Attrs []

instance Show Attrs where
  show (Attrs attrs) = intercalate " " $ (\(key, value) -> printf "%s=\"%s\"" (T.unpack key) (T.unpack value)) <$> attrs

-- |
newtype Classnames = Classnames [Text]

instance Semigroup Classnames where
  (Classnames xs) <> (Classnames ys) = Classnames (xs <> ys)

instance Monoid Classnames where
  mempty = Classnames []

instance Show Classnames where
  show (Classnames cs) = intercalate "," $ T.unpack <$> cs

-- |
data SentenceComponent = SText Text | SCode Text | SEquation Text | SBold Text

instance Show SentenceComponent where
  show (SText t) = T.unpack t
  show (SCode t) = printf "<code-snippet>%s</code-snippet>" (T.unpack t)
  show (SEquation t) = printf "<mathjax-panel>%s</mathjax-panel>" (T.unpack t)
  show (SBold t) = printf "<b>%s</b>" (T.unpack t)

data Element
  = Sentence [SentenceComponent]
  | Code Text
  | Equation Text
  | Component Text Classnames Attrs Document

newtype Document = Document [(Natural, Element)]

instance Show Document where
  show (Document elements) = intercalate "\n" $ fmap f elements
    where
      f (n, (Sentence p)) = printf "<p indent=\"%s\">%s</p>" (show n) (intercalate " " $ fmap show p)
      f (n, (Code code)) = printf "<code-snippet indent=\"%s\" block=\"true\">%s</code-snippet>" (show n) (renderHtml . toHtml . text $ code)
      f (n, (Equation eq)) = printf "<mathjax-panel indent=\"%s\" block=\"true\">%s</mathjax-panel>" (show n) (renderHtml . toHtml . text $ eq)
      f (n, (Component tag cs@(Classnames cs') attrs children)) =
        printf
          "<%s %s %s>%s</%s>"
          tag
          (if length cs' == 0 then "" else "class=" ++ show (show cs))
          (show attrs)
          (show children)
          tag