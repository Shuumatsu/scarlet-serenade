{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
  show (Classnames []) = ""
  show (Classnames cs) = printf "class=\"%s\"" $ intercalate "," $ T.unpack <$> cs

-- |
data SentenceComponent = SText Text | SCode Text | SEquation Text | SBold Text

instance Show SentenceComponent where
  show (SText t) = T.unpack t
  show (SCode t) = printf "<code-snippet>%s</code-snippet>" (renderHtml . toHtml . text $ t)
  show (SEquation t) = printf "<mathjax-panel>%s</mathjax-panel>" (renderHtml . toHtml . text $ t)
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
      f (n, (Component tag cs attrs children)) =
        let content = case children of
              Document [(0, (Sentence p))] -> intercalate " " $ fmap show p
              _ -> show children
         in printf "<%s %s %s>%s</%s>" tag (show cs) (show attrs) content tag

-- |
-- data Piece

-- data Component

-- data Element a where
--   Code :: Text -> Element Component
--   Equation :: Text -> Element Component
--   Element :: Text -> Classnames -> Attrs -> Document -> Element Component
--   Sentence :: [Element Piece] -> Element Component
--   PText :: Text -> Element Piece
--   PCode :: Text -> Element Piece
--   PEquation :: Text -> Element Piece
--   PBold :: Text -> Element Piece