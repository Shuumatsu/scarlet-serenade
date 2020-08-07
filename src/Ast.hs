module Ast where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

newtype Attrs = Attrs [(Text, Text)]

instance Show Attrs where
  show (Attrs attrs) = intercalate " " $ (\(key, value) -> printf "%s=\"%s\"" (T.unpack key) (T.unpack value)) <$> attrs

newtype Classnames = Classnames [Text]

instance Show Classnames where
  show (Classnames cs) = intercalate "," $ T.unpack <$> cs

data SentenceComponent = SText Text | SCode Text | SEquation Text | SBold Text

instance Show SentenceComponent where
  show (SText t) = T.unpack t
  show (SCode t) = printf "<code-snippet>%s</code-snippet>" (T.unpack t)
  show (SEquation t) = undefined
  show (SBold t) = undefined

data Ast
  = Sentence [SentenceComponent]
  | Code Text
  | Equation Text
  | Component Text Classnames Attrs Document

instance Show Ast where
  show (Sentence p) = printf "<p>%s</p>" (intercalate " " $ fmap show p)
  show (Code code) = printf "<code-snippet block>%s</code-snippet>" (T.unpack code)
  show (Equation eq) = printf "<mathjax-panel block>%s</mathjax-panel'>" (T.unpack eq)
  show (Component tag cs@(Classnames cs') attrs children) =
    printf
      "<%s %s %s>%s</%s>"
      tag
      (if length cs' == 0 then "" else "class=" ++ show (show cs))
      (show attrs)
      (show children)
      tag

newtype Document = Document [Ast]

instance Show Document where
  show (Document as) = intercalate "\n" $ fmap show as