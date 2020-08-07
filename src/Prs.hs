{-# LANGUAGE OverloadedStrings #-}

module Prs (pDocument, pText) where

import Ast
import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Control.Monad.Combinators
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

psc :: Parser ()
psc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

pscn :: Parser ()
pscn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme psc

symbol :: Text -> Parser Text
symbol = L.symbol psc

pCharLiteral :: Parser Char
pCharLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

pStringLiteral :: Parser Text
pStringLiteral = lexeme $ T.pack <$> p
  where
    p = char '\"' *> manyTill L.charLiteral (char '\"')

pIdent :: Parser Text
pIdent = lexeme $ T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '-'))

parens :: Parser a -> Parser a
parens = between (symbol "(") (char ')')

-- | sentence
pInlineCode :: Parser SentenceComponent
pInlineCode = dbg "inline_code" (lexeme p) <?> "inline_code"
  where
    content = manyTill anySingle (symbol "`")
    p = SCode . T.pack <$> (symbol "`" *> content)

pInlineEquation :: Parser SentenceComponent
pInlineEquation = dbg "inline_equation" (lexeme p) <?> "inline_equation"
  where
    content = manyTill anySingle (symbol "$")
    p = SEquation . T.pack <$> (symbol "$" *> content)

pText :: Parser SentenceComponent
pText = SText . T.pack <$> some p
  where
    p = do
      (e, r) <- (,) <$> lookAhead (optional eol) <*> lookAhead (count' 1 10 anySingle)
      case (e, r) of
        (Just _, _) -> unexpected (Tokens (head r :| []))
        (Nothing, c : r') | c == '`' || c == '$' -> unexpected (Tokens (head r :| []))
        (Nothing, c : r') -> c <$ skipCount (length r - length r') anySingle

pSentence :: Parser Ast
pSentence = L.lexeme pscn $ Sentence <$> some (pText <|> pInlineCode <|> pInlineEquation)

-- | block
pBlockCode :: Parser Ast
pBlockCode = dbg "block_code" (L.lexeme pscn p) <?> "block_code"
  where
    content = many $ do
      r <- lookAhead (count' 1 10 anySingle)
      case r of
        '`' : '`' : '`' : r' -> unexpected (Tokens (head r :| []))
        c : r' -> c <$ skipCount (length r - length r') anySingle
    p = Code . T.pack <$> between (symbol "```") (symbol "```") content

pBlockEquation :: Parser Ast
pBlockEquation = dbg "block_equation" (L.lexeme pscn p) <?> "block_equation"
  where
    content = many $ do
      r <- lookAhead (count' 1 10 anySingle)
      case r of
        '$' : '$' : r' -> unexpected (Tokens (head r :| []))
        c : r' -> c <$ skipCount (length r - length r') anySingle
    p = Equation . T.pack <$> between (symbol "$$") (symbol "$$") content

-- | component
pClassnames :: Parser Classnames
pClassnames = dbg "classnames" p <?> "classnames"
  where
    p = Classnames . (maybe [] id) <$> optional (char '.' >> sepBy1 pIdent (char ','))

pAttrs :: Parser Attrs
pAttrs = dbg "attrs" p <?> "attrs"
  where
    pAttr = (,) <$> pIdent <* string "=" <*> pStringLiteral
    p = Attrs . (maybe [] id) <$> (optional . parens) (sepBy1 pAttr (symbol ","))

pComponent :: Parser Ast
pComponent = dbg "component" (L.lexeme pscn p) <?> "component"
  where
    meta = lexeme $ (,,) <$> (char ':' *> pIdent) <*> pClassnames <*> pAttrs
    inline = dbg "inline" $ do
      (tag, classnames, attrs) <- meta
      Component tag classnames attrs . Document . pure <$> pSentence
    block = dbg "block" $ L.indentBlock pscn $ do
      (tag, classnames, attrs) <- meta
      pure $ L.IndentMany Nothing (pure . Component tag classnames attrs . Document) pElement
    p = try inline <|> block

-- | element
pElement :: Parser Ast
pElement = dbg "element" p <?> "element"
  where
    p = pComponent <|> pBlockCode <|> pBlockEquation <|> pSentence

pDocument :: Parser Document
pDocument = dbg "document" $ Document <$> p <* eof
  where
    p = many . L.lexeme pscn $ L.nonIndented pscn pElement
