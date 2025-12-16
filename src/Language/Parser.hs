{-# LANGUAGE OverloadedStrings #-}

module Language.Parser (parseProgram) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Language.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    lineComment
    blockComment
 where
  lineComment = try (string "#" *> notFollowedBy (char '-')) *> void (takeWhileP (Just "comment") (/= '\n'))
  blockComment = L.skipBlockComment "#-" "-#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

comma :: Parser ()
comma = void (symbol ",")

semi :: Parser ()
semi = void (symbol ";")

identifier :: Parser Name
identifier = lexeme $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  return (c : cs)

-- Types

parseType :: Parser Type
parseType = try functionType <|> baseType
 where
  functionType = do
    args <- try (parens (parseType `sepBy` comma)) <|> ((: []) <$> baseType)
    _ <- symbol "=>"
    ret <- parseType
    return (TFun args ret)

  baseType =
    (symbol "~" >> TDual <$> baseType)
      <|> atom

  atom =
    choice
      [ TTensor <$> brackets (parseType `sepBy` comma)
      , TPar <$> braces (parseType `sepBy` comma)
      , TAtom <$> identifier
      , parens parseType
      ]

-- Terms

term :: Parser Term
term = try lambda <|> appOrAtom
 where
  lambda = do
    -- (x:T, y:U) => Body
    params <- parens (parseTypedPattern `sepBy` comma)
    _ <- symbol "=>"
    body <- term
    return (Lambda params body)

  appOrAtom = do
    t <- atom
    args <- many (parens (term `sepBy` comma))
    return $ foldl App t args

  atom =
    choice
      [ Var <$> identifier
      , Tensor <$> brackets (term `sepBy` comma)
      , braces parseBraceContent -- Handles Par or Block
      , parens term
      ]

parseBraceContent :: Parser Term
parseBraceContent = try parseBlock <|> parsePar
 where
  parsePar = Par <$> (term `sepBy` comma)
  parseBlock = do
    ss <- many (try (stmt <* semi))
    _ <- symbol "return"
    t <- term
    _ <- optional semi
    return (Block ss t)

parseBracePattern :: Parser Pattern
parseBracePattern = try parseBlock <|> parsePar
 where
  parsePar = PPar <$> (parsePattern `sepBy` comma)
  parseBlock = do
    _ <- symbol "require"
    pt <- parsePattern
    _ <- semi
    ss <- many (try (stmt <* semi))
    return (PBlock pt ss)

parsePattern :: Parser Pattern
parsePattern =
  choice
    [ PVar <$> identifier
    , PTensor <$> brackets (parsePattern `sepBy` comma)
    , braces parseBracePattern
    , parens parsePattern
    ]

parseBraceTypedPattern :: Parser TypedPattern
parseBraceTypedPattern = try parseBlock <|> parsePar
 where
  parsePar = TPPar <$> (parseTypedPattern `sepBy` comma)
  parseBlock = do
    _ <- symbol "require"
    pt <- parseTypedPattern
    _ <- semi
    ss <- many (try (stmt <* semi))
    return (TPBlock pt ss)

parseTypedPattern :: Parser TypedPattern
parseTypedPattern =
  choice
    [ TPVar <$> identifier <* symbol ":" <*> parseType
    , TPTensor <$> brackets (parseTypedPattern `sepBy` comma)
    , braces parseBraceTypedPattern
    , parens parseTypedPattern
    ]

-- Statements
stmt :: Parser Stmt
stmt =
  choice
    [ try parseLet
    , try parseIntro
    , parseElim
    ]
 where
  parseLet = do
    _ <- symbol "let"
    pt <- parsePattern
    _ <- symbol "="
    t <- term
    return (Let pt t)

  parseIntro = do
    _ <- symbol "intro"
    x <- identifier
    comma
    y <- identifier
    _ <- symbol ":"
    t <- parseType
    return (Intro x y t)

  parseElim = do
    _ <- symbol "elim"
    t1 <- term
    comma
    t2 <- term
    return (Elim t1 t2)

-- Top Level

topLevel :: Parser TopLevel
topLevel = try parseTypeDef <|> parseGlobalLet
 where
  parseTypeDef = do
    _ <- symbol "type"
    n <- identifier
    _ <- symbol "="
    t <- parseType
    semi
    return (TypeDef n t)

  parseGlobalLet = do
    _ <- symbol "let"
    n <- identifier
    t <- optional $ do
      _ <- symbol ":"
      parseType
    _ <- symbol "="
    body <- term
    semi
    return (GlobalLet n t body)

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse (sc >> (Program <$> many topLevel) <* eof) "source"
