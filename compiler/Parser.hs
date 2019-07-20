module Parser (parseFile) where

import Syntax
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative hiding (optional)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec hiding (many, option, (<|>))
  
-- Tokens
tokenParser = T.makeTokenParser $ emptyDef
  { commentLine      = "%"
  , nestedComments   = False
  , identStart       = letter
  , identLetter      = satisfy idLetter
  , opStart          = opLetter haskellStyle
  , opLetter         = oneOf "+-*/=<>|@^~?!"
  , reservedNames    = ["case", "of", "end", "when", "if",
                        "fun", "and", "or", "do"]
  , caseSensitive    = True
  }
  where
    idLetter c = isAlphaNum c || c == ':'

-- Common tokens
identifier = T.identifier tokenParser
reservedOp = T.reservedOp tokenParser
reserved = T.reserved tokenParser
integer = T.integer tokenParser
parens = T.parens tokenParser
semi = T.semi tokenParser
comma = T.comma tokenParser
braces = T.braces tokenParser
brackets = T.brackets tokenParser
symbol = T.symbol tokenParser
operator = T.operator tokenParser
charLiteral = T.charLiteral tokenParser
stringLiteral = T.stringLiteral tokenParser
lexeme = T.lexeme tokenParser
whitespace = T.whiteSpace tokenParser

-- Qualified identifier
qualId :: Parser String
qualId = do
    id <- identifier
    when (not (ok id)) $ do
      unexpected ("malformed qualified name " ++ show id)
    return id
  where
    ok id = length (filter (== ':') id) <= 1
         && last id /= ':'
         && null [() | (':', c) <- zip id (tail id), isUpper c]

-- Unqualified identifier
unqualId :: Parser String
unqualId = do
  id <- identifier
  when (':' `elem` id) $ do
    unexpected ("qualified name " ++ show id)
  return id

-- Lists
list :: Parser Exp -> Parser Exp
list expr = try (do
               symbol "["
               e1 <- expr
               symbol "|" 
               e2 <- expr
               symbol "]" 
               return (Cons e1 e2))
        <|> try (do
               symbol "["
               es <- sepBy expr comma
               symbol "]"
               return (List es))
        <|> try (do
              symbol "["
              from <- expr
              reservedOp ".."
              to <- expr
              symbol "]"
              return (ListEnum from to))
        <|> do symbol "["
               e <- expr
               reservedOp "||"
               stmts <- sepBy1 listCompStmt comma
               symbol "]"
               return (ListComp e stmts)

-- Patterns
pat :: Parser (Exp, Guard)
pat = do
  p <- ugpat
  g <- optionMaybe (reserved "when" *> expr)
  return (p, g)

-- Unguarded patterns
ugpat :: Parser Exp
ugpat = list ugpat
    <|> pure Tuple <*> braces (sepBy ugpat comma)
    <|> pure Int <*> integer
    <|> pure Id <*> unqualId

-- Expressions
expBinOp op assoc = Infix (reservedOp op >> return apply2) assoc
  where apply2 a b = Apply (Fun op 2) [a, b]

expUnOp op = Prefix (reservedOp op >> return apply1)
  where apply1 a = Apply (Fun op 1) [a]

opTable =
  [ [ expUnOp "bnot", expUnOp "not" ]
  , [ expBinOp ">>" AssocLeft, expBinOp "<<" AssocLeft]
  , [ expBinOp "*" AssocLeft, expBinOp "div" AssocLeft
    , expBinOp "band" AssocLeft ]
  , [ expBinOp "+" AssocLeft, expBinOp "-" AssocLeft
    , expBinOp "bor" AssocLeft ]
  , [ expBinOp "++" AssocRight ]
  , [ expBinOp "==" AssocNone, expBinOp "/=" AssocNone
    , expBinOp "<" AssocNone , expBinOp "<=" AssocNone
    , expBinOp ">" AssocNone , expBinOp ">=" AssocNone
    ]
  , [ expBinOp "and" AssocRight, expBinOp "or" AssocRight]
  ]

expr :: Parser Exp
expr = buildExpressionParser opTable expr1

expr1 :: Parser Exp
expr1 = doBlock
    <|> caseExpr
    <|> ifExpr
    <|> patBind
    <|> funApp
    <|> list expr
    <|> pure Int <*> integer
    <|> pure Tuple <*> braces (sepBy expr comma)
    <|> lambda
    <|> parens expr

-- Pattern binding
-- Use bactracking for this
patBind :: Parser Exp
patBind =
  try $ do
    p <- ugpat
    reservedOp "="
    e <- expr
    return (Bind p e)

-- Function application
funApp :: Parser Exp
funApp = do
  id <- qualId
  m <- optionMaybe (parens (sepBy expr comma))
  case m of
    Nothing -> return (Id id)
    Just args -> return (Apply (Id id) args)

-- Case expression
caseExpr :: Parser Exp
caseExpr = do
  reserved "case"
  e <- expr
  reserved "of"
  alts <- sepBy1 caseAlt semi
  reserved "end"
  return (Case e alts)

-- Case alternative
caseAlt :: Parser (Exp, Guard, [Exp])
caseAlt = do
  (p, g) <- pat
  reservedOp "->"
  body <- exprSeq
  return (p, g, body)

-- If expression
ifExpr :: Parser Exp
ifExpr = do
  reserved "if"
  alts <- sepBy1 ifAlt semi
  reserved "end"
  return (If alts)

-- If alternative
ifAlt :: Parser (Exp, [Exp])
ifAlt = do
  g <- expr
  reservedOp "->"
  body <- exprSeq
  return (g, body)

-- Sequence of expressions
exprSeq :: Parser [Exp]
exprSeq = sepBy1 expr comma

-- Lambda expression
lambda :: Parser Exp
lambda = do
    reserved "fun"
    eqns <- sepBy1 eqn semi
    reserved "end"
    return (Lambda eqns)
  where
    eqn = do
      args <- parens (sepBy ugpat comma)
      g <- optionMaybe (reserved "when" *> expr)
      reservedOp "->"
      body <- exprSeq
      return (args, g, body)

-- List comprehesion statment
listCompStmt :: Parser ListCompStmt
listCompStmt =
      try (do
        p <- ugpat
        reservedOp "<-"
        e <- expr
        return (ListCompBind p e))
  <|> do e <- expr
         return (ListCompGuard e)

-- Do notation
doBlock :: Parser Exp
doBlock = do
  mod <- try (do
           id <- qualId
           guard (":do" `isSuffixOf` id)
           return (takeWhile (/= ':') id ++ ":"))
     <|> (reserved "do" >> return "")
  stmts <- sepBy1 doStmt comma
  reserved "end"
  return (Do mod stmts)

doStmt :: Parser DoStmt
doStmt =
      try (do
        p <- ugpat
        reservedOp "<-"
        e <- expr
        return (DoBind p e))
  <|> do e <- expr
         return (DoExpr e)

-- Function declarations
funDecl :: Parser Decl
funDecl = do
  id <- unqualId
  args <- parens (sepBy ugpat comma)
  g <- optionMaybe (reserved "when" *> expr)
  reservedOp "->"
  body <- exprSeq
  symbol "." <|> symbol ";"
  return (FunDecl id args g body)

-- Module declarations
importDecl :: Parser Decl
importDecl = do
  reserved "-import"
  d <- parens $ do
         m <- unqualId
         return (ImportDecl m)
  reservedOp "."
  return d

-- Declarations
decl = importDecl
   <|> funDecl

-- Programs
prog :: Parser [Decl]
prog = do
  whitespace
  many decl
 
parseFile :: SourceName -> IO [Decl]
parseFile f = parseFromFile (prog <* eof) f >>= \result ->
  case result of
    Left e  -> error . show $ e
    Right p -> return p
