{-
  a sample from:
  https://wiki.haskell.org/Parsing_a_simple_imperative_language

  reference of Parsec.Token
  http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Token.html

  What is token?
  -> string with an assigned and thus identified meaning
  https://en.wikipedia.org/wiki/Lexical_analysis

  lexical analysis in a nutshell (yacc & lex)
  http://kmaebashi.com/programmer/devlang/yacclex.html

  String conversion in Haskell
  https://qiita.com/satosystems/items/e07e9907e4da9ab853fc

  Attoparsec or Parsec
  https://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell

  parse of double quotation
  https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec/24106749
-}

import qualified System.IO as IO
import qualified Control.Monad as Monad
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Expr as Expr
import qualified Text.ParserCombinators.Parsec.Language as Lang
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Binary boolean operators
data BBinOp = And | Or deriving (Show)

-- Relational operators
data RBinOp = Greater | Less deriving (Show)

{-
  lexical analysis is a recursive job.
  Structures BExpr, AExpr, Stmt owe to the recursion
-}
-- boolean expressions
data BExpr = BoolConst Bool
            | Not BExpr
            | BBinary BBinOp BExpr BExpr
            | RBinary RBinOp AExpr AExpr
             deriving (Show)
-- arithmetic operators
data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
               deriving (Show)

-- arithmetic expressions
data AExpr = Var String
            | IntConst Integer
            | Neg AExpr
            | ABinary ABinOp AExpr AExpr
              deriving (Show)

-- statement: Seq is constructor
data Stmt = Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
             deriving (Show)

languageDef =
   Lang.emptyDef { Token.commentStart    = "/*"
                 , Token.commentEnd      = "*/"
                 , Token.commentLine     = "//"
                 , Token.identStart      = P.letter
                 , Token.identLetter     = P.alphaNum
                 , Token.reservedNames   = [ "if"
                                           , "then"
                                           , "else"
                                           , "while"
                                           , "do"
                                           , "skip"
                                           , "true"
                                           , "false"
                                           , "not"
                                           , "and"
                                           , "or"
                                           ]
                 , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                           , "<", ">", "and", "or", "not"
                                           ]
                 }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: P.Parser Stmt
whileParser = whiteSpace >> statement

statement :: P.Parser Stmt
statement =   parens statement
          P.<|> sequenceOfStmt
 
sequenceOfStmt =
   do list <- (P.sepBy1 statement' semi)
      -- If there's only one statement return it without using Seq.
      return $ if length list == 1 then head list else Seq list

statement' :: P.Parser Stmt
statement' =   ifStmt
           P.<|> whileStmt
           P.<|> skipStmt
           P.<|> assignStmt

ifStmt :: P.Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 
whileStmt :: P.Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt
 
assignStmt :: P.Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: P.Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: P.Parser AExpr
aExpression = Expr.buildExpressionParser aOperators aTerm
 
bExpression :: P.Parser BExpr
bExpression = Expr.buildExpressionParser bOperators bTerm

aOperators = [ [Expr.Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Expr.Infix  (reservedOp "*"   >> return (ABinary Multiply)) Expr.AssocLeft,
                Expr.Infix  (reservedOp "/"   >> return (ABinary Divide  )) Expr.AssocLeft]
             , [Expr.Infix  (reservedOp "+"   >> return (ABinary Add     )) Expr.AssocLeft,
                Expr.Infix  (reservedOp "-"   >> return (ABinary Subtract)) Expr.AssocLeft]
              ]
 
bOperators = [ [Expr.Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Expr.Infix  (reservedOp "and" >> return (BBinary And     )) Expr.AssocLeft,
                Expr.Infix  (reservedOp "or"  >> return (BBinary Or      )) Expr.AssocLeft]
             ]

aTerm =  parens aExpression
     P.<|> Monad.liftM Var identifier
     P.<|> Monad.liftM IntConst integer

bTerm =  parens bExpression
     P.<|> (reserved "true"  >> return (BoolConst True ))
     P.<|> (reserved "false" >> return (BoolConst False))
     P.<|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2
 
relation =   (reservedOp ">" >> return Greater)
         P.<|> (reservedOp "<" >> return Less)

parseString :: String -> Stmt
parseString str =
  case P.parse whileParser "" str of
     Left e  -> error $ show e
     Right r -> r
 
parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case P.parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

{-
  to run with a file in ghci:
  parseFile "sample_imperative_lang.txt"
-}

main = putStrLn "Hello"
