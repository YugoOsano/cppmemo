{-
  a sample from:
  https://wiki.haskell.org/Parsing_a_simple_imperative_language

  What is token?
  -> string with an assigned and thus identified meaning
  https://en.wikipedia.org/wiki/Lexical_analysis
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

-- statement
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

-- whileParser :: P.Parser Stmt
-- whileParser = Token.whiteSpace >> statement

statement = undefined 

main = putStrLn "Hello"
