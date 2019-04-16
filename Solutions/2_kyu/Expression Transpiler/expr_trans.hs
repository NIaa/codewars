module Transpiler where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative

alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

digit :: String
digit = ['0' .. '9']

tokenize :: String -> [String]
tokenize [] = []
tokenize xxs@(c : cs)
  | c == '-' && head cs == '>' = "->" : tokenize (tail cs)
  | c `elem` "(){}," = [c] : tokenize cs
  | not (null s) = s : tokenize ss
  | otherwise = tokenize cs
  where
    (s, ss) = span (`elem` alpha ++ digit) xxs


-----------------------------------------------------
-------------- your parser combinator ---------------
-----------------------------------------------------

newtype Parser val = Parser { parse :: String -> [(val, String)]  }
-----------------------------------------------------
-- instances
instance Functor Parser where
    fmap f p = Parser $ \inp -> case parse p inp of 
                                  [] -> []
                                  [(v, s)] -> [(f v, s)]

instance Applicative Parser where
    pure v =  Parser $ \inp -> [(v, inp)]
    pf <*> px = Parser $ \inp -> case parse pf inp of
                                   [] -> []
                                   [(f, s)] -> parse (fmap f px) s

instance Monad Parser where
    pa >>= apb = Parser $ \inp -> case parse pa inp of
                                    [] -> []
                                    [(a, s)] -> parse (apb a) s

instance Alternative Parser where
    empty = Parser $ const []
    p <|> q = Parser $ \inp -> case parse p inp of
                                 [(v, s)] -> [(v, s)]
                                 [] -> parse q inp
-----------------------------------------------------
-- derived primitives
item :: Parser Char
item = Parser $ \inp -> case inp of
                          [] -> []
                          (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

space :: Parser ()

char :: Char -> Parser Char

string :: String -> Parser String

token :: Parser a -> Parser a
-----------------------------------------------------

{-

function ::= expression "(" [parameters] ")" [lambda]         
           | expression lambda

expression ::= name                                       
             | lambda

parameters ::= expression ["," parameters]                

lambdaparam ::= nameOrNumber ["," lambdaparam]
lambdastmt  ::= nameOrNumber [lambdastmt]

lambda ::= "{" [lambdaparam "->"] [lambdastmt] "}"
-}
optional p =  p <|> return ""

:: Parser String
function = do  <- bracket "(" (optional parameters) ")"
              optional lambda
              return
                <|> do e <- expression; l <- 
                
                lambda; return
expression = name <|> lambda
parameters = 
lambdaparam =
lambdastmt = 
lambda = "->"

{-
function ::= expression "(" [parameters] ")"

expression ::= nameOrNumber
             | lambda

parameters ::= expression ["," parameters]

lambdaparam ::= nameOrNumber ["," lambdaparam]
lambdastmt  ::= nameOrNumber ";" [lambdastmt]

lambda ::= "(" [lambdaparam] "){" [lambdastmt] "}"
-}

name
-----------------------------------------------------
parseCode :: Parser a -> String -> Either String a
parseCode m s = case parse m s of
  [(res, [])] -> Right res
  _           -> Left "Hugh?"
--

(<~>) :: Alternative a => a b -> a b -> a b
(<~>) = flip (<|>)

-- the code given above is not necessary
-- but they might help
-- if you have your own idea you can simply remove all of them :D

transpile :: String -> Either String String
transpile = parseCode function
