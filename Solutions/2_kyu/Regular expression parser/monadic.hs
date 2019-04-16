module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Data.Char
import Control.Applicative

data RegExp = Normal Char       -- ^ A character that is not in "()*|."               'a'
            | Any               -- ^ Any charater                                     .
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp       (r)*
            | Or RegExp RegExp  -- ^ A choice between 2 regexps                        a|b
            | Str [RegExp]      -- ^ A sequence of regexps.                            "abc(d)"
            --()
  deriving (Show, Eq)
--------------------------------------------------------------------------------
regexp = or_R_R <|> str_Rs <|> zeroOrMore_R <|> factor

or_R_R = do e1 <- (str_Rs <|> zeroOrMore_R <|> factor)
            char '|'
            e2 <- (str_Rs <|> zeroOrMore_R <|> factor)
            return $ Or e1 e2

str_Rs = do es <- some (zeroOrMore_R <|> factor)
            if length es > 1 then return $ Str es else empty

zeroOrMore_R = do e <- factor
                  char '*'
                  return $ ZeroOrMore e

factor = normal_C <|> anyP <|> bracket regexp

normal_C = do c <- noneOf "()*|."
              return $ Normal c
anyP = do char '.'
          return Any

parseRegExp :: String -> Maybe RegExp
parseRegExp = parseCode regexp

--------------------------------------------------------------------------------
newtype Parser val = Parser { parse :: String -> [(val, String)]  }

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
--------------------------------------------------------------------------------
-- derived primitives
item :: Parser Char
item = Parser $ \inp -> case inp of
                          [] -> []
                          (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

oneOf :: String -> Parser Char
oneOf s = sat (`elem` s)

noneOf :: String -> Parser Char
noneOf s = sat $ not . (`elem` s)

char :: Char -> Parser Char
char = sat . (==)

digitP :: Parser Char
digitP = sat isDigit

space :: Parser ()
space = do many (sat isSpace)
           return ()

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

token :: Parser a -> Parser a
token p = do space
             a <- p
             space
             return a

bracket :: Parser a -> Parser a
bracket p = do string "("
               ret <- p
               string ")"
               return ret

num :: Parser Int
num = do x <- string "-" <|> return [] 
         xs <- some digitP
         return $ read $ x ++ xs

stringT = token . string 
-- binOp
binOp :: String -> (b -> b -> b) -> Parser (b -> b -> b)
binOp symbol func = do string symbol
                       return func

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do a <- p
                  rest a
                  where
                    rest a = (do f <- op; b <- p; rest $ f a b
                             <|> return a)
                             
parseCode :: Parser a -> String -> Maybe a
parseCode p s = case parse p s of
                  [(v, [])] -> Just v
                  _ -> Nothing
