module TinyThreePassCompiler where

import Data.Char
import Data.Maybe
import Control.Applicative
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

bracket :: String -> Parser a -> String -> Parser a
bracket bra p ket = do stringT bra
                       ret <- p
                       stringT ket
                       return ret

ident :: Parser String
ident = do x <- some (oneOf $ alpha ++ digit)
           return x


num :: Parser Int
num = do x <- string "-" <|> return [] 
         xs <- some digitP
         return $ read $ x ++ xs

stringT = token . string 
identifier = token ident
-- binOp
binOp :: String -> (b -> b -> b) -> Parser (b -> b -> b)
binOp symbol func = do string symbol
                       return func

addOp = binOp "+" $ \x y -> Add x y
subOp = binOp "-" $ \x y -> Sub x y
mulOp = binOp "*" $ \x y -> Mul x y
divOp = binOp "/" $ \x y -> Div x y

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do a <- p
                  rest a
                  where
                    rest a = (do f <- op; b <- p; rest $ f a b
                             <|> return a)
                             
parseCode :: Parser a -> String -> a
parseCode p s = case parse p s of
                  [(v, [])] -> v
                  err -> error $ "#" ++ s ++ "#"
--------------------------------------------------------------------------------
data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

compile :: String -> [String]
compile = pass3 . pass2 . pass1
--------------------------------------------------------------------------------
function :: Parser AST
function = do dic <- bracket "[" arg_list "]"
              exp <- expression dic
              return exp

arg_list :: Parser [(String, Int)]
arg_list = do as <- many identifier
              return $ zip as [0..]
            

expression ::  [(String, Int)] -> Parser AST
expression dic = (chainl1 (token $ term dic) $ addOp <|> subOp)
                    <|> term dic

term :: [(String, Int)] -> Parser AST
term dic = (chainl1 (token $ factor dic) $ mulOp <|> divOp)
              <|> factor dic

factor :: [(String, Int)] -> Parser AST
factor dic = number <|> variable dic <|> bracket "(" (expression dic) ")"

number :: Parser AST
number = do n <- num
            return $ Imm n

variable :: [(String, Int)] -> Parser AST
variable dic = do i <- identifier
                  return $ Arg $ fromMaybe 0 (lookup i dic)
                  --   return $ Arg i

pass1 :: String -> AST
pass1 = parseCode function
--------------------------------------------------------------------------------
pass2 :: AST -> AST
pass2 (Add a1 a2) = case (pass2 a1, pass2 a2) of
                      (Imm x, Imm y) -> Imm (x + y)
                      (t1, t2) -> Add t1 t2
pass2 (Sub a1 a2) = case (pass2 a1, pass2 a2) of
                      (Imm x, Imm y) -> Imm (x - y)
                      (t1, t2) -> Sub t1 t2
pass2 (Mul a1 a2) = case (pass2 a1, pass2 a2) of
                      (Imm x, Imm y) -> Imm (x * y)
                      (t1, t2) -> Mul t1 t2
pass2 (Div a1 a2) = case (pass2 a1, pass2 a2) of
                      (Imm x, Imm y) -> Imm (x `div` y)
                      (t1, t2) -> Div t1 t2
pass2 a = a 
--------------------------------------------------------------------------------
pass3 :: AST -> [String]
pass3 (Imm n)   = ["IM " ++ show n]
pass3 (Arg i)   = ["AR " ++ show i]
pass3 (Add a b) = pass3 a ++ ["PU"] ++ pass3 b ++ ["SW", "PO", "AD"]
pass3 (Sub a b) = pass3 a ++ ["PU"] ++ pass3 b ++ ["SW", "PO", "SU"]
pass3 (Mul a b) = pass3 a ++ ["PU"] ++ pass3 b ++ ["SW", "PO", "MU"]
pass3 (Div a b) = pass3 a ++ ["PU"] ++ pass3 b ++ ["SW", "PO", "DI"]
