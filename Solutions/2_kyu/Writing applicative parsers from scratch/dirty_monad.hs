module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

pa >>>= apb = P $ \inp -> case unP pa inp of
                           [] -> []
                           [(s, a)] -> unP (apb a) s 
p >>> q = p >>>= \_ -> q

p <|> q = P $ \inp -> case unP p inp of
                        [(v, s)] -> [(v, s)]
                        [] -> unP q inp

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \inp -> case unP p inp of
                              [] -> []
                              [(s, v)] -> [(s, f v)]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
a <# p = const a <#> p

infixl 4 <#>
infixl 4 <#



-- | Parse a character only when a predicate matches.
item :: Parser Char
item = P $ \inp -> case inp of
                     [] -> []
                     (x:xs) ->  [(xs, x)]
                     

predP :: (Char -> Bool) -> Parser Char
predP p = item >>>= \x -> if p x then inject x else emptyP

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \inp -> [(inp, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \inp -> case unP pf inp of
    [] -> []
    [(s, f)] -> unP (pmap f px) s

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = (const <#> pa) <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = (flip const <#> pa) <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = inject []
stringP (x:xs) = charP x >>> stringP xs >>> inject (x:xs)

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []


-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \inp -> unP p inp ++ unP q inp

infixl 3 <<>>

some x = inject (:) <@> x <@> many x
many x = some x <|> inject []

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = case unP p cs of
                   l -> [v | (s, v) <- l, s == []]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
                         [a] -> Just a
                         _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)


evalExpr :: Expr -> Int
evalExpr (ConstE n) = n
evalExpr (NegE e) = 0 - evalExpr e
evalExpr ZeroE = 0
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
expr_ :: Parser Expr
expr_ = const_ <|> binOpExpr_ <|> neg_ <|> zero_

const_ :: Parser Expr
const_ = int_

binOpExpr_ :: Parser Expr
binOpExpr_ =  charP '(' >>> expr_ >>>= \e1 -> charP ' ' >>> binOp_ >>>= \op ->
                charP ' ' >>>  expr_ >>>= \e2 -> charP ')' >>>= \_ -> inject $ BinOpE op e1 e2

binOp_ :: Parser BinOp
binOp_ = (charP '+' >>> inject AddBO) <|> (charP '*' >>> inject MulBO)


neg_ :: Parser Expr
neg_ = charP '-' >>>
          expr_ >>>= \e ->
          inject $ NegE e 

zero_ :: Parser Expr
zero_ = charP 'z' >>> inject ZeroE

int_ :: Parser Expr
int_ = some digit_ >>>= \ns ->
          inject $ ConstE $ read ns
          
digit_ :: Parser Char
digit_ = predP isDigit

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr_