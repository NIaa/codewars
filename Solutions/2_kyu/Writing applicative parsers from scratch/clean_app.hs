module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- ok this time from nothing

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \inp -> [(s, f x) | (s, x) <- unP p inp]


-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
a <# p = const a <#> p

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \inp -> case inp of
                [] -> []
                (x:xs) -> if p x then [(xs, x)] else []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject a = P $ \inp -> [(inp, a)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \inp -> concat [unP (f <#> px) s | (s, f) <- unP pf inp]

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = flip const <#> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = inject []
stringP (x:xs) = (:) <#> charP x <@> stringP xs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \inp -> unP p inp ++ unP q inp

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = inject [] <<>> some p
-- use some

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p
-- use many


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = [a | (s, a) <- unP p cs, null s]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of 
                         [a] -> Just a
                         _   -> Nothing

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

expr_ = const_ <<>> binOpExpr_ <<>> neg_ <<>> zero_
const_ = ConstE . read <#> some digit_
binOpExpr_ = flip BinOpE <#> (charP '(' @> expr_ <@ charP ' ') <@> binOp_ <@> (charP ' ' @> expr_ <@ charP ')')
binOp_ = (AddBO <# charP '+') <<>> (MulBO <# charP '*')
neg_ = NegE <#> (charP '-' @> expr_)
zero_ = ZeroE <# charP 'z'
digit_ = predP isDigit

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr_