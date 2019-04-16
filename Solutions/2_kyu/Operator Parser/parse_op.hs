{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    ) 
where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Functor
import Control.Applicative hiding (many)

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree f tree = case tree of
  Term b -> b
  Op t1 a t2 -> f a (foldTree f t1) (foldTree f t2)

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s a = (string s) $> a

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree. 

-- parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
-- parseOperators pss t = foldr oneAssoc term pss
--  where term = termP t <|> withPara (foldr oneAssoc term pss)
       --re = 

parseOperators [] t = termP t <|> withPara (parseOperators [] t)
parseOperators (ps:pss) t = oneAssoc ps (parseOperators pss term) where
   term = termP t <|> withPara (foldr oneAssoc term pss)
{-

foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

-}

oneAssoc :: Associativity [ReadP a] -> ReadP (OpTree a b) -> ReadP (OpTree a b)
oneAssoc (L               ops) t = chainl1 t $ foldr1 (<|>) $ opP <$> ops
oneAssoc (R               ops) t = chainr1 t $ foldr1 (<|>) $ opP <$> ops
oneAssoc (NoAssociativity ops) t = chainr1 t $ foldr1 (<|>) $ opP <$> ops

-- utilties
spaces = many $ char ' ' <|> char '\n'
withPara p = char '(' *> spaces *> p <* spaces <* char ')'
termP p = Term <$> p <* spaces
opP p = (\a t1 t2 -> Op t1 a t2) <$> p <* spaces
----------------------------------------------------------------------
-- local test
arithOps :: [Associativity [ReadP String]]
arithOps = 
    map (fmap (map (\s -> op s s) . words)) 
        [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

arithParser :: ReadP (OpTree String String) 
arithParser = parseOperators arithOps (munch1 isDigit) <* eof

brackets :: OpTree String String -> String
brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")