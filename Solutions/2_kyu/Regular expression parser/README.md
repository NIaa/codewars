<https://preview.codewars.com/kata/regular-expression-parser/train/haskell> <br> <br>
### An example using STL by AJFarmar
```haskell
module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative ((<$>),(<*>),(<*),(*>))

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any character
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)
  
parseRegExp :: String -> Maybe RegExp
parseRegExp s = helper $ readP_to_S regExp s
  where
    helper rs = case [a | (a,"") <- rs] of
      []    -> Nothing
      (a:_) -> Just a

regExp :: ReadP RegExp
regExp = strExp +++ (Or <$> (strExp <* char '|') <*> strExp)

strExp :: ReadP RegExp
strExp = charExp
     +++ (Str <$> many1 charExp)

charExp :: ReadP RegExp
charExp = atom +++ (ZeroOrMore <$> atom <* char '*')

atom :: ReadP RegExp
atom = (Normal <$> satisfy (not . flip elem "()*|."))
   +++ (char '.' *> return Any)
   +++ (char '(' *> regExp <* char ')')
```

### An example using Parsec  
```haskell
module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.ParserCombinators.Parsec

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)



parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse startP "myRE" s of
  (Right x) -> Just x
  (Left  _) -> Nothing

startP :: Parser RegExp
startP = try (nonOrP <* eof)
      <|> orP   <* eof

expP :: Parser RegExp
expP = try orP <|> try nonOrP

normalP :: Parser RegExp
normalP = Normal <$> noneOf "()*|."

anyP :: Parser RegExp
anyP = Any <$ string "."

charP :: Parser RegExp
charP = anyP <|> normalP

unitP :: Parser RegExp -> Parser RegExp
unitP tp = try (ZeroOrMore <$> tp <* char '*')
    <|> tp

tokenP :: Parser RegExp
tokenP = try charP <|> inBracketsP

inBracketsP :: Parser RegExp
inBracketsP = char '(' *> expP <* char ')'

orP :: Parser RegExp
orP = do
  op1 <- nonOrP
  char '|'
  op2 <- nonOrP
  return $ Or op1 op2
  
-- TODO: if only one.
nonOrP :: Parser RegExp
nonOrP = try . fmap g . many1 $ unitP tokenP where
  g [x] = x
  g xs  = Str xs
```