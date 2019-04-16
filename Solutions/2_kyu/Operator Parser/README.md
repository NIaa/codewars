<https://preview.codewars.com/kata/operator-parser/train/haskell> <br> <br>
really enjoyed this one  
```haskell
arithOps :: [Associativity [ReadP String]]
arithOps = 
    map (fmap (map (\s -> op s s) . words)) 
        [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

arithParser :: ReadP (OpTree String String) 
arithParser = parseOperators arithOps (munch1 isDigit) <* eof

brackets :: OpTree String String -> String
brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")
```