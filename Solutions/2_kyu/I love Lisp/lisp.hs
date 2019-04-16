module LispLovesMe where

import Data.Char
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

digit :: Parser Char
digit = sat isDigit

spaces :: Parser ()
--space = sat isSpace ?
spaces = do many $ oneOf " ,\r\n\t"
            return ()

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

token :: Parser a -> Parser a
token p = do spaces
             a <- p
             spaces
             return a

parenthesis :: Parser a -> Parser a 
parenthesis p = do stringT "("
                   ret <- p
                   stringT ")"
                   return ret

num :: Parser Int
num = do x <- string "-" <|> return [] 
         xs <- some digit
         return $ read $ x ++ xs



stringT = token . string
numT = token num
--------------------------------------------------------------------------------
-- lisp
int :: Parser AST
int = do n <- numT
         return $ I32 n

sym :: Parser AST
sym = do x  <- noneOf $ " ,\n\t\r()" ++ ['0' .. '9']
         xs <- many $ noneOf "() ,\n\t\r"
         spaces
         return $ Sym $ x:xs

nul :: Parser AST
nul = do stringT "null"
         return Nul


boo :: Parser AST
boo = true <|> false

true  = stringT "true"  >>= \_ -> return $ Boo True
false = stringT "false" >>= \_ -> return $ Boo False

nod :: Parser AST
nod = do ns <- parenthesis $ many expr
         return $ case ns of
           [] -> Nul
           (e:e_) -> Nod e e_


expr :: Parser AST
expr = token $ int <|> boo <|> nul <|> sym <|> nod
--------------------------------------------------------------------------------

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)
--



preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", f_arithmetic (+))
  , ("*", f_arithmetic (*))
  , ("-", f_arithmetic (-))
  , ("/", f_arithmetic (/))
  , ("^", undefined)
  , (">", f_bb (>))
  , ("<", undefined)
  , ("!", undefined)
  , ("list", undefined)
  , ("size", undefined)
  , ("reverse", undefined)
  , ("..", undefined)
  , ("==", undefined)
  , (">=", undefined)
  , ("<=", undefined)
  , ("!=", undefined)
  , ("if", undefined)
  ]

check :: Bool -> AST -> AST
check f1 f2 = if f1 then f2 else Err
-------------------------------------------
-- +-*/
f_arithmetic op =  \l -> check (legal_art l) (arithmetic op) 

arithmetic op l = I32 . foldl1 op . (toInt <$>)
  where toInt (I32 n) = n

legal_art [] = True
legal_art (I32 _ : xs) = legal_art xs
-------------------------------------------
-- binary bool
f_bb op = 

-------------------------------------------
--






--"(do )" ->> Err
--"(begin ())" ->> Err
--"(shit)" ->> Err
--"(+ true 1)" ->> Err
--"(* 2 3 4 5 6 false)" ->> Err
"(if () ())" ->> Err
"(if 1 2)" ->> Err
"(! 2 3)" ->> Err
"(!=)" ->> Err
---------------------------
parseCode :: Parser a -> String -> Maybe a
parseCode p s = case parse p s of 
                  [(v, [])] -> Just v
                  _  -> Nothing 

lispParse :: String -> Maybe AST
lispParse = parseCode expr

lispPretty :: String -> Maybe String
lispPretty s = pretty <$> lispParse s

pretty :: AST -> String
pretty (Boo True) = "true"
pretty (Boo False) = "false"
pretty Nul = "null"
pretty (Sym s) = s
pretty (I32 n) = show n
pretty (Err) = "error"
pretty (Lst l) = concat ((' ':) . helper <$> l)
pretty (Nod e e_) = "(" ++ helper e ++ concat ((' ':) . helper <$> e_) ++ ")"

lispEval :: String -> Maybe AST
lispEval s = eval <$> lispParse s

eval :: AST -> AST
eval (Nod e e_) = case e of
  (Sym s) -> invoke s e_
  -- (Nod expr expr_) -> undefined
  n -> n
eval n = n

invoke :: String -> [AST] -> AST
invoke s l = case lookup s preludeFunctions of
               (Just f) -> f $ eval <$> l
               Nothing -> Err
