module Haskell.Codewars.Skyscrapers where
import Data.List(permutations)
import Data.Set(fromList, size)

type Puzzle = [[Int]]
type Clues = [[Int]]

-- row !! col ??
(??) :: Puzzle -> Int -> [Int]
p ?? i = map (!! i) p

inc :: [Int] -> [Int]
inc [] = []
inc [x] = [x]
inc (x0:x1:xs)  | x0 < x1 = x0 : inc (x1:xs)
                | otherwise = inc (x0:xs)

see :: [Int] -> Int
see = length . inc

puzzles :: [Puzzle]
puzzles = [[x, y, z, w] | x <- prm, y <- prm, z <- prm, w <- prm] where
    prm = permutations [1..4]    

colSizes :: Puzzle -> [Int]
colSizes p = map (size.fromList) (map (p ??) [0..3])

valid :: Puzzle -> Bool
valid p = (colSizes p) == [4,4,4,4]

getClues :: Puzzle -> Clues
getClues p = [f c0_3, f c4_7, f c8_11, f c12_15] where
    f = map see
    c0_3   = map (p ??) [0..3]
    c4_7   = map (reverse . (p !!)) [0..3] 
    c8_11  = map (reverse . (p ??)) [3,2..0]
    c12_15 = map (p !!) [3,2..0] 

eq :: Clues -> Clues -> Bool
eq c1 c2 = and [(c1!!i)!!j == 0 || (c2!!i)!!j == 0 || (c1!!i)!!j == (c2!!i)!!j | i <- [0..3], j <- [0..3]]

solve :: Clues -> Puzzle
solve c | null clues = []
        | otherwise = clues !! 0
          where clues = filter (\p -> (valid p && (getClues p `eq` c)) ) puzzles 
    
        