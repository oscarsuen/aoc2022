import System.Environment
import Data.List
import Data.Char

main :: IO ()
main =
    do input <- getinput
       let x = parse input
       print $ solve1 x
       print $ solve2 x

getinput :: IO String
getinput = do
    args <- getArgs
    let filename = case args of
                     [] -> "input.txt"
                     _  -> head args
    readFile filename

parse :: String -> [(Char, Char)]
parse = map (\l -> (l!!0, l!!2)) . lines . dropWhileEnd isSpace

split :: (Eq a) => a -> [a] -> [[a]]
split = splithelper []

splithelper :: (Eq a) => [a] -> a -> [a] -> [[a]]
splithelper c m []                  = [c]
splithelper c m (x:xs) | m == x     = [c] ++ splithelper [] x xs
                       | otherwise  = splithelper (c ++ [x]) m xs

index :: (Eq a) => a -> [a] -> Integer
index = indexhelper 0

indexhelper :: (Eq a) => Integer -> a -> [a] -> Integer
indexhelper _ _ []                  = -1
indexhelper n m (x:xs) | m == x     = n
                       | otherwise  = indexhelper (n+1) m xs

{- def func(c1, c2):
    i1 = l1.index(c1)
    i2 = l2.index(c2)
    return (i2+1) + (((((i2-i1)%3)+1)*3)%9) -}

func1 :: (Char, Char) -> Integer
func1 (x, y) = (j+1) + (mod' ((*) 3 $ (+) 1 $ mod' (j-i) 3) 9)
    where i = index x ['A', 'B', 'C']
          j = index y ['X', 'Y', 'Z']

mod' :: (Integral a) => a -> a -> a
-- mod' :: Integer -> Integer -> Integer
mod' x y | r < 0 = y + r
         | otherwise = r
    where r = rem x y

solve1 :: [(Char, Char)] -> Integer
solve1 = sum . map func1

{- def func(c1, c2):
    i1 = l1.index(c1)
    i2 = l2.index(c2)
    return (i2*3) + ((((i2-1)+i1)%3)+1) -}

func2 :: (Char, Char) -> Integer
func2 (x, y) = (j*3) + ((+) 1 $ mod' ((j-1)+i) 3)
    where i = index x ['A', 'B', 'C']
          j = index y ['X', 'Y', 'Z']

solve2 :: [(Char, Char)] -> Integer
solve2 = sum . map func2
