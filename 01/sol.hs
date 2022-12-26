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

parse :: String -> [[Int]]
parse = map (map read) . split "" . lines . dropWhileEnd isSpace

split :: (Eq a) => a -> [a] -> [[a]]
split = splithelper []

splithelper :: (Eq a) => [a] -> a -> [a] -> [[a]]
splithelper c m []                  = [c]
splithelper c m (x:xs) | m == x     = [c] ++ splithelper [] x xs
                       | otherwise  = splithelper (c ++ [x]) m xs

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum
