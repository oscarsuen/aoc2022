import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main :: IO ()
main =
    do input <- getinput
       let x = parse input
       let r1 = solve1 x
       print $ fromJust r1
       let r2 = solve2 x
       print $ fromJust r2

getinput :: IO String
getinput = do
    args <- getArgs
    let filename = case args of
                     [] -> "input.txt"
                     _  -> head args
    readFile filename

parse :: String -> String
parse = dropWhileEnd isSpace

unique :: [Char] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

marker :: Int -> [Char] -> Maybe Int
marker = marker' 0

marker' :: Int -> Int -> [Char] -> Maybe Int
marker' i n [] = Nothing
marker' i n xs = case (unique $ take n xs) of
                    True -> Just (i+n)
                    False -> marker' (i+1) n $ tail xs


solve1 :: String -> Maybe Int
solve1 = marker 4

solve2 :: String -> Maybe Int
solve2 = marker 14
