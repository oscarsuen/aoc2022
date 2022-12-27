import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main :: IO ()
main =
    do input <- getinput
       let x = parse input
       let r1 = solve1 x
       print $ r1
       let r2 = solve2 x
       print $ r2

getinput :: IO String
getinput = do
    args <- getArgs
    let filename = case args of
                     [] -> "input.txt"
                     _  -> head args
    readFile filename

parse :: String -> [(Interval, Interval)]
parse = map (getfirst2 . map (uncurry Interval . getfirst2 . map read . split '-') . split ',') . lines . dropWhileEnd isSpace

getfirst2 :: [a] -> (a, a)
getfirst2 (x:y:_) = (x, y)

data Interval = Interval Int Int
    deriving (Eq, Show)

split :: Eq a => a -> [a] -> [[a]]
split m [] = []
split m xs = case (break (==m) xs) of
                (f, []) -> [f]
                (f, s)  -> [f] ++ split m (tail s)

mod' :: (Integral a) => a -> a -> a
mod' x y | r < 0 = y + r
         | otherwise = r
    where r = rem x y

contains :: Interval -> Interval -> Bool
contains (Interval a b) (Interval c d) = a <= c && d <= b

anycontains :: (Interval, Interval) -> Bool
anycontains (i, j) = contains i j || contains j i

overlaps :: Interval -> Interval -> Bool
overlaps (Interval a b) (Interval c d) = not (b < c || d < a)

count :: [Bool] -> Int
count = length . filter id

solve1 :: [(Interval, Interval)] -> Int
solve1 = count . map anycontains

solve2 :: [(Interval, Interval)] -> Int
solve2 = count . map (uncurry overlaps)
