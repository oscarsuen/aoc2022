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

parse :: String -> (Crate, [Move])
parse = (\s -> (parse1 (s!!0), map parse2 (s!!1))) . split "" . lines . dropWhileEnd isSpace

parse1 :: [String] -> Crate
parse1 = map (init . dropWhile isSpace) . filter (isDigit . last) . transpose

parse2 :: String -> Move
parse2 = (\w -> (read (w!!1), (read (w!!3), read (w!!5)))). words

split :: Eq a => a -> [a] -> [[a]]
split m [] = []
split m xs = case (break (==m) xs) of
                (f, []) -> [f]
                (f, s)  -> [f] ++ split m (tail s)

type Crate = [[Char]]
type Move = (Int, (Int, Int))

movecrates1 :: Crate -> Move -> Crate
movecrates1 c (n, (f, t)) | f < t = (take (f-1) c) ++ [f'] ++ (drop f $ take (t-1) c) ++ [t'] ++ (drop t c)
                          | t < f = (take (t-1) c) ++ [t'] ++ (drop t $ take (f-1) c) ++ [f'] ++ (drop f c)
    where f' = drop n (c!!(f-1))
          t' = (reverse $ take n (c!!(f-1))) ++ (c!!(t-1))

gettops :: Crate -> String
gettops = map head

solve1 :: (Crate, [Move]) -> String
solve1 = gettops . (uncurry $ foldl movecrates1)

movecrates2 :: Crate -> Move -> Crate
movecrates2 c (n, (f, t)) | f < t = (take (f-1) c) ++ [f'] ++ (drop f $ take (t-1) c) ++ [t'] ++ (drop t c)
                          | t < f = (take (t-1) c) ++ [t'] ++ (drop t $ take (f-1) c) ++ [f'] ++ (drop f c)
    where f' = drop n (c!!(f-1))
          t' = (take n (c!!(f-1))) ++ (c!!(t-1))

solve2 :: (Crate, [Move]) -> String
solve2 = gettops . (uncurry $ foldl movecrates2)
