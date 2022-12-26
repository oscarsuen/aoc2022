import System.Environment
import Data.List
import Data.Char
import Data.Maybe

main :: IO ()
main =
    do input <- getinput
       let x1 = parse1 input
       let s2 = solve1 x1
       print $ fromJust s2
       let x2 = parse2 input
       let s2 = solve2 x2
       print $ fromJust s2

getinput :: IO String
getinput = do
    args <- getArgs
    let filename = case args of
                     [] -> "input.txt"
                     _  -> head args
    readFile filename

parse1 :: String -> [(BST Char, BST Char)]
parse1 = map (\(f,s) -> (bcreate f, bcreate s)) . map (\line -> splitAt (quot (length line) 2) line) . lines . dropWhileEnd isSpace

bcreate :: String -> BST Char
bcreate = foldr binsert Empty

parse2 :: String -> [[BST Char]]
parse2 = map (map bcreate) . splitevery 3 . lines . dropWhileEnd isSpace

split :: Eq a => a -> [a] -> [[a]]
split m [] = []
split m xs = case (break (==m) xs) of
                (f, []) -> [f]
                (f, s)  -> [f] ++ split m (tail s)

index :: Eq a => a -> [a] -> Int
index m xs = case (break (==m) xs) of
                (_, []) -> -1
                (f, _)  -> length f

splitevery :: Int -> [a] -> [[a]]
splitevery n = takeWhile (not . null) . unfoldr (Just. splitAt n)

mod' :: (Integral a) => a -> a -> a
mod' x y | r < 0 = y + r
         | otherwise = r
    where r = rem x y

data BST a = Node a (BST a) (BST a) | Empty
    deriving (Show)

instance Ord a => Eq (BST a) where
    (==) = bequal

bequal :: Ord a => BST a -> BST a -> Bool
bequal a b = bsubset a b && bsubset b a

bsubset :: Ord a => BST a -> BST a -> Bool
bsubset Empty b = True
bsubset (Node n l r) b = bsearch n b && bsubset l b && bsubset r b

instance Ord a => Semigroup (BST a) where
    (<>) = bmerge

instance Ord a => Monoid (BST a) where
    mempty = Empty

instance Foldable BST where
    foldMap = bfoldmap
    -- foldr = bfoldr

bfoldmap :: Monoid m => (a -> m) -> BST a -> m
bfoldmap f Empty = mempty
bfoldmap f (Node n r l) = (f n) <> bfoldmap f l <> bfoldmap f r

{-
bfoldr :: (a -> b -> b) -> b -> BST a -> b
bfoldr f z = foldr f z . btolist

bfoldr' :: [BST a] -> (a -> b -> b) -> b -> BST a -> b
bfoldr' [] f z Empty = z
bfoldr' (x:xs) f z Empty = bfoldr' xs f z x
bfoldr' c f z (Node n l r) = f n $ bfoldr' (r:c) f z l
-}

{- instance Functor BST where
    fmap = bmap -}

-- Does not preserve BST invariant
{- bmap :: (a -> b) -> BST a -> BST b
bmap f Empty = Empty
bmap f (Node n l r) = Node (f n) (bmap f l) (bmap f r) -}
bmap :: (Ord a, Ord b) => (a -> b) -> BST a -> BST b
bmap f Empty = Empty
bmap f (Node n l r) = binsert (f n) $ bmerge (bmap f l) (bmap f r)

bsearch :: Ord a => a -> BST a -> Bool
bsearch m Empty = False
bsearch m (Node n left right) | m == n = True
                             | m < n  = bsearch m left
                             | m > n  = bsearch m right

binsert :: Ord a => a -> BST a -> BST a
binsert m Empty = Node m Empty Empty
binsert m (Node n left right) | m == n = Node n left right
                              | m < n  = Node n (binsert m left) right
                              | m > n  = Node n left (binsert m right)

bmerge :: Ord a => BST a -> BST a -> BST a
bmerge Empty b = b
bmerge (Node n l r) b = binsert n $ bmerge l $ bmerge r b

{-
bintersectall :: Ord a => [BST a] -> BST a
bintersectall = foldr bintersect Empty
-}

bintersect :: Ord a => BST a -> BST a -> BST a
bintersect Empty b = Empty
bintersect (Node n l r) b | s = binsert n m
                          | otherwise = m
    where s = bsearch n b
          m = bmerge (bintersect l b) (bintersect r b)

{-
bintersect :: Ord a => BST a -> BST a -> [a]
bintersect Empty b = []
bintersect (Node n l r) b | s  = [n] ++ bintersect l b ++ bintersect r b
                          | otherwise = bintersect l b ++ bintersect r b
    where s = bsearch n b
-}

btolist :: BST a -> [a]
btolist Empty = []
btolist (Node n left right) = [n] ++ btolist left ++ btolist right

bextract :: BST a -> Maybe a
bextract (Node n Empty Empty) = Just n
bestract _ = Nothing

points :: Maybe Char -> Maybe Int
points Nothing = Nothing
points (Just c) | isLower c = Just $ ord c - ord 'a' + 1
                | isUpper c = Just $ ord c - ord 'A' + 27
                | otherwise = Nothing

solve1 :: [(BST Char, BST Char)] -> Maybe Int
solve1 = fmap sum . sequence . map (points . bextract) . map (uncurry bintersect)

solve2 :: [[BST Char]] -> Maybe Int
solve2 = fmap sum . sequence . map (points . bextract) . map (foldl1 bintersect)
