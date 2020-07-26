module Main where

import Log
import qualified Data.Set as Set

main :: IO ()
main = putStrLn "Hello, Haskell!"

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = [0]
toDigitsRev i = toDigitsRevRec i

toDigitsRevRec :: Integer -> [Integer]
toDigitsRevRec 0 = []
toDigitsRevRec i = let car = i `mod` 10 in
             let dividend = i `div` 10 in
             car:toDigitsRevRec dividend

toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits i = toDigitsRec i

toDigitsRec :: Integer -> [Integer]
toDigitsRec 0 = []
toDigitsRec i = let car = i `mod` 10 in
             let dividend = i `div` 10 in
             (toDigitsRec dividend) ++ [car]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther vec = let fr = \c -> \(even, acc) -> if even then (not even, (2*c):acc) else (not even, c:acc) in
                        snd $ foldr fr (False, []) vec

sumDigits :: [Integer] -> Integer
sumDigits digs = sum $ digs >>= toDigits

validate :: Integer -> Bool
validate x = (== 0) $ flip mod 10 $ sumDigits $ doubleEveryOther $ toDigits x

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b
            ++ hanoi 1 a b c
            ++ hanoi (n - 1) c b a

-- e2 

parseMessage :: String -> LogMessage
parseMessage s = case words s of
    "I" : timeStamp : rest -> LogMessage Info (read timeStamp) (unwords rest)
    "W" : timeStamp : rest -> LogMessage Warning (read timeStamp) (unwords rest)
    "E" : ecode : timeStamp : rest -> LogMessage (Error (read ecode)) (read timeStamp) (unwords rest)
    _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node mtl m'@(LogMessage _ ts' _) mtr) = if ts < ts' 
    then Node (insert m mtl) m' mtr
    else Node mtl m' (insert m mtr)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mtl m mtr) = (inOrder mtl) ++ m:(inOrder mtr)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map (\(LogMessage _ _ s) -> s) $ filter severeFilter $ inOrder $ build ms

severeFilter :: LogMessage -> Bool
severeFilter (LogMessage (Error ecode) _ _) = if ecode >= 50 then True else False 
severeFilter _ = False

--e3
everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (c:car) = c:(everyNth n $ drop (n-1) car)

skips :: [a] -> [[a]]
skips l = map (\n -> everyNth n l) [1..length l]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest) = if a < b && c < b 
    then b:(localMaxima (c:rest))
    else localMaxima (c:rest)
localMaxima _ = []

-- e3.3 is a bit tedious, so skipping
-- e4

fun1 :: [Integer] -> Integer
fun1 = foldl (\acc -> \x -> (x-2) * acc) 1 . filter even

-- e4.1.b is a bit silly, the point being (I think) to use iterate and takeWhile... skipping

data Trees a = Leafs
        | Nodes Integer (Trees a) a (Trees a)
        deriving (Show, Eq)

balancedInsert :: a -> Trees a -> Trees a
balancedInsert c Leafs = Nodes 0 Leafs c Leafs
balancedInsert c (Nodes h Leafs v Leafs) = Nodes (h+1) (Nodes h Leafs c Leafs) v Leafs 
balancedInsert c (Nodes h Leafs v tr) = Nodes h (Nodes (h+1) Leafs c Leafs) v tr
balancedInsert c (Nodes h tl v Leafs) = Nodes h tl v (Nodes (h+1) Leafs c Leafs)
balancedInsert c (Nodes h tl@(Nodes tlh _ _ _) v tr@(Nodes trh _ _ _)) = if tlh < trh
    then Nodes h (balancedInsert c tl) v tr
    else Nodes h tl v (balancedInsert c tr)
-- this is a bit tedious to get the heights correct... skipping (also fold is trivial from there)

xor :: [Bool] -> Bool
xor = odd . foldl (\numtrue -> \c -> if c then numtrue + 1 else numtrue) 0

-- e4.3.2 is silly, skipping

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

getBads :: Integer -> Set.Set Integer
getBads n = Set.fromList $ filter (\x -> x <= n) $ map (\(i, j) -> i + j + 2*i*j) $ [1..n `div` 2] >>= (\x -> zip [1..x] (cycle [x]))

sieveSundaram :: Integer -> [Integer]
sieveSundaram i = Set.elems $ Set.map (\x -> 2*x + 1)  $ Set.fromList [1..i] Set.\\ (getBads i) 
