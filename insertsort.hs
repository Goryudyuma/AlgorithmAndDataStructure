{-# OPTIONS -Wall -Werror #-}

source :: [Int]
source = [3,62,13,85,34,24]

insert :: [Int] -> [Int]
insert = foldl (\acc x -> sort x acc) []

sort :: Int -> [Int] -> [Int]
sort x [] = [x]
sort x ys = if x >= last ys then ys ++ [x] else sort x (init ys) ++ [last ys]

main :: IO ()
main = print $ insert source
