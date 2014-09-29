{-# OPTIONS -Wall -Werror #-}

source :: [Int]
source = [32,6,23,85,26,51]

search :: Int -> [Int] -> Maybe Int
search x ys = foldl (\acc n -> if n == x then index x ys  else acc) Nothing ys

index :: Int -> [Int] -> Maybe Int
index _ [] = Nothing
index x ys = if x == last ys then Just $ length ys - 1 else index x $ init ys

main :: IO ()
main = print $ search 5 source
