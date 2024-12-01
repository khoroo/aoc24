module Main where
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()
main = do
    input <- lines <$> getContents
    let nums = map (map read . words) input :: [[Int]]
        xs = map head nums
        ys = map last nums
    print $ solve1 xs ys
    print $ solve2 xs ys


solve1 :: (Ord a, Num a) => [a] -> [a] -> a
solve1 xs ys 
    | length xs == length ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)
    | otherwise = error "Lists must be of equal length"

frequenciesMap :: [Int] -> Map.Map Int Int
frequenciesMap = Map.fromListWith (+) . map (\x -> (x, 1))

solve2 :: [Int] -> [Int] -> Int
solve2 xs ys = 
    let freqs = frequenciesMap ys
    in sum [x * Map.findWithDefault 0 x freqs | x <- xs]
