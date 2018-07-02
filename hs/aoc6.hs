import System.IO

main :: IO()
main = do
    handle <- openFile "aoc6_input" ReadMode
    contents <- hGetContents handle
    let mem = map (read::String->Int) (words contents)
    let max_idx = findMaxBank mem (-1) 0 0
    print max_idx

findMaxBank :: [Int] -> Int -> Int -> Int -> Int
findMaxBank (mem:[]) cur_max max_idx cur_idx
    | cur_max >= mem = max_idx
    | otherwise = cur_idx
findMaxBank (mem:mems) cur_max max_idx cur_idx
    | mem > cur_max = findMaxBank mems mem cur_idx (cur_idx+1)
    | otherwise = findMaxBank mems cur_max max_idx (cur_idx + 1)

createRedistList :: [Int] -> Int -> Int -> Int -> [Int]
createRedistList banks blocks idx len
    | blocks == 0 = banks
    | d > 0 = zipWith (+) (replicate len d) (createRedistList banks (blocks - d * len) idx len)
    | otherwise = createRedistList banks' (blocks - 1) ((idx + 1) `rem` len) len
        where d = blocks `div` len
              banks' = incrementNth idx banks

incrementNth :: Int -> [Int] -> [Int]
incrementNth n (x:xs)
    | n == 0 = (x+1):xs
    | otherwise = x:incrementNth (n-1) xs

redistributeBlocks :: [Int] -> Int -> [Int]
redistributeBlocks mem idx = 
