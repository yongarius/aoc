import System.IO

main :: IO()
main = do
        handle <- openFile "aoc2_input" ReadMode
        contents <- hGetContents handle
        let ls = lines contents
        print $ calcCheckSum ls

calcCheckSum :: [String] -> Int
calcCheckSum (x:[]) = calcCheckSumSingleLine (words x)
calcCheckSum (x:xs) = calcCheckSumSingleLine (words x) + calcCheckSum xs

calcCheckSumSingleLine :: [String] -> Int
calcCheckSumSingleLine xs = max_val - min_val where
                            (min_val, max_val) = calcMinMax xs (1000000, 0)

calcMinMax :: [String] -> (Int, Int) -> (Int, Int)
calcMinMax [] cur_out = cur_out
calcMinMax xs cur_out
    | val < (fst cur_out) && val > (snd cur_out) = calcMinMax (tail xs) (val, val)
    | val < (fst cur_out) = calcMinMax (tail xs) (val, snd cur_out)
    | val > (snd cur_out) = calcMinMax (tail xs) (fst cur_out, val)
    | otherwise = calcMinMax (tail xs) cur_out
        where val = read (head xs) :: Int
