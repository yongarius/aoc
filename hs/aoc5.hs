import System.IO


main :: IO()
main = do
    handle <- openFile "aoc5_input" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let ns = map (read::String->Int) ls
    let steps = calcJumpSteps (ns, 0)
    print steps

calcJumpSteps :: ([Int], Int) -> Int
calcJumpSteps (ns, cur)
    | next >= length ns' = 1
    | next < 0 = 1
    | otherwise = 1 + calcJumpSteps (ns', next)
        where (ns', next) = jumpToNext (ns, cur)

jumpToNext :: ([Int], Int) -> ([Int], Int)
jumpToNext (ns, cur) = (incrementNth cur ns, (cur + ns!!cur))

incrementNth :: Int -> [Int] -> [Int]
incrementNth n (x:xs)
    | n == 0 = (x+1):xs
    | otherwise = x:incrementNth (n-1) xs
