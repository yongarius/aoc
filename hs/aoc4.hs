import System.IO

main :: IO()
main = do
    handle <- openFile "aoc4_input" ReadMode
    contents <- hGetContents handle
    let ls = lines contents
    let num = checkValidPassPhrasesNum ls
    print num

checkValidPassPhrasesNum :: [String] -> Int
checkValidPassPhrasesNum (l:[])
    | isPassPhraseValid l = 1
    | otherwise = 0
checkValidPassPhrasesNum (l:ls)
    | isPassPhraseValid l = 1 + checkValidPassPhrasesNum ls
    | otherwise = checkValidPassPhrasesNum ls

isPassPhraseValid :: String -> Bool
isPassPhraseValid str = not $ existDuplicateWords ws
                        where
                            ws = words str


existDuplicateWords :: [String] -> Bool
existDuplicateWords (w:[]) = False
existDuplicateWords (w:ws)
    | elem w ws = True
    | otherwise = existDuplicateWords ws
