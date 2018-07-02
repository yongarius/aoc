

calcDist :: Int -> Int
calcDist num = 2 * n - (calcDistDiff num n) where
                n = takeShellNumber num
                calcDistDiff x y
                    | y == 0 = 0
                    | diff > y = diff - y
                    | otherwise = diff
                    where diff = ((2 * y + 1) * (2 * y + 1) - x) `rem` (2 * y)

takeShellNumber :: Int -> Int
takeShellNumber num = (head (dropWhile (compareShellNumber num) [0,1..])) where
                        compareShellNumber x y
                            | x > ((2 * y + 1) * (2 * y + 1)) = True
                            | otherwise = False
