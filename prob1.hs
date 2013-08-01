
isValid :: Int -> Int
isValid x
    | x `rem` 3 == 0   = x
    | x `rem` 5 == 0   = x
    | otherwise        = 0


sum35 :: Int -> Int
sum35 x | x < 3             = 0
        | x `rem` 3 == 0    = x + (sum35 (x-1))
        | x `rem` 5 == 0    = x + (sum35 (x-1))
        |otherwise          = (sum35 (x-1))



main :: IO ()
main = do
    putStrLn $ show $ sum [isValid(x) | x <- [1..999]]
    putStrLn $ show $ sum35 999
    putStrLn $ show $ sum [x | x <- [1..999], (x `rem` 3 == 0) || (x `rem` 5 == 0)]
