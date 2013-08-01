factors :: Int -> [Int]
factors x = factors' x 3 []
    where
        factors' x a f
            | even x            = factors' (x `div` 2) a (2:f)
            | a * a > x         = x:f
            | x `rem` a == 0    = factors' (x `div` a) a (a:f)
            | otherwise         = factors' x (a+2) f

main :: IO ()
main = do
    putStrLn . show . maximum $ factors 600851475143
