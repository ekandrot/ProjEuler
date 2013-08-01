import Data.List (sort)

factors :: Int -> [Int]
factors x = factor2s x []
    where
        factors' x a f
            | x == 1            = f
            | a * a > x         = x:f
            | x `rem` a == 0    = factors' (x `div` a) a (a:f)
            | otherwise         = factors' x (a+2) f
        factor2s x f
            | even x        = factor2s (x `div` 2) (2:f)
            | otherwise     = factors' x 3 f

---
--- [2,2,2,3,3,5,7,7,11] -> [(2,3),(3,2),(5,1),(7,2),(11,1)]
---
group :: [Int] -> [(Int, Int)]
group (x:xs) = countHead xs x 1 
    where
        countHead [] m c = [(m, c)]
        countHead (x:xs) m c
            | x == m    = countHead xs m (c+1)
            | otherwise = (m, c) : countHead xs x 1

---
--- returns a list of the largest power of each factor
--- turns [(2,1),(2,1)(2,2),(2,3),(2,4),(3,1),(3,1),(3,2),(3,2),(5,1),(5,1),(5,1),(7,1),(7,1),(11,1),(13,1),(17,1),(19,1)]
--- into [(2,4),(3,2),(5,1),(7,1),(11,1),(13,1),(17,1),(19,1)]
---
compress :: [(Int, Int)] -> [(Int, Int)]
compress (x:xs) = compress' xs (fst x) (snd x)
    where
        compress' [] m c = [(m,c)]
        compress' (x:xs) m c
            | (fst x) == m    = if snd x > c
                                    then compress' xs m (snd x)
                                    else compress' xs m c
            | otherwise     = (m, c) : compress' xs (fst x) (snd x)


multiplyOut :: [(Int, Int)] -> Int
multiplyOut xs = mult xs 1
    where
        mult [] t = t
        mult (x:xs) t = mult xs t * ( (fst x) ^ (snd x))


multiplyOut2 :: [(Int, Int)] -> Int
multiplyOut2 = foldl (\x y -> ( x * (fst y) ^ (snd y))) 1


main :: IO ()
main = do
    --putStrLn . show $ foldl lcm 1 [1..20]
    --putStrLn . show $ map factors [2..20]
    --putStrLn . show $ group [2,2,2,3,3,5,7,7,11]
    putStrLn . show $ multiplyOut2 $ compress $ sort $ concat $ map group $ map factors [2..20]
