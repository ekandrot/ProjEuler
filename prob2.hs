
fibsum limit = fibsum' 1 2 0
    where
        fibsum' a b x
            | b >= limit    = x
            | even b        = fibsum' b (a+b) (x+b)
            | otherwise     = fibsum' b (a+b) x


main :: IO ()
main = do
    putStrLn $ show $ fibsum 4000000

