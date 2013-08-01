isPalindrome :: Int -> Bool
isPalindrome x =   s == reverse s
    where s = show x

main :: IO ()
main = do
    putStrLn . show $ maximum [(a*b,a,b) | a <- [1..999], b <- [a..999], isPalindrome (a*b)]

