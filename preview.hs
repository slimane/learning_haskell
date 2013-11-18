pas [] = [1]
pas fs = zipWith (+) (0:fs) (fs++[0])

run n = scanl (\x y -> pas x) [] [1..n]

toS = foldl (\x y->if odd y then x++"@" else x++" ") ""

asc = map toS

main = putStrLn $ unlines $ asc $ run 64

