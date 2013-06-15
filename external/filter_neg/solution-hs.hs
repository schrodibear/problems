import Data.List

solution = (\l -> (length l, l)) . map fst . filter ((< 0) . snd) . zip [1..]

run lst = putStrLn $ "[" ++ (intercalate "; " $ map show $ snd $ solution lst) ++ "]"

lst = [-1, -1, 2, 3, -4, -5, -6, 1]

main = run lst
