import Data.List

exMatrix :: [[Int]]
exMatrix = [[1,2,3],
            [4,5,6],
            [7,8,9]]

checkElem :: [[Int]] -> Int -> Int -> Int
checkElem m r c = (m !! r) !! c

flipRows :: [[Int]] -> Int -> Int -> [[Int]] {-row1 has to be less than row2-}
flipRows m row1 row2 = take row1 m ++ [(m !! row2)] ++ (take (row2-(row1+1)) (drop (row1 + 1) m)) ++
                       [(m !! row1)] ++ (drop (row2 + 1) m)

mulRow :: [[Int]] -> Int -> Int -> [[Int]]
mulRow m i k = take (i) m ++ [(map (*k) (m !! i))] ++ drop (i+1) m

mulAddRow :: [[Int]] -> Int -> Int -> Int -> [[Int]]
mulAddRow m i1 i2 k = take (i2) m ++
                      [(zipWith (+) ((mulRow m i1 k) !! i1) (m !! i2))] ++ {-i2-}
                      drop (i2+1) m



{-flipRowsLite :: [Int] -> Int -> Int -> [Int]
flipRowsLite l i1 i2 = take i1 l ++ [(l !! i2)] ++ (take (i2-(i1+1)) (drop (i1 + 1) l)) ++
                      [(l !! i1)] ++ (drop (i2 + 1) l)
-}
