module Gaussian where

import Data.List (elemIndices)
import System.Random (randoms, getStdGen, newStdGen)
import GHC.Conc ( numCapabilities, pseq )
import System.Environment (getArgs)
import GHC.Conc.Sync (par)

type Row = [Float]
type Matrix = [Row]
type Vector = [Float]
type Point = (Int, Int)


minusInf :: Float
minusInf = -100000000000

eps :: Float
eps = 0.00000001

subRows :: Row -> Row -> Row
subRows _ [] = []
subRows [] _ = []
subRows (x:xs) (y:ys) = (x - y) : subRows xs ys


multiplyRowNumber :: Row -> Float -> Row
multiplyRowNumber [] _ = []
multiplyRowNumber (x:xs) value = x*value : multiplyRowNumber xs value


valueToMul :: Int -> Row -> Row -> Float
valueToMul ind x y
    | (ind < length x) && (ind < length y) && ind >= 0  = x !! ind / y !! ind
    | otherwise                                         = 0


changeValue :: Vector -> Float -> Int -> Vector
changeValue [] _ _ = []
changeValue vec newValue ind
    | ind < length vec && ind >= 0  = take ind vec ++ [newValue] ++ drop (ind + 1) vec
    | otherwise                     = vec


customMax :: Row -> Float
customMax [] = minusInf
customMax [x] = minusInf
customMax (x:xs)
    | x > val   = x
    | otherwise = val
    where val = customMax xs


maxIndex :: Matrix -> [Int] -> Float -> Int -> Point -> Point
maxIndex [] _ _ _ ind = ind
maxIndex (x:xs) usedRows value rowId ind
    | elem rowId usedRows || value >= customMax x = maxIndex xs usedRows value (rowId + 1) ind
    | value < customMax x && customMax x /= 0 = maxIndex xs usedRows (customMax x) (rowId + 1) (rowId, newInd)
    | otherwise  = (-1, -1)
    where
        newInd = head $ elemIndices (customMax x) x


goAlongRows :: Matrix -> Int -> Row -> Point -> Bool -> Matrix
goAlongRows [] _ _ _ _ = []
goAlongRows (mat:mats) rowInd rowToSub maxInd@(maxRowInd, maxColInd) paral
    | paral && maxRowInd == rowInd = nextRowsResult `par` (force divOnOwn `pseq` (divOnOwn:nextRowsResult))
    | paral             = nextRowsResult `par` ( force subResult  `pseq` (subResult:nextRowsResult))
    | maxRowInd == rowInd = divOnOwn:nextRowsResult
    | otherwise           = subResult:nextRowsResult
    where
        subResult = if checkLessZero res
                    then multiplyRowNumber res (-1)
                    else res
        divOnOwn = multiplyRowNumber mat (1/(mat !! maxColInd))
        nextRowsResult = goAlongRows mats (rowInd + 1) rowToSub maxInd paral
        res = subRows mat (multiplyRowNumber rowToSub (valueToMul maxColInd mat rowToSub))
        checkLessZero :: Row -> Bool
        checkLessZero [] = True
        checkLessZero [_] = True
        checkLessZero (xRow:xsRow)
            | xRow > 0  = False
            | otherwise = checkLessZero xsRow


force :: [a] -> ()
force [] = ()
force (x:xs) = x `pseq` force xs

force2 :: [[a]] -> ()
force2 [] = ()
force2 (x:xs) = (force x) `pseq` force2 xs


addVectorToMartix :: Matrix -> Vector -> Matrix
addVectorToMartix [] [] = []
addVectorToMartix [] _  = []
addVectorToMartix _ []  = []
addVectorToMartix (mat:mats) (vec:vecs) = (mat ++ [vec]) : addVectorToMartix mats vecs


findMainPivot :: Matrix -> [Int] -> Bool -> Matrix
findMainPivot mat usedRows paral
    | row == -1 || col == -1        = mat
    | length usedRows /= length mat = findMainPivot (goAlongRows mat 0 rowToSub maxInd paral) (row:usedRows) paral
    | otherwise                     = mat
    where
        maxInd@(row, col) = maxIndex mat usedRows minusInf 0 (-1, -1)
        rowToSub = mat !! row

data CustomAns = Exists Vector
                | minusInf
                | NotExists
    deriving (Show, Eq)

gaussian :: Matrix -> Vector -> Bool -> CustomAns
gaussian [] vec _ = NotExists
gaussian mat vec paral = checkResult $ findMainPivot (addVectorToMartix mat vec) [] paral


checkRowResult :: Row -> Int
checkRowResult (x:xs)
    | null xs && abs x < eps = -1
    | null xs                = 0
    | abs x < eps            = checkRowResult xs
    | otherwise              = 1


checkResult :: Matrix -> CustomAns
checkResult []          = Exists []
checkResult (mat:mats)
    | ans == NotExists  = NotExists
    | ans == minusInf   = minusInf
    | curCheck == 0     = NotExists
    | curCheck == -1    = minusInf
    | otherwise         = Exists value
    where
        curCheck = checkRowResult mat
        ans = checkResult mats
        value = case ans of
            Exists a -> last mat:a
            _ -> []


generateMatrix :: Int -> Int -> IO Matrix
generateMatrix 0 m = return []
generateMatrix n m = do
    g <- newStdGen
    let row = take m (randoms g :: [Float])
    ans <- generateMatrix (n-1) m
    return (row:ans)

generateRandom :: Int -> IO (Matrix, Vector)
generateRandom n = do
    mat <- generateMatrix n n
    g <- newStdGen
    let vec = take n (randoms g :: [Float])
    return (mat, vec)
