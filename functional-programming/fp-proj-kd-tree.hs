import Control.Monad (when)

class Point point where

    dimension :: point -> Int

    coord :: Int -> point -> Double

    distance :: point -> point -> Double
    distance a b = sum . map diff2 $ [0..dimension a - 1]
        where diff2 i = (coord i a - coord i b)^2

    prettyPrint :: point -> String
    prettyPrint point = '(' : drop 2 (foldr (\h t -> ", " ++ (show h) ++ t) "" (map (\ax -> (coord ax point)) [0..dimension point - 1])) ++ ")"

-- |compareDistance p a b  compares the distances of a and b to p.
--compareDistance :: (Point point) => point -> point -> point -> Ordering
--compareDistance p a b = distance p a `compare` distance p b

data Point1D = Point1D { p1x :: Double }
     deriving (Eq, Show)

instance Point Point1D where
    dimension _ = 1

    coord 0 p = p1x p

data Point2D = Point2D { p2x :: Double, p2y :: Double }
     deriving (Eq, Show)

instance Point Point2D where
   dimension _ = 2

   coord 0 p = p2x p
   coord 1 p = p2y p

data Point3D = Point3D { p3x :: Double, p3y :: Double, p3z :: Double }
     deriving (Eq, Show)

instance Point Point3D where
    dimension _ = 3

    coord 0 p = p3x p
    coord 1 p = p3y p
    coord 2 p = p3z p

data PointND = PointND { dim :: Int, coordinates :: [Double] }
     deriving (Eq, Show)

instance Point PointND where
    dimension p = dim p

    coord axis p = (coordinates p)!!axis

  

data Tree median point axis = Node median (Tree median point axis) (Tree median point axis) axis | Leaf [point]
              deriving (Eq, Show)

sortPoints :: (Point a) => Int -> [a] -> [a]
sortPoints _ [] = []
sortPoints index (x:xs) =
    let smallerSorted = sortPoints index [a | a <- xs, (coord index a) <= (coord index x)]
        biggerSorted = sortPoints index [a | a <- xs, (coord index a) > (coord index x)]
    in  smallerSorted ++ [x] ++ biggerSorted

ps2d = [Point2D 2 3, Point2D 5 1, Point2D 0 0, Point2D 10 7, Point2D 12 5, Point2D 8 9]
ps3d =  [Point3D 2 3 5, Point3D 5 4 1, Point3D 9 6 10, Point3D 4 7 2, Point3D 8 1 0, Point3D 7 2 7]


fromListToKDTreeWithMaxDepth :: (Point point) => Int -> [point] -> Tree Double point Int
fromListToKDTreeWithMaxDepth _ [] = Leaf []
fromListToKDTreeWithMaxDepth _ [x] = Leaf [x]
fromListToKDTreeWithMaxDepth maxDepth points = parseFunc 0 points
    where parseFunc depth points = if depth + 1 >= maxDepth || (length points) <= 1 then Leaf points else Node median (parseFunc (depth + 1) (take takeDropInd sortedPoints)) (parseFunc (depth + 1) (drop takeDropInd sortedPoints)) axis
                           where axis = depth `mod` (dimension (head points))
                                 sortedPoints = sortPoints axis points
                                 middleListInd = (length points `div` 2)
                                 takeDropInd = middleListInd + (length points `mod` 2)
                                 median = ((coord axis (sortedPoints!!middleListInd)) + (coord axis (sortedPoints!!(middleListInd - (1 - (length points `mod` 2)))))) / 2

fromListToKDTreeWithMaxLPoints :: (Point point) => Int -> [point] -> Tree Double point Int
fromListToKDTreeWithMaxLPoints _ [] = Leaf []
fromListToKDTreeWithMaxLPoints _ [x] = Leaf [x]
fromListToKDTreeWithMaxLPoints maxLeafPoints points = parserFunc 0 points
    where parserFunc depth points = if (length points) <= maxLeafPoints then Leaf points else Node median (parserFunc (depth + 1) (take takeDropInd sortedPoints)) (parserFunc (depth + 1) (drop takeDropInd sortedPoints)) axis
                           where axis = depth `mod` (dimension (head points))
                                 sortedPoints = sortPoints axis points
                                 middleListInd = (length points `div` 2)
                                 takeDropInd = middleListInd + (length points `mod` 2)
                                 median = ((coord axis (sortedPoints!!middleListInd)) + (coord axis (sortedPoints!!(middleListInd - (1 - (length points `mod` 2)))))) / 2


findInInterval :: (Point point) => Tree Double point Int -> [(Double,Double)] -> [point]
findInInterval (Leaf points) intervals = filter (\point -> (all (\ax -> (fst $ intervals!!ax) <= (coord ax point) && (coord ax point) <= (snd $ intervals!!ax))) [0..(dimension point) - 1]) points
findInInterval (Node median left right axis) intervals
                                                        | snd (intervals!!axis) <= median = findInInterval left intervals
                                                        | fst (intervals!!axis) > median = findInInterval right intervals
                                                        | otherwise = (findInInterval left intervals) ++ (findInInterval right intervals)


fNPHelper :: (Point point) => Tree Double point Int -> point -> (point -> point -> Double) -> (point, Double) -> (point, Double)
fNPHelper (Leaf []) _ _ result = result
fNPHelper (Leaf (p:ps)) point distAlgo result = if (distAlgo point p) <= (snd (fNPHelper (Leaf ps) point distAlgo result)) then (p, (distAlgo point p)) else (fNPHelper (Leaf ps) point distAlgo result)
fNPHelper (Node median left right axis) point distAlgo result
    | (coord axis point) <= median
        = if (coord axis point) - (snd result) <= median then
            if (coord axis point)
                + snd (fNPHelper left point distAlgo result)
                > median then
                fNPHelper
                    right point distAlgo (fNPHelper left point distAlgo result)
            else
                fNPHelper left point distAlgo result
        else
            if (coord axis point) + snd result > median then
                fNPHelper right point distAlgo result
            else
                result
    | otherwise
        = if (coord axis point) + (snd result) > median then
            if (coord axis point)
                - snd (fNPHelper right point distAlgo result)
                <= median then
                fNPHelper
                    left point distAlgo (fNPHelper right point distAlgo result)
            else
                (fNPHelper right point distAlgo result)
        else
            if (coord axis point) - snd result <= median then
                fNPHelper left point distAlgo result
            else
                result
                                                         -- | (coord axis point) <= median = if (coord axis point) - (snd result) <= median then if (coord axis point) + snd (fNPHelper left point distAlgo result) > median then fNPHelper right point distAlgo (fNPHelper left point distAlgo result) else fNPHelper left point distAlgo result else if (coord axis point) + snd result > median then fNPHelper right point distAlgo result else result
                                                         -- | otherwise = if (coord axis point) + (snd result) > median then if (coord axis point) - snd (fNPHelper right point distAlgo result) <= median then fNPHelper left point distAlgo (fNPHelper right point distAlgo result) else (fNPHelper right point distAlgo result) else if (coord axis point) - snd result <= median then fNPHelper left point distAlgo result else result
                                                         -- | (coord axis point) <= median && (coord axis point) - (snd result) <= median && (coord axis point) + snd (fNPHelper left point distAlgo result (depth + 1)) > median = fNPHelper right point distAlgo (fNPHelper left point distAlgo result (depth + 1)) (depth + 1)
                                                         -- | (coord axis point) <= median && (coord axis point) - (snd result) <= median = fNPHelper left point distAlgo result (depth + 1)
                                                         -- | (coord axis point) <= median && (coord axis point) + (snd result) > median = fNPHelper right point distAlgo result (depth + 1)
                                                         -- | (coord axis point) <= median = result
                                                         -- | (coord axis point) > median && (coord axis point) + (snd result) > median && (coord axis point) - snd (fNPHelper right point distAlgo result (depth + 1)) <= median = fNPHelper left point distAlgo (fNPHelper right point distAlgo result (depth + 1)) (depth + 1)
                                                         -- | (coord axis point) > median && (coord axis point) + (snd result) > median = fNPHelper right point distAlgo result (depth + 1)
                                                         -- | (coord axis point) > median && (coord axis point) - (snd result) <= median = fNPHelper left point distAlgo result (depth + 1)
                                                         -- | (coord axis point) > median = result
                                                         -- | otherwise = result


findNearestPoint :: (Point point) => Tree Double point Int -> point -> (point -> point -> Double) -> (point, Double)
findNearestPoint kdTree point distAlgo = fNPHelper kdTree point distAlgo (point, 1/0)


split :: Char -> String -> [String]
split delim ['\n'] = [""]
split delim [] = [""]
split delim (x:xs)
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = split delim xs

indent :: [String] -> [String]
indent = map ("      " ++)

layoutTree :: (Point point, Show point) => Tree Double point Int -> [String]
layoutTree (Leaf points) = ['[' : drop 2 (foldr (\h t -> ", " ++ (prettyPrint h) ++ t) "" points) ++ "]"]
layoutTree (Node median left right axis)
         = indent (layoutTree right) ++ [show median] ++ indent (layoutTree left)

prettyTree :: (Point point, Show point) => Tree Double point Int -> String
prettyTree = unlines . layoutTree

kdTreeToFile :: (Point point, Show point) => FilePath -> Tree Double point Int -> IO ()
kdTreeToFile filePath kdTree = do
    let newContents = prettyTree kdTree
    when (length newContents > 0) $
        writeFile filePath newContents

fromFileToKDTreeWithMaxLPoints :: FilePath -> Int -> IO ()
fromFileToKDTreeWithMaxLPoints filePath maxLeafPoints = do
    contents <- readFile filePath
    let (l:ls) = split '\n' contents
    let dim = read l :: Int
    let parseListToDouble numbers = map (\x -> (read x :: Double)) numbers
    let points = map (\line -> (PointND dim (parseListToDouble (split ' ' line)))) ls
    print (fromListToKDTreeWithMaxLPoints maxLeafPoints points)

fromFileToKDTreeWithMaxDepth :: FilePath -> Int -> IO ()
fromFileToKDTreeWithMaxDepth filePath maxDepth = do
    contents <- readFile filePath
    let (l:ls) = split '\n' contents
    let dim = read l :: Int
    let parseListToDouble numbers = map (\x -> (read x :: Double)) numbers
    let points = map (\line -> (PointND dim (parseListToDouble (split ' ' line)))) ls
    print (fromListToKDTreeWithMaxDepth maxDepth points)