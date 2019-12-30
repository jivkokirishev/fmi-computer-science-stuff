import Control.Monad (when)

-- General:

-- consider using 2 spaces for a tab instead of 4

-- run hlint on your file

-- run this file with -Wall and -Werror


-- why did you decide to use a class instead of having a datatype
-- data Point = Point {dimensions :: Int, coords :: [Double]}
-- or
-- data Point = Point {dimensions :: Int, coords :: Int -> Double}
-- ?

class Point point where

    dimension :: point -> Int
    -- ^ you can remove this by using TypeApplications and AllowAmbiguousTypes
    -- instead of saying "dimension (undefined :: Point1D)" to get the dimensions
    -- you would say dimension @Point1D

    coord :: Int -> point -> Double
    -- ^ there is a way to make the first argument of coord be the same as
    -- what dimension returns :)
    -- but it needs some explaining
    -- I can show you some ideas in person

    distance :: point -> point -> Double
    distance a b = sum . map diff2 $ [0..dimension a - 1]
        where diff2 i = (coord i a - coord i b)^2
    -- ^ this doesn't need to be in the class
    -- it could instead be top-level
    -- and have a Point constraint

    prettyPrint :: point -> String
    prettyPrint point = '(' : drop 2 (foldr (\h t -> ", " ++ (show h) ++ t) "" (map (\ax -> (coord ax point)) [0..dimension point - 1])) ++ ")"
    -- ^ this *definitely* shouldn't be in the class

-- |compareDistance p a b  compares the distances of a and b to p.
--compareDistance :: (Point point) => point -> point -> point -> Ordering
--compareDistance p a b = distance p a `compare` distance p b

data Point1D = Point1D { p1x :: Double }
     deriving (Eq, Show)
-- ^ weird indentation on all your derivings,
-- it's not 4 spaces

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
-- at this point consider using newlines, e.g.:
-- data Point3D = Point3D
--     { p3x :: Double
--     , p3y :: Double
--     , p3z :: Double
--     }
     deriving (Eq, Show)

instance Point Point3D where
    dimension _ = 3

    coord 0 p = p3x p
    coord 1 p = p3y p
    coord 2 p = p3z p

data PointND = PointND { dim :: Int, coordinates :: [Double] }
-- ah the thing I suggested above
--
-- in fact it is possible to write these two functions:
-- f :: Point p => p -> PointND
-- and
-- f' :: PointND -> (Point p => p)
-- (second one is much more complicated though)
-- such that
-- f . f' == id
-- f' . f == id
     deriving (Eq, Show)

instance Point PointND where
    dimension p = dim p
    -- ^ you could eta-reduce this
    -- dimension = dim

    coord axis p = (coordinates p)!!axis
                   -- ^ redundant brackets, hlint would warn about this



data Tree median point axis = Node median (Tree median point axis) (Tree median point axis) axis | Leaf [point]
-- definitely use some newlines:
-- data Tree median point axis
--     = Node median (Tree median point axis) (Tree median point axis) axis
--     | Leaf [point]
-- also try to put your base case first:
-- data Tree median point axis
--     = Leaf [point]
--     | Node median (Tree median point axis) (Tree median point axis) axis
              deriving (Eq, Show)
-- weird deriving indentation

sortPoints :: (Point a) => Int -> [a] -> [a]
-- ^ this is
-- sortPoints index = Data.List.sortOn $ coord index
-- alternatively
-- import Data.Function (on)
-- sortPoints index = Data.List.sortBy (compare `on` coord index)
-- (first one is more time efficient)
sortPoints _ [] = []
sortPoints index (x:xs) =
    let smallerSorted = sortPoints index [a | a <- xs, (coord index a) <= (coord index x)]
        biggerSorted = sortPoints index [a | a <- xs, (coord index a) > (coord index x)]
        -- ^ you can use Data.List.partition to make these two at the same time
    in  smallerSorted ++ [x] ++ biggerSorted

ps2d = [Point2D 2 3, Point2D 5 1, Point2D 0 0, Point2D 10 7, Point2D 12 5, Point2D 8 9]
ps3d =  [Point3D 2 3 5, Point3D 5 4 1, Point3D 9 6 10, Point3D 4 7 2, Point3D 8 1 0, Point3D 7 2 7]


-- isn't it possible to write the function below in this way:
-- * write an insert :: a -> Tree -> Tree function
-- * fromListToKDblabla = foldr insert EmprtTree
fromListToKDTreeWithMaxDepth :: (Point point) => Int -> [point] -> Tree Double point Int
fromListToKDTreeWithMaxDepth _ [] = Leaf []
fromListToKDTreeWithMaxDepth _ [x] = Leaf [x]
fromListToKDTreeWithMaxDepth maxDepth points = parseFunc 0 points
-- ^ what does parseFunc even mean?
-- also you should definitely add a type signature to parseFunc
-- (you can add type signatures in where blocks)
--
-- btw are you sure you're not doing too much at once in parseFunc?
--
-- also you have weird indentation on your second where - it's way too indented,
-- only indent it once from parseFunc
    where parseFunc depth points = if depth + 1 >= maxDepth || (length points) <= 1 then Leaf points else Node median (parseFunc (depth + 1) (take takeDropInd sortedPoints)) (parseFunc (depth + 1) (drop takeDropInd sortedPoints)) axis
          -- please use newlines @_@
                           where axis = depth `mod` (dimension (head points))
                                 sortedPoints = sortPoints axis points
                                 middleListInd = (length points `div` 2)
                                 takeDropInd = middleListInd + (length points `mod` 2)
                                 median = ((coord axis (sortedPoints!!middleListInd)) + (coord axis (sortedPoints!!(middleListInd - (1 - (length points `mod` 2)))))) / 2

fromListToKDTreeWithMaxLPoints :: (Point point) => Int -> [point] -> Tree Double point Int
fromListToKDTreeWithMaxLPoints _ [] = Leaf []
fromListToKDTreeWithMaxLPoints _ [x] = Leaf [x]
fromListToKDTreeWithMaxLPoints maxLeafPoints points = parserFunc 0 points
-- pretty much same comments as above
-- this looks very copy-pasted from fromListToKDTreeWithMaxDepth
-- I'm pretty sure you can factor out the common bits
-- (it seems like the only difference is the check at the beginning?)
    where parserFunc depth points = if (length points) <= maxLeafPoints then Leaf points else Node median (parserFunc (depth + 1) (take takeDropInd sortedPoints)) (parserFunc (depth + 1) (drop takeDropInd sortedPoints)) axis
                           where axis = depth `mod` (dimension (head points))
                                 sortedPoints = sortPoints axis points
                                 middleListInd = (length points `div` 2)
                                 takeDropInd = middleListInd + (length points `mod` 2)
                                 median = ((coord axis (sortedPoints!!middleListInd)) + (coord axis (sortedPoints!!(middleListInd - (1 - (length points `mod` 2)))))) / 2


findInInterval :: (Point point) => Tree Double point Int -> [(Double,Double)] -> [point]
findInInterval (Leaf points) intervals = filter (\point -> (all (\ax -> (fst $ intervals!!ax) <= (coord ax point) && (coord ax point) <= (snd $ intervals!!ax))) [0..(dimension point) - 1]) points
-- ^ the filter function could use a where binding + a name for itself
-- and some newlines, very hard to read atm
findInInterval (Node median left right axis) intervals
                                                        | snd (intervals!!axis) <= median = findInInterval left intervals
                                                        | fst (intervals!!axis) > median = findInInterval right intervals
                                                        | otherwise = (findInInterval left intervals) ++ (findInInterval right intervals)
                                                      -- ^ only use one indentation level for these:
--    | snd (intervals!!axis) <= median = findInInterval left intervals
--    | fst (intervals!!axis) > median = findInInterval right intervals
--    | otherwise = (findInInterval left intervals) ++ (findInInterval right intervals)


-- -100 points for the name
fNPHelper :: (Point point) => Tree Double point Int -> point -> (point -> point -> Double) -> (point, Double) -> (point, Double)
-- this type signature could use some newlines:
-- fNPHelper
--     :: (Point point)
--     => Tree Double point Int
--     -> point
--     -> (point -> point -> Double)
--     -> (point, Double)
--     -> (point, Double)
fNPHelper (Leaf []) _ _ result = result
fNPHelper (Leaf (p:ps)) point distAlgo result = if (distAlgo point p) <= (snd (fNPHelper (Leaf ps) point distAlgo result)) then (p, (distAlgo point p)) else (fNPHelper (Leaf ps) point distAlgo result)
fNPHelper (Node median left right axis) point distAlgo result
    | (coord axis point) <= median
    -- ^ you're already using guards here, why then switch to ifs, you can add more conditions here
        = if (coord axis point) - (snd result) <= median then
             -- ^ don't these calculations have better names?
             -- and the checks etc..
            if (coord axis point)
                + snd (fNPHelper left point distAlgo result)
                > median then
                -- ^ I haven't seen anyone aligning an if like this
                -- much more common is to align your "then" and your "else" with the opening "if", like so:
                -- if cond
                -- then e1
                -- else e2
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
    -- ^ the otherwise case seems to have a lot of copy-paste code from above
    -- can't you factor it out?
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
-- ^ 1/0, you should use a name for 1/0


-- functions is named badly: it not only splits, but also does something to newlines
-- use two functions instead of doing two things at once
split :: Char -> String -> [String]
split delim ['\n'] = [""]
split delim [] = [""]
split delim (x:xs)
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
                      -- ^ get rid of head and tail
                      -- you can pattern match instead
    where
        rest = split delim xs

indent :: [String] -> [String]
indent = map ("      " ++)

layoutTree :: (Point point, Show point) => Tree Double point Int -> [String]
layoutTree (Leaf points) = ['[' : drop 2 (foldr (\h t -> ", " ++ (prettyPrint h) ++ t) "" points) ++ "]"]
layoutTree (Node median left right axis)
         = indent (layoutTree right) ++ [show median] ++ indent (layoutTree left)
-- ^ weird indentation here

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
    -- ^ bad idea, handle your errors properly..
    let dim = read l :: Int
    let parseListToDouble numbers = map (\x -> (read x :: Double)) numbers
                                        -- ^ if you enable TypeApplications you can instead write
                                        -- map (read @Double)
                                        -- also you can eta-reduce here
    let points = map (\line -> (PointND dim (parseListToDouble (split ' ' line)))) ls
    -- ^ try using ($) instead of so many parentheses
    -- also when you do that, you'll see that this is actually
    -- map (ParseND dim . parseListToDouble . split ' ') ls

    -- ^ you can only write let once:
    -- let (l:ls) = split '\n' contents
    --     dim = read l :: Int
    --     parseListToDouble numbers = map (\x -> (read x :: Double)) numbers
    --     points = map (\line -> (PointND dim (parseListToDouble (split ' ' line)))) ls
    print (fromListToKDTreeWithMaxLPoints maxLeafPoints points)
    -- ^ you could use ($) instead

fromFileToKDTreeWithMaxDepth :: FilePath -> Int -> IO ()
fromFileToKDTreeWithMaxDepth filePath maxDepth = do
    contents <- readFile filePath
    let (l:ls) = split '\n' contents
    let dim = read l :: Int
    let parseListToDouble numbers = map (\x -> (read x :: Double)) numbers
    let points = map (\line -> (PointND dim (parseListToDouble (split ' ' line)))) ls
    print (fromListToKDTreeWithMaxDepth maxDepth points)
-- ^ same comments as above, because copy-paste
-- when you see something like this, try to factor out the common bits
-- it's very easy to do in fp, and it's one of the big advantages of fp
