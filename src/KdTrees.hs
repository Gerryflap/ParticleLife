module KdTrees (
    getAll,
    fromList,
    KdTree(..),
    findAllInRange    
) where

import System.Random
import PlDefinitions

data KdTree =
    Node Double Int (KdTree) (KdTree) |
    Leaf Particle |
    Empty
    deriving (Show, Eq)

initEmptyKdTree :: KdTree
initEmptyKdTree = Empty

axisval :: Particle -> Int -> Double
axisval p axis | axis == 0 = fst $ position p
                | axis == 1 = snd $ position p
                | otherwise = error "Unknown axis"


getAll :: KdTree -> [Particle]
getAll (Node _ _ t1 t2) = (getAll t1) ++ (getAll t2)
getAll (Leaf p) = [p]
getAll Empty = []

getAllInRange :: Particle -> Double -> KdTree -> [Particle]
getAllInRange p range (Node threshold axis t1 t2)  | (mx >= threshold) && (mn <= threshold) = inRange1 ++ inRange2
                                                    | mx >= threshold = inRange2
                                                    | mn <= threshold = inRange1
                                                    | otherwise = error "something is not adding up here :3"
                                                    where
                                                        v = axisval p axis
                                                        mx = v + range
                                                        mn = v - range
                                                        inRange1 = getAllInRange p range t1
                                                        inRange2 = getAllInRange p range t2
getAllInRange p1 range (Leaf p2)    | distance p1 p2 <= range = [p2]
                                    | otherwise = []
getAllInRange _ _ Empty = []

instance LookupTable KdTree where 
    findAllInRange lt p range = getAllInRange p range lt

fromList :: [Particle] -> KdTree
fromList ps = insertAll' 0 ps

insertAll' :: Int -> [Particle] -> KdTree
insertAll' _ []     = Empty
insertAll' _ [x]    = Leaf x
insertAll' axis xs  = Node median axis t1 t2
                    where
                        medianX = getNthLargestItemWith (\po -> axisval po axis) (div (length xs) 2) xs
                        median = axisval medianX axis
                        leftAndMiddleXs = filter (\po -> median > (axisval po axis)) xs
                        rightXs = filter (\po -> median <= (axisval po axis)) xs

                        nextAxis = mod (axis + 1) 2
                        t1 = insertAll' nextAxis leftAndMiddleXs
                        t2 = insertAll' nextAxis rightXs

-- Gets the Nth largest element in a reasonably fast way
getNthLargestItemWith :: Ord a => (b -> a) -> Int -> [b] -> b
getNthLargestItemWith = getNthLargestItemWith' (randoms $ mkStdGen 42)

-- Helper function, which receives and infinite list of pseudo-random integers in order to pick the next pivot element
getNthLargestItemWith' :: Ord a => [Int] -> (b -> a) -> Int -> [b] -> b
getNthLargestItemWith' _ _ 0 [x] = x
getNthLargestItemWith' _ _ n [_] =  error $ "Got a 1-element list while N was not 0 but " ++ show (n) ++ ", this should never happen!"
getNthLargestItemWith' (rand:rands) f n xs  | n >= leftLen && n < rightStart = pivot    -- n is somewhere in the list of elements equal to the pivot
                                            | n >= leftLen = rightItem
                                            | otherwise = leftItem
                                            where
                                                pivotIndex = mod rand (length xs - 1)
                                                pivot = xs !! pivotIndex
                                                leftXs = filter (\x -> (f x) < (f pivot)) xs
                                                rightXs = filter (\x -> (f x) > (f pivot)) xs
                                                leftLen = length leftXs
                                                rightStart = (length xs) - (length rightXs)

                                                rightItem = getNthLargestItemWith' rands f (n - rightStart) rightXs
                                                leftItem = getNthLargestItemWith' rands f n leftXs
getNthLargestItemWith' rands _ n xs = error $ "Got invalid inputs for getNthLargestItems:\n\trands = " ++ (show rands) ++ ",\n\tn = " ++ (show n) ++ ",\n\tand number of xs = " ++ (show $ length xs)


