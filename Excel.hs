
module Excel
(
    Spreadsheet,
    newS,
    insert,
    update,
    delete,
    cNum,
    cSum,
    cStr
) where


import qualified Data.Map as Map
import Data.Maybe



data Cell = CString String
        | CNum { value :: Double, dependent :: [(Int, Int)] }
        | CSum { value :: Double
               , dependent :: [(Int, Int)]
               , dependsOn :: [(Int, Int)] }
        | CMul { value :: Double
               , dependent :: [(Int, Int)]
               , dependsOn :: [(Int, Int)] }
        | CAvg { value :: Double
               , dependent :: [(Int, Int)]
               , dependsOn :: [(Int, Int)] } deriving (Show, Read)

type Spreadsheet = (Map.Map (Int, Int) Cell)


newS :: [((Int, Int), Cell)] -> Spreadsheet
newS a = Map.fromList a


insert :: ((Int, Int), Cell) -> Spreadsheet -> Spreadsheet
insert (p, (CSum v d o)) s = updateDep (p, nc)
                                       (Map.insert p nc (createDeps p o s))
    where nc = attachDep (p, (CSum v d o)) s
insert (p, c) s = updateDep (p, nc) (Map.insert p nc s)
    where nc = attachDep (p, c) s

-- no dependencies
insert2 :: ((Int, Int), Cell) -> Spreadsheet -> Spreadsheet
insert2 (p, c) s = Map.insert p c s


createDeps :: (Int, Int) -> [(Int, Int)] -> Spreadsheet -> Spreadsheet
createDeps p ps s = foldl (\acc x -> insert2 x acc) s (zip l (map (createDep p) r))
    where (l, r) = unzip $ slookupPos ps s

createDep :: (Int, Int) -> Cell -> Cell
createDep _ (CString v) = cStr v
createDep p (CNum v d) = CNum v (p:d)
createDep p (CSum v d o) = CSum v (p:d) o

createDepL :: [(Int, Int)] -> Cell -> Cell
createDepL _ (CString v) = cStr v
createDepL p (CNum v d) = CNum v (p++d)
createDepL p (CSum v d o) = CSum v (p++d) o


slookup :: [(Int, Int)] -> Spreadsheet -> [Cell]
slookup a s = catMaybes (map (`Map.lookup` s) a)

slookupPos :: [(Int, Int)] -> Spreadsheet -> [((Int, Int), Cell)]
slookupPos a s = catMaybePair $ zip a (map (`Map.lookup` s) a)

catMaybePair :: [((Int, Int), Maybe Cell)] -> [((Int, Int), Cell)]
catMaybePair [] = []
catMaybePair ((a, Nothing):s) = catMaybePair s
catMaybePair ((a, Just b):s) = (a, b):(catMaybePair s)

cellValue :: Cell -> Maybe Double
cellValue (CNum a []) = Just a
cellValue (CString a) = Nothing
cellValue a = Just (value a)


attachDep :: ((Int, Int), Cell) -> Spreadsheet -> Cell
attachDep (p, (CString v)) s = cStr v
attachDep (p, c) s = createDepL (findDep p s) c

findDep :: (Int, Int) -> Spreadsheet -> [(Int, Int)]
findDep p s = Map.keys $ Map.filter (cellDependsOn p) s

cellDependsOn :: (Int, Int) -> Cell -> Bool
cellDependsOn _ (CString _) = False
cellDependsOn _ (CNum _ _) = False
cellDependsOn p c = elem p (dependsOn c)




updateDep :: ((Int, Int), Cell) -> Spreadsheet -> Spreadsheet
updateDep (p, (CString _)) s = s
updateDep (p, c) s = updateDeps (p, c) [] s

updateDeps :: ((Int, Int), Cell) -> [(Int, Int)] -> Spreadsheet -> Spreadsheet
updateDeps (p, c) v s = (foldl (\acc x -> Map.union (updateDeps x (p : v) acc) acc) ns
                        (filter (\(l, r) -> not (elem l v)) deps))
    where deps = slookupPos (dependent c) s
          (l, r) = unzip deps
          ns = foldl (\acc x -> insert2 x acc) s (zip l (map (`updateDepCell` s) r))

updateDepCell :: Cell -> Spreadsheet -> Cell
updateDepCell (CString v) s = cStr v
updateDepCell (CNum v d) s = CNum v d
updateDepCell (CSum _ d o) s = cSumD d o s



update :: (Int, Int) -> Cell -> Spreadsheet -> Spreadsheet
update op nc s = insert (op, nc) (delete op s)


delete :: (Int, Int) -> Spreadsheet -> Spreadsheet
delete p s | isNothing c = s
           | otherwise = delete2 (p, fromJust c) s
           where c = Map.lookup p s

delete2 :: ((Int, Int), Cell) -> Spreadsheet -> Spreadsheet
delete2 (p, (CNum v d)) s = updateDep (p, (CNum v d)) (Map.delete p s)
delete2 (p, (CSum v d o)) s = updateDep (p, (CSum v d o)) (Map.delete p (removeDeps p o s))
delete2 (p, _) s = Map.delete p s


removeDeps :: (Int, Int) -> [(Int, Int)] -> Spreadsheet -> Spreadsheet
removeDeps p ps s = foldl (\acc x -> insert2 x acc) s (zip l (map (removeDep p) r))
    where (l, r) = unzip $ slookupPos ps s

removeDep :: (Int, Int) -> Cell -> Cell
removeDep p (CString v) = cStr v
removeDep p (CNum v d) = CNum v (remove p d)
removeDep p (CSum v d o) = CSum v (remove p d) o


-- remove element from list
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove a (b:bs) | a == b = remove a bs
                | otherwise = b:remove a bs


-- create cells
cStr :: String -> Cell
cStr a = CString a

cNum :: Double -> Cell
cNum a = CNum a []

cSum :: [(Int, Int)] -> Spreadsheet -> Cell
cSum [] _ = CSum 0 [] []
cSum a s = CSum (sum . catMaybes $ map cellValue (slookup a s)) [] a

cSumD :: [(Int, Int)] -> [(Int, Int)] -> Spreadsheet -> Cell
cSumD d o s = CSum (sum . catMaybes $ map cellValue (slookup o s)) d o

-- instance Show Cell where
--     show (CString a) = "CStr " ++ show a
--     show (CNum a _) = "CNum " ++ show a
--     show (CSum a _ _) = "CSum " ++ show a
--     show (CMul a _ _) = "CMul " ++ show a
--     show (CAvg a _ _) = "CAvg " ++ show a


{-
:l app/Excel.hs
a = cNum 1
b = cNum 2
c = CString "a"
s = Map.fromList [((0,0), a), ((0,1), b), ((0,2), c)]
d = cSum [(0,0), (0,1), (0,2)] s
ss = insert ((1,0), d) s
e = cSum [(0,0), (0,1), (0,3), (1,0)] ss
sss = insert ((2,0), e) ss

delete (0,0) ss
lol=delete (0,0) sss


a = CNum 1 [(2,0), (1,0)]
aa = ((0,0),a)
sx = Map.delete (0,0) sss
v = []
(p,c)=aa
deps = slookupPos (dependent c) sx
(l, r) = unzip deps
ncs = (zip l (map (`updateDepCell` sx) r))
ns = foldl (\acc x -> insert2 x acc) sx ncs
omg = filter (\(l, r) -> not (elem l v)) deps




a = CNum 1 []
s = Map.fromList [((0,0), a)]
b = cSum [(0,0)] s
ss = insert ((1,0),b) s
s = insert ((2,0),b) ss
c = cSum [(1,0)] s
d = cSum [(2,0)] s
ss = insert ((3,0),c) s
s = insert ((4,0),d) ss
e = cSum [(3,0),(4,0)] s
ss = insert ((5,0),e) s

lol=delete (1,0) ss
insert ((1,0),(cSum [(0,0)] lol)) lol


s = Map.fromList [((0,0), a)]
b = cSum [(0,0), (2,0)] s
ss = insert ((1,0), b) s
c = cSum [(0,0), (1,0)] ss
sss = insert ((2,0), c) ss


-}