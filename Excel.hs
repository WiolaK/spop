
module Excel
(
    Spreadsheet,
    newS,
    insert,
    cNum,
    cSum,
    cStr
) where


import qualified Data.Map as Map
import Data.Maybe (catMaybes)



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


insert :: (Int, Int) -> Cell -> Spreadsheet -> Spreadsheet
insert p c s = (Map.insert p c s)


slookup :: [(Int, Int)] -> Spreadsheet -> [Cell]
slookup a s = catMaybes (map (`Map.lookup` s) a)

cellValue :: Cell -> Maybe Double
cellValue (CNum a []) = Just a
cellValue (CString a) = Nothing
cellValue a = Just (value a)

update :: Cell -> Spreadsheet -> Spreadsheet
udpate (CString _) s = s
update (CNum _ _) s = s

updateCell :: ((Int, Int), Cell) -> Spreadsheet -> Spreadsheet
updateCell (p, (CString _)) s = s
updateCell (_, (CNum _ _)) s = s
updateCell (p, (CSum v _ d)) s = Map.insert p (cSum d s)  s


-- create cells
cStr :: String -> Cell
cStr a = CString a

cNum :: Double -> Cell
cNum a = CNum a []

cSum :: [(Int, Int)] -> Spreadsheet -> Cell
cSum [] _ = CSum 0 [] []
cSum a s = CSum (sum . catMaybes $ map cellValue (slookup a s)) [] a

-- instance Show Cell where
--     show (CString a) = "CStr " ++ show a
--     show (CNum a _) = "CNum " ++ show a
--     show (CSum a _ _) = "CSum " ++ show a
--     show (CMul a _ _) = "CMul " ++ show a
--     show (CAvg a _ _) = "CAvg " ++ show a

a = cNum 1
b = cNum 2
c = CString "a"
s = Map.fromList [((0,0), a), ((0,1), b), ((0,2), c)]
d = cSum [(0,0), (0,1), (0,2)] s
ss = Map.insert (1,0) d s
e = cSum [(0,0), (0,1), (1,0)] ss
sss = Map.insert (2,0) e s

{-

a=CNum 1 []
b=CNum 2 []
c=CSum [] [a,b]
d=CSum [] [a,b,c]
e=CString "a"

-}