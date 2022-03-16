---Gruppe A1
-- module (NICHT ÄNDERN!)
module CrazyhouseBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char

-- Weitere Modulen können hier importiert werden


import Util
exp2 :: Int -> Int
exp2 = (2^)



--- external signatures (NICHT ÄNDERN!)
--dummy getMoves (takes the first move in the list)
getMove :: String -> String
getMove (x:xs) = [x] -- YOUR IMPLEMENTATION HERE



listMoves :: String -> String 
listMoves s = "[" ++ init((movesFromReserve1 (removeBlankSpaces (filteredReserve s)) 'x' (returnFreeSpots s)) ++ (allFiguresMoves (weissDran (last s)) (convertToOnes s) ++ vKnightMVs (weissDran (last s)) s)) ++ "]" 

combineMoves :: Bool -> String -> String 
combineMoves bool s = filterMovesReserve bool (digitToInt((getKpos bool s) !! 0)) (digitToInt ((getKpos bool s) !! 1)) (movesFromReserve (removeBlankSpaces (filteredReserve s)) (returnFreeSpots s)) s  ++ 
                    filterMoves bool (digitToInt((getKpos bool s) !! 0)) (digitToInt ((getKpos bool s) !! 1)) (allFiguresMoves bool (convertToOnes s) ++ vKnightMVs bool s) s 

-- YOUR IMPLEMENTATION FOLLOWS HERE
--combineMoves :: Bool -> String -> String 
--combineMoves bool s  | (tmpX bool s) == "Z" = (movesFromReserve (removeBlankSpaces (filteredReserve s)) (returnFreeSpots s)) ++ (allFiguresMoves bool (convertToOnes s) ++ vKnightMVs bool s)
  --                  |otherwise = filterMovesReserve bool (rInt (tmpX bool s)) (rInt (tmpY bool s)) (movesFromReserve (removeBlankSpaces (filteredReserve s)) (returnFreeSpots s)) s  ++ 
    --                filterMoves bool (rInt (tmpX bool s)) (rInt (tmpY bool s)) (allFiguresMoves bool (convertToOnes s) ++ vKnightMVs bool s) s --- ++ show (rInt (tmpX bool s)) ++ show (rInt (tmpY bool s))

wKingPosition :: String -> Int -> Int -> String
wKingPosition _ _ 0 = []
wKingPosition ('K':b) x y =  show x ++  show y
wKingPosition ('/':b) x y = wKingPosition b 1 (y-1)
wKingPosition (a:b) x y =   if (isNum a) then wKingPosition b (x+ (digitToInt a)) y 
                            else wKingPosition b (x+1) y

bKingPosition :: String -> Int -> Int -> String
bKingPosition _ _ 0 = []
bKingPosition ('k':b) x y =  show x ++  show y
bKingPosition ('/':b) x y = bKingPosition b 1 (y-1)
bKingPosition (a:b) x y =   if (isNum a) then bKingPosition b (x+ (digitToInt a)) y 
                            else bKingPosition b (x+1) y

getKpos :: Bool -> String -> String
getKpos True board = wKingPosition board 1 8
getKpos False board = bKingPosition board 1 8

konigPosX :: Bool -> String -> Char
konigPosX bool board  | null (getKonigPositions bool 1 8 (convertToOnes board)) = 'Z'
                       |otherwise = (head(getKonigPositions bool 1 8 (convertToOnes board)) )

konigPosY :: Bool -> String -> Char
konigPosY bool board = last (getKonigPositions bool 1 8 (convertToOnes board))

toStr :: Char -> String 
toStr s = [s]

tmpX :: Bool -> String -> String
tmpX bool board = toStr (konigPosX bool board)

tmpY :: Bool -> String -> String
tmpY bool board = toStr (konigPosY bool board)

-----MOVES FROM RESEERVE
movesFromReserve1 :: String -> Char -> String -> String  --Parameters:(Reserve, freeSpots: a1,a4,v5,h8...)-->Moves
movesFromReserve1 _ _ (x:[]) = []
movesFromReserve1 _ _ [] =[]
movesFromReserve1 [] _ _ = []
movesFromReserve1 (x:xs) dup a =  if(x == dup) then movesFromReserve1 xs x a
                                  else 
                                      iterate x a ++ movesFromReserve1 xs x a
                                where 
                                  iterate :: Char -> String -> String
                                  iterate _ [] = []
                                  iterate x (a:f:fs) = ([x]++['-']++[a]++[f]++[',']) ++ iterate x fs
--gives all the possible moves fromm the reserve !!!HAS AN EXTRA COMMA IN THE END!!!
movesFromReserve ::String -> String -> String  --Parameters:(Reserve, freeSpots: a1,a4,v5,h8...)-->Moves
movesFromReserve _ (x:[]) = []
movesFromReserve _ []=[]
movesFromReserve [] _ = []
movesFromReserve (x:xs) (a:f:fs) = ([x]++['-']++[a]++[f]++[',']) ++ movesFromReserve [x] fs  ++ movesFromReserve xs (a:f:fs)



getReserve:: Int -> String-> String 
getReserve 0 s = s
getReserve n (x:xs) = if x=='/' then getReserve (n-1) xs else getReserve n xs

--filters the reserve
filteredReserve:: String -> String  
filteredReserve s = if weissDran (last s) then findWhite (init(init(getReserve 8 s))) else findBlack (init(init(getReserve 8 s)))

removeDup :: String -> String
removeDup [] = []
removeDup (x:[]) = []
removeDup (x:y:ys) | x==y =[x]++ removeDup ys
                    |otherwise = [x] ++ [y]++ removeDup [y]++ ys 
    --gives all the free spots in the field
getFreeSpots:: Int -> Int->String->String --use getFreeSpots 1 8 String 
getFreeSpots _ _ []=[]
getFreeSpots _ 0 _ =[]
getFreeSpots 9 y s = getFreeSpots 1 (y-1) s
getFreeSpots x y (f:fs) =  if isDig f then --then it's an 1 because we converted every int to 1
                            show x ++ show y ++  getFreeSpots (x+1) y fs --no commas added here!!!! 
                            else getFreeSpots (x+1) y fs 



returnFreeSpots :: String -> String 
returnFreeSpots s = convertToChar(getFreeSpots 1 8 (convertToOnes s))

convertToChar :: String -> String --converts y values to a,b,c,d,...,h
convertToChar []=[]
convertToChar (y:x:xs) = [matchChar y]++[x] ++ convertToChar xs

--converts all the Integers in the string to multiple 1's (b3r-->b111r) and removes all '/'
convertToOnes :: String -> String 
convertToOnes [] = []
convertToOnes ('/':s) = convertToOnes s
convertToOnes (x:xs) = if isDig x then rep (rInt [x]) ['1'] ++ convertToOnes xs else x : convertToOnes xs


---------MOVES FROM THE EXISTING FIGURES ON THE BOARD
--returns the enemy figures' positions (doesnt include the free spots)(!!LATER ALSO CONSIDER KING!!)
enemyPositions:: Bool->Int -> Int ->String->String 
enemyPositions _ _ _ []=[]
enemyPositions _ _ 0 _ =[]
enemyPositions b 9 y s = enemyPositions b 1 (y-1) s
enemyPositions True x y (f:fs) = if isBlack f then show x ++ show y ++ [','] ++ enemyPositions True (x+1) y fs else enemyPositions True (x+1) y fs
enemyPositions False x y (f:fs) = if isWhite f then show x ++ show y ++ [','] ++ enemyPositions False (x+1) y fs else enemyPositions False (x+1) y fs

eigeneFiguren :: Bool->Int->Int->String->String
eigeneFiguren _ _ _ []=[]
eigeneFiguren _ _ 0 _ =[]
eigeneFiguren b 9 y s = eigeneFiguren b 1 (y-1) s
eigeneFiguren False x y (f:fs) = if isBlack f then show x ++ show y ++ [','] ++ eigeneFiguren True (x+1) y fs else eigeneFiguren True (x+1) y fs
eigeneFiguren True x y (f:fs) = if isWhite f then show x ++ show y ++ [','] ++ eigeneFiguren False (x+1) y fs else eigeneFiguren False (x+1) y fs

---1. Bauer

--gives the positions of the pawns in the board
getBauerPositions :: Bool->Int-> Int -> String-> String 
getBauerPositions _ _ _ []=[]
getBauerPositions _ _ 0 _ =[]
getBauerPositions b 9 y s = getBauerPositions b 1 (y-1) s
getBauerPositions True x y ('P':fs) = show x ++ show y ++ getBauerPositions True (x+1) y fs --gives the white pawns' position
getBauerPositions False x y ('p':fs) = show x ++ show y ++ getBauerPositions False (x+1) y fs
getBauerPositions b x y (f:fs) =  getBauerPositions b (x+1) y fs

--gives forward moves only (True means white is playing)
giveBauerMoves :: Bool -> String -> String -> String --parameters: WhiteisDran Spieszustand BauerPositions ->> Moves
giveBauerMoves _ _ [] = []
giveBauerMoves _ _ (x:[]) = []
giveBauerMoves _ _ ('0':_) =[]
giveBauerMoves _ _ (_:'0':_) =[]
giveBauerMoves _ _ ('9':_) =[]
giveBauerMoves _ _ (_:'9':_) =[]
--for whtie pawns in position 2: if there is no enemy in field 3 then check if there is an enemy in field 4 
    --if both of these fields are free, then both fields are available
    --if only 3 is free, then just return 3
    --if thre is an enemy in 3 return no move (pawn cant jump over to 4)
giveBauerMoves True s (y:'2':xs)   | isDig (head(getElement (rInt [y]) 3 1 8 s)) = if isDig (head(getElement (rInt [y]) 4 1 8 s)) 
                                            then [matchChar y] ++ "2" ++ ['-'] ++ [matchChar y] ++ "4" ++ [','] 
                                                 ++ [matchChar y] ++ "2" ++ ['-'] ++ [matchChar y] ++ "3" ++ [','] ++ giveBauerMoves True s xs 
                                                 else  [matchChar y] ++ "2" ++ ['-'] ++ [matchChar y] ++ "3" ++ [','] ++ giveBauerMoves True s xs
                                            | otherwise =giveBauerMoves True s xs

giveBauerMoves False s (y:'7':xs)  | isDig (head(getElement (rInt [y]) 6 1 8 s)) = if isDig (head(getElement (rInt [y]) 5 1 8 s)) 
                                            then [matchChar y] ++ "7" ++ ['-'] ++ [matchChar y] ++ "6" ++ [','] 
                                                 ++ [matchChar y] ++ "7" ++ ['-'] ++ [matchChar y] ++ "5" ++ [','] ++ giveBauerMoves False s xs 
                                                 else  [matchChar y] ++ "7" ++ ['-'] ++ [matchChar y] ++ "7" ++ [','] ++ giveBauerMoves False s xs
                                            | otherwise =giveBauerMoves False s xs
--parameter s stands for the enemy positions, y:x is the position of a pawn. 
--Example: pawn's positions: 23 (means b-3 in the board) then check if there is an enemy in 33
giveBauerMoves True s (y:x:xs)  | isDig (head(getElement (rInt [y]) (rInt [x]+1) 1 8 s)) = [matchChar y] ++ [x] ++ ['-'] ++ [matchChar y] ++ show ((rInt [x])+1) ++ [','] ++ giveBauerMoves True s xs
                                |  otherwise = giveBauerMoves True s xs
giveBauerMoves False s (y:x:xs) | isDig (head(getElement (rInt [y]) (rInt [x]-1) 1 8 s)) = [matchChar y] ++ [x] ++ ['-'] ++ [matchChar y] ++ show ((rInt [x])-1) ++ [','] ++ giveBauerMoves False s xs
                                |  otherwise = giveBauerMoves False s xs

bauerHitsLeft :: Bool -> String -> [Char] -> [Char]
bauerHitsLeft _ _ (x:[]) = []
bauerHitsLeft _ _ [] = []
bauerHitsLeft _ _ ('1':_) =[]
bauerHitsLeft _ _ (_:'0':_) =[]
bauerHitsLeft _ _ ('9':_) =[]
bauerHitsLeft _ _ (_:'9':_) =[]

bauerHitsLeft True s (x:y:ys)   | length (getElement (rInt [x]-1) (rInt [y]+1) 1 8 s) == 0 = bauerHitsLeft True s ys
                                | isBlack(head(getElement (rInt [x]-1) (rInt [y]+1) 1 8 s)) = [matchChar x] ++ [y] ++ ['-'] ++ [matchChar (head (show(rInt [x]-1)))] ++ show (rInt [y]+1) ++ [','] ++ bauerHitsLeft True s ys
                                | otherwise = bauerHitsLeft True s ys

bauerHitsLeft False s (x:y:ys)  | length (getElement (rInt [x]-1) (rInt [y]+1) 1 8 s) == 0 =bauerHitsLeft False s ys
                                | isWhite(head(getElement (rInt [x]-1) (rInt [y]-1) 1 8 s)) = [matchChar x] ++ [y] ++ ['-'] ++ [matchChar (head (show(rInt [x]-1)))] ++ show (rInt [y]-1) ++ [','] ++ bauerHitsLeft False s ys
                                | otherwise = bauerHitsLeft False s ys

bauerHitsRight :: Bool -> String -> [Char] -> [Char]
bauerHitsRight _ _ (x:[]) = []
bauerHitsRight _ _ [] = []
bauerHitsRight _ _ ('0':_) =[]
bauerHitsRight _ _ (_:'8':_) =[]
bauerHitsRight _ _ ('9':_) =[]
bauerHitsRight _ _ (_:'9':_) =[]
bauerHitsRight True s (x:y:ys)  | length (getElement (rInt [x]+1) (rInt [y]+1) 1 8 s) == 0 = bauerHitsRight True s ys
                                | isBlack(head(getElement (rInt [x]+1) (rInt [y]+1) 1 8 s)) = [matchChar x] ++ [y] ++ ['-'] ++ [matchChar (head (show(rInt [x]+1)))] ++ show (rInt [y]+1) ++ [',']++ bauerHitsRight True s ys
                                | otherwise = bauerHitsRight True s ys

bauerHitsRight False s (x:y:ys) | length (getElement (rInt [x]+1) (rInt [y]+1) 1 8 s) == 0 = bauerHitsRight False s ys
                                | isWhite(head(getElement (rInt [x]+1) (rInt [y]-1) 1 8 s)) = [matchChar x] ++ [y] ++ ['-'] ++ [matchChar (head (show(rInt [x]+1)))] ++ show (rInt [y]-1) ++ [','] ++ bauerHitsRight False s ys
                                | otherwise = bauerHitsRight False s ys

bauerAllMoves :: Bool -> String -> String -> [Char]
bauerAllMoves bo s p = giveBauerMoves bo s p ++ bauerHitsLeft bo s p ++ bauerHitsRight bo s p

--Turm
--gives the positions of the pawns in the board
getTurmPositions :: Bool->Int-> Int -> String-> String 
getTurmPositions _ _ _ []=[]
getTurmPositions _ _ 0 _ =[]
getTurmPositions b 9 y s = getTurmPositions b 1 (y-1) s
getTurmPositions True x y ('R':fs) = show x ++ show y ++ getTurmPositions True (x+1) y fs --gives the white pawns' position
getTurmPositions False x y ('r':fs) = show x ++ show y ++ getTurmPositions False (x+1) y fs
getTurmPositions b x y (f:fs) =  getTurmPositions b (x+1) y fs


--gives the moves on vertical ---> x is fixed
giveTurmMovesVertical :: Bool -> [Char] -> String -> [Char]
giveTurmMovesVertical _ (x:[]) _ = []
giveTurmMovesVertical _ [] _ = []
giveTurmMovesVertical True (a:b:bs) s = iterateTurm True (scanVerticalUp (rInt [a]) (rInt [b]) s) (scanVerticalDown (rInt [a]) (rInt [b]) s) a b s ++ giveTurmMovesVertical True bs s
giveTurmMovesVertical False (a:b:bs) s = iterateTurm False (scanVerticalUp (rInt [a]) (rInt [b]) s) (scanVerticalDown (rInt [a]) (rInt [b]) s) a b s ++ giveTurmMovesVertical False bs s

iterateTurm :: Bool -> Int -> Int -> Char ->Char -> String -> [Char]
iterateTurm _ 0 _ _ _ _ =[]
iterateTurm True y1 y2 a b s    | length (getElement (rInt [a]) y1 1 8 s)  == 0 =[]
                                |  not(isWhite (head(getElement (rInt [a]) y1 1 8 s))) 
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar a] ++ show y1 ++[','] ++ iterateTurm True (y1-1) y2 a b s
                                |  y1==(rInt [b]) || y1> y2 = iterateTurm True (y1-1) y2 a b s 
                                | otherwise = []
iterateTurm False y1 y2 a b s   | length (getElement (rInt [a]) y1 1 8 s)  == 0 =[]
                                | not(isBlack (head(getElement (rInt [a]) y1 1 8 s))) 
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar a] ++ show y1 ++ [','] ++ iterateTurm  False (y1-1) y2 a b s 
                                |  y1==(rInt [b]) || y1> y2 = iterateTurm False (y1-1) y2 a b s 
                                | otherwise = []


--gives the moves on  horizontal ---> y is fixed
giveTurmMovesHorizontal :: Bool -> [Char] -> String -> [Char]
giveTurmMovesHorizontal _ (x:[]) _ = []
giveTurmMovesHorizontal _ [] _ = []
giveTurmMovesHorizontal True (a:b:bs) s = iterateTurmHorizontal True (scanHorizontalRight (rInt [a]) (rInt [b]) s) (scanHorizontalLeft (rInt [a]) (rInt [b]) s) a b s ++ giveTurmMovesHorizontal True bs s
giveTurmMovesHorizontal False (a:b:bs) s = iterateTurmHorizontal False (scanHorizontalRight (rInt [a]) (rInt [b]) s) (scanHorizontalLeft (rInt [a]) (rInt [b]) s) a b s ++ giveTurmMovesHorizontal False bs s

iterateTurmHorizontal :: Bool -> Int -> Int -> Char ->Char -> String -> [Char]
iterateTurmHorizontal _ 0 _ _ _ _ =[]
iterateTurmHorizontal True x1 x2 a b s  --- |(length (getElement x1 (rInt [b]) 1 8 s) == 0) = []
                                        | not(isWhite (head(getElement x1 (rInt [b]) 1 8 s))) 
                                             = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show x1))] ++ [b] ++[','] ++ iterateTurmHorizontal True (x1-1) x2 a b s
                                        | x1 == (rInt [a]) || x1>x2= iterateTurmHorizontal True (x1-1) x2 a b s
                                        | otherwise = []
iterateTurmHorizontal False x1 x2 a b s --- |(length (getElement x1 (rInt [b]) 1 8 s) == 0) = []
                                        | not(isBlack (head(getElement x1  (rInt [b]) 1 8 s))) 
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show x1))] ++ [b] ++[','] ++ iterateTurmHorizontal False (x1-1) x2 a b s 
                                        | x1 == (rInt [a]) || x1>x2= iterateTurmHorizontal False (x1-1) x2 a b s
                                        | otherwise = []

turmAllMoves :: Bool -> [Char] -> String -> [Char]
turmAllMoves True s p = giveTurmMovesVertical True s p ++ giveTurmMovesHorizontal True s p
turmAllMoves False s p = giveTurmMovesVertical False s p ++ giveTurmMovesHorizontal False s p


------- LAUFER
getLauferPositions :: Bool->Int-> Int -> String-> String 
getLauferPositions _ _ _ []=[]
getLauferPositions _ _ 0 _ =[]
getLauferPositions b 9 y s = getLauferPositions b 1 (y-1) s
getLauferPositions True x y ('B':fs) = show x ++ show y ++ getLauferPositions True (x+1) y fs --gives the white pawns' position
getLauferPositions False x y ('b':fs) = show x ++ show y ++ getLauferPositions False (x+1) y fs
getLauferPositions b x y (f:fs) =  getLauferPositions b (x+1) y fs



--digonale to the "right" (0,0)-(1,1)----(8,8) ----> (x+1,y+1)
giveLauferMovesRight :: Bool -> [Char] -> String -> [Char]
giveLauferMovesRight _ [] _ = []
giveLauferMovesRight _ (x:[]) _ = []
giveLauferMovesRight True (a:b:bs) s = iterateLaufer True (scanDioganalRightUp (rInt [a]) (rInt [b]) s) (scanDioganalLeftDown(rInt [a]) (rInt [b]) s) a b s ++ giveLauferMovesRight True bs s
giveLauferMovesRight False (a:b:bs) s = iterateLaufer False (scanDioganalRightUp (rInt [a]) (rInt [b]) s) (scanDioganalLeftDown (rInt [a]) (rInt [b]) s) a b s ++ giveLauferMovesRight False bs s

iterateLaufer :: Bool -> [Int] -> [Int] -> Char ->Char -> String -> [Char]
iterateLaufer _ (0:_) _ _ _ _ =[]
iterateLaufer _ (_:0:_) _ _ _ _ =[]
iterateLaufer True y1 y2 a b s  |  not(isWhite (head(getElement (head y1) (last y1) 1 8 s))) && (getElement (head y1) (last y1) 1 8 s)/="Z"
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show(head y1)))] ++ show (last y1) ++[','] ++ iterateLaufer True [head y1-1, last y1-1 ] y2 a b s
                                |  head y1==(rInt [a])||(head y1)> (head y2)  = iterateLaufer True [head y1-1, last y1-1 ] y2 a b s 
                                | otherwise = []
iterateLaufer False y1 y2 a b s |  not(isBlack (head(getElement (head y1) (last y1) 1 8 s))) && (getElement (head y1) (last y1) 1 8 s)/="Z"
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show(head y1)))] ++ show (last y1) ++[','] ++ iterateLaufer False [head y1-1, last y1-1 ] y2 a b s
                                |  head y1==(rInt [a])||(head y1)> (head y2)   = iterateLaufer False [head y1-1, last y1-1 ] y2 a b s 
                                | otherwise = []

--digonale to the the other side: (1,8)-(2,7)-(3,6)---(8,1) ------> (x+1,y-1)
giveLauferMovesLeft :: Bool -> [Char] -> String -> [Char]
giveLauferMovesLeft _ [] _ = []
giveLauferMovesLeft _ (x:[]) _ = []
giveLauferMovesLeft True (a:b:bs) s = iterateLauferLeft True (scanDioganalLeftUp (rInt [a]) (rInt [b]) s) (scanDioganalRightDown (rInt [a]) (rInt [b]) s) a b s ++ giveLauferMovesLeft True bs s
giveLauferMovesLeft False (a:b:bs) s = iterateLauferLeft False (scanDioganalLeftUp (rInt [a]) (rInt [b]) s) (scanDioganalRightDown (rInt [a]) (rInt [b]) s) a b s ++ giveLauferMovesLeft False bs s

iterateLauferLeft :: Bool -> [Int] -> [Int] -> Char ->Char -> String -> [Char]
iterateLauferLeft _ (0:_) _ _ _ _ =[]
iterateLauferLeft _ (_:0:_) _ _ _ _ =[]
iterateLauferLeft True x1 x2 a b s  |  not(isWhite (head(getElement (head x1) (last x1) 1 8 s))) 
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show (head x1)))] ++ show (last x1) ++[','] ++ iterateLaufer True [head x1+1, last x1-1 ] x2 a b s
                                    |  head x1==(rInt [a])||(head x1) < (head x2) || (last x1)> (last x2) = iterateLaufer True [head x1+1, last x1-1 ] x2 a b s 
                                    | otherwise = []
iterateLauferLeft False x1 x2 a b s |   not(isBlack (head(getElement (head x1) (last x1) 1 8 s))) 
                                            = [matchChar a] ++ [b] ++ ['-'] ++ [matchChar (head(show (head x1)))] ++ show (last x1) ++[','] ++ iterateLaufer False [head x1+1, last x1-1 ] x2 a b s
                                    |  head x1==(rInt [a]) || (head x1)< (head x2) || (last x1)> (last x2) = iterateLaufer False [head x1+1, last x1-1 ] x2 a b s 
                                    | otherwise = []


lauferAllMoves :: Bool -> [Char] -> String -> [Char]
lauferAllMoves True s p = giveLauferMovesRight True s p ++ giveLauferMovesLeft True s p
lauferAllMoves False s p = giveLauferMovesRight False s p ++ giveLauferMovesLeft False s p

----QUEEN
getDamePositions :: Bool->Int-> Int -> String-> String 
getDamePositions _ _ _ []=[]
getDamePositions _ _ 0 _ =[]
getDamePositions b 9 y s = getDamePositions b 1 (y-1) s
getDamePositions True x y ('Q':fs) = show x ++ show y ++ getDamePositions True (x+1) y fs --gives the white pawns' position
getDamePositions False x y ('q':fs) = show x ++ show y ++ getDamePositions False (x+1) y fs
getDamePositions b x y (f:fs) =  getDamePositions b (x+1) y fs

dameAllMoves :: Bool -> [Char] -> String -> [Char]
dameAllMoves bo s p = lauferAllMoves bo s p ++ turmAllMoves bo s p

-----KING
getKonigPositions :: Bool->Int-> Int -> String-> String 
getKonigPositions _ _ _ []=[]
getKonigPositions _ _ 0 _ =[]
getKonigPositions b 9 y s = getKonigPositions b 1 (y-1) s
getKonigPositions True x y ('K':fs) = show x ++ show y -- ++ getKonigPositions True (x+1) y fs --gives the white kings' position
getKonigPositions False x y ('k':fs) = show x ++ show y -- ++ getKonigPositions False (x+1) y fs
getKonigPositions b x y (f:fs) =  getKonigPositions b (x+1) y fs


--dummy solution with a funtion for each direction
giveKonigMoves1 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves1 True x y s  |(x==8 || y==8) = []
                            |not(isWhite (head(getElement (x+1) (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y+1) ++[',']
                            | otherwise = []
giveKonigMoves1 False x y s  |(x==8 || y==8) = []
                            |not(isBlack (head(getElement (x+1) (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y+1) ++[',']
                            | otherwise = []                           

giveKonigMoves2 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves2 True x y s  |(x==8 || y==1) = []
                            |not(isWhite (head(getElement (x+1) (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y-1) ++[',']
                            | otherwise = []
giveKonigMoves2 False x y s |(x==8 || y==1) = []
                            |not(isBlack (head(getElement (x+1) (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y-1) ++[',']
                            | otherwise = []

giveKonigMoves3 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves3 True x y s  |(x==1 || y==8) = []
                            |not(isWhite (head(getElement (x-1) (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y+1) ++[',']
                            | otherwise = []
giveKonigMoves3 False x y s  |(x==1 || y==8) = []
                            |not(isBlack (head(getElement (x-1) (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y+1) ++[',']
                            | otherwise = []

giveKonigMoves4 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves4 True x y s   |(x==1 || y==1) = []
                            |not(isWhite (head(getElement (x-1) (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y-1) ++[',']
                            | otherwise = []
giveKonigMoves4 False x y s   |(x==1 || y==1) = []
                            |not(isBlack (head(getElement (x-1) (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y-1) ++[',']
                            | otherwise = []

giveKonigMoves5 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves5 True x y s  |x==8 = []
                            |not(isWhite (head(getElement (x+1) y 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y) ++[',']
                            | otherwise =  []
giveKonigMoves5 False x y s  |x==8 = []
                            |not(isBlack (head(getElement (x+1) y 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x+1)))] ++ show (y) ++[',']
                            | otherwise =  []

giveKonigMoves6 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves6 True x y s  |x==1 = []
                            |not(isWhite (head(getElement (x-1) y 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y) ++[',']
                            | otherwise = []
giveKonigMoves6 False x y s  |x==1 = []
                            |not(isBlack (head(getElement (x-1) y 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x-1)))] ++ show (y) ++[',']
                            | otherwise = []


giveKonigMoves7 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves7 True x y s   |y==8 = []
                            |not(isWhite (head(getElement x (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x)))] ++ show (y+1) ++[',']
                            | otherwise = []
giveKonigMoves7 False x y s   |y==8 = []
                            |not(isBlack (head(getElement x (y+1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x)))] ++ show (y+1) ++[',']
                            | otherwise = []

giveKonigMoves8 :: Bool -> Int -> Int -> String  -> [Char]
giveKonigMoves8 True x y s  |y==1 = []
                            |not(isWhite (head(getElement x (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x)))] ++ show (y-1) ++[',']
                            | otherwise = []
giveKonigMoves8 False x y s  |y==1 = []
                            |not(isBlack (head(getElement x (y-1) 1 8 s))) = [matchChar (head(show x))] ++ show y ++['-']++ [matchChar (head(show (x)))] ++ show (y-1) ++[',']
                            | otherwise = []
kingAllMoves:: Bool -> String -> String -> String
kingAllMoves bo (x:y:ys) s = giveKonigMoves1 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves2 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves3 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves4 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves5 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves6 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves7 bo (rInt [x]) (rInt [y]) s ++ giveKonigMoves8 bo (rInt [x]) (rInt [y]) s 

--SCAN THE BOARD
--give the position of the first occupied field in that direction -- if nothing occupied then return 8 or 1

--return a single integer value, either the x position (when horizontal) or the y position (if vertical)
scanVerticalUp :: Int -> Int -> String -> Int
scanVerticalUp x 8 s = 8
scanVerticalUp x y s = if isDigit (head(getElement x (y+1) 1 8 s)) then scanVerticalUp x (y+1) s else y+1

scanVerticalDown :: Int -> Int -> String -> Int
scanVerticalDown x 1 s = 1
scanVerticalDown x y s = if isDigit (head(getElement x (y-1) 1 8 s)) then scanVerticalDown x (y-1) s else y-1

scanHorizontalRight :: Int -> Int -> String -> Int
scanHorizontalRight 8 y s = 8
scanHorizontalRight x y s = if isDigit (head(getElement (x+1) y 1 8 s)) then scanHorizontalRight (x+1) y s else x+1

scanHorizontalLeft :: Int -> Int -> String -> Int
scanHorizontalLeft 1 y s = 1
scanHorizontalLeft x y s = if isDigit  (head(getElement (x-1) y 1 8 s)) then scanHorizontalLeft (x-1) y s else x-1

--Diganonal: returns a list of x y values
scanDioganalRightUp :: Int -> Int -> String -> [Int]
scanDioganalRightUp x 8 _ = [x,8]
scanDioganalRightUp 8 y _ = [8,y]
scanDioganalRightUp x y s = if isDigit  (head(getElement (x+1) (y+1) 1 8 s)) then scanDioganalRightUp (x+1) (y+1) s else [x+1,y+1]

scanDioganalLeftUp :: Int -> Int -> String -> [Int]
scanDioganalLeftUp x 8 _ = [x,8]
scanDioganalLeftUp 1 y _ = [1,y]
scanDioganalLeftUp x y s = if isDigit  (head(getElement (x-1) (y+1) 1 8 s)) then scanDioganalLeftUp (x-1) (y+1) s else [x-1,y+1]

scanDioganalRightDown :: Int -> Int -> String -> [Int]
scanDioganalRightDown x 1 _ = [x,1]
scanDioganalRightDown 8 y _ = [8,y]
scanDioganalRightDown x y s = if isDigit  (head(getElement (x+1) (y-1) 1 8 s)) then scanDioganalRightUp (x+1) (y-1) s else [x+1,y-1]

scanDioganalLeftDown :: Int -> Int -> String -> [Int]
scanDioganalLeftDown x 1 _ = [x,1]
scanDioganalLeftDown 1 y _ = [1,y]
scanDioganalLeftDown x y s = if isDigit  (head(getElement (x-1) (y-1) 1 8 s)) then scanDioganalRightUp (x-1) (y-1) s else [x-1,y-1]

-----check shach
checkMate :: Bool -> Int -> Int -> String -> Bool
checkMate True x_king y_king board  | (getElement (scanHorizontalRight x_king y_king board) y_king 1 8 board == "r") || (getElement (scanHorizontalRight x_king y_king board) y_king 1 8 board == "q") = True
                                    | (getElement (scanHorizontalLeft x_king y_king board) y_king 1 8 board == "r") || (getElement (scanHorizontalLeft x_king y_king board) y_king 1 8 board == "q") = True
                                    | (getElement x_king  (scanVerticalUp x_king y_king board) 1 8 board == "r") || (getElement (scanVerticalUp x_king y_king board) y_king 1 8 board == "q") = True 
                                    | (getElement x_king  (scanVerticalDown x_king y_king board) 1 8 board == "r") || (getElement (scanVerticalUp x_king y_king board) y_king 1 8 board == "q") = True
                                    | (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "b") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "q") = True
                                    | (getElement (head (scanDioganalRightUp x_king y_king board)) (last (scanDioganalRightUp x_king y_king board)) 1 8 board == "b") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "q") = True
                                    | (getElement (head (scanDioganalLeftDown x_king y_king board)) (last (scanDioganalLeftDown x_king y_king board)) 1 8 board == "b") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "q") = True
                                    | (getElement (head (scanDioganalRightUp x_king y_king board)) (last (scanDioganalRightUp x_king y_king board)) 1 8 board == "b") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "q") = True
                                    | (getElement (x_king+1) (y_king+1) 1 8 board == "p") || (getElement (x_king-1) (y_king+1) 1 8 board == "p") = True
                                    | otherwise = False
checkMate False x_king y_king board  | (getElement (scanHorizontalRight x_king y_king board) y_king 1 8 board == "R") || (getElement (scanHorizontalRight x_king y_king board) y_king 1 8 board == "Q") = True
                                    | (getElement (scanHorizontalLeft x_king y_king board) y_king 1 8 board == "R") || (getElement (scanHorizontalLeft x_king y_king board) y_king 1 8 board == "Q") = True
                                    | (getElement x_king  (scanVerticalUp x_king y_king board) 1 8 board == "R") || (getElement (scanVerticalUp x_king y_king board) y_king 1 8 board == "Q") = True 
                                    | (getElement x_king  (scanVerticalDown x_king y_king board) 1 8 board == "R") || (getElement (scanVerticalUp x_king y_king board) y_king 1 8 board == "Q") = True
                                    | (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "B") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "Q") = True
                                    | (getElement (head (scanDioganalRightUp x_king y_king board)) (last (scanDioganalRightUp x_king y_king board)) 1 8 board == "B") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "Q") = True
                                    | (getElement (head (scanDioganalLeftDown x_king y_king board)) (last (scanDioganalLeftDown x_king y_king board)) 1 8 board == "B") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "Q") = True
                                    | (getElement (head (scanDioganalRightUp x_king y_king board)) (last (scanDioganalRightUp x_king y_king board)) 1 8 board == "B") || (getElement (head (scanDioganalLeftUp x_king y_king board)) (last (scanDioganalLeftUp x_king y_king board)) 1 8 board == "Q") = True
                                    | (getElement (x_king+1) (y_king-1) 1 8 board == "P") || (getElement (x_king-1) (y_king-1) 1 8 board == "P") = True
                                    | otherwise = False

----all moves

allFiguresMoves :: Bool -> String -> [Char]
allFiguresMoves bool board = bauerAllMoves bool board (getBauerPositions bool 1 8 board) ++ turmAllMoves bool (getTurmPositions bool 1 8 board) board ++ 
                    lauferAllMoves bool (getLauferPositions bool 1 8 board) board ++  dameAllMoves bool (getDamePositions bool 1 8 board) board ++ kingAllMoves bool (getKonigPositions bool 1 8 board) board

filterMoves :: Bool -> Int-> Int -> String -> String -> String 
filterMoves _ _ _ [] _ = []
filterMoves True x_king y_king (x1:y1:'-':x2:y2:',':fs) board | checkMate True x_king y_king (moveFigure (rInt [x1]) (rInt [y1]) (rInt [x2]) (rInt [y2]) board) = filterMoves True x_king y_king fs board
                                                | otherwise = [x1]++[y1]++['-']++[x2]++[y2]++[','] ++ filterMoves True x_king y_king fs board


filterMoves False x_king y_king (x1:y1:'-':x2:y2:',':fs) board | checkMate False x_king y_king (moveFigure (rInt [x1]) (rInt [y1]) (rInt [x2]) (rInt [y2]) board) = filterMoves False x_king y_king fs board
                                                | otherwise = [x1]++[y1]++['-']++[x2]++[y2]++[','] ++ filterMoves False x_king  y_king fs board
filterMoves _ _ _ _ _ =[]

filterMovesReserve :: Bool -> Int-> Int -> String -> String -> String 
filterMovesReserve _ _ _ [] _ = []

filterMovesReserve True x_king y_king (f:'-':x:y:',':fs) board   | checkMate True x_king y_king (setField [f] (rInt[x]) (rInt[y]) 1 8 board "") = filterMovesReserve True x_king y_king fs board
                                            | otherwise = [f]++['-']++[x]++[y]++[','] ++ filterMovesReserve True x_king y_king fs board

filterMovesReserve False x_king y_king (f:'-':x:y:',':fs) board   | checkMate False x_king y_king (setField [f] (rInt[x]) (rInt[y]) 1 8 board "") = filterMovesReserve False x_king y_king fs board
                                            | otherwise = [f]++['-']++[x]++[y]++[','] ++ filterMovesReserve False x_king  y_king fs board
filterMovesReserve _ _ _ _ _ =[]

-------------HELPER FUNCTIONS
--- gives the element in position x y

getElement :: Int -> Int -> Int -> Int ->String -> String --(call with: x_position y_position 1 8 String)
getElement _ _ _ _ [] = []
getElement _ _ _ 0 _ = []
getElement x y 9 tmpy s = getElement x y 1 (tmpy-1) s
getElement x y tmpx tmpy (f:fs) | x<1 || y>8  = []
                                | otherwise = if (x == tmpx) && (y == tmpy) then [f] else getElement x y (tmpx+1) tmpy fs

setField :: String -> Int -> Int ->Int -> Int -> String ->String-> String 
setField _ _ _ _ _ [] _= []
setField _ _ _ _ 0 _  _ = []
setField f x y 9 tmpy s alist = setField f x y 1 (tmpy-1) s alist
setField f x y tmpx tmpy (a:as) alist = if (x == tmpx) && (y == tmpy) then alist ++ f ++ as else setField f x y (tmpx+1) tmpy as (alist ++ [a])

moveFigure :: Int -> Int -> Int -> Int -> String ->  String
moveFigure x1 y1 x2 y2 board = setField "1" x1 y1 1 8 (setField (getElement x1 y1 1 8 board) x2 y2 1 8 board "") ""

addCommas :: [Char] -> [Char]
addCommas (a:[])=[]
addCommas [] = []
addCommas (a:f:fs)= [a] ++ [f] ++ [','] ++ addCommas fs

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

tails :: [a] -> [[a]]
tails xs = xs : case xs of 
                    [] -> [] 
                    _  : xs' -> tails xs'
isInfixOf  :: (Eq a) => [a] -> [a] -> Bool ---returns true if "12," is an infix of "67,56,45...,11,12,"
isInfixOf needle haystack = any (isPrefix needle) (tails haystack) 

rInt :: String -> Int --converts '3' to 3
rInt "" = -1
rInt "a" =1
rInt "b" = 2
rInt "c" = 3
rInt "d"= 4
rInt "e" = 5
rInt "f" = 6
rInt "g" = 7
rInt "h" = 8
rInt a = read a

rep :: Int -> [a] -> [a]--repeats an elements (from Tut)
rep 0 _ = []
rep n xs = xs ++ rep (n-1) xs

matchChar :: Char -> Char
matchChar '1'='a'
matchChar '2'='b'
matchChar '3'='c'
matchChar '4'='d'
matchChar '5'='e'
matchChar '6'='f'
matchChar '7'='g'
matchChar '8'='h'


isDig :: Char -> Bool
isDig '1' = True
isDig '2' = True
isDig '3' = True
isDig '4' = True
isDig '5' = True
isDig '6' = True
isDig '7' = True
isDig '8' = True
isDig _ = False

isWhite:: Char -> Bool 
isWhite('K') = True
isWhite('Q') = True
isWhite('B') = True
isWhite('N') = True
isWhite('R') = True
isWhite('P') = True
isWhite _ = False

isBlack:: Char -> Bool 
isBlack('k') = True
isBlack('q') = True
isBlack('b') = True
isBlack('n') = True
isBlack('r') = True
isBlack('p') = True
isBlack _ = False

isNotBlank:: Char -> Bool
isNotBlank ' ' = False
isNotBlank _ =True

removeBlankSpaces :: String -> String
removeBlankSpaces [] =[]
removeBlankSpaces a = filter isNotBlank a

weissDran :: Char  -> Bool 
weissDran 'w' = True  --True wenn weiss dran ist
weissDran _ = False --Schwarz ist dran


findWhite :: String -> String 
findWhite a = filter isWhite a--gets only white elements

findBlack :: String -> String 
findBlack a = filter isBlack a
--------------------------------------------------


----AMMAR
bPos :: String -> Int -> Int -> [String]
bPos [] _ _ = []
bPos (a:b) x y = if(isBlack a) then [show x ++ show y] ++ bPos b (x+1) y
                                  else if(isNum a) then bPos b (x + (digitToInt a)) y
                                  else if(a == '/') then bPos b 1 (y-1)
                                  else bPos b (x+1) y

wPos :: String -> Int -> Int -> [String]
wPos [] _ _ = []
wPos (a:b) x y = if(isWhite a) then [show x ++ show y] ++ wPos b (x+1) y
                                  else if(isNum a) then wPos b (x + (digitToInt a)) y
                                  else if(a == '/') then wPos b 1 (y-1)
                                  else wPos b (x+1) y 

-- rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
fPos :: String -> Int -> Int -> [String] 
fPos _ _ 0 = []
fPos (a:b) x y = if(isNum a) then iterate (read [a]) x y ++ fPos b (x+(digitToInt a)) y
                 else if(a == '/') then fPos b 1 (y-1)
                 else fPos b (x+1) y
                 where
                   iterate ::  Int -> Int -> Int -> [String]
                   iterate 0 _ _ = []
                   iterate i x y = [show x ++ show y] ++ iterate (i-1) (x+1) y

outOfBoard :: String -> Bool
outOfBoard (a:b)
        | read [a] >= 1 && read [a] <= 8 = True
        | read b >= 1 && read b <= 8 = True
        | otherwise = False


knightVM :: Bool -> String -> String -> Bool
knightVM True (a:b) (c:d)    
    | abs(read [a] - read [c]) == 2 && abs(read b - read d) == 1  = True
    | abs(read [a] - read [c]) == 1 && abs(read b - read d) == 2  = True
    | otherwise = False
knightVM False (a:b) (c:d)    
    | abs(read [a] - read [c]) == 2 && abs(read b - read d) == 1  = True
    | abs(read [a] - read [c]) == 1 && abs(read b - read d) == 2  = True
    | otherwise = False

pawnVM :: Bool -> String -> String -> Bool
pawnVM True (a:b) (c:d)  
    | abs(read [a] - read [c]) == 2 && abs(read b - read d) == 1  = True
    | abs(read [a] - read [c]) == 1 && abs(read b - read d) == 2  = True
    | otherwise = False
pawnVM False (a:b) (c:d)  
    | abs(read [a] - read [c]) == 2 && abs(read b - read d) == 1  = True
    | abs(read [a] - read [c]) == 1 && abs(read b - read d) == 2  = True
    | otherwise = False 

-- returns a list of the white knights
wknights :: String -> Int -> Int -> [String]
wknights _ _ 0 = [] 
wknights (a:b) x y = if(a == 'N') then [show x ++ show y] ++ wknights b (x+1) y
                                  else if(isNum a) then wknights b (x+(digitToInt a)) y
                                  else if(a == '/') then wknights b 1 (y-1)
                                  else wknights b (x+1) y

-- returns a list of the black knights
bknights :: String -> Int -> Int -> [String]
bknights _ _ 0 = [] 
bknights (a:b) x y = if(a == 'n') then [show x ++ show y] ++ bknights b (x+1) y
                                  else if(isNum a) then bknights b (x+(digitToInt a)) y
                                  else if(a == '/') then bknights b 1 (y-1)
                                  else bknights b (x+1) y

wPawns :: String -> Int -> Int -> [String]
wPawns [] _ _ = [] 
wPawns (a:b) x y = if(a == 'P') then [show x ++ show y] ++ wPawns b (x+1) y
                                  else if(isNum a) then wPawns b (x+(digitToInt a)) y
                                  else if(a == '/') then wPawns b 1 (y-1)
                                  else wPawns b (x+1) y
                                  
bPawns :: String -> Int -> Int -> [String]
bPawns [] _ _ = [] 
bPawns (a:b) x y = if(a == 'p') then [show x ++ show y] ++ bPawns b (x+1) y
                                  else if(isNum a) then bPawns b (x+(digitToInt a)) y
                                  else if(a == '/') then bPawns b 1 (y-1)
                                  else bPawns b (x+1) y

bRook :: String -> Int -> Int -> [String]
bRook [] _ _ = [] 
bRook (a:b) x y = if(a == 'r') then [show x ++ show y] ++ bRook b (x+1) y
                                  else if(isNum a) then bRook b (x+(digitToInt a)) y
                                  else if(a == '/') then bRook b 1 (y-1)
                                  else bRook b (x+1) y

wRook :: String -> Int -> Int -> [String]
wRook [] _ _ = [] 
wRook (a:b) x y = if(a == 'R') then [show x ++ show y] ++ wRook b (x+1) y
                                  else if(isNum a) then wRook b (x+(digitToInt a)) y
                                  else if(a == '/') then wRook b 1 (y-1)
                                  else wRook b (x+1) y

rookPos :: Bool -> String -> [String]
rookPos True board = wRook board 1 8
rookPos False board = bRook board 1 8




kPos :: Bool -> String -> String
kPos True a = wKingPosition a 1 8
kPos False a = bKingPosition a 1 8





str2norm :: String -> String
str2norm (a:b) = [matchChar a] ++ b

str2move :: String -> String -> String
str2move a b = str2norm a ++ "-" ++ str2norm b


vMovesKnight :: [String] ->[String] -> String
vMovesKnight [] _ = []
vMovesKnight (a:b) c = iterate a c ++ vMovesKnight b c
                 where
                 iterate :: String -> [String] -> String
                 iterate _ [] = []
                 iterate src (dst:rest) = if(knightVM True src dst) then  str2move src dst ++ "," ++ iterate src rest
                                          else iterate src rest     


vKnightMVs :: Bool -> String -> String
vKnightMVs _ [] = []
vKnightMVs True board = vMovesKnight (wknights board 1 8) ((bPos board 1 8) ++ (fPos board 1 8)) 
vKnightMVs False board = vMovesKnight (bknights board 1 8) ((wPos board 1 8) ++ (fPos board 1 8))       

isNum :: Char -> Bool
isNum '1' = True
isNum '2' = True
isNum '3' = True
isNum '4' = True
isNum '5' = True
isNum '6' = True
isNum '7' = True
isNum '8' = True
isNum _ = False

matchChar1 :: Int -> Char
matchChar1 1 ='a'
matchChar1 2 ='b'
matchChar1 3 ='c'
matchChar1 4 ='d'
matchChar1 5 ='e'
matchChar1 6 ='f'
matchChar1 7 ='g'
matchChar1 8 ='h'