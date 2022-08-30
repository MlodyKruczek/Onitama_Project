{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use :" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Lib
  ( generateGame
  , simulateGame
  , countGames
  ) where

import Data.Char (toUpper, toLower, isSpace)
import Data.List ( elemIndex, intersperse, isPrefixOf, sort, intersect, dropWhileEnd )
import Control.Monad ( when)
import Text.Read (readMaybe)
import Foreign.Marshal.Unsafe ( unsafeLocalState )
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace (trace)


type Cards = [String]
type Player =  [(Int , Int )]
type MoveList = [MoveRow]
type Pown = (Int , Int)
type MoveRow = ((Int,Int),(Int,Int),String)
type Turn = Int
type Game =  (Cards,Player,Player,Turn ,Bool,Bool)

tiger =  map (\(x,y) -> (y,x))[(0,2),(0,-1)]
dragon = map (\(x,y) -> (y,x)) [(-2,1), (-1,-1), (1,-1), (2,1)]
frog = map (\(x,y) -> (y,x)) [(-2,0), (-1,1), (1,-1)]
rabbit = map (\(x,y) -> (y,x))  [(-1,-1), (1,1), (2,0)]
crab = map (\(x,y) -> (y,x))  [(-2,0 ), (0, 1), (2, 0)]
elephant = map (\(x,y) -> (y,x)) [(-1, 0), (-1, 1), (1, 1), (1, 0)]
goose = map (\(x,y) -> (y,x)) [(-1, 1), (-1, 0), (1, 0), (1, -1)]
rooster =  map (\(x,y) -> (y,x)) [(-1, 0), (-1, -1), (1, 1), (1, 0)]
monkey = map (\(x,y) -> (y,x)) [(1, -1), (1, 1), (-1, 1), (-1, -1)]
mantis = map (\(x,y) -> (y,x)) [(-1, 1), (0, -1), (1, 1)]
horse = map (\(x,y) -> (y,x)) [(-1, 0), (0, 1), (0, -1)]
ox = map (\(x,y) -> (y,x)) [(1, 0), (0, 1), (0, -1)]
crane = map (\(x,y) -> (y,x)) [(-1, -1), (0, 1), (1, -1)]
boar = map (\(x,y) -> (y,x)) [(-1, 0), (0, 1), (1, 0)]
eel = map (\(x,y) -> (y,x)) [(-1, 1), (-1, -1), (1, 0)]
cobra = map (\(x,y) -> (y,x)) [(1, 1), (1, -1), (-1, 0)]

moveVectors = [(tiger,"Tiger"),(dragon,"Dragon"),(frog,"Frog"),(rabbit,"Rabbit"),(crab,"Crab"),(elephant,"Elephant"),(goose,"Goose"),(rooster,"Rooster"),(monkey,"Monkey"),(mantis,"Mantis"),(horse,"Horse"),(ox,"Ox"),(crane,"Crane"),(boar,"Boar"),(eel,"Eel"),(cobra,"Cobra")]

names =  map capitalized ["tiger","dragon","frog","rabbit","crab","elephant","goose","rooster","monkey","mantis","horse","ox","crane","boar","eel","cobra"]


sortPairs list = l1 ++ l2 ++ [last list]
    where
    l1 = sort $ take 2 list;
    l2 = sort $ take 2  $ drop 2 list


getMoveName ((b1,b2),(c1,c2),a) =  a
getPownPos ((b1,b2),(c1,c2),a) = (b1,b2)
getPownMove ((b1,b2),(c1,c2),a) = (c1,c2)


printResNotOver game =  ("(" ++ show deck ) ++ "," ++ show p1' ++ "," ++ show p2' ++ "," ++ int2char t ++ ","++  bool2char sm1 ++ "," ++ bool2char sm2 ++ ")"
    where
        deck =  map capitalized x;
        p1 = getP1Powns game;
        p2 = getP2Powns game;
        p1' = (p1 !! 0): (sort $ tail' p1)
        p2' = (p2 !! 0): (sort $ tail' p2)
        t = getTurn game;
        sm1 = getP1SMove game;
        sm2 = getP2SMove game
        x = (sort (take 2  $ getCards game )) ++ (sort (take 2 (drop 2 $ getCards game))) ++ (drop 4 $ getCards game )

                                                             --init( init(listIntToString p'))
sortPowns [] = []
sortPowns p1 = (p1 !! 0): (sort $ tail' p1)

printResGameOverP1Wins game = ("(" ++ show deck ) ++ ",[" ++ init( init(listIntToString p')) ++ "],[" ++ "]," ++ int2char t ++ ","++  bool2char sm1 ++ "," ++ bool2char sm2 ++ ")"
    where
        deck = map capitalized x;
        p = getP1Powns game;
        p' = (p !! 0): (sort $ tail' p)
        t = getTurn game;
        sm1 = getP1SMove game;
        sm2 = getP2SMove game
        x = (sort (take 2  $ getCards game )) ++ (sort (take 2 (drop 2 $ getCards game))) ++ (drop 4 $ getCards game )

printResGameOverP2Wins game = ("(" ++ show deck ) ++ ",[" ++ "],[" ++ init( init(listIntToString p')) ++ "]," ++ int2char t ++ ","++  bool2char sm1 ++ "," ++ bool2char sm2 ++ ")"
    where
        deck =  map capitalized  x;
        p = getP2Powns game;
        p' = (p !! 0): (sort $ tail' p)
        t = getTurn game;
        sm1 = getP1SMove game;
        sm2 = getP2SMove game;
        x = (sort (take 2  $ getCards game )) ++ (sort (take 2 (drop 2 $ getCards game))) ++ (drop 4 $ getCards game )


listIntToString [] = " "
listIntToString  xs = "(" ++ int2char (fst $ head xs) ++ "," ++ int2char (snd  $ head xs) ++ ")," ++ listIntToString (tail' xs)

tail' [] = []
tail' (x:xs) = xs

head' :: [a] -> [a]
head' (x:xs) = [x]
head' [] = []


capitalized :: [Char] -> [Char]
capitalized [] = []
capitalized (h:t) = Data.Char.toUpper h : map Data.Char.toLower t

getCards :: Game -> Cards
getCards (a,p1,p2,t,sm1,sm2) = a

getP1Powns :: (Cards, Player, Player, Turn ,Bool, Bool ) -> Player
getP1Powns (a,p1,p2,t,sm1,sm2) = if null p1 then [] else p1

getP2Powns :: (Cards, Player, Player, Turn ,Bool, Bool ) -> Player
getP2Powns (a,p1,p2,t,sm1,sm2) = if null p2 then [] else p2

getTurn :: (Cards, Player, Player, Turn ,Bool, Bool ) -> Turn
getTurn (a,p1,p2,t,sm1,sm2) = t

getP1SMove :: (Cards, Player, Player, Turn ,Bool, Bool ) -> Bool
getP1SMove (a,p1,p2,t,sm1,sm2) = sm1

getP2SMove :: (Cards, Player, Player, Turn ,Bool, Bool ) -> Bool
getP2SMove (a,p1,p2,t,sm1,sm2) = sm2


unSuperMove :: [Char] -> [Char]
unSuperMove moveName =
    last $ words moveName


superDeck :: [String ] -> [String ]
superDeck deck = map ("Super " ++) deck

isMoveVectorValid :: [Char] -> (Int, Int) -> Int ->  Bool
isMoveVectorValid mv move t = case mv of  
     "Tiger"      -> check' tiger
     "Dragon"     -> check' dragon
     "Frog"       -> check' frog
     "Rabbit"     -> check' rabbit
     "Crab"       -> check' crab
     "Elephant"   -> check' elephant
     "Goose"      -> check' goose
     "Rooster"    -> check' rooster
     "Monkey"     -> check' monkey
     "Mantis"     -> check' mantis
     "Horse"      -> check' horse
     "Ox"         -> check' ox
     "Crane"      -> check' crane
     "Cobra"      -> check' cobra
     "Boar"       -> check' boar
     "Eel"        -> check' eel
    where
        check ls = elem move ls;
        check' ls  = if t == 0 then check ls else check $ map (\(x,y) -> (-x,-y)) ls

isSuperMoveVectorValid :: Pown -> Pown -> Bool
isSuperMoveVectorValid (x1,y1) (x2,y2) = (a == 0 || a == 1 || a == -1) && (b == 0 || b == 1 || b == -1)
    where a = x2-x1; b = y2-y1


isMovePossible :: Pown -> Player -> Bool
isMovePossible (a,b) player = (a >= 0 && a <=4  && b >= 0 && b <=4) && notElem (a,b) player

isMoveNameValid :: (Num a1, Eq a1, Eq a2) => [a2] -> a2 -> a1 -> Bool
isMoveNameValid deck movename turn = if turn == 0 then  movename `elem` init ( init (init deck))   else movename `elem` drop 2 (init deck )

makeMove :: (Num a1, Eq a1, Eq a2) => a2 -> a2 -> [a2] -> [a2] -> a1 -> [a2]
makeMove pownPos pownMove player1 player2 turn =
    if turn == 0
    then
        makeMove' pownPos pownMove player1
    else
        makeMove' pownPos pownMove player2
    where
        makeMove' pownPos pownMove player =
            setAt x pownMove player
                where Just x = elemIndex pownPos player

sndPlayer :: (Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)] -> [(a, b)]
sndPlayer pownMove player =
    if pownMove `elem` player
    then setAt x (-1,-1) player
    else  player
        where Just x = elemIndex pownMove player

swapElementsAt :: Cards -> String  -> Cards
swapElementsAt list movename = list1 ++ [list !! 4] ++ list2 ++ [list !! a] ++ list3
    where   list1 = take a list;
            list2 = drop (succ a) (take 4 list);
            list3 = drop (succ 4) list;
            Just a = elemIndex movename list


setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

bool2char :: Show a => a -> String
bool2char a =show a

int2char :: (Eq a, Num a) => a -> [Char]
int2char  x= case x of 
      0    ->  "0"
      1    ->  "1"
      2    ->  "2"
      3    ->  "3"
      4    ->  "4"



isMoveStateValid :: MoveRow -> Turn -> Player -> Player -> Cards -> Bool -> Bool -> IO Bool
isMoveStateValid move turn p1 p2 deck sm1 sm2  = do
    let movename = getMoveName move
    let pownPos= getPownPos move
    let pownMove = getPownMove move
    let pownMoveVector = calcMove pownPos pownMove

    let validname = if elem movename (superDeck deck)
        then
            isMoveNameValid  ((superDeck deck))  movename turn
        else
            isMoveNameValid  (map capitalized deck) movename turn

    let validNoOverlapingPowns = allDifferent $  filter (\(x,y) -> (x,y) /=(-1,-1)) $ p1++p2

    let validSuperMove = ((notElem movename (superDeck deck)) || (if turn == 0 then not sm1 else not sm2))

    let validMoveVector = if movename `elem` (superDeck deck) then isSuperMoveVectorValid pownPos pownMove else  isMoveVectorValid movename pownMoveVector turn

    let validInitalMoveState = if turn == 0 then  pownPos `elem` p1 else pownPos `elem` p2

    let validMovePossible =  if turn == 0 then isMovePossible pownMove p1 else isMovePossible pownMove p2


    return ( validname && validNoOverlapingPowns && validMoveVector && validInitalMoveState && validMovePossible && validSuperMove)

arePownsOnGrid :: Player -> Bool
arePownsOnGrid  [] = True
arePownsOnGrid ((a,b):xs)=
    btw a && btw b && arePownsOnGrid xs
        where btw a = a >= 0 && a <= 4

allDifferent :: Eq a => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

calcMove (x1,y1) (x2,y2) = (x2-x1,y2-y1)
simulateGameAux :: Game -> MoveList -> IO String
simulateGameAux game moves = do
    let deck = getCards game
    let superdeck = superDeck deck
    let p1 = getP1Powns game
    let p2 = getP2Powns game
    let turn = getTurn game
    let sm1 = getP1SMove game
    let sm2 = getP2SMove game

    if null moves
    then do  gg p1 p2 game
    else do
        let move = head moves
        let movename = getMoveName move
        let pownPos= getPownPos move
        let pownMove = getPownMove move
        let movename' = if movename `elem` superdeck then unSuperMove movename else movename

        validMoveState <- isMoveStateValid move turn p1 p2 deck sm1 sm2
        if not validMoveState
            then
                return $ "InvalidMove " ++ (show  move)
            else do
            let firstPlayerAfterMove = makeMove pownPos pownMove p1 p2 turn
            let sndplayerAfterMove = sndPlayer pownMove (if turn == 0 then  p2 else p1 )

            let deck' =  map capitalized $ sortPairs $ swapElementsAt deck movename'

            let turn' = if turn == 0 then 1 else 0
            let p1' =  if turn == 0 then firstPlayerAfterMove else sndplayerAfterMove
            let p2' =  if turn == 0 then sndplayerAfterMove  else  firstPlayerAfterMove
            let sm1' = if not sm1 && elem movename superdeck && turn == 0 then not sm1 else sm1
            let sm2' = if not sm2 && elem movename superdeck && turn == 1 then not sm2 else sm2
            let game' = (deck',filter (\(a,b)->(a,b)/=(-1,-1)) $ head p1':(sort $ tail p1'),filter (\(a,b)->(a,b)/=(-1,-1)) $ head p2':(sort $ tail p2'),turn',sm1',sm2')

            if tail' moves /= [] && isGameOver p1' p2' /=  0
                then return $ "InvalidMove " ++ (show $ head $ tail' moves)
                else do
                    if tail' moves /= [] && isGameOver p1' p2' ==  0
                        then do
                            simulateGameAux game' $ tail' moves
                        else do
                            gg p1' p2' game'


gg :: Monad m => Player -> Player -> Game -> m [Char]
gg p1' p2' game' =
    case isGameOver p1' p2' of
        2 -> return $ printResGameOverP2Wins game'
        1 -> return $ printResGameOverP1Wins game'
        _ -> return (printResNotOver game')


isGameOver :: [(Int, Int)] -> [(Int, Int)] -> Int
isGameOver p1 p2
  | p1 == [] = 2
  | p2 == [] = 1
  | head p1 == (-1,-1) = 2
  | head p2 == (-1,-1) = 1
  | head p1 == (4,2) = 1
  | head p2 == (0,2) = 2
isGameOver _ _  = 0



readMoves1 :: String  -> Maybe (Pown, Pown, String) 
readMoves1 m = 
    readMaybe  m :: Maybe MoveRow


readMoves :: Traversable t => t String -> Maybe (t (Pown, Pown, String))
readMoves movelist = mapM readMoves1 movelist



trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

readGame :: FilePath-> IO ([Char], Maybe Game)
readGame fname = do
    contents <- readFile fname
    let nonempty' = filter (\x->  x /= "" && x /= "    ") (lines contents)
    let nonempty'' = map trim nonempty'
    let nonempty =  filter (\x->  x /= "" && x /= "    ") ( nonempty'')


    if not (null nonempty)
    then do
        game <- createIGameState $ head nonempty
        if isNothing $ snd game
        then return (fst game,Nothing )
        else do
            let err = fst game
            return (err,   snd game )

    else do
        return ("InvalidFormat",Nothing )

readGame' :: FilePath -> IO ([Char], Maybe [(Pown, Pown, String)])
readGame' fname = do

    --let fname = "a.txt"
    contents <- readFile fname --"a.txt"
    let nonempty' = filter (\x->  x /= "" && x /= "    ") (lines contents)
    let nonempty'' = map trim nonempty'
    let nonempty =  filter (\x->  x /= "" && x /= "    ") ( nonempty'')
    if null $ tail' nonempty
        then return ("OK", Just [])
        else do
        let moveList = tail' nonempty

        case readMoves moveList of
            Nothing  -> return ("InvalidFormat",Nothing)
            Just x -> return ("OK",Just x)

slist :: [Char] -> [Char]
slist [] = ""
slist l = if head l == '\\' then slist $ tail l else head l :( slist $ tail l)

createIGameState :: [Char]-> IO ([Char], Maybe Game)
createIGameState pureLine =
        if  (not $ null pureLine) && length  (filter (\x -> x /= ' ' && x/=  '\t') pureLine) >= 53
        then do
            let pureLine' = filter (\x -> x /= ' ') pureLine
            let lowerLine = takeWhile (\x->x/=']' )  pureLine'
            let finalLine = filter (\x -> x /= ' ') lowerLine
            let xd =  drop 1 $ init $ show ((drop 1 finalLine)++"]")
            let mcards = readMaybe ( slist xd)  :: Maybe [String]

            if isNothing mcards
            then do

                return ("InvalidFormat",Nothing)
            else do

                let lineTotakePawns = prepareTotakePownsFirst (drop (length (lowerLine)) pureLine'  )
                let cards = fromJust mcards

                if length  cards  /= 5 || not ( checkCards' cards &&  lineTotakePawns  /= "InvalidForamt") || cards /= sortPairs cards   ||  take 2 (drop (length (lowerLine)) pureLine'  ) /= "],"
                then
                    if length  cards  /= 5 || intersect cards names /= cards
                    then do
                        return ("InvalidState",Nothing )
                    else do
                        return ("InvalidFormat",Nothing )
                else
                    let forP1 = toPownsLine lineTotakePawns
                        forP2 = prepareTotakePownsFirst (toPownsLine (drop (length forP1) lineTotakePawns))
                        player1powns' = readMaybe forP1 :: Maybe Player
                        player2powns' = readMaybe forP2 :: Maybe Player in
                    if isNothing player1powns' || isNothing player2powns'
                    then 
                        return ("InvalidFormat",Nothing)
                    else
                        let player1powns = fromJust player1powns'
                            player2powns = fromJust player2powns'
                            sp1 = sortPowns player1powns
                            sp2 = sortPowns player2powns
                            validPownsOnGrid = arePownsOnGrid  player1powns && arePownsOnGrid player2powns in
                        if not validPownsOnGrid || length player1powns > 5 || length player2powns > 5 || sp1 /= player1powns  || sp2 /= player2powns
                        then do
                            if sp1 /= player1powns  || sp2 /= player2powns
                            then return  ("InvalidFormat",Nothing)
                            else return ("InvalidState",Nothing)
                        else do
                            let tsm1 = turnAndSuperMoves $ '(':drop ((length $ forP1 ++ forP2) + 2) lineTotakePawns
                            if isJust tsm1 then
                                let tsm = fromJust tsm1
                                    turn = fst tsm
                                    sm1 = fst $ snd tsm
                                    sm2 = snd $ snd tsm in
                                if turn  < 0 || turn >1
                                    then do
                                        return ("InvalidFormat",Nothing)
                                    else do
                                        let game = (cards, player1powns, player2powns, turn ,sm1, sm2)
                                        return ( "OK" , Just game )
                            else do
                                return ("InvalidFormat",Nothing)
        else do
            return ("InvalidFormat",Nothing)

turnAndSuperMoves' :: String -> Maybe (Int, Bool, Bool)
turnAndSuperMoves'  ls =  readMaybe ls :: Maybe (Int,Bool,Bool)

turnAndSuperMoves :: [Char] ->  Maybe (Int,(Bool,Bool))
turnAndSuperMoves (l:ls) = do
    let a = turnAndSuperMoves'  (l:ls)

    if isNothing a then
            Nothing
        else
            let row = fromJust a
                turn = f row
                sm1 = s row
                sm2 = t row in
            return (turn, (sm1, sm2))
    where
        f (a,b,c) = a
        s (a,b,c) = b
        t (a,b,c) = c


toPownsLine :: [Char] -> [Char]
toPownsLine xs = takeWhile (\a -> a/=']') xs ++ "]"

checkCards' :: [[Char]] -> Bool
checkCards' c  =
    vn (head c) && vn (c !! 1) && vn (c !! 2) && vn (c !! 3) && vn (c !! 4)

vn :: [Char] -> Bool
vn l = l `elem` names

prepareTotakePownsFirst :: [Char] -> [Char]
prepareTotakePownsFirst (x:xs) =  case x of 
     ']'  -> prepareTotakePownsFirst xs
     ','  -> prepareTotakePownsFirst xs
     '['  -> x:xs

prepareTotakePownsFirst []  = []

simulateGame :: FilePath -> IO [Char]
simulateGame filePath = do

    input <- readGame filePath
    let error = fst input
    let gamerow' = snd input
    if isNothing  gamerow' || error /=  "OK"
    then return error
    else do 
        let gamerow = fromJust gamerow'    
        input2 <- readGame' filePath
        let error1 = fst input2
        let moves = snd input2
        if isJust moves && error1 == "OK"
        then
            if (null (getP1Powns gamerow) && null (getP2Powns gamerow)) || (getP1Powns gamerow == getP2Powns gamerow)
            then return "InvalidState"
            else do simulateGameAux gamerow $ fromJust moves
        else
            return error1


pickCards :: Int -> Cards
pickCards ms  = do
    let a = 11
    let c1 =  last $ take ms $ concat $ ( repeat names)
    let c2 = last $ take (ms + a) $ concat $ repeat ((filter (\x -> x/= c1)) names)
    let c3 = last $ take (ms + a*2) $ concat $ repeat ((filter (\x -> x/= c1 && x/= c2)) names)
    let c4 = last $ take (ms + a*3) $concat $ repeat ((filter (\x -> x/= c1 && x/= c2 && x/= c3)) names)
    let c5 = last $ take (ms + a*4) $ concat $ repeat ((filter (\x -> x/= c1 && x/= c2 && x/= c3 && x/= c4)) names)
    [c1,c2,c3,c4,c5]

pickPowns :: Int -> ([(Int, Int)], [(Int, Int)])
pickPowns seed = do
    let p1len = read ( show $ mod (seed*417317) 5)  :: Int
        p2len = read ( show $ mod (seed*999431) 5)  :: Int
        p1 = pickPowns' p1len seed [] $ tt
        p2 = pickPowns' p2len seed [] $ reverse (snd p1)
    if p1len+p2len==0
    then pickPowns $ seed*3
    else do   (fst p1,fst p2)
    where
        tt = do
            a <-  [0..4]
            b <- [0..4]
            [(a,b)]

smGrid :: [(Int, Int)]
smGrid = do
    c <- [-1..1]
    c1 <- [-1..1]
    filter ((\x->x/= (0,0))) [(c,c1)]

reverseInt :: Int -> Integer
reverseInt = read . reverse . show

pickPowns' :: Int -> Int -> [(Int, Int)]  -> [(Int, Int)] -> ([(Int, Int)] , [(Int, Int)] )
pickPowns' l seed list ml=
    if l == 0 then (list,ml) else do
        let x = last (take ((mod seed 17 )) $ concat $ repeat ml)
        let ml' = filter (\(a,b) -> (a,b) /= x ) ml
        pickPowns' (l-1) seed (x:list) ml'

genMove ::  Int -> Int -> Game -> MoveList -> IO MoveList
genMove seed n game ml = do
    let turn = getTurn game
    let deck = if turn == 0 then take 2 $ getCards game else take 2 (drop 2 $ getCards game)
    let pl = if turn == 0 then getP1Powns game else getP2Powns game
    let p2 = getP2Powns game
    let p1 = getP1Powns game
    if n>0 && (isGameOver p1 p2 ) == 0 then do

        let sm = if turn == 0 then  getP1SMove game else getP2SMove game

        let a'= [(x,x2,y) | x <- pl , (x2,test) <- moveVectors,  y<- deck, y == test  ]

        let a = if not sm
            then  a' ++ [( x,smGrid,"Super " ++ y) | x <- pl ,  y<- deck ]
            else  a'


        moveList <- allPossibleValidMoves a game

        let move = last $ take seed $ concat $ (repeat moveList)

        game' <-simulateGameAux game [move]

        let gameF = read game' :: Game

        if isGameOver (getP1Powns  gameF) (getP2Powns  gameF ) == 0
            then
                genMove (seed+2) (n-1) (gameF) (move:ml)
            else
                return (move:ml)

    else
        return ml

allPossibleValidMoves :: [((Int, Int), [(Int, Int)], String)] -> Game -> IO [MoveRow]
allPossibleValidMoves list@((a,b,c):ls) game = do
    let turn = getTurn game
    let powns =  map f list
    let names =  map t list
    let vectors = map s list
    b <- allPossibleMoves powns vectors names [] turn
    allPossibleValidMoves' b game
    where
        f (a,b,c) = a
        s (a,b,c) = b
        t (a,b,c) = c

allPossibleValidMoves' :: [MoveRow] -> Game -> IO [MoveRow]
allPossibleValidMoves' [] _ =  return []
allPossibleValidMoves' (x:xs) game = do
    let turn = getTurn game
    let deck = getCards game
    let p2 = getP2Powns game
    let p1 = getP1Powns game
    let sm1 = getP1SMove game
    let sm2 = getP2SMove game

    test <- (isMoveStateValid x turn p1 p2 deck sm1 sm2)
    if test
    then do
        a<- (allPossibleValidMoves' xs game)
        return (x:a)
    else
        allPossibleValidMoves' xs game


allPossibleMoves :: [(Int,Int)] -> [[(Int,Int)]] -> [String] -> [MoveRow] -> Int -> IO MoveList
allPossibleMoves [] _ _ l _ = return  l
allPossibleMoves (p:ps) vectors@(v:vs) (n:ns) l t= do

    if  (concat $ head' (vectors)) /= []
    then do
        allPossibleMoves (p:ps) ((tail' $ concat $ head' (vectors)):vs) (n:ns) (l ++ [(p, sumTup p  (head' $ concat $ head' vectors) t,   n)]) t
    else do
        allPossibleMoves (ps) (vs) (ns) l t



sumTup :: (Eq a1, Num a1, Num a2, Num a3) => (a2, a3) -> [(a2, a3)] -> a1 -> (a2, a3)
sumTup (a,b) ((a2,b2):as) t = if t == 0 then (a+a2,b+b2) else (a-a2,b-b2)


gameToString :: String -> MoveList -> String  -> String
gameToString game ml mls= do
    if length ml == 0 then ((game ++ "\n")++ mls)
    else do gameToString game (tail' ml) (( drop 1 $ init(show ( head' ml)) )++"\n"++mls)


generateGameAux :: Integer -> Integer -> IO String
generateGameAux seed' n = do

    let seed = read ( show seed') :: Int
    let moveS = read $ show $ mod (seed) (length  names) :: Int
    let cards = pickCards $ moveS+ 1
    let p1p2 = pickPowns $moveS +1
    let t = (seed `mod` 2)
    let sm1 = ((seed+1) `mod` 2) == 1
    let sm2 = (reverseInt seed `mod` 2) == 1
    let game = (sortPairs cards, fst p1p2, snd  p1p2, t, sm1, sm2   )
    game' <-simulateGameAux game []
    gg <- createIGameState game'
    ml <- genMove seed (read $ show n :: Int) game []
    let a = gameToString game' ml []
    return a


generateGame :: Integer -> Integer ->  String
generateGame seed n =  unsafeLocalState $ generateGameAux seed n

countGames :: Integer -> FilePath -> IO String
countGames n filePath = do

    input <- readGame filePath

    let error = fst input
    let gamerow = snd input
    if isJust gamerow
        then do
            if n == 0 then return "(1,0,0)"
                else do
                a<-countGamesAux n $ fromJust gamerow
                return $ show a
        else
            return error

countGamesAux :: (Ord a, Num a) => a -> Game -> IO (Int, Int, Int)
countGamesAux n game  = do
    let turn = getTurn game
    let deck = if turn == 0 then take 2 $ getCards game else take 2 (drop 2 $ getCards game)
    let pl = if turn == 0 then getP1Powns game else getP2Powns game
    let p2 = getP2Powns game
    let p1 = getP1Powns game
    if n>0 && (isGameOver p1 p2 ) == 0 then do
        let sm = if turn == 0 then  getP1SMove game else getP2SMove game
        let a'= [(x,x2,y) | x <- pl , (x2,test) <- moveVectors,  y<- deck, y == test  ]
        let ml = if not sm
            then  a' ++ [( x,smGrid,"Super " ++ y) | x <- pl ,  y<- deck ]
            else  a'
        moveList <- allPossibleValidMoves ml game
        allPossibleGames' <- allPossibleGames moveList  [] game
        countGamesAux' n allPossibleGames'
    else
        if (isGameOver p1 p2 ) == 2 then
            return (1,0,1)
        else if (isGameOver p1 p2 ) == 1 then
            return (1,1,0)
        else
            return (1,0,0)

allPossibleGames ::  [MoveRow] -> [Game] -> Game -> IO [Game]
allPossibleGames  []    gl   _    =  return gl
allPossibleGames (m:ml)  gl (game) = do
    game' <- simulateGameAux game [m]
    let x = read game' :: Game
    ( allPossibleGames ml  (x:gl) game)

countGamesAux' :: (Ord t, Num t) => t -> [Game] -> IO (Int, Int, Int)
countGamesAux' _ [] = return (0,0,0)
countGamesAux' n (g:games) =
    sumTri (unsafeLocalState $ countGamesAux (n-1) g) (unsafeLocalState $ countGamesAux' n games)

sumTri ::  (Int , Int, Int) -> (Int, Int, Int) -> IO (Int, Int, Int)
sumTri  ( (a,b,c)) ( (x,y,z)) =  return (a+x,b+y,c+z)