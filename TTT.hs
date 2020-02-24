-- Welsey Blount
-- PLS Graduate Haskell Assignment
-- May 2019


-- main function
main = do  
 let board = ['_','_','_',   
              '_','_','_', 
              '_','_','_']

 printLabeledGrid 
 run board 
    

-- starts with an empty board, does the io, updates the board, passes it back to 
-- itself and repeates until the board is final (win, loss, or draw)
run :: [Char] -> IO ()
run board | final board = printMessage board 
run board = do 
   putStrLn "Your move ('Os'). Type a position (1-9)"   
   printBoard board
   value <- getLine
   let board' = (move board (read value :: Int) False) 
   putStrLn ""  
   printBoard board' 
   putStrLn "" 
 
   let board'' = (move board' (bestMove board')  True)            
   run board''  



-- print the current board 
printBoard :: [Char] -> IO ()
printBoard [a,b,c,d,e,f,g,h,i] = do  
    putStrLn [a,'|',b,'|',c]
    putStrLn [d,'|',e,'|',f] 
    putStrLn [g,'|',h,'|',i]


-- displayed at the beginning. So the user knows which numbers to to which cells
printLabeledGrid = do
  putStrLn "To play, type the number of the cell you wish to move to. You are 'Os'"
  putStrLn "1|2|3"
  putStrLn "4|5|6"
  putStrLn "7|8|9" 
  putStrLn ""  



-- Takes the board and dtermines whcih message is most appropriate to print
printMessage :: [Char] -> IO ()
printMessage board 
    | hueristic board == 10 = putStrLn "You Loose. Better Luck Next Time.."
    | hueristic board == -10 = putStrLn "You Win!"
    | otherwise = putStrLn "Looks like its a draw..."

-- End IO Section --    








-- select which cell to move to next. Found by performing minmax
-- on each potential move and selecting first move that does not lead to a loss
bestMove :: [Char] -> Int
bestMove board 
    | x > 0 = x
    | otherwise = search board board 1 0 
    where x = search board board 1 10
    

-- helper for bestMove. Actually does all the work
search :: [Char] -> [Char] -> Int -> Int -> Int
search [] board index valueWanted = -1
search (x:xs) board index valueWanted
    | final board = 0
    | x == '_' && (currentMoveValue == valueWanted) = index   
    | otherwise = search xs board (index + 1) valueWanted 
    where newBoard = (move board index True)
          currentMoveValue = minmax newBoard newBoard 1 False



-- minmax function (takes current board and potential move and
-- returns total hueristic value of that move)
minmax :: [Char] -> [Char] -> Int -> Bool -> Int 
minmax (x:xs) board index isMax | empty board = 0 
minmax [] board index isMax = returnBound isMax 
minmax (x:xs) board index isMax
    | final board = hueristic board 
    | potentialMove && isMax = max valueCurrentMove valueNextMove
    | potentialMove && not isMax = min valueCurrentMove valueNextMove  
    | otherwise = minmax xs board (index + 1) isMax 
    where potentialMove = x == '_'
          newBoard = move board index isMax
          valueCurrentMove = minmax newBoard newBoard 1 (not isMax)
          valueNextMove = minmax xs board (index + 1) isMax

 

-- helper for minmax, returns either the upper or lower int bouds based on 
-- if the caller is maximizer or minimizer
returnBound :: Bool -> Int
returnBound isMax | isMax = minBound | otherwise = maxBound



-- helper for minmax, returns 0 if board is empty
empty :: [Char] -> Bool
empty [] = True 
empty (x:xs) | x /= '_' = False | otherwise = empty xs



-- returns new list with given index changed to 'X' index(1-9)
move :: [Char] -> Int -> Bool -> [Char]
move [] index isMax = []  
move (x:xs) index isMax
  | index == 1 && isMax = 'X' : xs 
  | index == 1 = 'O' : xs
  | otherwise = x : move xs (index - 1) isMax




-- check if board is at a finalized state (winner or draw)
final :: [Char] -> Bool
final [a,b,c,d,e,f,g,h,i]
    | homog a b c = True
    | homog d e f = True
    | homog g h i = True
    | homog a d g = True
    | homog b e h = True
    | homog c f i = True
    | homog g e c = True
    | homog a e i = True
    | full [a,b,c,d,e,f,g,h,i] = True
    | otherwise = False



-- helper function for final and value. takes 3 chars and returns if  
-- homogenous
homog :: Char -> Char -> Char -> Bool
homog a b c
    | a /= '_' && a == b && b == c = True 
    | otherwise = False



-- helper for final returns true if all cells are full (no remaining moves)
full :: [Char] -> Bool
full a = elem '_' (a) == False 



-- return hueristic value of a board state. X is maximizer
hueristic :: [Char] -> Int
hueristic board | draw board = 0 
hueristic [a,b,c,d,e,f,g,h,i]
    | value a b c /= 0 = value a b c
    | value d e f /= 0 = value d e f
    | value g h i /= 0 = value g h i
    | value a d g /= 0 = value a d g
    | value b e h /= 0 = value b e h
    | value c f i /= 0 = value c f i
    | value g e c /= 0 = value g e c
    | value a e i /= 0 = value a e i
       

-- helper function for hueristic, takes 3 chars and returns -10 if all Os,
-- 10 if all Xs, and 0 if neither 
value :: Char -> Char -> Char -> Int
value a b c
    | homog a b c && a == 'X' = 10
    | homog a b c && a == 'O' = -10
    | otherwise = 0


    
-- helper function for hueristic, returns true if there is a draw
draw :: [Char] -> Bool
draw [a,b,c,d,e,f,g,h,i]
    | homog a b c = False
    | homog d e f = False
    | homog g h i = False
    | homog a d g = False
    | homog b e h = False
    | homog c f i = False
    | homog g e c = False
    | homog a e i = False
    | otherwise = True














