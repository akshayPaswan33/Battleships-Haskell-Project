import Data.List.Split
import Data.Char

add_space :: String -> String
add_space ms = concat[ ([x]++"   ") | x<-ms]

add_space' :: String -> String
add_space' ms = concat[ ([x]++" ") | x<-ms]

printArray arr =    if arr==[]
                    then do putStrLn ""
                    else do
                            putStrLn (add_space(take 11 arr))
                            putStrLn ""
                            printArray (drop 11 arr)

printArray' arr =   if arr==[]
                    then do putStr ""
                    else do
                            putStrLn (add_space' (take (length arr) arr))
                            --putStrLn ""
                            printArray (drop (length arr) arr)


extract x z = [b |(a,b)<-z,x==a]

belongsto y [] = False
belongsto y (x:xs)= (y==x) || (belongsto y xs)

g' x z = x:[if belongsto y (extract x z) then 'S' else '~'|y<-['0'..'9']]
f z = ['/':['0'..'9']]++[g' x z| x<-['A'..'J']]
h z = concat (f z)

playersname :: IO (String,String)                       -- It takes player's name from the user (terminal) and binds them into a pair and returns it.
playersname = do
                putStrLn "Who is Player 1 ?"
                player1 <- getLine
                putStrLn "Who is Player 2 ?"
                player2 <- getLine
                return (player1,player2)

convert_positions' :: (Char,Char) -> ((Char,Char),Int)
convert_positions' (a,b) = ((a,b),0)

convert_positions :: [[(Char,Char)]] -> [[((Char,Char),Int)]]
convert_positions xss = [ map convert_positions' xs|xs <- xss]

poslist :: Int -> Char -> Char -> Char -> [(Char,Char)]
poslist 0 c i d = []
poslist s c i d = if (d=='H')
                then (c,i):(poslist (s-1) c (chr ((ord i)+1)) d)
                else (c,i):(poslist (s-1) (chr ((ord c)+1)) i d)

validate1 :: Char -> IO Bool
validate1 c = if ((belongsto c ['A'..'J']) == True) then return True else return False

validate2 :: Char -> IO Bool
validate2 i = if ((belongsto i ['0'..'9']) == True) then return True else return False

validate3 :: Char -> IO Bool
validate3 d = if ((belongsto d ['H','V']) == True) then return True else return False

validate5 :: String -> Char -> Char -> IO Bool
validate5 f c i = let v = (((fromEnum c)-64)*11+(fromEnum i)-47)
                  in if (v<121 && (f!!v == '~')) then return True else return False

validate4 :: String -> Int -> Char -> Char -> Char -> IO Bool
validate4 f 0 c i d = return True
validate4 f s c i d = do
                        valid1 <- validate1 c
                        valid2 <- validate2 i
                        valid3 <- validate5 f c i
                        v <- if (d=='H')
                             then do
                                   v'<-validate4 f (s-1) c (chr ((ord i)+1)) d
                                   return v'
                             else do
                                   v'<-validate4 f (s-1) (chr ((ord c)+1)) i d
                                   return v'
                        return (valid1 && valid2 && valid3 && v)


positions' :: String -> Int -> Char -> Char -> Char -> IO [(Char,Char)]
positions' f s c i d = do
                        valid <-validate4 f s c i d
                        if (valid==True)
                        then return (poslist s c i d)
                        else positions f s

positions :: String->Int->IO [(Char,Char)]                     --It takes size of that ship which needs placement currently in the player's map and asks user from the terminal to fill the list in the format [(row ind, column ind)] 
positions f s = do
                 c<-getChar
                 i<-getChar
                 --putChar ";"
                 d<-getChar
                 valid1 <- validate1 c
                 valid2 <- validate2 i
                 valid3 <- validate3 d
                 if (valid1 && valid2 && valid3)
                 then positions' f s c i d
                 else positions f s

change_xs :: String->(Char,Char)->IO String
change_xs f x = let v = (((fromEnum (fst x))-64)*11+(fromEnum (snd x))-47)
                in return (replace v 'S' f)

modify :: String -> [(Char,Char)] -> IO String
modify f [] = return f
modify f (x:xs) = do
                   ys <- change_xs f x
                   zs <- modify ys xs
                   return zs

printmaps :: String -> [(Char,Char)] -> IO ()                       -- It takes the player's name and list of (row ind,column ind) and prints the players' field map after making appropriate changes at the positions given in the list
printmaps name z = do
                    putStrLn ("Current map of "++name++" ships")
                    printArray (h z)

move :: String -> (Char,Char) -> String -> IO Bool                      -- checks whether the move made by the current player is a hit or a miss
move player (c,i) f = if (f!!(((fromEnum c)-64)*11+(fromEnum i)-47))=='S' then return True else return False

replace pos newval list = take pos list ++ newval : drop (pos+1) list

change :: String -> (Char,Char) -> Bool -> String -> IO String                      -- change the displaying field of player's opponent on the basis of 'move' output is true or false
change player (c,i) hit f = let v = (((fromEnum c)-64)*11+(fromEnum i)-47)
                        in if hit==True then return (replace v 'X' f) else return (replace v 'O' f)

valid_box_attack :: String -> IO (Char,Char)
valid_box_attack g2= do
                      c<-getChar
                      i<-getChar
                      v1<- validate1 c
                      v2<- validate2 i
                      if (v1 && v2) == True
                      then do
                             v5<- validate5 g2 c i
                             if v5==True
                             then return (c,i)
                             else valid_box_attack g2 
                      else valid_box_attack g2

pads :: Int -> String
pads x = [ '\n'|y<-[1..x]]

shiplist_string :: [[((Char,Char),Int)]] -> String
shiplist_string xss = [ chr(48+(length x))|x<-xss]

func' :: ((Char,Char),Int)->[((Char,Char),Int)]->[((Char,Char),Int)]
func' ((c,i),0) xs = [  if ((c,i),0)==x then ((c,i),1) else x| x <- xs]

func :: ((Char,Char),Int) -> [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
func ((c,i),0) xss = [ func' ((c,i),0) xs|xs<-xss]

change_pos :: Bool -> (Char,Char) -> [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
change_pos strike (c,i) xss = if strike
                              then func ((c,i),0) xss
                              else xss

isSunked :: [((Char,Char),Int)] -> Bool
isSunked [] = True
isSunked (x:xs) = (snd x == 1) && (isSunked xs)

isnotSunked :: [((Char,Char),Int)] -> Bool
isnotSunked [] = False
isnotSunked (x:xs) = (snd x == 0) || (isnotSunked xs)

nSunk_list:: [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
nSunk_list xss = [ xs| xs<-xss, isSunked xs]

notSunk_list :: [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
notSunk_list xss = [ xs| xs<-xss, isnotSunked xs]

flag_sunk :: Int -> [[((Char,Char),Int)]] -> Int
flag_sunk now curr_pos_list = if now == (length (nSunk_list curr_pos_list)) then 0 else 1

charp_intp :: Char->Char-> Int
charp_intp c i = (((fromEnum c)-64)*11+(fromEnum i)-47)

get_int_pos_l' :: [((Char,Char),Int)] -> [Int]
get_int_pos_l' xs = [ charp_intp a b|((a,b),c)<-xs]

get_int_pos_l :: [[((Char,Char),Int)]] -> [Int]
get_int_pos_l xss = concat [ get_int_pos_l' xs|xs<-xss] 

replace_x_y :: String -> [Int] -> String
replace_x_y mp [] = mp
replace_x_y mp (x:xs) = replace_x_y (replace x 'Y' mp) xs


playgame :: (String,String)->[[((Char,Char),Int)]]->[[((Char,Char),Int)]]->String -> String -> String -> String->Int -> Int -> IO ()                 -- play the game
playgame (p1,p2) pos1 pos2 f1 f2 g1 g2 c1 c2 = if c1==17 then putStrLn ("\n"++p1++" wins")
                               else if c2==17 then putStrLn ("\n"++p1++" wins")
                               else do
                                     putStrLn ("\nCurrent map of "++p2++" ships\n")
                                     printArray g2
                                     putStrLn ("your move "++p1)
                                     remain_ships <- return (notSunk_list pos2)
                                     print_ships <- return (shiplist_string remain_ships)
                                     putStrLn "Ships of following sizes are remaining"
                                     printArray' print_ships
                                     box <- valid_box_attack g2
                                     strike <- move p1 box f2
                                     pos2' <- return (change_pos strike box pos2)
                                     g2' <- change p1 box strike g2
                                     if strike then putStrLn "\nIt's a hit\n" else putStrLn "\nIt's a miss\n"
                                     g2' <- return (replace_x_y g2' (get_int_pos_l (nSunk_list pos2')))
                                     putStrLn ("\n"++p2++" ships")
                                     printArray g2' --players' name
                                     --putStrLn "\n"
                                     --if strike then putStrLn "It's a hit\n" else putStrLn "It's a miss\n"
                                     if strike then playgame (p1,p2) pos1 pos2' f1 f2 g1 g2' (c1+1) c2 else playgame (p2,p1) pos2' pos1 f2 f1 g2' g1 c2 c1



main :: IO ()
main = do
        players <- playersname
        putStrLn "\n"
        
        putStrLn (fst players ++ " place your ships man!")
        
        printmaps (fst players) []
        putStrLn ("place ships of size 5")
        pos_5_p1 <- positions (h []) 5
        f' <- modify (h []) pos_5_p1
        putStrLn ""
        coordinatesOfShips_p1 <- return [pos_5_p1]
        
        printmaps (fst players) (concat coordinatesOfShips_p1)
        putStrLn ("\nplace ships of size 4")
        pos_4_p1 <- positions f' 4
        f' <- modify f' pos_4_p1
        coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1]
        
        printmaps (fst players) (concat coordinatesOfShips_p1)
        putStrLn ("\nplace ships of size 3")
        pos_31_p1 <- positions f' 3
        f' <- modify f' pos_31_p1
        coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1]

        printmaps (fst players) (concat coordinatesOfShips_p1)
        putStrLn ("\nplace ships of size 3")
        pos_32_p1 <- positions f' 3
        f' <- modify f' pos_32_p1
        coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1,pos_32_p1]
        
        printmaps (fst players) (concat coordinatesOfShips_p1)
        putStrLn ("\nplace ships of size 2")
        pos_2_p1 <- positions f' 2
        f' <- modify f' pos_2_p1
        coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1,pos_32_p1,pos_2_p1]
        init_positions_list_p1 <- return (convert_positions coordinatesOfShips_p1)
        
        printmaps (fst players) (concat coordinatesOfShips_p1)
        
        putStr (pads 20)

        putStrLn ("\nNow it's your turn to place your ships " ++ snd players)
       
        printmaps (snd players) []
        putStrLn ("place ships of size 5")
        pos_5_p2 <- positions (h []) 5
        f' <- modify (h []) pos_5_p2
        coordinatesOfShips_p2 <- return [pos_5_p2]
        
        printmaps (snd players) (concat coordinatesOfShips_p2)
        putStrLn ("\nplace ships of size 4")
        pos_4_p2 <- positions f' 4
        f' <- modify f' pos_4_p2
        coordinatesOfShips_p2 <- return [pos_5_p2,pos_4_p2]
        
        printmaps (snd players) (concat coordinatesOfShips_p2)
        putStrLn ("\nplace ships of size 3")
        pos_31_p2 <- positions f' 3
        f' <- modify f' pos_31_p2
        coordinatesOfShips_p2 <- return [pos_5_p2,pos_4_p2,pos_31_p2]

        printmaps (snd players) (concat coordinatesOfShips_p2)
        putStrLn ("\nplace ships of size 3")
        pos_32_p2 <- positions f' 3
        f' <- modify f' pos_32_p2
        coordinatesOfShips_p2 <- return [pos_5_p2,pos_4_p2,pos_31_p2,pos_32_p2]
        
        printmaps (snd players) (concat coordinatesOfShips_p2)
        putStrLn ("\nplace ships of size 2")
        pos_2_p2 <- positions f' 2
        f' <- modify f' pos_2_p2
        coordinatesOfShips_p2 <- return [pos_5_p2,pos_4_p2,pos_31_p2,pos_32_p2,pos_2_p2]
        init_positions_list_p2 <- return (convert_positions coordinatesOfShips_p2)
        
        printmaps (snd players) (concat coordinatesOfShips_p2)

        putStr (pads 20)
        
        playgame players init_positions_list_p1 init_positions_list_p2 (h (concat coordinatesOfShips_p1)) (h (concat coordinatesOfShips_p2)) (h []) (h []) 0 0