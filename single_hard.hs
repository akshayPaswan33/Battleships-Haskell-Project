import Data.List
import Data.Char
import System.Random

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

printmaps :: String -> [(Char,Char)] -> IO ()                       -- It takes the player's name and list of (row ind,column ind) and prints the players' field map after making appropriate changes at the positions given in the list
printmaps name z = do
                    putStrLn ("Current map of "++name++" ships")
                    printArray (h z)

update :: String -> Int -> Char
update m i =    if m!!i == '~'
                then 'S'
                else m!!i

full_map :: String -> [((Char,Char),Int)] -> String
full_map m [] = m
full_map m (((x1,x2),x3):ps)= full_map ((take (((ord x1) - 64)*11+ (ord x2)-47) m) ++ [update m (((ord x1) - 64)*11+ (ord x2)-47)] ++ (drop ((((ord x1) - 64)*11+ (ord x2)-47)+1) m)) ps

convert_positions' :: (Char,Char) -> ((Char,Char),Int)
convert_positions' (a,b) = ((a,b),0)

convert_positions'' :: ((Char,Char),Int) -> (Char,Char)
convert_positions'' ((a,b),i) = (a,b)

convert_positions :: [[(Char,Char)]] -> [[((Char,Char),Int)]]
convert_positions xss = [ map convert_positions' xs|xs <- xss]

convert_pos_field :: [[((Char,Char),Int)]] -> [(Char,Char)]
convert_pos_field xss = [ convert_positions'' x|xs<-xss,x<-xs]

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


positions'' :: String -> Int -> Char -> Char -> Char -> IO [(Char,Char)]
positions'' f s c i d = do
                        valid <-validate4 f s c i d
                        if (valid==True)
                        then return (poslist s c i d)
                        else positions' f s

positions' :: String->Int->IO [(Char,Char)]                     --It takes size of that ship which needs placement currently in the player's map and asks user from the terminal to fill the list in the format [(row ind, column ind)] 
positions' f s =    do
                        cs <- getLine
                        if (length cs) < 3 || (length cs) > 3
                        then positions' f s
                        else    do
                                    c<- return(cs!!0)
                                    i<- return(cs!!1)
                                    --putChar ";"
                                    d<- return(cs!!2)
                                    valid1 <- validate1 c
                                    valid2 <- validate2 i
                                    valid3 <- validate3 d
                                    if (valid1 && valid2 && valid3)
                                    then positions'' f s c i d
                                    else positions' f s

change_xs :: String->(Char,Char)->IO String
change_xs f x = let v = (((fromEnum (fst x))-64)*11+(fromEnum (snd x))-47)
                in return (replace v 'S' f)

modify :: String -> [(Char,Char)] -> IO String
modify f [] = return f
modify f (x:xs) = do
                   ys <- change_xs f x
                   zs <- modify ys xs
                   return zs

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

playername :: IO String
playername = do
                putStrLn "Enter your player name:"
                player <- getLine
                return player

positions :: Int -> (Int,Int,Int) -> IO [((Char,Char),Int)]
positions size (k1,k2,k3) = if size == 0
                            then do return []
                            else if k1 == 1      
                                 then do 
                                         ks <- (positions (size-1) (k1, k2, (k3+1)))
                                         return (((chr(64+k2),chr(k3+47)),0):ks)
                                 else do
                                         ks <- (positions (size-1) (k1, k2, (k3+1)))
                                         return (((chr(64+k3),chr(k2+47)),0):ks)

parameters :: Int -> IO (Int,Int,Int)
parameters size =   if size == 5
                    then do
                            g <- newStdGen
                            k1 <- return (fst(randomR (1,2) g))
                            g <- newStdGen
                            k2 <- return (fst(randomR (1,10) g))
                            g <- newStdGen
                            k3 <- return (fst(randomR (1,6) g))
                            return (k1,k2,k3)
                    else if size == 4
                         then do
                                  g <- newStdGen
                                  k1 <- return (fst(randomR (1,2) g))
                                  g <- newStdGen
                                  k2 <- return (fst(randomR (1,10) g))
                                  g <- newStdGen
                                  k3 <- return (fst(randomR (1,7) g))
                                  return (k1,k2,k3)
                         else if size == 3
                              then do 
                                       g <- newStdGen
                                       k1 <- return (fst(randomR (1,2) g))
                                       g <- newStdGen
                                       k2 <- return (fst(randomR (1,10) g))
                                       g <- newStdGen
                                       k3 <- return (fst(randomR (1,8) g))
                                       return (k1,k2,k3)
                              else if size == 2
                                   then do
                                            g <- newStdGen
                                            k1 <- return (fst(randomR (1,2) g))
                                            g <- newStdGen
                                            k2 <- return (fst(randomR (1,10) g))
                                            g <- newStdGen
                                            k3 <- return (fst(randomR (1,9) g))
                                            return (k1,k2,k3)
                                   else do return (1,2,3)

comp_positions :: Int -> [[((Char,Char),Int)]] -> IO [((Char,Char),Int)]
comp_positions size ps = do
                                k <- (parameters size)
                                p <- (positions size k)
                                if ps == []
                                then return p
                                else if (intersection_check p ps) == True
                                     then return p
                                     else (comp_positions size ps)


intersection_check :: [((Char,Char),Int)] -> [[((Char,Char),Int)]] -> Bool
intersection_check xs yss = if (length([x | x <- xs,ys <- yss,y <- ys,x==y]) == 0)
                            then True
                            else False


add_list :: [Int] -> Int
add_list [] = 0
add_list (x:xs) = x + add_list xs

padded_list0 :: Int -> Int -> [Int]
padded_list0 a x = [ 0 | y <- [1..11]]

padded_list1 :: Int -> Int -> [Int]
padded_list1 a x = [ 0 | y<-[1..x `mod` 11]] ++ [ 1 | y<-[1..a]] ++ [ 0 | y<- [1..(11-(x `mod` 11)-a)]]

valid_ind :: Int -> String -> Bool
valid_ind y m = if y<121 && m !! y == '~' then True else False

valid_ind' :: Int -> String -> Bool
valid_ind' y m = if (y<121 && (m !! y == '~' || m !! y == 'X')) then True else False

valid_list :: [Bool] -> Bool
valid_list [] = True
valid_list (x:xs) = x && (valid_list xs)

valid_seq :: Int -> Int -> String -> Int -> Bool
valid_seq hunt_flag a m x = if hunt_flag==1
                            then valid_list [ valid_ind y m|y <- [x..(x+a-1)]]
                            else valid_list [ valid_ind' y m|y <- [x..(x+a-1)]]

compute_prob'' :: Int -> Int -> String -> Int -> [Int]
compute_prob'' hunt_flag a m x =  if valid_seq hunt_flag a m x
                                  then padded_list1 a x
                                  else padded_list0 a x

non_padded_list :: Int -> String -> Int -> String
non_padded_list a m x = [ m!!y | y <- [x..x+a-1]]

binary_list :: String -> [Int]
binary_list [] = []
binary_list (c:cs) =    if c=='~'
                        then (0:(binary_list cs))
                        else (1:(binary_list cs))

-- multiplier :: Int -> String -> Int -> Int
-- multiplier a m x =  if (add_list (binary_list (non_padded_list a m x)))==0
--                     then 0
--                     else (2*(add_list (binary_list (non_padded_list a m x))))

padded_list1' :: Int -> String -> Int -> [Int]
padded_list1' a m x = [ 0 | y<-[1..x `mod` 11]] ++ (map (* (5*(add_list (binary_list (non_padded_list a m x))))) (zipWith (-) (take a [1,1..]) (binary_list(non_padded_list a m x)))) ++ [ 0 | y<- [1..(11-(x `mod` 11)-a)]]

compute_prob''' :: Int -> Int -> String -> Int -> [Int]
compute_prob''' hunt_flag a m x = if valid_seq hunt_flag a m x
                                  then padded_list1' a m x
                                  else padded_list0 a x

myadd :: [[Int]] -> [Int]
myadd (xs:xss) =    if xss==[]
                    then xs
                    else myadd ([(zipWith (+) xs (head(xss)))]++(tail xss))

compute_prob' :: Int -> Int -> String -> [Int] -> [Int]
compute_prob' hunt_flag a m xs =   if hunt_flag==1
                                then myadd [compute_prob'' hunt_flag a m x | x <- xs]
                                else myadd [compute_prob''' hunt_flag a m x | x <- xs]

compute_prob :: Int -> Int -> String -> [Int] -> [Int]
compute_prob hunt_flag a m [] = []
compute_prob hunt_flag a m xs = (compute_prob' hunt_flag a m (take 11 xs)) ++ (compute_prob hunt_flag a m (drop 11 xs))

chunk [] = []
chunk xs = take 11 xs : (chunk (drop 11 xs))

compute_final_prob :: Int -> Int -> String -> [Int]
compute_final_prob hunt_flag a m = zipWith (+) (compute_prob hunt_flag a m [0..120]) (concat (transpose (chunk (compute_prob hunt_flag a (concat (transpose (chunk m))) [0..120]))))

compute_hunt_prob :: Int -> [(Int,Int)] -> String -> [Int]
compute_hunt_prob hunt_flag xs m = myadd [ compute_final_prob hunt_flag a m|(a,b)<-xs,b==0]

ind_prob_pair :: Int -> [(Int,Int)] -> String -> [(Int,Int)]
ind_prob_pair hunt_flag xs m = zip [0..120] (compute_hunt_prob hunt_flag xs m)

mysort :: [(Int,Int)] -> [(Int,Int)]
mysort [] = []
mysort (x:xs) = mysort ([ y | y <- xs, (snd y) >= (snd x)]) ++ [x] ++ mysort ([ y | y <- xs, (snd y) < (snd x)])

hunt :: [(Int,Int)] -> String -> Int
hunt xs m = fst(head (mysort (ind_prob_pair 1 xs m)))

target :: [(Int,Int)] -> String -> Int
target xs m = fst(head (mysort (ind_prob_pair 0 xs m)))

comp_play :: Int -> [(Int,Int)] -> String -> IO (Char,Char)
comp_play hunt_flag xs m =  if hunt_flag == 1
                            then do
                                    x <- return(hunt xs m)
                                    (c1,c2) <- return ((quot x 11)-1,(mod x 11)-1)
                                    return (chr(65+c1),chr(48+c2))
                            else do
                                    x <- return(target xs m)
                                    (c1,c2) <- return ((quot x 11)-1,(mod x 11)-1)
                                    return (chr(65+c1),chr(48+c2))

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

myfind :: Char -> String -> Bool
myfind c [] = False
myfind c (x:xs) = (c==x)||(myfind c xs)

playgame_prob_user :: (String,String)->[[((Char,Char),Int)]]->[[((Char,Char),Int)]]->String->String->String->String->Int->Int->Int->[(Int,Int)]->IO ()
playgame_prob_user (p1,p2) pos1 pos2 f1 f2 g1 g2 c1 c2 hunt_flag xs =   if c1 == 17
                                                                        then  if p1 == "comp"
                                                                              then do
                                                                                      putStrLn ("\n"++p1++" wins")
                                                                                      putStrLn "comp map"
                                                                                      printArray (full_map g1 (concat pos1))            
                                                                              else putStrLn ("\n"++p1++" wins")    
                                                                        else do
                                                                              if p1 /= "comp"
                                                                              then do
                                                                                    putStrLn ("\nCurrent map of "++p2++" ships\n")
                                                                                    printArray g2
                                                                                    putStrLn ("your move "++p1)
                                                                              else  putStrLn ""
                                                                              remain_ships <- return (notSunk_list pos2)
                                                                              print_ships <- return (shiplist_string remain_ships)
                                                                              if p1 /= "comp"
                                                                              then do
                                                                                    putStrLn "Ships of following sizes are remaining"
                                                                                    printArray' print_ships
                                                                              else putStrLn ""
                                                                              hunt_flag' <- if myfind 'X' g2 then return 0 else return 1
                                                                              box <- if p1=="comp"
                                                                                      then comp_play hunt_flag' xs g2
                                                                                      else valid_box_attack g2
                                                                              if p1=="comp"
                                                                              then putStrLn ([fst box]++[snd box])
                                                                              else putStrLn ""
                                                                              --now <- return (length (nSunk_list pos2))
                                                                              strike <- move p1 box f2
                                                                              pos2' <- return (change_pos strike box pos2)
                                                                              g2' <- change p1 box strike g2
                                                                              --hunt_flag' <- if strike && (p1=="comp") then return (flag_sunk now pos2') else return 0
                                                                              -- hunt_flag' <- if myfind 'X' g2' then return 0 else return 1
                                                                              xs' <- if p1 == "comp"
                                                                                     then return [ if isSunked ps then (length ps,1) else (length ps,0) |ps<-pos2']
                                                                                     else return (xs)
                                                                              if strike then putStrLn "\nIt's a hit\n" else putStrLn "\nIt's a miss\n"
                                                                              g2' <- return (replace_x_y g2' (get_int_pos_l (nSunk_list pos2')))
                                                                              if p1 == "comp"
                                                                              then printArray g2'
                                                                              else putStrLn ""
                                                                              if strike then playgame_prob_user (p1,p2) pos1 pos2' f1 f2 g1 g2' (c1+1) c2 hunt_flag' xs' else playgame_prob_user (p2,p1) pos2' pos1 f2 f1 g2' g1 c2 c1 hunt_flag' xs'



main = do
            player <- playername
            players <- return (player,"comp")
            -- setting up ships for computer
            -- for size 5
            pos_5 <- comp_positions 5[]
            pos <- return [pos_5]
            -- for size 4
            pos_4 <- comp_positions 4 pos
            pos <- return[pos_5,pos_4]
            -- for size 3
            pos_31 <- comp_positions 3 pos
            pos <- return[pos_5,pos_4,pos_31]

            pos_32 <- comp_positions 3 pos
            pos <- return[pos_5,pos_4,pos_31,pos_32]
            -- for size 2
            pos_2 <- comp_positions 2 pos
            pos <- return [pos_5,pos_4,pos_31,pos_32,pos_2]
            -- printArray pos
            putStrLn ("\nComputer has set its ships")
            putStrLn ("\nNow it's your turn to place your ships, " ++ player ++ ".")

            printmaps (fst players) []
            
            putStrLn ("place ships of size 5")
            pos_5_p1 <- positions' (h []) 5
            f' <- modify (h []) pos_5_p1
            putStrLn ""
            coordinatesOfShips_p1 <- return [pos_5_p1]
        
            printmaps (fst players) (concat coordinatesOfShips_p1)
            putStrLn ("\nplace ships of size 4")
            pos_4_p1 <- positions' f' 4
            f' <- modify f' pos_4_p1
            coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1]
        
            printmaps (fst players) (concat coordinatesOfShips_p1)
            putStrLn ("\nplace ships of size 3")
            pos_31_p1 <- positions' f' 3
            f' <- modify f' pos_31_p1
            coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1]

            printmaps (fst players) (concat coordinatesOfShips_p1)
            putStrLn ("\nplace ships of size 3")
            pos_32_p1 <- positions' f' 3
            f' <- modify f' pos_32_p1
            coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1,pos_32_p1]
        
            printmaps (fst players) (concat coordinatesOfShips_p1)
            putStrLn ("\nplace ships of size 2")
            pos_2_p1 <- positions' f' 2
            f' <- modify f' pos_2_p1
            coordinatesOfShips_p1 <- return [pos_5_p1,pos_4_p1,pos_31_p1,pos_32_p1,pos_2_p1]
            init_positions_list_p1 <- return (convert_positions coordinatesOfShips_p1)
        
            printmaps (fst players) (concat coordinatesOfShips_p1)

            --playgame players init_positions_list_p1 pos (h (concat coordinatesOfShips_p1)) (h (convert_pos_field pos)) (h []) (h []) 0 0 0 (-1,-1) (-1,-1) 0

            playgame_prob_user players init_positions_list_p1 pos (h (concat coordinatesOfShips_p1)) (h (convert_pos_field pos)) (h []) (h []) 0 0 1 [(5,0),(4,0),(3,0),(3,0),(2,0)]