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

convert_pos_field :: [[((Char,Char),Int)]] -> [(Char,Char)]
convert_pos_field xss = [ convert_positions'' x|xs<-xss,x<-xs]

convert_positions'' :: ((Char,Char),Int) -> (Char,Char)
convert_positions'' ((a,b),i) = (a,b)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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


-----------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------

charp_intp :: Char->Char-> Int
charp_intp c i = (((fromEnum c)-64)*11+(fromEnum i)-47)

get_int_pos_l' :: [((Char,Char),Int)] -> [Int]
get_int_pos_l' xs = [ charp_intp a b|((a,b),c)<-xs]

get_int_pos_l :: [[((Char,Char),Int)]] -> [Int]
get_int_pos_l xss = concat [ get_int_pos_l' xs|xs<-xss] 

replace_x_y :: String -> [Int] -> String
replace_x_y mp [] = mp
replace_x_y mp (x:xs) = replace_x_y (replace x 'Y' mp) xs

----------------------------------------------------------------
----------------------------------------------------------------

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

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

padded_list0 :: Int -> Int -> [Int]
padded_list0 a x = [ 0 | y <- [1..11]]

padded_list1 :: Int -> Int -> [Int]
padded_list1 a x = [ 0 | y<-[1..x `mod` 11]] ++ [ 1 | y<-[1..a]] ++ [ 0 | y<- [1..(11-(x `mod` 11)-a)]]

valid_ind :: Int -> String -> Bool
valid_ind y m = if y<121 && (m !! y == '~') then True else False

valid_list :: [Bool] -> Bool
valid_list [] = True
valid_list (x:xs) = x && (valid_list xs)

valid_seq1 :: Int -> String -> Int -> Bool
valid_seq1 a m x = valid_list [ valid_ind y m|y <- [x..(x+a-1)]]

compute_prob1'' :: Int -> String -> Int -> [Int]
compute_prob1'' a m x =  if valid_seq1 a m x
                         then padded_list1 a x
                         else padded_list0 a x

myadd :: [[Int]] -> [Int]
myadd (xs:xss) =    if xss==[]
                    then xs
                    else myadd ([(zipWith (+) xs (head(xss)))]++(tail xss))

compute_prob1' ::Int -> String -> [Int] -> [Int]
compute_prob1' a m xs =  myadd [compute_prob1'' a m x | x <- xs]

compute_prob1 :: Int -> String -> [Int] -> [Int]
compute_prob1 a m [] = []
compute_prob1 a m xs = (compute_prob1' a m (take 11 xs)) ++ (compute_prob1 a m (drop 11 xs))

chunk [] = []
chunk xs = take 11 xs : (chunk (drop 11 xs))

compute_final_prob1 :: Int -> String -> [Int]
compute_final_prob1 a m = zipWith (+) (compute_prob1 a m [0..120]) (concat (transpose (chunk (compute_prob1 a (concat (transpose (chunk m))) [0..120]))))

compute_hunt_prob1 :: [(Int,Int)] -> String -> [Int]
compute_hunt_prob1 xs m = myadd [ compute_final_prob1 a m|(a,b)<-xs,b==0]

ind_prob_pair1 :: [(Int,Int)] -> String -> [(Int,Int)]
ind_prob_pair1 xs m = zip [0..120] (compute_hunt_prob1 xs m)

mysort :: [(Int,Int)] -> [(Int,Int)]
mysort [] = []
mysort (x:xs) = mysort ([ y | y <- xs, (snd y) >= (snd x)]) ++ [x] ++ mysort ([ y | y <- xs, (snd y) < (snd x)])

hunt1 :: [(Int,Int)] -> String -> IO (Int,Int)
hunt1 xs m = do
                t <- return(fst(head (mysort (ind_prob_pair1 xs m))))
                return ((quot t 11)-1,(mod t 11)-1)

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

add_list :: [Int] -> Int
add_list [] = 0
add_list (x:xs) = x + add_list xs

valid_ind' :: Int -> String -> Bool
valid_ind' y m = if (y<121 && (m !! y == '~' || m !! y == 'X')) then True else False

valid_seq2 :: Int -> Int -> String -> Int -> Bool
valid_seq2 hunt_flag a m x = if hunt_flag==1
                             then valid_list [ valid_ind y m|y <- [x..(x+a-1)]]
                             else valid_list [ valid_ind' y m|y <- [x..(x+a-1)]]

compute_prob2'' :: Int -> Int -> String -> Int -> [Int]
compute_prob2'' hunt_flag a m x =  if valid_seq2 hunt_flag a m x
                                   then padded_list1 a x
                                   else padded_list0 a x

non_padded_list :: Int -> String -> Int -> String
non_padded_list a m x = [ m!!y | y <- [x..x+a-1]]

binary_list :: String -> [Int]
binary_list [] = []
binary_list (c:cs) =    if c=='~'
                        then (0:(binary_list cs))
                        else (1:(binary_list cs))

padded_list1' :: Int -> String -> Int -> [Int]
padded_list1' a m x = [ 0 | y<-[1..x `mod` 11]] ++ (map (* (5*(add_list (binary_list (non_padded_list a m x))))) (zipWith (-) (take a [1,1..]) (binary_list(non_padded_list a m x)))) ++ [ 0 | y<- [1..(11-(x `mod` 11)-a)]]

compute_prob2''' :: Int -> Int -> String -> Int -> [Int]
compute_prob2''' hunt_flag a m x = if valid_seq2 hunt_flag a m x
                                  then padded_list1' a m x
                                  else padded_list0 a x

compute_prob2' :: Int -> Int -> String -> [Int] -> [Int]
compute_prob2' hunt_flag a m xs =   if hunt_flag==1
                                    then myadd [compute_prob2'' hunt_flag a m x | x <- xs]
                                    else myadd [compute_prob2''' hunt_flag a m x | x <- xs]

compute_prob2 :: Int -> Int -> String -> [Int] -> [Int]
compute_prob2 hunt_flag a m [] = []
compute_prob2 hunt_flag a m xs = (compute_prob2' hunt_flag a m (take 11 xs)) ++ (compute_prob2 hunt_flag a m (drop 11 xs))

compute_final_prob2 :: Int -> Int -> String -> [Int]
compute_final_prob2 hunt_flag a m = zipWith (+) (compute_prob2 hunt_flag a m [0..120]) (concat (transpose (chunk (compute_prob2 hunt_flag a (concat (transpose (chunk m))) [0..120]))))

compute_hunt_prob2 :: Int -> [(Int,Int)] -> String -> [Int]
compute_hunt_prob2 hunt_flag xs m = myadd [ compute_final_prob2 hunt_flag a m|(a,b)<-xs,b==0]

ind_prob_pair2 :: Int -> [(Int,Int)] -> String -> [(Int,Int)]
ind_prob_pair2 hunt_flag xs m = zip [0..120] (compute_hunt_prob2 hunt_flag xs m)

hunt2 :: [(Int,Int)] -> String -> Int
hunt2 xs m = fst(head (mysort (ind_prob_pair2 1 xs m)))

target2 :: [(Int,Int)] -> String -> Int
target2 xs m = fst(head (mysort (ind_prob_pair2 0 xs m)))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------COMP1 PLAY-----------------------

check :: (Int,Int) -> String -> Char
check (a1,a2) m = m!!((a1+1)*11+a2+1)

find_all_X :: String -> [(Int,Int)]
find_all_X m = [((quot b 11)-1,(mod b 11)-1) | (a,b) <- zip m [0..120],a=='X']

scan_X :: String -> (Int,Int)
scan_X m =  if [y | y <- find_all_X m,[z | z <- adjacency_list y, check z m == '~'] /= []] /= [] 
            then head [y | y <- find_all_X m,[z | z <- adjacency_list y, check z m == '~'] /= []]
            else (-1,-1)

adjacent :: (Int,Int) -> (Int,Int) -> Bool
adjacent (a1,a2) (b1,b2) = if a1 == b1
                           then if(a2-b2)==1||(b2-a2)==1
                                then True
                                else False
                           else if  a2==b2
                                then    if (a1-b1)==1||(b1-a1)==1
                                        then True
                                        else False
                                else False

find_inline :: (Int,Int) -> (Int,Int) -> Int -> String -> IO (Int,Int)
find_inline (a1,a2) (b1,b2) i_flag m =  if a1==b1
                                        then    if b2<a2
                                                then    if (b2-1)<0
                                                        then    if i_flag == 1
                                                                then do
                                                                        (x1,x2) <- return (scan_X m)
                                                                        adjacent_random_fire (x1,x2) m
                                                                else  find_inline (b1,b2) (a1,a2) 1 m
                                                        else    if check (b1,b2) m == 'X'
                                                                then if check (b1,b2) m == 'X' then find_inline (b1,b2) (b1,b2+1) i_flag m else find_inline (b1,b2) (b1,b2+1) 0 m
                                                                else    if check (a1,b2-1) m == 'O'||check (a1,b2-1) m == 'Y'
                                                                        then  if i_flag == 1
                                                                              then do
                                                                                      (x1,x2) <- return (scan_X m)
                                                                                      adjacent_random_fire (x1,x2) m
                                                                              else  find_inline (b1,b2) (a1,a2) 1 m
                                                                        else return (a1,b2-1)
                                                else    if (b2+1)>9
                                                        then    if i_flag == 1
                                                                then do
                                                                        (x1,x2) <- return (scan_X m)
                                                                        adjacent_random_fire (x1,x2) m
                                                                else  find_inline (b1,b2) (a1,a2) 1 m
                                                        else    if check (a1,b2+1) m == 'X'
                                                                then if check (b1,b2) m == 'X' then find_inline (b1,b2) (b1,b2+1) i_flag m else find_inline (b1,b2) (b1,b2+1) 0 m
                                                                else    if check (a1,b2+1) m == 'O'||check (a1,b2+1) m == 'Y'
                                                                        then  if i_flag == 1
                                                                              then do
                                                                                      (x1,x2) <- return (scan_X m)
                                                                                      adjacent_random_fire (x1,x2) m
                                                                              else  find_inline (b1,b2) (a1,a2) 1 m
                                                                        else return (a1,b2+1)
                                        else    if b1<a1
                                                then    if (b1-1)<0
                                                        then    if i_flag == 1
                                                                then do
                                                                        (x1,x2) <- return (scan_X m)
                                                                        adjacent_random_fire (x1,x2) m
                                                                else  find_inline (b1,b2) (a1,a2) 1 m
                                                        else    if check (b1-1,a2) m == 'X'
                                                                then    if check (b1,b2) m == 'X' then find_inline (b1,b2) (b1,b2+1) i_flag m else find_inline (b1,b2) (b1,b2+1) 0 m
                                                                else    if check (b1-1,a2) m == 'O'||check (b1-1,a2) m == 'Y'
                                                                        then  if i_flag == 1
                                                                              then do
                                                                                      (x1,x2) <- return (scan_X m)
                                                                                      adjacent_random_fire (x1,x2) m
                                                                              else  find_inline (b1,b2) (a1,a2) 1 m
                                                                        else return (b1-1,a2)
                                                else    if (b1+1)>9
                                                        then    if i_flag == 1
                                                                then do
                                                                        (x1,x2) <- return (scan_X m)
                                                                        adjacent_random_fire (x1,x2) m
                                                                else  find_inline (b1,b2) (a1,a2) 1 m
                                                        else    if check (b1+1,a2) m == 'X'
                                                                then if check (b1,b2) m == 'X' then find_inline (b1,b2) (b1,b2+1) i_flag m else find_inline (b1,b2) (b1,b2+1) 0 m
                                                                else    if check (b1+1,a2) m == 'O'||check (b1+1,a2) m == 'Y'
                                                                        then  if i_flag == 1
                                                                              then do
                                                                                      (x1,x2) <- return (scan_X m)
                                                                                      adjacent_random_fire (x1,x2) m
                                                                              else  find_inline (b1,b2) (a1,a2) 1 m
                                                                        else return (b1+1,a2)

adjacency_list :: (Int,Int) -> [(Int,Int)]
adjacency_list (c1,c2) = [(x1,x2) | (x1,x2) <- [(c1+1,c2),(c1-1,c2),(c1,c2+1),(c1,c2-1)],x1>=0,x1<=9,x2>=0,x2<=9]


find_X :: (Int,Int) -> String -> IO (Int,Int)
find_X (c1,c2) m = if ([(x1,x2) | (x1,x2) <- adjacency_list (c1,c2),m!!(((x1+1)*11)+x2+1)=='X']) == []
                   then return (-1,-1)
                   else return (head ([(x1,x2) | (x1,x2) <- adjacency_list (c1,c2),check (x1,x2) m =='X']))

adjacent_random_fire :: (Int,Int) -> String -> IO (Int,Int)
adjacent_random_fire (c1,c2) m = do
                                    xs <- return ([(x1,x2) | (x1,x2) <- adjacency_list (c1,c2),check (x1,x2) m =='~'])
                                    g <- newStdGen
                                    n <- return (length xs)
                                    return (xs!!((fst(randomR (1,n) g))-1))


find_next :: (Int,Int) -> String -> IO (Int,Int)                                    
find_next (c1,c2) m = do
                        (x1,x2) <- find_X (c1,c2) m
                        if (x1==(-1))&&(x2==(-1))
                        then    adjacent_random_fire (c1,c2) m
                        else    if x1==c1
                                then    if x2<c2
                                        then    if (c2+1)>9
                                                then find_inline (c1,c2) (x1,x2) 1 m
                                                else    if check (c1,c2+1) m /='~'
                                                        then  if check (c1,c2+1) m =='X'
                                                              then find_inline (c1,c2) (c1,c2+1) 0 m
                                                              else find_inline (c1,c2) (x1,x2) 1 m
                                                        else return (c1,c2+1)
                                        else    if (c2-1)<0
                                                then find_inline (c1,c2) (x1,x2) 1 m
                                                else    if check (c1,c2-1) m /='~'
                                                        then  if check (c1,c2-1) m =='X'
                                                              then find_inline (c1,c2) (c1,c2-1) 0 m
                                                              else find_inline (c1,c2) (x1,x2) 1 m
                                                        else return (c1,c2-1)
                                else    if x1<c1
                                        then    if (c1+1)>9
                                                then find_inline (c1,c2) (x1,x2) 1 m
                                                else    if check (c1+1,c2) m /='~'
                                                        then  if check (c1+1,c2) m =='X'
                                                              then find_inline (c1,c2) (c1+1,c2) 0 m
                                                              else find_inline (c1,c2) (x1,x2) 1 m
                                                        else return (c1+1,c2)
                                        else    if (c1-1)<0
                                                then find_inline (c1,c2) (x1,x2) 1 m
                                                else    if check (c1-1,c2) m /='~'
                                                        then  if check (c1-1,c2) m =='X'
                                                              then find_inline (c1,c2) (c1-1,c2) 0 m
                                                              else find_inline (c1,c2) (x1,x2) 1 m
                                                        else return (c1-1,c2)

comp_play1 :: String -> Int -> Int -> (Int,Int) -> (Int,Int) -> Int -> [(Int,Int)] -> IO (Char,Char)
comp_play1 m hunt_flag hit_flag last_hit last_miss sunk xs   =   if hunt_flag == 1
                                                                 then    do
                                                                             (c1,c2) <- hunt1 xs m
                                                                             return (chr(65+c1),chr(48+c2))
                                                                 else    if hit_flag == 0
                                                                         then    if (adjacent last_hit last_miss)&&(check last_hit m /= 'Y')
                                                                                 then do
                                                                                         (c1,c2) <- find_inline last_miss last_hit 1 m
                                                                                         return (chr(65+c1),chr(48+c2))
                                                                                 else    do
                                                                                             (x1,x2) <- return (scan_X m)
                                                                                             (c1,c2) <- adjacent_random_fire (x1,x2) m
                                                                                             return (chr(65+c1),chr(48+c2))
                                                                         else    if sunk == 1
                                                                                 then    do
                                                                                             (c1,c2) <- return (scan_X m)
                                                                                             (x1,x2) <- find_next (c1,c2) m
                                                                                             return (chr(65+x1),chr(48+x2))
                                                                                 else do
                                                                                         (c1,c2) <- find_next last_hit m
                                                                                         return (chr(65+c1),chr(48+c2))

---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------

--------------------COMP2 PLAY----------------------------------------

comp_play2 :: Int -> [(Int,Int)] -> String -> IO (Char,Char)
comp_play2 hunt_flag xs m =  if hunt_flag == 1
                             then do
                                     x <- return(hunt2 xs m)
                                     (c1,c2) <- return ((quot x 11)-1,(mod x 11)-1)
                                     return (chr(65+c1),chr(48+c2))
                             else do
                                     x <- return(target2 xs m)
                                     (c1,c2) <- return ((quot x 11)-1,(mod x 11)-1)
                                     return (chr(65+c1),chr(48+c2))

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

playgame :: (String,String)->[[((Char,Char),Int)]]->[[((Char,Char),Int)]]->String->String->String->String->Int->Int->Int->Int->[(Int,Int)]->[(Int,Int)]->Int->(Int,Int)->(Int,Int)->Int->IO ()
playgame (p1,p2) pos1 pos2 f1 f2 g1 g2 c1 c2 hunt_flag1 hunt_flag2 xs1 xs2 hf lh lm sunk =      if c1 == 17
                                                                                                then putStrLn ("\n"++p1++" wins")
                                                                                                else  do
                                                                                                        putStrLn ("\nCurrent map of "++p2++" ships\n")
                                                                                                        printArray g2
                                                                                                        putStrLn ("move of "++p1)
                                                                                                        remain_ships <- return (notSunk_list pos2)
                                                                                                        print_ships <- return (shiplist_string remain_ships)
                                                                                                        putStrLn "Ships of following sizes are remaining"
                                                                                                        printArray' print_ships
                                                                                                        --garbage <- getLine
                                                                                                        hunt_flag' <- if myfind 'X' g2 then return 0 else return 1
                                                                                                        box <- if p1=="comp1" 
                                                                                                               then comp_play1 g2 hunt_flag' hf lh lm sunk xs2
                                                                                                               else comp_play2 hunt_flag' xs2 g2
                                                                                                        putStrLn ([fst box,snd box])
                                                                                                        now <- return (length (nSunk_list pos2))
                                                                                                        strike <- move p1 box f2
                                                                                                        pos2' <- return (change_pos strike box pos2)
                                                                                                        g2' <- change p1 box strike g2
                                                                                                        if strike then putStrLn "\nIt's a hit\n" else putStrLn "\nIt's a miss\n"
                                                                                                        hf <- if strike && (p1=="comp1") then return 1 else return 0
                                                                                                        lh' <- if strike && (p1=="comp1") then return (ord(fst box)-65,ord(snd box)-48) else return lh
                                                                                                        lm' <- if (not strike) && (p1=="comp1") then return (ord(fst box)-65,ord(snd box)-48) else return lm
                                                                                                        sunk' <- if strike && (p1=="comp1") then return (flag_sunk now pos2') else return 0
                                                                                                        xs2' <- return [ if isSunked ps then (length ps,1) else (length ps,0) |ps<-pos2']
                                                                                                        g2' <- return (replace_x_y g2' (get_int_pos_l (nSunk_list pos2')))
                                                                                                        putStrLn (p2++" ships")
                                                                                                        printArray g2'
                                                                                                        if strike then playgame (p1,p2) pos1 pos2' f1 f2 g1 g2' (c1+1) c2 hunt_flag' hunt_flag' xs1 xs2' hf lh' lm' sunk' else playgame (p2,p1) pos2' pos1 f2 f1 g2' g1 c2 c1 hunt_flag' hunt_flag' xs2' xs1 hf lh' lm' sunk'





-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------

--------------------------COMP1 = INLINE------------COMP2 = TARGET-----------------------------------
main = do
            players <- return ("comp1","comp2")
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

            pos1 <- return pos
            -- printArray pos
            putStrLn ("\nComputer1 has set its ships")
            printArray (full_map (h[]) (concat pos1))

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

            pos2 <- return pos
            -- printArray pos
            putStrLn ("\nComputer2 has set its ships")
            printArray (full_map (h[]) (concat pos2))

            playgame players pos1 pos2 (h (convert_pos_field pos1)) (h (convert_pos_field pos2)) (h []) (h []) 0 0 1 1 [(5,0),(4,0),(3,0),(3,0),(2,0)] [(5,0),(4,0),(3,0),(3,0),(2,0)] 0 (-1,-1) (-1,-1) 0
            