import Data.Char
import System.Random

add_space :: String -> String
add_space ms = concat[ ([x]++"   ") | x<-ms]

printArray arr =    if arr==[]
                    then do putStrLn ""
                    else do
                            putStr "    "
                            putStrLn (add_space(take 11 arr))
                            putStrLn ""
                            printArray (drop 11 arr)

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

nSunk_list:: [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
nSunk_list xss = [ xs| xs<-xss, isSunked xs]

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

check :: (Int,Int) -> String -> Char
check (a1,a2) m = m!!((a1+1)*11+a2+1)

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

scan :: String -> Int -> (Int,Int)
scan (m:ms) t = if t == 120
                then (0,0)
                else  if m == 'O' || m == 'Y'
                      then ((quot t 11)-1,(mod t 11)-1)
                      else scan ms (t+1)

find_all_X :: String -> [(Int,Int)]
find_all_X m = [((quot b 11)-1,(mod b 11)-1) | (a,b) <- zip m [0..120],a=='X']

scan_X :: String -> (Int,Int)
scan_X m =  if [y | y <- find_all_X m,[z | z <- adjacency_list y, check z m == '~'] /= []] /= [] 
            then head [y | y <- find_all_X m,[z | z <- adjacency_list y, check z m == '~'] /= []]
            else (-1,-1)

parity :: (Int,Int) -> Int
parity (a,b) = mod (a+b) 2

check_fire :: (Int,Int) -> String -> Bool
check_fire (c1,c2) m = if check (c1,c2) m == '~'
                       then True
                       else False

new_fire :: Int -> String -> IO (Int,Int)
new_fire par m = do
                    g <- newStdGen
                    c1 <- return ((fst(randomR (1,10) g))-1)
                    g <- newStdGen
                    c2 <- return ((fst(randomR (1,10) g))-1)
                    if parity (c1,c2) == par
                    then if check_fire (c1,c2) m == True
                         then return (c1,c2)
                         else new_fire par m
                    else new_fire par m

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
                                                        else    if check (a1,b2-1) m == 'X'
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
                                                                then if check (b1,b2) m == 'X' then find_inline (b1,b2) (b1,b2+1) i_flag m else find_inline (b1,b2) (b1,b2+1) 0 m
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

comp_play :: String -> Int -> (Int,Int) -> (Int,Int) -> Int -> IO (Char,Char)
comp_play m hit_flag last_hit last_miss sunk =  if hit_flag == 0
                                                then    if last_hit == (-1,-1) && last_miss == (-1,-1)
                                                        then    do
                                                                    g <- newStdGen
                                                                    par <- return (fst(randomR (0,1) g))
                                                                    (c1,c2) <- (new_fire par m)
                                                                    return (chr(65+c1),chr(48+c2))
                                                        else    if (adjacent last_hit last_miss)&&(check last_hit m /= 'Y')
                                                                then do
                                                                        (c1,c2) <- find_inline last_miss last_hit 1 m
                                                                        return (chr(65+c1),chr(48+c2))
                                                                else    if (scan_X m == (-1,-1)) 
                                                                        then    do
                                                                                    f <- return (scan m 0)
                                                                                    par <- return (parity f)
                                                                                    (c1,c2) <- new_fire par m
                                                                                    return (chr(65+c1),chr(48+c2))
                                                                        else    do
                                                                                    (x1,x2) <- return (scan_X m)
                                                                                    (c1,c2) <- adjacent_random_fire (x1,x2) m
                                                                                    return (chr(65+c1),chr(48+c2))
                                                else    if sunk == 1
                                                        then    if scan_X m == (-1,-1)
                                                                then    do
                                                                            f <- return (scan m 0)
                                                                            par <- return (parity f)
                                                                            (c1,c2) <- new_fire par m
                                                                            return (chr(65+c1),chr(48+c2))
                                                                else    do
                                                                            (c1,c2) <- return (scan_X m)
                                                                            (x1,x2) <- find_next (c1,c2) m
                                                                            return (chr(65+x1),chr(48+x2))
                                                        else do
                                                                (c1,c2) <- find_next last_hit m
                                                                return (chr(65+c1),chr(48+c2))



func' :: ((Char,Char),Int)->[((Char,Char),Int)]->[((Char,Char),Int)]
func' ((c,i),0) xs = [  if ((c,i),0)==x then ((c,i),1) else x| x <- xs]

func :: ((Char,Char),Int) -> [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
func ((c,i),0) xss = [ func' ((c,i),0) xs|xs<-xss]

change_pos :: Bool -> (Char,Char) -> [[((Char,Char),Int)]] -> [[((Char,Char),Int)]]
change_pos strike (c,i) xss = if strike
                              then func ((c,i),0) xss
                              else xss

playgame :: (String,String)->[[((Char,Char),Int)]]->[[((Char,Char),Int)]]->String->String->String->String->Int->Int->Int->(Int,Int)->(Int,Int)->Int->IO ()
playgame (p1,p2) pos1 pos2 f1 f2 g1 g2 c1 c2 hf lh lm sunk=       if c1 == 17
                                                                  then   if p1 == "comp"
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
                                                                         box <- if p1=="comp"
                                                                                then comp_play g2 hf lh lm sunk
                                                                                else valid_box_attack g2
                                                                         if p1=="comp"
                                                                         then do
                                                                                putStrLn ([fst box]++[snd box])
                                                                                putStrLn ("last hit : "++[chr(65+(fst lh))]++[chr(48+(snd lh))])
                                                                                putStrLn ("last miss : "++[chr(65+(fst lm))]++[chr(48+(snd lm))])
                                                                         else do putStrLn ""
                                                                         now <- return (length (nSunk_list pos2))
                                                                         strike <- move p1 box f2
                                                                         pos2' <- return (change_pos strike box pos2)
                                                                         g2' <- change p1 box strike g2
                                                                         --putStrLn ("\n"++p2++" ships")
                                                                         --printArray g2'
                                                                         --putStrLn ""
                                                                         if strike then putStrLn "\nIt's a hit\n" else putStrLn "\nIt's a miss\n"
                                                                         hf <- if strike && (p1=="comp") then return 1 else return 0
                                                                         lh' <- if strike && (p1=="comp") then return (ord(fst box)-65,ord(snd box)-48) else return lh
                                                                         lm' <- if (not strike) && (p1=="comp") then return (ord(fst box)-65,ord(snd box)-48) else return lm
                                                                         sunk' <- if strike && (p1=="comp") then return (flag_sunk now pos2') else return 0
                                                                         putStrLn ("now : " ++ [chr(48+now)])
                                                                         putStrLn ("sunk : " ++ [chr(48+sunk')])
                                                                         g2' <- return (replace_x_y g2' (get_int_pos_l (nSunk_list pos2')))
                                                                         putStrLn ("\n"++p2++" ships")
                                                                         if p1 == "comp"
                                                                         then printArray g2'
                                                                         else putStrLn ""
                                                                         putStrLn ""
                                                                         if strike then playgame (p1,p2) pos1 pos2' f1 f2 g1 g2' (c1+1) c2 hf lh' lm' sunk' else playgame (p2,p1) pos2' pos1 f2 f1 g2' g1 c2 c1 hf lh' lm' sunk'





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

            playgame players init_positions_list_p1 pos (h (concat coordinatesOfShips_p1)) (h (convert_pos_field pos)) (h []) (h []) 0 0 0 (-1,-1) (-1,-1) 0

