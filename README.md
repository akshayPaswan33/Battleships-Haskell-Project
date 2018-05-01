# Battleships-Haskell-Project
Multiplayer, singleplayer as well as computer vs computer simulation of battleship board game on terminal.

Project - Battleships
language - Haskell

Features - Multiplayer - multiplayer.hs
	   Single player - single_medium.hs
                           single_hard.hs
           
           Computer Simulations - comp1 vs comp2
                                  where comp1 - computer in single_medium
                                        comp2 - computer in single_hard
                                  file - comp_sim.hs

For rules see the reference - http://www.datagenetics.com/blog/december32011/
                              https://en.wikipedia.org/wiki/Battleship_(game)

How to play - Multiplayer
run - multiplayer.hs in ghci

you will be asked to enter the players name
and first the player1 will see his/her board
and it will ask to set your ships of size 'x', (x can be 5,4,3,2) 

setting ships -- e.g. - press A5H and then press enter to set your ship starting from cell A5 and spanning horizontally to cell A(5+(x-1)) 
                        press A5V and then press enter to set your ship starting from cell A5 and spanning vertically to cell (A+x-1)5, where (A+1 => B)
                        If by any chance you enter wrong sequence then no worries just enter it again in newline.
After the two players has set the ships
now you can play the game

you can attack only one cell of the enemy board at a time
to attack the cell just press the coordinates in the format AlphaNumeric e.g D4,A5,B6 

first the player1 will get the chance to attack the enemy's (player2) board if he missed (the cell will become 'O') then the chance goes to player 2 or else if he got the hit (the cell will become 'X') then player1 again get the chance to attack the enemy's board
once an entire fleet has been hit by a user we will notify that fleet by sequence of 'Y' and tell him the no. of fleets that is remaining to be sunked in the opponent's board
whoever first sunk all the opponent's fleet is the winner and we will notify him.

How to play - Single Player
medium level
run - single_medium.hs in ghci

computer will set its ships at random positions
then the user will be asked to set his ship same as above in multiplayer but this time only for one player
Then start playing the game same as above

How to play - Single Player
Hard level
run - single_hard.hs in ghci

computer will set its ships at random positions
then the user will be asked to set his ship same as above in multiplayer but this time only for one player
Then start playing the game same as above

If you want to do comp1 vs comp2 simulation
then just run comp_sim.hs in ghci and enjoy.

For step by step simulation just uncomment the 'garbage <- getLine' in line number 519 of the code. Then just keep on pressing 'Enter' for the same.

Thank you.



