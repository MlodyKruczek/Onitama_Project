Project made by: Michał Krukowski
University of Southern Denmark, SDU 2022

Onitama is a two-player strategy board game created in 2014 by Japanese game designer
Shimpei Sato. The game is played on a 5x5 board. Each player controls 5 pawns – 1 master
pawn and 4 student pawns. 5 out of the total 16 move cards dictate the movement of the
pawns during the game.

Initially, each player’s master pawn is placed in the middle square of their closest row (known as
the shrine or temple), with the 4 students beside it. The 16 cards (see Figure 2) are shuffled
and 5 are chosen randomly, with each player receiving 2 cards and 1 being set aside by the
board. The remaining cards are not used. Both players place their 2 cards face up in front of
them, while the stamp (red or blue) of the 5th card determines which player goes first.

Each player’s turn consists of using one of the 2 available move cards to move any of their
pawns. The move is applied relative to the player’s perspective and the position of the pawn,
with the dark colored square on the card representing the pawn’s position. Landing on an
opponent’s pawn captures the pawn and removes it from the game. Moving out of the board
or onto one of your own pawns is forbidden. After playing a move, the played card is swapped
with the 5th card and the player’s turn ends.

A player can win in two ways – by capturing the opponent’s master pawn, known as the Way
of the Stone, or by moving their own master pawn into the opponent’s temple, known as the
Way of the Stream

Coverage:

 98% expressions used (2516/2559)
100% boolean coverage (68/68)
     100% guards (6/6)
     100% 'if' conditions (60/60)
     100% qualifiers (2/2)
 97% alternatives used (178/182)
100% local declarations used (164/164)
100% top-level declarations used (87/87)

Unfortunate, it's not 100%.. In "alternatives used" i got 97% becouse in functions:
 tail' [] = []
 head' [] = []
 capitalized [] = []
 prepareTotakePownsFirst []  = []. 
 Output is never used. I'm returning [] to stop calculation or prevent any errors.

98% expressions used - here mainly it's cased by not using each field of data structures, but I can assume that everything will work correctly because some fields in different lists are the same or for example: I am never using goose field in list of tuples "moveVectors" but I'm using it in other place anyway.

It was my first touch with  functional programming language, so this was very challenging for me at the beginning. I had to refactor my program often, but during the course I became more comfortable with Haskell. 
The worst part in the whole project for me was parsing from file. 
After making a working version of code, I found readMaybe function, so I decided to refactor the whole parsing code. 
The most satisfying part was "count games" functionality 