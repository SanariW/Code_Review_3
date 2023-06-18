# assignment3_hangman

**Welcome to Hangman!**

To run this game, please follow these instructions:
1) Open "Assignment_3_Hangman.R" in R Studio by cloning this repo. 
2) Ensure that the working directory is set correctly.
3) If the working directory is set correctly, "Helper_Functions.R" and "word_list.txt"
should be present in the "Files" section on the lower right corner. 
4) Run the game! To do this, press "Source" at the top right of the R-Studio window.

**Miscellaneous Notes: **
1) Helper functions were created for various repetitive use cases. These are located
in "Helper_Functions.R". Comments in the "Assignment_3_Hangman.R" script may direct you to this file.

2) The script for this game has been written to repeat the game until the user decides to no longer play. 
As such, after each iteration of the game please select if you would like to play again or not (this then quits the script).
As such, the user can play as many rounds as they want. At the end of each round they have the decision to exit the game 
and the script executes. As such, there is no infinite loop that is not exitable. 
The user COULD play the game infinitely many times if they want... effects on user sanity: TBD. However, they can choose to
quit the loop after each round played. As such, there is a designed escape path for this loop.

3) This game is not case-sensitive. Words and guesses will be valid no matter if they are lower or uppercase. 

4) The user has 10 guesses per round (one round = guessing one word). They can either guess a single letter OR  
a word of the same length as the mystery word.

5) Duplicate guesses are not allowed. Each guess must be unique (per round).

6) If letters are repeated in the word, both will be revealed. This only counts as one guess (oh yeah)!

7) This game uses a text file with random words as the "database" of possible solutions. 
This file has 25 words on it. So if you play enough rounds there's  a chance a word will be repeated.
