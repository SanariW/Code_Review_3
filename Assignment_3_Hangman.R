# Assignment 3: Hangman Game

#' INSTRUCTIONS TO USER: HOW TO LOAD WORD LIST & RUN SCRIPT ** (TO DO)

# Tell reader that im sourcing helper functions

# THIS IS THE MAIN FUNCTION FOR MY GAME. Put it inside of a function so I can re-play the game at the end

quit.game <- FALSE
source("Helper_Functions.R")

start.game <- function(){
  
  # BEGINNING OF SECTION FOR DATA LOADING & RANDOM WORD CHOOSING
  
  # 25 words. Access particular row using my_data[row#, 1] (1=only one col)
  # Data frame: my_data
  my_data <- read.csv("word_list.txt", header=FALSE, sep="")
  
  # This gets the number of words (rows) in the data frame my_data. Used later.
  my_data.length <- nrow(my_data)
  
  # Get a (one) number from 1 to the size of my_data data frame (number of words). Choose one number.
  word.index <- sample(my_data.length, size=1)
  
  #Using the number from above as an index to retrieve the word from my_data data frame.
  guess.word <- my_data[word.index, 1]
  print(guess.word)
  print(paste(nchar(guess.word), "length"))
  
  # BEGINNING OF SECTION FOR LOADING THE GAME
  
  # Setting the maximum number of guesses
  guess.cap <- 10 
  
  # Printing rules to the player including max guess count & what valid gueses are
  cat("===> Welcome to Hangman!", "Here's how to play: <===\n",
              "1. A random word has been chosen by the computer. You need to guess it!\n",
              "2. You have exactly", guess.cap, "guesses to do so.\n",
              "3. You may either guess one letter per attempt OR guess the whole word at once!\n", 
              "4. But be careful! If you try to guess the whole word at once", 
              "you will only win if it's a perfect match! No information will",
              "be provided about any letter overlap between the two words!!")
  
  
  start_game_consent("Please enter 'Y' or 'YES' now to start the game:")
  # CODE BELOW THE LINE ABOVE WILL ONLY EXECUTE IF USER CONSENTS TO STARTING THE GAME
  
  print("Running Game...")
  
  if(check.play.again() == FALSE){
    quit.game <<- TRUE 
    # This above is used to make a superassignment. Makes global variable quit.game TRUE
  }
  
} # END OF START GAME FUNC


#' BELOW IS THE MAIN ENGINE OF MY GAME:
#' FIRST IT CALLS STAR.GAME ONCE TO EXECUTE THE GAME FUNCTION
#' THEN IT HAS A WHILE LOOP TO CHECK IF USER WANTS TO PLAY AGAIN.
#' GAME EXECUTES UNTIL USER TYPES "NO" or "N".

start.game()
while(quit.game != TRUE){
  start.game()
}
print("Thanks for playing!")

