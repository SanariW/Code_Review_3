# Assignment 3: Hangman Game

#' To run this game, please follow these instructions:
#' 1) Ensure that the working directory is set correctly to "Assignment_3_Hangman"
#' 2) If the working directory is set correctly, "Helper_Functions.R" and "word_list.txt"
#' should be present in the "Files" section on the lower right corner. 
#' 3) Run the game! To do this, press "Source" at the top right of this window. 

#' Miscellaneous Notes: 
#' 1) Helper functions were created for various repetitive use cases. These are located
#' in "Helper_Functions.R". Comments in this script may direct you to this file.
#' 2) The script for this game has been written to repeat the game until the user
#' decides to no longer play. As such, after each iteration of the game please 
#' select if you would like to play again or not (this then quits the script).
#' 3) This game is not case-sensitive. Words and guesses will be valid no matter
#' if they are lower or uppercase. 
#' 4) The user has 10 guesses per round (one round = guessing one word). They
#' can either guess a single letter OR a word of the same length as the mystery
#' word.
#' 5) Duplicate guesses are not allowed. Each guess must be unique (per round).

# BEGINNING OF HANGMAN SCRIPT BELOW

#' The Boolean below is a check for our while loop on line XXXX**!!!!!
#' This is required for the logic to check if the user wants to play another
#' round or not. The user can play as many successive games as they would like.
quit.game <- FALSE 

#' The source function below ensures our helper functions from "Helper_Functions.R"
#' are loaded into memory and accessible in this script. Please refer to that
#' file for an overview of the different helper functions.
source("Helper_Functions.R") 

#' The start.game() function below is the main function for our game. It is first
#' called on Line XXXXX!!!!!!*** which loads the game for the first time. 
#' Later on, it is called after each round in the while() loop on LINE XXXXX until
#' the user decides to stop playing. 
start.game <- function(){
  
  # BEGINNING OF SECTION A: Data loading & random word choosing.
  
  #' The file word_list.txt is read and loaded as a data frame 'my_data'.
  #' There are 25 random words in this file that have been preloaded. 
  #' As there is no header & each word is on a separate line, header=F and sep="".
  my_data <- read.csv("word_list.txt", header=FALSE, sep="")
  
  #' This gets the number of words (rows) in the data frame my_data. 
  #' Stored as my_data.length and to be used later.
  my_data.length <- nrow(my_data)
  
  #' Get a single (one) number from the range 1 to the size of my_data data frame 
  #' (number of words). This will be the index of the random word chosen for this
  #' round. The sample() picks a random element from this index vector. 
  word.index <- sample(my_data.length, size=1)
  
  #' Using the index picked above, we use this to retrieve the random word in
  #' the respective position in our data frame. This will be the word on the respective
  #' row in the data frame. We apply our helper function string.upper() to this word
  #' to make it all uppercase. This will be a common feature in this code. All of
  #' the guesses, words, and characters will be made uppercase to facilitate 
  #' comparisons of values (without issues regarding uppercase vs. lowercase).
  #' Assumption: this game is NOT case-sensitive.
  #' We also store the length of this chosen word under 'word.length'. This will
  #' be used later.
  guess.word <- string.upper(my_data[word.index, 1])
  word.length <- nchar(guess.word)
  
  #' We now initiate a new vector called 'visual'. This will be a vector of 
  #' underscore characters repeated n times. Here n is equal to the length of 
  #' the random word chosen above. This will act as a visual indicator to the
  #' player of their progress in the game. The underscore represents a letter
  #' that they have not guessed yet. If they guess a correct letter, that letter
  #' will take the position of the respective underscore. 
  visual <- rep("_", word.length)
  
  # BEGINNING OF SECTION B: Initiating the Game
  
  #' Below we are setting a maximum number of guesses per round (thus per word).
  #' This is set to 10 and assigned to variable 'guess.cap'.
  #' We also initiate variable 'guess.counter' which will be used to track how
  #' many guesses the player has made in a round. Before the round starts,
  #' it is set to 0 by default. 
  guess.cap <- 10 
  guess.counter <- 0
  
  #' Printing rules to the player below.
  #' Including: maximum guess count per round & what constitutes a valid guess.
  #' This will be printed before each round.
  
  cat("===> Welcome to Hangman!", "Here's how to play: <===\n",
              "1. A random word has been chosen by the computer. You need to guess it!\n",
              "2. You have exactly", guess.cap, "guesses to do so.\n",
              "3. You may either guess one letter per attempt OR guess the whole word at once!\n",
              "4. If you guess the whole word at once, it must be of the same length as the mystery word.\n",
              "5. Be careful! If you try to guess the whole word at once", 
              "you will only win if it's a perfect match! No information will",
              "be provided about any letter overlap between the two words!!")
  
  #' We have added an extra function to this game. Once the rules of the game are
  #' printed (above), the user is prompted to begin the round. They must answer with
  #' 'YES' or 'Y' to begin the game. If they do not enter one of these inputs (upper
  #' or lowercase) the user will continue to be prompted indefinitely. 
  #' This function is dependent on the helper function 'start_game_consent(). Please
  #' refer to the helper functions file to understand its functionality.
  
  start_game_consent("Please enter 'Y' or 'YES' now to start the game:")
  
  # If the user has consented to start the round, the code below this comment will execute.
  
  #' Here we are telling the user the game has started.
  #' The user is provided the length of the mystery word.
  #' The user is also shown the visual indicator of their current progress. This
  #' progress is the set of underscore characters initiated earlier with variable 
  #' name 'visual'. As the user has not guessed anything yet, it will be all underscores.
  
  cat("THE GAME HAS STARTED! Hint: The secret word is ", word.length, " letters long.\n",
      "Your current progress: ", visual, sep="")
  
  #' To make the future guess evaluation easier, we are going to split the
  #' mystery word into a vector of it's single letters. For example "dog" will
  #' be split into "d", "o", "g" and assigned to vector 'split_word'.
  split_word <- strsplit(guess.word, split = "", fixed = TRUE)[[1]]
  
  #' We are also going to create an empty vector called 'already.guessed'.
  #' This will be used in each round to store the guesses the user has already
  #' made. This will be useful to validate subsequent guesses to ensure the
  #' user does not guess the same thing multiple times.
  already.guessed <- c()
  
  # BEGINNING OF SECTION C: User guessing & guess validation
  
  # While loop while guessing is allowed - under guess cap and visual != word:
  while(guess.counter < guess.cap & paste(visual, collapse = "") != guess.word){
    valid.guess <- guess.validator(word.length, already.guessed)
    
    #adding valid guess to end of vector of guesses. Repository to not guess again -- update notes in helper func
    already.guessed <- append(already.guessed, valid.guess)
    
    # For each valid guess, add one to our guess counter to keep track of the guess total
    guess.counter <- guess.counter + 1
    
    # FIRST CHECKING IF IT IS A WHOLE WORD GUESS - IF THE WHOLE WORD IS CORRECT
    if(string.upper(valid.guess) == string.upper(guess.word)){
      cat("SUCCESS! You have guessed the word correctly!\n",
          string.upper(guess.word), " was the word!")
      break
    }
    
    # If it was a word guess (nchar same == for guess and word) but incorrect, print this:
    if(nchar(valid.guess) == nchar(guess.word) & string.upper(valid.guess) != string.upper(guess.word)){
      cat("You have guessed the word: ", valid.guess, " - ","Unfortunately, that is incorrect!\n",
          "You have ", guess.cap-guess.counter, " guesses remaining!\n",
          "Your current progress:", visual, "\n", sep="")
    } else {
    
    # Else if it was not word guess --> do character loop checks
    # CHECKING EACH CHARACTER OF GUESS WORD TO SEE IF ITS A MATCH
    
      for(i in 1:length(split_word)){
        
        # if guess character is in word
        if(string.upper(valid.guess) == string.upper(split_word[i])){
          
          visual[i] <- string.upper(valid.guess)
          
          cat("Success! ", string.upper(valid.guess), " is in the word!\n",
              "You have ", guess.cap-guess.counter, " guesses remaining.\n",
              "Your current progress: ", visual, "\n", sep="")
        }
      }
      # once the for loop has run, if there was no successes -> 
      #check if guessed character is in the vector of chars from word
      # if it is not, inform the user
      if(!(string.upper(valid.guess) %in% split_word)){
        cat(valid.guess, " is not in the word. Try again!\n",
            "You have ", guess.cap-guess.counter, " guesses remaining\n",
            "Your current status:", visual, "\n", sep="")
      }
    }
    
  } # end of guess count checker while loop
  
  # If while loop is over, no more tries left
  # if out of guesses & final guess was not the last needed letter:
  if(guess.counter == guess.cap & paste(visual, collapse = "") != guess.word){
    cat("You are out of guesses.. GAME OVER!\n",
        "The secret word was: ", guess.word, sep="")
  }
  # if the user got it: 
  if(paste(visual, collapse = "") == guess.word){
    print("Congrats! You guessed the word!")
  }
  
  
  
  # Below is a condition check at the end of each round: see if the user wants to play again by calling
  # helper function check.play.again()
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


# TO DO
# Fix comments here
# Fix helper function comments
# AFTER EACH GUESS / WHEN TRYING SAME GUESS AGAIN SHOW ALREADY GUESSED VECTOR already.guessed