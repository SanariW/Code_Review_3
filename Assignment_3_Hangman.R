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
  # also making it uppercase
  guess.word <- string.upper(my_data[word.index, 1])
  word.length <- nchar(guess.word)
  
  # Creating a visual of _____ underscores to visually show user their pgoress. 
  # The size of this is dependent on the random word chosen
  visual <- rep("_", word.length)
  
  # BEGINNING OF SECTION FOR LOADING THE GAME
  
  # Setting the maximum number of guesses
  guess.cap <- 10 
  guess.counter <- 0
  
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
  
  # Telling user length of secret word and showing visual cue.
  cat("THE GAME HAS STARTED! Hint: The secret word is ", word.length, " letters long.\n",
      "Your current progress: ", visual, sep="")
  
  # Splitting guess word into a vector of individual characters
  split_word <- strsplit(guess.word, split = "", fixed = TRUE)[[1]]
  
  # Repository of guessed words and letters so user cant guess again
  already.guessed <- c()
  
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