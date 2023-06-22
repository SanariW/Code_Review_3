# Helper Functions for Hangman Game - Assignment 3
# Must be used in conjunction with "Assignment_3_Hangman.R"

# FUNCTION 1:
# Text Capitalizer: Capitalizes all text in a string and returns the capitalized string.
string.upper <- function(input_string){
  
  #' Here I am splitting input_string into each a vector of characters and applying the
  #' toupper() built-in function on it. This makes each element upper case. 
  #' Then sticking them back together to return the whole string as one object.
  upper.str <- paste(sapply(strsplit(input_string, ""), toupper), collapse = "")
  
  # Return capitalized string
  return(upper.str)
} # End of helper function 1

#' FUNCTION 2: 
#' Loop for getting user input: accepting only ("Y" OR "Yes") for round start consent.
#' Takes in parameter 'my_prompt' which will be used for readline() to ask the user
#' for their input. 
start_game_consent <- function(my_prompt){
  
  # Repeat loop until break is reached inside the if() condition this is when consent is given.
  repeat{
    # Prompting user with readline() function for input. Prompt is passed in from the hangman game.
    answer <- readline(prompt = my_prompt)
    
    #' Using helper function string.upper() to standardize all logic in upper
    #' characters only. Reduces errors when doing checks of equivalence. 
    if(string.upper(answer) == "YES" | string.upper(answer) == "Y"){
      break
    } else {
      cat("Please enter YES or Y to start the game")
    }
  }
} # End fo helper function 2


# FUNCTION 3:
#' Check if user wants to play again. Prompt them for their input. Valid inputs
#' are either YES/Y or NO/N (case insensitive). 
#' Function either returns True or False.
check.play.again <- function(){
  
  #' User input prompt is repeated until a valid input is given. 
  #' Either YES/Y or NO/N (case insensitive).
  repeat{
    answer <- readline(prompt = "Want to play again? Enter YES or NO:")
    
    # Check if user wants to play again. If yes, return True.
    if(string.upper(answer) == "YES" | string.upper(answer) == "Y"){
      return(TRUE)
    }
    # Check if user wants to stop playing. If they want to stop, return False.
    if(string.upper(answer) == "NO" | string.upper(answer) == "N"){
      return(FALSE)
    }
  }
} # End of helper function 3. 


#' FUNCTION 4:
#' This is the Guess Validator Function. It is used to prompt the user for an input.
#' This input is the guess for the Hangman game. Each user input is evaluated to check
#' if it is valid. If the input is valid, it is returned. If it is not, this is 
#' printed to console and the user is prompted to input again. 
#' The function takes in two parameters: guess.word.len and vector_of_guesses.
#' The first is the length of our target mystery word. The second is the vector
#' of guesses the user has already made. This is used to validate if the input
#' given is valid. 

guess.validator <- function(guess.word.len, vector_of_guesses){
  
  repeat{
    # Getting user input via readline() function.
    answer <- readline(prompt = "Please enter your guess:")
    
    #Check different input types for their respective defensive programming validations
    
    #' If input is numeric, print the error to the user and tell them their input is numeric.
    #' Remind the user of the input criteria: single letter or the word they think it is. 
    
    if(!is.na(as.numeric(answer))){
      cat("You have entered: ", answer, " That is not a valid entry!\n",
          "This is a number. ",
          "Please only guess one letter OR the word you think it is.\n", sep="")
    }
    
    
    #' IF input is not a number, check if it's a boolean. If the input is a boolean
    #' (True or False) print the error to the user and remind them of the input criteria:
    #' a single letter or the word they think it is. 
    else if(answer == "TRUE" | answer == "FALSE"){
      cat("You have entered: ", answer, " That is not a valid entry!\n",
          "This is a boolean. ",
          "Please only guess one letter OR the word you think it is.\n", sep="")
    }
    
    #' IF input is not a number or a boolean, check if the input has already been guessed before.
    #' If it has been guessed before it will be in the 'vector_of_guesses'. 
    #' Should this is the case, print the guess to the user and remind them that
    #' they have already guessed this previously. They should try something else. 
    else if(answer %in% vector_of_guesses){
      cat("You have already guessed: ", answer, " previously. ",
          "Please guess something else!", sep="")
    }
    
    #' If the input is not a number, not a boolean, and has not been guessed before
    #' check the following. Here we are checking if the regular expression of the
    #' user input follows the 'alpha' pattern. That is, it's only consisting of letters.
    #' This is done using the grepl() function.
    #' We are also checking if the number of characters in the input is equal to one.
    #' If both conditions are met, this is a valid one letter guess. Return it. 
    else if(grepl("^[[:alpha:]]+$", answer) == TRUE & nchar(answer) == 1){
      return(answer)
    }
    
    #' If the input is not a number, not a boolean, has not been guessed before, and
    #' is not a one-letter guess, check the following. Here we are checking if the
    #' regular expression of the user input follows the 'alpha' pattern. That is,
    #' it's only consisting of letters. This is done using the grepl() function.
    #' We are also checking if the number of characters in the input is equal to
    #' the number of characters in the mystery word. If they are of the same length
    #' the following is a valid letter-only word guess. Return this guess as it's OK.
    else if(grepl("^[[:alpha:]]+$", answer) == TRUE & nchar(answer) == guess.word.len){
      return(answer)
    }
    
    #' If for some reason, none of the validation checks above are met, then
    #' run this else statement to tell the user the input guess is invalid. Also
    #' show them what their input was (in case it was a typo).
    #' Remind them what the criteria for the valid inputs is. 
    else{
      cat("Invalid Entry. You have guessed: ", answer, "\n",
          "This does not meet the criteria for valid inputs.\n",
          "Please only guess one letter OR the word you think it is.\n", sep="")
    }
    
  } # end of repeat loop
} # end of helper function 4

