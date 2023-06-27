# Assignment 3: Hangman Game
# SW: CODE REVIEW by Sanari Wickramaratne (SW)
# SW: Code review comments tagged with SW at the beginning of each line. 
# SW: Code review comments are dispersed throughout this script under sections of code it is referring to. 

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
#' As such, the user can play as many rounds as they want. At the end of each round
#' they have the decision to exit the game and the script executes. As such, there
#' is no infinite loop that is not exitable. There is by design an infinite number
#' of loops (the user COULD play the game infinitely many times if they want...).
#' 3) This game is not case-sensitive. Words and guesses will be valid no matter
#' if they are lower or uppercase. 
#' 4) The user has 10 guesses per round (one round = guessing one word). They
#' can either guess a single letter OR a word of the same length as the mystery
#' word.
#' 5) Duplicate guesses are not allowed. Each guess must be unique (per round).
#' 6) If letters are repeated in the word, both will be revealed. This only
#' counts as one guess (oh yeah)!
#' 7) This game uses a text file with random words as the "database" of possible
#' solutions. This file has 25 words on it. So if you play enough rounds there's 
#' a chance a word will be repeated.

# SW: Very clear and detailed instructions for running this code as well as explaination of rules for the game. 
# SW: I like how you made sure the game is not case-sensitive. 

# BEGINNING OF HANGMAN SCRIPT BELOW

#' The Boolean below is a check for our while loop on line 153.
#' This is required for the logic to check if the user wants to play another
#' round or not. The user can play as many successive games as they would like.
quit.game <- FALSE 

#' The source function below ensures our helper functions from "Helper_Functions.R"
#' are loaded into memory and accessible in this script. Please refer to that
#' file for an overview of the different helper functions.
source("Helper_Functions.R") 

# SW: The descriptions of all of your helper functions were very clear and easy to follow along. 
# SW: I like the use of toupper() to make each element upper case to account for upper and lower case input. 
# SW: You could also use grepl, regex and [a-zA-z] to account for upper and lower case letter inputs. 
# SW: For function 2 & 3, I like the prompt for users to start the game and play the game again. 
# SW: The repeat loop as part of function 4 looks great in determining valid entries and 
# SW: all of the different possibilities a user can input. 

#' The start.game() function below is the main function for our game. It is first
#' called on Line 319 which loads the game for the first time. 
#' Later on, it is called after each round in the while() loop on Line 320 until
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

  # SW: You could also use the readLines() to load your text file with your wordlist instead of using read.csv(). 
  # SW: However, I understand why you used read.csv(). In order to simplify your code, this could have been an option. 
  # SW: An alternative to retrieve the random word could be to use the tolower(sample()) to 
  # SW: make the user input lower case and retrieve. 
  # SW: I like how all of these operations were stored under the start.game function.  
  # SW: It makes your code very organized and easy to follow along with! 
  # SW: The use of rep() for the visual progress of game looks great! 
  # SW: A minor suggestion would be to seperate your underscores with a space. 
  # SW: It was a bit difficult to see how many underscores there were in total. 
  # SW: I would suggest to include sep = " " in your function to achieve this. 
  # SW: Overall, this section is very comprehensive, works perfectly, and includes all necessary elements!!  
  
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
              "you will only win if it's a perfect match!\n", 
              "No information will be provided about any letter overlap between the two words!!\n",
              "6. If letters are repeated in the mystery word, both will be revealed.",
              "This only counts as one guess (oh yeah)!")
    
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

  # SW: I appreciate the very descriptive and easy to understand set of rules at the beginning of the game for the user!!
  # SW: This section of the code runs well. Good use of the strsplit() to split the mystery word. 

  # BEGINNING OF SECTION C: User guessing & guess validation
  
  # While loop while guessing is allowed - under guess cap and visual != word:
  
  #' The while() loop below acts as a checker for the game to ensure the player
  #' is still allowed to guess. The players is still allowed to guess if:
  #' (a) their number of guesses 'guess.counter' is less than the maximum number
  #' of guesses which is 10 'guess.cap'.
  #' (b) the concatenation of guessed characters or words does not equal the
  #' target mystery word. This is the second part of the AND statement below.
  #' In this we are combining the 'visual' progress of the player and equating
  #' it to the guess word to check if it's the same or not. 
  while(guess.counter < guess.cap & paste(visual, collapse = "") != guess.word){
    
    #' If the user is still allowed to guess, we are prompting them to make a guess.
    #' This user input prompt is located within helper function guess.validator().
    #' Once the user input is received, it is validated to ensure it is a valid guess.
    #' The helper function guess.validator() takes in two arguments: one is
    #' the word length of our mystery word, and the second is the vector of guesses
    #' the user has already made.
    #' For details on how the user guess is validated, please refer to the helper function.
    #' The user's guess will only be assigned to 'valid.guess' if it passes the
    #' validation of the helper function, which returns the guess and assigns
    #' it to this variable.
    valid.guess <- guess.validator(word.length, already.guessed)
    
    #' If the guess is valid and has been assigned to 'valid.guess' we will append (add)
    #' it to our vector of user guesses. This guess will now be prohibited in subsequent
    #' guesses for this word. This check is also done in the helper function guess.validator().
    already.guessed <- append(already.guessed, valid.guess)
    
    #' Similarly, if the guess is valid we also increase our 'guess.counter' by one
    #' to keep track of the number of guesses the user has made for this word.
    guess.counter <- guess.counter + 1
    
    # FIRST CHECKING IF IT IS A WHOLE WORD GUESS - IF THE WHOLE WORD IS CORRECT
    
    #' Here we check if the user guessed the whole word (of the same length as mystery word).
    #' This would be like a 'perfect match'. E.g. word is 'dog' and user guesses 'dog'.
    #' If the uppercase of the guess is equal to the uppercase of the guess word, we
    #' set the 'visual' progress bar equal to the word (will be used as a check later)
    #' and we break out of the while loop (used for guessing rounds) as it is no longer 
    #' needed. Helper function string.upper() is used again to make the comparison
    #' of strings easier.
  
    if(string.upper(valid.guess) == string.upper(guess.word)){
      
      visual <- string.upper(guess.word)
      break
    }
    
    #' If the user did guess a whole word (of same length as mystery word) but
    #' the word they guessed was not the mystery word, we inform them that this
    #' guess is incorrect. We print their guess to remind them what it was. We
    #' also print the number of guesses they have left (guess.cap minus guess.counter).
    #' Lastly, we also print the visual underscore progress cue to show them which
    #' characters (if any) they have already guessed correctly. Helper function
    #' string.upper() is used again to make string comparison easier.
    #' Note: if a whole word is guessed no partial letter matches are returned. Whole
    #' word guesses only work if they get the perfect match to the mystery word. 
    
    if(nchar(valid.guess) == nchar(guess.word) & string.upper(valid.guess) != string.upper(guess.word)){
      cat("You have guessed the word: ", valid.guess, " - ","Unfortunately, that is incorrect!\n",
          "You have ", guess.cap-guess.counter, " guesses remaining!\n",
          "Your current progress:", visual, "\n", sep="")
    } 
    
    #' The if block below is run if the guess was a single character. The two
    #' if statements previously were for word guesses, whereas the logic in 
    #' this section will be for single character guesses. No stricter validation
    #' of the guess is required because our helper function in the valid.guess
    #' variable assignment already did this. The if statement below is just used
    #' to dictate what to do next in the game. 
    if(nchar(valid.guess) == 1){
    
    # CHECKING EACH CHARACTER OF GUESS WORD TO SEE IF ITS A MATCH
    
      #' This for loop iterates over the vector of numbers ranging from 1 to
      #' the length of the mystery word. For example if the mystery word is
      #' 'house', the for loop will run through the vector (1,2,3,4,5) as
      #' the length of 'house' is 5 characters. In this case i will be 
      #' assigned to each of the numbers in this character vector. We will
      #' use i to access the ith element in the vector 'split_word'. This
      #' vector houses the individual characters of our mystery word.
      for(i in 1:word.length){
        
        #' Here we are checking if the single letter guess 'valid.guess'
        #' is equal to the value of the letter at the ith position in the
        #' 'split_word' vector which consists of all letters in our mystery word.
        #' As such, we loop over each character to see if there is a match.
        #' Helper function string.upper() is used again to make the equivalence 
        #' check easier (case insensitive).
        if(string.upper(valid.guess) == string.upper(split_word[i])){
          
          #' If there is a match of the guessed letter with the ith position of
          #' our 'split_word' vector, we updated our visual progress tracker 'visual'
          #' at the respective position (i). For example, if the user guesses 'e'
          #' and there is an 'e' at the 2nd and 3rd position of the word visual will
          #' update to show the character 'e' at thos positions instead of an underscore.
          visual[i] <- string.upper(valid.guess)
          
          #' Similarly, if there is a match of the guessed letter at the ith
          #' position of the mystery word, we print this success to the console.
          #' The user can see what letter they guessed, that there was a match,
          #' how many guesses they have left, and what their current progress is
          #' respective to the 'visual' progress indicator (underscores and characters).
          cat("Success! ", string.upper(valid.guess), " is in the word!\n",
              "You have ", guess.cap-guess.counter, " guesses remaining.\n",
              "Your current progress: ", visual, "\n", sep="")
        }
      }
      # once the for loop has run, if there was no successes -> 
      #check if guessed character is in the vector of chars from word
      # if it is not, inform the user
      
      #' Once all the characters have been looped through for the mystery word
      #' (in split_word vector), if the user's letter guess is not in the target
      #' vector of letters from the mystery word, print a failed guess alert to user.
      #' In this case, we will print their guess, the finding that this letter
      #' is not in the word, the number of guesses they have left (guess cap minus
      #' guess counter) and their current progress stored in 'visual'.
      if(!(string.upper(valid.guess) %in% split_word)){
        cat(valid.guess, " is not in the word!\n",
            "You have ", guess.cap-guess.counter, " guesses remaining\n",
            "Your current status:", visual, "\n", sep="")
      }
    }
    
  } # This is the end of the while loop which checks that the user can still guess. 
  
  #' BEGINNING OF SECTION D: User can no longer guess. Either because they
  #' or out of attempts OR because they have guessed the correct word. 
  
  #' If the user has reached the maximum number of guesses and they have not
  #' yet fully guessed the word (concatenation of visual is not equal to the
  #' mystery word) then inform the user the game is over and they are
  #' out of guesses. Reveal the secret word to the user.
  #' Note: if the user guessed the last letter / or the right word on their
  #' last attempt this will not run as the concatenation of visual will match
  #' the mystery word. As such, this should not run as the user managed a last
  #' guess miracle to guess the word!
  #' Note: helper function string.upper() is used to ensure consistent equivalence
  #' checks of the words and characters. 
  
  if(guess.counter == guess.cap & paste(string.upper(visual), collapse = "") != string.upper(guess.word)){
    cat("You are out of guesses.. GAME OVER!\n",
        "The secret word was: ", guess.word, sep="")
  }
  
  #' If the concatenation of the visual progress characters (letters that replace
  #' default underscores if guessed correctly) equals the target mystery word,
  #' the user has won! As such, print a success message and show them what
  #' the mystery word was. 
  #' Note: helper function string.upper() is used to ensure consistent equivalence
  #' checks of the words and characters. 
  if(paste(string.upper(visual), collapse = "") == string.upper(guess.word)){
    cat("Congrats! You guessed the word! The word was: ", guess.word, sep="")
  }
  
  #' Below is a condition check at the end of each round: 
  #' See if the user wants to play again by calling helper function check.play.again()
  #' If the user decides not to play again use a super assignment on variable quit.game
  #' Make this boolean TRUE which will break the while() loop to run successive rounds.
  
  if(check.play.again() == FALSE){
    quit.game <<- TRUE 
  }
  
} # This is the end of my start.game() function which is the core logic for the game.


#' BELOW IS THE MAIN ENGINE OF MY GAME:
#' First it calls start.game() function once to execute the game for the first time.
#' Then it has a while() loop running which checks the condition of variable quit.game.
#' After each round, the user can decide to play again or exit the hangman game.
#' If the user types "NO" or "N" after a round the while loop condition is not me
#' and hence breaks the loop. No more calls to start.game() are made. 

start.game()
while(quit.game != TRUE){
  start.game()
}

#' If while loop no longer executes, the user has chosen to stop playing.
#' Print a thank you message to user for playing the game.
cat("Thanks for playing!\n", "Terminating Game....\n", sep="")


# SW: Final Code Review Comments ####

# SW: Required functionality: 
# SW: Overall, this code was easy to follow along with, ran well without any errors, 
# SW: was clearly explained, and included ALL required elements. 
# SW: This code included a dictionary of words in a txt. file and the word list was effectively read using read.csv(). 
# SW: A random element from the word list was chosen through the correct use of sample(). 
# SW: The length of the secret word was provided through the use of nchar(). 
# SW: Great work initializing all of the variables and effectively providing number of wrong guesses and tries. 
# SW: I like the added features of determining if a character is a letter (as seen in the helper functions). 
# SW: The code effectively determined if input was in the mystery word. 
# SW: It accurately provides a visual progress of game and the remaining number of guesses and wrong tries. 
# SW: In general, I really liked how you split your code into sections and included in-depth comments for each line of code.
# SW: Your while loop seems to working well and provided the expected outputs. 
# SW: By seperating your while loop by comments explaining the purpose of each function and organizing your code into sections,
# SW: it contributes to the overall readability of your code.
# SW: Overall, your code worked perfectly, provided the outputs I was expecting, 
# SW: was clear and easy to understand, with a consistent style guide used throughout. 
# SW: Your game was a lot of fun to play! Excellent work!! 
