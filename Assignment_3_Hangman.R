# Assignment 3: Hangman Game

#' INSTRUCTIONS TO USER: HOW TO LOAD WORD LIST & RUN SCRIPT ** (TO DO)

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
