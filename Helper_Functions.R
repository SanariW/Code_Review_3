# Helper Function

# FUNCTION 1:
# Only accepting strings with letters


# FUNCTION 2:
# Text Capitalizers: Capitalizes all text in a string and returns it.

string.upper <- function(input_string){
  # Here I am splitting input_string into each character and applying the
  # toupper() built-in function on it. Then sticking them back together. 
  upper.str <- paste(sapply(strsplit(input_string, ""), toupper), collapse = "")
  
  # Return capitalized string
  return(upper.str)
}

# FUNCTION 3: 
# Loop for getting user input: accepting only ("Y" OR "Yes").
start_game_consent <- function(my_prompt){
  
  repeat{
    answer <- readline(prompt = my_prompt)
    # Using helper function string.upper() to standarize all logic in upper
    # characters only. Reduces errors when doing checks
    if(string.upper(answer) == "YES" | string.upper(answer) == "Y" ){
      break
    }
  }
  
  return(answer)
}

