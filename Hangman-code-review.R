# MSC2011H Hangman Assignment - CODE REVIEW
# IMRAN RHEMTULLA

# GENERAL COMMENT: Placing the entirety of the code into a single function and then running that function helps with user readability
  # of printed messages ie)

hangman <- function(){
  #Read the word dictionary 
  WD <- read.csv("hangman_words.txt", sep="", header = F) 
  # REVIEW COMMENT 1: I would include a test file with your next submission if needed
  
  #Make data frame into vector so the sample function can read a 1D object
  WDvec <- as.vector(WD$V1)
  
  #Randomly selecting a word from the sample
  secret_word <- WDvec[sample(1:length(WDvec), 1)]
  
  #Getting the length of the random secret word
  wordlength <- nchar(secret_word)
  
  #Informing the user about the length of their secret word
  print(paste("Your secret word has", wordlength, "letters"))
  
  #Informing the user about the rules of the game
  print(paste("You have 9 turns to guess a word or you lose the game"))
  
  # Number of wrong guesses the player starts with
  turnsleft <- 9
  
  # To be used later when showing the letters guessed
  lettersused <- "letters"
  
  #Get user input and check that input is only one letter
  while(turnsleft > 0 ) {
    guess <- readline("Please input ONE letter: ") # REVIEW COMMENT: Changed to make user input cleaner
    # check if the input is more than one letter; if it is ask for new input
    if(nchar(guess) > 1) {
      
      # REVIEW COMMENT: This error handling method doesn't account for other types of invalid entries such as non-letters
        # you could use a character library of valid ones and the %in% operator to ensure that the guess is one of those 
        # valid characters
      
      print("Error. More than one letter. Please input another letter: ") # REVIEW COMMENT: Changed to make user input cleaner
    } else {
      # Check if the letter input is a letter in the secret word
      if(grepl(guess, secret_word, ignore.case = T)) { 
        
        # REVIEW COMMENT: good use of grep, but this only accounts for 1 of 2 letters if there are 2 of the same letter of the word
          # you could use grep to find out where in the word the letter is and then sum those TRUE values:
          # (sum(grepl(guess, secret_word[[1]], ignore.case = T))
        
        # Adds correct letter to guessed letters
        lettersused <- paste(lettersused, guess)
        # prints all of the correct guesses 
        print(paste(lettersused,"_"))
        # tells the user the letter is in the word
        print("correct guess")
        # subtracts the amount of letters to guess from the word
        wordlength <- wordlength - 1
        # if all letters in the word have been guessed, player wins. 
        if(wordlength == 0) {
          print(paste("Your Seceret word is", secret_word))
          break
        } 
        # When the guess is not part of the secret word 
      } else {
        #Take away 1 turn
        turnsleft <- turnsleft - 1
        # Tell use how many turns left
        print(paste("No,", turnsleft, "guesses left."))
        # When turns left is at 0, print the game is done 
        if(turnsleft == 0) {
          print(paste("No more turns. Your secret word is: ", secret_word))
        } 
      }
    }
  }
}

hangman()

