# set the working directory for the session to where the datasets are stored
setwd("C:/Users/epauste/Desktop/Coursera/assignments/R_Programming/week4")

rankall <- function( outcome, num) {
      
        # remove later 
         outcome <- "heart failure"
         num <- 4
      # create outcome_ds from csv file 
      #
      outcome_ds <- read.csv( "outcome-of-care-measures.csv", stringsAsFactor =FALSE, na.strings = 'Not Available')
      
      # check if the outcome variable is valid
      #
      outcome_lookup <- c("heart attack"   = 11, 
                          "heart failure"  = 17,
                          "pneumonia"      = 23
                          )
      
      if (!outcome %in% names(outcome_lookup)) stop('invalid outcome')
      
      # check if num variable is valid
      # Reference : http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer
      if (num != "best" && num != "worst" && num%%1 != 0) stop ('invalid num supplied: Must be either an integer value, or character best or worst')
      
      
      
      # create a dataframe that contains no missing values
      #
      
      valid_state_list <- unique(outcome_ds$State)

      
      cleaned_df <- na.omit(outcome_ds[ , c(2,7,outcome_lookup[outcome])])
      names(cleaned_df) <- c("Hospital", "State", "req_outcome")
      
      #sort by requested outcome and hospital name
      #
      sorted <- cleaned_df[order( cleaned_df$State, cleaned_df$req_outcome) , ] 
      View(sorted)
      
      
      
      test <- sorted
      x <- split(test, test$State)
     # x <- lapply(split(test, test$State), )
      
      print(x)
      print(class(x))
      str(x)
      
      if (num == "best") {
        return ( sorted[1,1] )
      
      } else if (num == "worst") {
          
        return ( sorted[nrow(sorted),1] )
      
      } else {
          return ( sorted[num,1] )
      }

}
rankall("heart failure", 4)
#rankall("heart attack", 5000)
#rankall("heart attack", "worst")
#rankall("heart attack", "best")
