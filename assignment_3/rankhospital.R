# set the working directory for the session to where the datasets are stored
setwd("C:/Users/epauste/Desktop/Coursera/assignments/R_Programming/week4")

rankhospital <- function( state, outcome, num) {
      
      # create outcome_ds from csv file 
      #
      outcome_ds <- read.csv( "outcome-of-care-measures.csv", stringsAsFactor =FALSE, na.strings = 'Not Available')
      
      # Check that state is a valid entry. first get the unique state list, then test the received state matched from the unique list
      #
      valid_state_list <- unique(outcome_ds$State)
      if( !state %in% valid_state_list ) stop('invalid state')
      
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
      cleaned_df <- na.omit(outcome_ds[ outcome_ds$State  == state  , c(2,7,outcome_lookup[outcome])])
      names(cleaned_df) <- c("Hospital", "State", "req_outcome")
      
      #sort by requested outcome and hospital name
      #
      sorted <- cleaned_df[order( cleaned_df$req_outcome, cleaned_df$Hospital) , ] 
      View(sorted)
      
      if (num == "best") {
        return ( sorted[1,1] )
      
      } else if (num == "worst") {
          
        return ( sorted[nrow(sorted),1] )
      
      } else {
          return ( sorted[num,1] )
      }

}

rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)
rankhospital("MN", "heart attack", "worst")
rankhospital("MN", "heart attack", "best")
