# set the working directory for the session to where the datasets are stored
setwd("C:/Users/paul/Desktop/Coursera/assignments/R_Programming/week4")

#-------------------------------------------------------------------------------
# Create a function that 
#     reads outcome dataset
#     returns character vector of hostpital with the lowest 30 day mortality rate for specified outcome in the state
#     hostpital name is taked from variable Hospital.Name Variable
# The outcomes can be one of 
#     heart attack, heart failure, pneumonia 
#
# If there is a tie for a given outcome, sort hostipital names alphabetically ( A - Z ) and higest wins ( i.e A) 


best <- function( state, outcome) {
      
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
      
      # create a dataframe that contains no missing values
      #
      cleaned_df <- na.omit(outcome_ds[ outcome_ds$State  == state  , c(2,7,outcome_lookup[outcome])])
      names(cleaned_df) <- c("Hospital", "State", "req_outcome")
      
      #sort by requested outcome and hospital name
      #
      sorted <- cleaned_df[order( cleaned_df$req_outcome, cleaned_df$Hospital) , ] 
      View(sorted)
      return( sorted[1,1] )
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("BB", "hert attack")
