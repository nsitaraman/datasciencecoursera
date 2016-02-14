rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_measures <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    #conversions
    outcome <- tolower(outcome)
    
    # convert best and worst rank to numeric
    if (num == "best") num = 1 
    
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check that outcome is valid
    if (!(outcome %in% names(outcomes))) stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    
    # For a given outcome find the subset of hospitals
    hospitals <- outcome_measures[ , c(2, 7, outcomes[outcome])]
    
    # Sort hospitals by state, then mortality rate and finally hospital name, remove missing values
    hospitals <- hospitals[order(hospitals[ , 2], hospitals[ , 3], hospitals[ , 1], na.last = NA), ]
    
    # split hospitals by state
    state_hospitals <- split(hospitals, hospitals[ , 2])
    
    #subset state hospital names with rank number and unlist to get named char vector 
    if (num == "worst") ranked_hospitals <- unlist(lapply(lapply(lapply(state_hospitals, "[", 1), tail, 1), "[[", 1))
    else ranked_hospitals <- unlist(lapply(lapply(state_hospitals, "[", 1), "[", num, ))
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data.frame(hospital = ranked_hospitals, state = names(ranked_hospitals), row.names = names(ranked_hospitals))
    
    
}
