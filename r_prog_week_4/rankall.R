source("rankhospital.R")

# Provides Hospital Name in each state for given ranking and outcome

rankall <- function(outcome, num = "best"){
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    outcomes <- outcomes[order(outcomes$State),]
    states <- unique(outcomes$State)
    num_states <- length(states)

    rankings <- data.frame(hospital <- numeric(0), state <- numeric(0))
    for (state in states){
        name <- rankhospital(state, outcome, num)
        new_row <- c(name, state)
        rankings <- rbind(rankings, new_row) 
    }
    return (rankings)
}
