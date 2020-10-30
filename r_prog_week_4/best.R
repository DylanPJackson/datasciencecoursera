best <- function(state, outcome){
    # Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #outcomes[,11] <- as.numeric(outcomes[,11])
    #outcomes[,17] <- as.numeric(outcomes[,17])
    #outcomes[,23] <- as.numeric(outcomes[,23])

    # Check that state and outcome are valid
    if (!(state %in% outcomes$State)){
        stop("invalid state")
    }
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% valid_outcomes)){
        stop("invalid outcome") 
    }

    # Need to get sub dataframe containing hospital name which are in the
    # specified state, and the death rate for given outcome
    names <- outcomes$Hospital.Name[outcomes$State == state]
    rates <- if (outcome == "heart attack"){
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcomes$State == state]
            } else if (outcome == "heart failure"){
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcomes$State == state]
            } else {
                outcomes$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcomes$State == state]
            }
    valid_inds <- (rates != "Not Available")
    names <- names[valid_inds]
    rates <- as.numeric(rates[valid_inds])
    hosp_df <- data.frame(name = names, rate = rates)

    # Sort by death rate, name
    hosp_df_sorted <- hosp_df[order(hosp_df$rate, hosp_df$name),]
    #return (hosp_df_sorted)
    best_hosp <- hosp_df_sorted$name[1]
    return (best_hosp)
}
