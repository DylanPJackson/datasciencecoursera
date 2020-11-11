# Provides the num ranked hospital for a given outcome in a state

rankhospital <- function(state, outcome, num = "best"){
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Check that state and outcome are valid
    if (!(state %in% outcomes$State)){
        stop("invalid state")
    }
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% valid_outcomes)){
        stop("invalid outcome")
    }
    
    names <- outcomes$Hospital.Name[outcomes$State == state]
    rates <- if(outcome == "heart attack"){
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

    # Return the numth Hospital Name
    length_df <- nrow(hosp_df_sorted)
    if (num == "best"){
        return (hosp_df_sorted$name[1])
    } else if (num == "worst"){
        return (hosp_df_sorted$name[length_df])
    } else {
        if (num > length_df){
            return (NA)
        } else {
            return (hosp_df_sorted$name[num])        
        }
    }
}
