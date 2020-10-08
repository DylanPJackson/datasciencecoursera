# filename
#       corr.R
# 
# description
#       Take a directory of data files and a threshold for complete cases
#       and calculate the correlation between sulfate and nitrate for monitor
#       locations where the number of completely observed cases is greater than
#       the threshold.
#
# author
#       Dylan P. Jackson

corr <- function(directory, threshold = 0){
    # Initialize return vector
    corrs <- numeric()

    # Read in the entire specdata directory
    path <- sprintf("%s/%s.csv", directory, "%03d")
    paths <- lapply(1:332, sprintf, fmt = path)
    tbl <- lapply(paths, read.csv)
    size <- length(tbl)
    
    for (table_num in 1:size){
        # Get indices of reports with sulfate data reported
        sulfate_ind <- !is.na(tbl[[table_num]]$sulfate)
        # Get indices of reports with nitrate data reported
        nitrate_ind <- !is.na(tbl[[table_num]]$nitrate)
        # Get indices where both pollutants are reported
        both_pol_ind <- sulfate_ind & nitrate_ind
        # Get summary of complete cases
        comp_cases <- table(both_pol_ind) 
        num_comp <- comp_cases[2]
        if (is.na(num_comp)){
            num_comp <- list(0)
        }
        # Check if number of complete cases is greater than the threshold
        if (num_comp[[1]] > threshold){
            # Add correlation of sulfate and nitrate to vector
            sf_cor <- cor(x = tbl[[table_num]]$sulfate[both_pol_ind], 
                          y = tbl[[table_num]]$nitrate[both_pol_ind])                    
            corrs <- append(corrs, sf_cor)
        }
    }
    return (corrs)
}
