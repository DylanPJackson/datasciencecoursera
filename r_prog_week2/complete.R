# filename
#       complete.R
# 
# description
#       Reads a directory full of files and reports the number of completely
#       observed cases in each data file.
#
# author
#       Dylan P. Jackson

complete <- function(directory, id = 1:332){
    # Read in all files with given id(s)
    path <- sprintf("%s/%s.csv", directory, "%03d")
    paths <- lapply(id, sprintf, fmt=path)
    tbl <- lapply(paths, read.csv)
    size <- length(tbl)

    # Initialize data frame of complete reports
    df_comp <- data.frame(id = numeric(), nobs = numeric())

    for (table_num in 1:size){
        # Get indices of reports with sulfate data reported
        sulfate_ind <- !is.na(tbl[[table_num]]$sulfate)
        # Get indices of reports with nitrate data reported
        nitrate_ind <- !is.na(tbl[[table_num]]$sulfate)
        # Get indices where both pollutants are reported
        both_pol_ind <- sulfate_ind & nitrate_ind
        # Get summary of complete cases
        comp_cases <- table(both_pol_ind)
        # Get number of complete cases
        # Doesn't currently acccount for if there are no complete cases 
        num_comp <- comp_cases[2]
        # Add to dataframe
        new_row_df <- data.frame(id = id[table_num], nobs = num_comp[[1]]) 
        df_comp <- rbind(df_comp, new_row_df)
    }
    return (df_comp)
}
