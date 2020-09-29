# filename
#       pollutantmean.R
#
# description
#       Calculates the mean of a pollutant across a specified list of monitrs
#
# author
#       Dylan P. Jackson

pollutantmean <- function(directory, pollutant, id = 1:332){
    # Read in all files with given id(s)
    path <- sprintf("%s/%s.csv", directory, "%03d") 
    paths <- lapply(id, sprintf, fmt=path)
    tbl <- lapply(paths, read.csv)
    size <- length(tbl)

    # Variables to compute mean
    sum <- 0
    count <- 0
    for (table_num in 1:size){
        # Get column index of specified pollutant
        col_ind <- which(colnames(tbl[[table_num]]) == pollutant) 
        # Get vector of pollutant values 
        vals <- tbl[[table_num]][,col_ind][!is.na(tbl[[table_num]][,col_ind])]
        # Add to the total sum 
        sum <- sum + sum(vals)
        # Add to the total count 
        count <- count + length(vals)
    }
    return(sum / count)
}
