above_n <- function(x, n = 10){
    ind <- x > n
    x[ind]
}

mean_cols <- function(x, removeNA = TRUE){
    n_col <- ncol(x)
    means <- numeric(n_col) 
    for(i in 1:n_col){
        means[i] <- mean(x[,i], na.rm = removeNA)
    }
    means
}
