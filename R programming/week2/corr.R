corr <- function (directory, threshold = 0)  {
    
    
    complete_cases <- complete(directory)
    corrlist <- complete_cases$nobs > threshold
    correlations <- numeric(sum(corrlist))
    
    corr_index <- 1
    
    for (i in seq_along(corrlist))
    {
        if (corrlist[i])
        {
            raw_data <- read.csv(paste(directory,"/",formatC(i, width=3, flag="0"), ".csv",sep=""))
            data <- raw_data[complete.cases(raw_data), ]
            correlations[corr_index] <- cor(data$sulfate,data$nitrate)
            corr_index <- corr_index + 1
        }
    }
    
    correlations
}
