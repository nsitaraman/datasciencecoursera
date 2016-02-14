pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    accum_sum <- 0  
    accum_len <- 0
    
    for(i in id)  
    {
        data <- read.csv(paste(directory,"/",formatC(i, width=3, flag="0"), ".csv",sep=""))
        data_clean <- data[[pollutant]][!is.na(data[[pollutant]])]
        accum_sum <- accum_sum + sum(data_clean)
        accum_len <- accum_len + length(data_clean)
    }
    accum_sum/accum_len
}
