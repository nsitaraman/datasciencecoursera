complete <-  function (directory, id = 1:332)  {
    
    df <- data.frame()
    
    for(i in id)
    {
        data <- read.csv(paste(directory,"/",formatC(i, width=3, flag="0"), ".csv",sep=""))
        num_cases <- sum(as.integer(complete.cases(data)))
        df <- rbind(df,c(i,num_cases))
    }
    names(df) <- c("id","nobs")
    df
}
