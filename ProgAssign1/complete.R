complete <- function(directory, id = 1:332) {
        
        df <- data.frame(matrix(ncol = 2, nrow = length(id)))
        names(df)[1]<-paste("id")
        names(df)[2]<-paste("nobs")
        
        
        for (i in 1:length(id)) {
        
                ## create filepath that points to data files
                path = paste(directory, sprintf("%03d.csv", id[i]), sep="/")
                
                ## read a single data file into the data_monitor variable
                data_mon <- read.csv(path, header = TRUE)
        
                cases <- sum(complete.cases(data_mon[,2],data_mon[,3]))
                df[i,1] <- id[i]
                df[i,2] <- cases
                
        
        }
        
        df
        
}