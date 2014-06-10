pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        ## initialize a vector where data is stored
        data_vect = c()             
        
        ## loop over all XXX.csv files
        for (i in id) {
                
                ## create filepath that points to data files
                path = paste(directory, sprintf("%03d.csv", i), sep="/")
                
                ## read a single data file into the data_monitor variable
                data_monitor <- read.csv(path, header = TRUE)
                
                ## append the data to our initialized vector
                data_vect <- c(data_vect,data_monitor[[pollutant]])                                                  
        }
        
        ## calculate the mean value of the data discarding the NA??s
        mean_value <- mean(data_vect, na.rm = TRUE) 
        ## show the result with a 3-digit precision
        round(mean_value,digits=3)
}
