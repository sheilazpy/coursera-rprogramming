corr <- function(directory, threshold = 0) {
        
        ## initialize a vector in which the correlation data is stored
        data_vect = c()
        
        files <- list.files( path = directory )
        
        for(i in 1:length(files)){        
                
                
                cases <- complete(directory, i)[1,2]
                               
                if(cases > threshold){
                                
                        ## read a single data file into the data_monitor variable
                        data_mon <- read.csv(paste(directory, sprintf("%03d.csv", i), sep="/"), header = TRUE) 
                        
                        ## calulate the correlation value of the sulfate and nitrate levales
                        cor_value <- cor(data_mon[["nitrate"]], data_mon[["sulfate"]], use = "complete.obs")
                        
                        ## add result to data matrix
                        data_vect <- c(data_vect, cor_value)
                        
                }                    
        
        }
        
        data_vect <- as.numeric(data_vect)
        data_vect
       

}