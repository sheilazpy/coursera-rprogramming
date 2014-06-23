rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
	Data <- read.csv("outcome-of-care-measures.csv",
		colClasses = "character")

	## Outcome Validation
	out <- c("heart attack", "heart failure", "pneumonia")
	matchOut <- match(outcome, out, nomatch = 0)
	if (matchOut == 0) stop("invalid outcome")

	## Subset the data depending on outcome
	if (outcome == out[1]) sData <- subset(Data, select = c(2,7,11))
	if (outcome == out[2]) sData <- subset(Data, select = c(2,7,17))	
	if (outcome == out[3]) sData <- subset(Data, select = c(2,7,23))	

	## Store columns in easy to identify variables
	names <- sData[,1]
	states <- sData[,2]
	rates <- as.numeric(sData[,3])
	
	## Order sData by 1) State, 2) Rate, 3) Name Hosp & Remove NAs
	sDataOrd <- sData[order(states, rates, names, decreasing = F, na.last = TRUE),]
	sDataOrd <- subset(sDataOrd, sDataOrd[,3] != "Not Available")

	## Split sDataOrd in a list for every state & determine # of states
	sDataState <- split(sDataOrd, sDataOrd[,2])
	numberOfStates <- as.integer(length(sDataState))

	## Initializa a dataframe in which the results will be stored
	res <- data.frame(matrix(ncol = 2, nrow = numberOfStates))
	colnames(res) <- c("hospital","state")

	## Create an index vector for the loop (depending on the passed "num" arg)
	if (num == "best") num <- rep(as.integer(1), numberOfStates)
	if (num == "worst") num <- sapply(sDataState, nrow)
	num <- rep(as.integer(num), numberOfStates)

		for (i in 1:numberOfStates) {
			res[i,] <- cbind(sDataState[[i]][num[i],1], sDataState[[i]][1,2])
		}

	return(res)


}