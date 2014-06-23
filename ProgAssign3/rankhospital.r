rankhospital <- function(state, outcome, num = "best") {
	
	## Read outcome data
	Data <- read.csv("outcome-of-care-measures.csv",
		colClasses = "character")

	## State Validation
	matchState <- match(state, Data$State, nomatch = 0)
	if (matchState == 0) stop("invalid state")

	## Outcome Validation
	out <- c("heart attack", "heart failure", "pneumonia")
	matchOutcome <- match(outcome, out, nomatch = 0)
	if (matchOutcome == 0) stop("invalid outcome")

	## Return hospital name in that state with lowest 30-day death rate
	## 1. First, subset the data depending on state and outcome
	if (outcome == out[1]) sData <- subset(Data, State == state, select = c(2,11))
	if (outcome == out[2]) sData <- subset(Data, State == state, select = c(2,17))	
	if (outcome == out[3]) sData <- subset(Data, State == state, select = c(2,23))

  	##2. Second, order the sData table in descending order
	Names <- sData[,1]
	Rates <- as.numeric(sData[,2])
	sDataOrd <- sData[order(Rates, Names, decreasing = c(F, F), na.last = TRUE),]
	
	##3. Subset "sDataOrd"-entries with !na results
	sDataOrd <- subset(sDataOrd, !is.na(as.numeric(sDataOrd[,2])))

	##4. Result Conditioning
	if (num == "best") num = as.integer(1)
	if (num == "worst") num = as.integer(dim(sDataOrd)[1])	

	##5. Return Result
	if (as.integer(num) > dim(sData)[1]) return(NA)

	return(sDataOrd[as.integer(num),1])

}