best <- function(state, outcome) {
     
    ## Read outcome data
	hospitalData <- read.csv("outcome-of-care-measures.csv",
		colClasses = "character")

	## State Validation
	matchState <- match(state, hospitalData$State, nomatch = 0)
	if (matchState == 0) stop("invalid state")

	## Outcome Validation
	validOutcomes <- c("heart attack", "heart failure", "pneumonia")
	matchOutcome <- match(outcome, validOutcomes, nomatch = 0)
	if (matchOutcome == 0) stop("invalid outcome")

	## Return hospital name in that state with lowest 30-day death rate
	## 1. First, select appropiate column depending on outcome
	if (outcome == validOutcomes[1]) col <- as.integer(11) 	
	if (outcome == validOutcomes[2]) col <- as.integer(17)	
	if (outcome == validOutcomes[3]) col <- as.integer(23)

	## 2. Create numeric vector of hospital data and carry out tapply
	dataVector <- as.numeric(hospitalData[,col])
	minValues <- tapply(dataVector, hospitalData$State, min, na.rm = TRUE)

	## 3. Select entries hospitalData that represent minima for selected state
	entry <- which(dataVector == minValues[state] & hospitalData$State == state)

	## 4.
	result = hospitalData[entry,2]
	return(sort(result))

}