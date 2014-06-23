rankhospital <- function(state, outcome, num = "best") {
	
	## Read outcome data
	Data <- read.csv("outcome-of-care-measures.csv",
		colClasses = "character")

	## State Validation
	matchState <- match(state, Data$State, nomatch = 0)
	if (matchState == 0) stop("invalid state")

	## Outcome Validation
	Outcomes <- c("heart attack", "heart failure", "pneumonia")
	matchOutcome <- match(outcome, Outcomes, nomatch = 0)
	if (matchOutcome == 0) stop("invalid outcome")

	## Return hospital name in that state with lowest 30-day death rate
	## 1. First, subset the data depending on state and outcome
	if (outcome == Outcomes[1]) subData <- subset(Data, State == state, select = c(2,11))
	if (outcome == Outcomes[2]) subData <- subset(Data, State == state, select = c(2,17))	
	if (outcome == Outcomes[3]) subData <- subset(Data, State == state, select = c(2,23))

  	##2. Second, order the subData table in descending order
	subDataOrdered <- subData[order(as.numeric(subData[,2]), subData[,1], decreasing = c(F, F), na.last = TRUE),]
	
	##3. Subset "subDataOrdered"-entries with !na results
	subDataOrdered <- subset(subDataOrdered, !is.na(as.numeric(subDataOrdered[,2])))

	##4. Result Conditioning
	if (num == "best") num = as.integer(1)
	if (num == "worst") num = as.integer(dim(subDataOrdered)[1])	

	##5. Return Result
	if (as.integer(num) > dim(subData)[1]) return(NA)

	return(subDataOrdered[as.integer(num),1])

}