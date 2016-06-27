rankall <- function (outcome, num = "best") {
			
	# populate a vector with columns to check
	c2Check <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	reqCol <- c2Check[outcome]
	if (is.na (reqCol)) stop ("invalid outcome")

	# read file
	outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# list of states
	statesList <- sort (unique (outcomeData$State))

	# remove rows from consideration that do not have values available
	outcomeData <- outcomeData [outcomeData[reqCol] != "Not Available", ]
			
	# coerce the required column to numeric so order function will order it properly
	# this will introduce NAs which need to be removed
	outcomeData [, reqCol] <- as.numeric (outcomeData [, reqCol] )
	
	# eliminate NAs
	# outcomeData <- outcomeData [!is.na (outcomeData[reqCol]), ]
		
	# now sort the columns by the state & then by outcome & then by hospital 
	#(where outcome rate is the same for multiple hospitals
	outcomeData <- outcomeData [ order ( outcomeData$State, outcomeData [, reqCol], outcomeData$Hospital.Name), ]

	outputList <- data.frame (hospital = character(), state = character())
	for (curState in statesList) {
		outcomeDataForState <- outcomeData [outcomeData$State == curState, ]
		numRows <- nrow(outcomeDataForState) 
				
		# set num properly
		reqNum <- num
		if (num == "best") {
			reqNum <- 1
		}
		else if (num == "worst") {
			reqNum <- numRows
		}
				
		if (numRows == 0 || numRows < reqNum) {
			outputList <- rbind(outputList, data.frame (hospital = NA, state = curState))
		}
		else
		{
			outputList <- rbind(outputList, data.frame (hospital = outcomeDataForState$Hospital.Name[reqNum], state = curState))
		}
	}

	outputList
}