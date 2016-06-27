rankhospital <- function (state, outcome, num = "best") {
		
	# populate a vector with columns to check
	c2Check <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	reqCol <- c2Check[outcome]
	if (is.na (reqCol)) stop ("invalid outcome")

	# read file
	outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# filter for required state
	outcomeData <- outcomeData [outcomeData$State == state, ]
	if (nrow(outcomeData) == 0) stop ("invalid state")
			
	outcomeData <- outcomeData [outcomeData[reqCol] != "Not Available", ]
			
	# coerce the required column to numeric so order function will order it properly
	# this will introduce NAs which need to be removed
	outcomeData [, reqCol] <- as.numeric (outcomeData [, reqCol] )
		
	# eliminate NAs
	outcomeData <- outcomeData [!is.na (outcomeData[reqCol]), ]
		
	# set num properly
	if (num == "best") {
		num <- 1
	}
	else if (num == "worst") {
		num <- nrow (outcomeData)
	}
	else if (num < 1 || num > nrow (outcomeData)) {
		return (NA)
	}

	# now sort the columns by the outcome & then by hospital 
	#(where outcome rate is the same for multiple hospitals
	outcomeData <- outcomeData [ order ( outcomeData [, reqCol], outcomeData$Hospital.Name), ]

	outcomeData$Hospital.Name [num]
}
