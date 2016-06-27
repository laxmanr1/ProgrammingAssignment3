best <- function (state, outcomeName) {
	# populate a vector with columns to check
	c2Check <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	if (is.na (c2Check [outcomeName])) stop ("invalid outcome")

	# read file
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	# split file by state
	stateHospitals <- split (outcome, outcome$State)

	# get data set for specific state
	if (is.null (reqStateInfo <- stateHospitals[[state]])) stop ("invalid state")
			
	# eliminate rows that have "Not Available" as value in the required column
	reqStateInfo <- reqStateInfo [ reqStateInfo [ c2Check [ outcomeName] ] != "Not Available" ,  ]
			
	# now get those rows that have the minimum value
	reqHospital <- reqStateInfo [   as.numeric(reqStateInfo [,c2Check [outcomeName]])
						==
						min (as.numeric (reqStateInfo [,c2Check [outcomeName] ] ) )
					        , 2 ]
	if (length (reqHospital) > 1) {
		reqHospital <- sort (reqHospital)
	}
	reqHospital[1]
}
