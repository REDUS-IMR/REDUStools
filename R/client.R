#' @importFrom httr POST content
getAssessmentData <- function(src, date, ageVec, yearVec, type, format, useHeader, target)
{
	
	if(target$mode == "local")
		out <- getAssessmentData.core(src, date, ageVec, yearVec, type, format, useHeader)
	else {
		url <- paste0(target$url, "/R/getAssessmentData.core/json")
		query <- list(src=src, date=date, ageVec=ageVec, yearVec=yearVec, type=type, format=format, useHeader=useHeader)
		out <- httr::POST(url, body = query, encode = "json")
		out <- unlist(httr::content(out, "parsed"))
	}

	return(out)
}

#' Update or add a survey index abundance in an ICES format file.
#'
#' \code{updateICESSurveyFile} returns TRUE if succeeded or FALSE if failed.
#' @param surveyFile The path to the survey file.
#' @param srcHead Survey title to be (partially) matched.
#' @param stsName Survey time series name that will served as the source.
#' @param stsDate The timestamp of the survey time series source.
#' @param useSourceAge The timestamp of the iter.
#' @param useSourceYear The timestamp of the iter.
#' @param target The timestamp of the iter.
#'
#' @export
updateICESSurveyFile <- function(surveyFile, srcHead, stsName, stsDate = NULL, useSourceAge = FALSE, useSourceYear = FALSE, target)
{
	if(!file.exists(surveyFile))
		return(FALSE)

	surveyLines <- readLines(surveyFile)

	# Get starting point
	start <- grep(srcHead, surveyLines)[1]

	# Placeholder for overrides
	ageVec <- NA
	yearVec <- NA

	# If exists, update, otherwise add
	if(!is.na(start)) {
		# Get number of lines
		str <- surveyLines[start + 1]
		tmp <- trimws(unlist(strsplit(str, "\\s+")))
		startYear <- as.numeric(tmp[1]) 
		endYear <- as.numeric(tmp[2])
		len <- endYear - startYear
		end <- start + len + 4

		# Get start and end age
		str <- surveyLines[start + 3]
		tmp <- trimws(unlist(strsplit(str, "\\s+")))
		startAge <- as.numeric(tmp[1]) 
		endAge <- as.numeric(tmp[2])

		#print(surveyLines[start:end])
		#print(startAge)
		#print(endAge)

		# Get data
		if(useSourceAge)
			ageVec <- c(startAge, endAge)

		if(useSourceYear)
			yearVec <- c(startYear, endYear)
		
		newLines <- getAssessmentData(stsName, stsDate, ageVec, yearVec, type="STS", format="SAM", useHeader = FALSE, target)

		# Combine data
		newSurveyLines <- c(surveyLines[1:start], newLines, surveyLines[(end + 1):length(surveyLines)])
	} else {
		# Update number of cruises
		surveyLines[2] <- as.numeric(trimws(surveyLines[2])) + 1
		
		# Get data
		newLines <- getAssessmentData(stsName, stsDate, ageVec, yearVec, type="STS", format="SAM", useHeader = FALSE, target)

		# Combine data
		newSurveyLines <- c(surveyLines[1:length(surveyLines)], srcHead, newLines)	
	}

	#print(newSurveyLines)
	# Save new file
	file.copy(surveyFile, paste0(surveyFile, ".original"))
	write(newSurveyLines, file=surveyFile)

	return(TRUE)
}



