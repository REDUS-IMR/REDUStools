#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
getAssessmentData <- function(src, date, ageVec, yearVec, type, format, useHeader, target)
{
	
	if(target$mode == "local")
		out <- getAssessmentData.core(src, date, ageVec, yearVec, type, format, useHeader)
	else {
		url <- paste0(target$url, "/R/getAssessmentData.core/json")
		query <- list(src=src, date=date, ageVec=ageVec, yearVec=yearVec, type=type, format=format, useHeader=useHeader)
		out <- httr::POST(url, body = query, encode = "json")
		out <- jsonlite::fromJSON(httr::content(out, "text"))
	}

	return(out)
}

#' Update or add a survey index abundance in an ICES format file.
#'
#' \code{updateICESSurveyFile} returns TRUE if succeeded or FALSE if failed.
#' @param surveyFile The path to the survey file.
#' @param type Data type from source to put into the file. Either "data" or "data_var" (covariance).
#' @param text If not NULL then text data in this variable will be put into the survey file.
#' @param srcHead Survey title to be (partially) matched.
#' @param stsName Survey time series name that will served as the source.
#' @param stsDate The timestamp of the survey time series source.
#' @param useSourceAge The timestamp of the iter.
#' @param useSourceYear The timestamp of the iter.
#' @param target The timestamp of the iter.
#'
#' @export
updateICESSurveyFile <- function(surveyFile, type, text, srcHead, stsName, stsDate = NULL, useSourceAge = FALSE, useSourceYear = FALSE, target)
{

	# Assuming the file exists
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

		if(is.null(text))		
			newLines <- getAssessmentData(stsName, stsDate, ageVec, yearVec, type="STS", format="SAM", useHeader = FALSE, target)
		else
			newLines <- text

		# Combine data
		newSurveyLines <- c(surveyLines[1:start], newLines[[type]], surveyLines[(end + 1):length(surveyLines)])
	} else {
		# Update number of cruises
		surveyLines[2] <- as.numeric(trimws(surveyLines[2])) + 1
		
		# Get data
		if(is.null(text))
			newLines <- getAssessmentData(stsName, stsDate, ageVec, yearVec, type="STS", format="SAM", useHeader = FALSE, target)
		else
			newLines <- text

		# Combine data
		newSurveyLines <- c(surveyLines[1:length(surveyLines)], srcHead, newLines[[type]])
	}

	# Save new file
	if(file.exists(surveyFile) && !file.exists(paste0(surveyFile, ".original")))
		file.copy(surveyFile, paste0(surveyFile, ".original"))
	
	write(newSurveyLines, file=surveyFile)

	return(TRUE)
}

# Do pre-process on survey data
preprocess.survey <- function(query, target, file, config) {

	file_var <- paste0(tools::file_path_sans_ext(file), "_var.dat")

	mode <- config[[paste0(query, ".mode")]]
	header <- config[[paste0(query, ".header")]]
	sts <- config[[paste0(query, ".stssource")]]
	stsDate <- config[[paste0(query, ".stsdate")]]
	sourceAge <- config[[paste0(query, ".useSourceAge")]]
	sourceYear <- config[[paste0(query, ".useSourceYear")]]

	# Texts
	text <- NULL

	# Check if we want to rebuid time series using stox
	if(mode == "build") {
		processRstoxSTS(config[[paste0(query, ".surveyBuildConf")]])
		# Set mode to local
		mode <- "local"
	} else if (mode == "manual") {
		text <- list()
		text[["data"]] <- config[[paste0(query, ".data")]]
		text[["data_var"]] <- config[[paste0(query, ".data_var")]]
		mode <- "text"
	}

	# Set target (remote or local or text)
	target[["mode"]] <- mode

	# Do process
	ret <- updateICESSurveyFile(file, "data", text, header, sts, stsDate, useSourceAge = sourceAge, useSourceYear = sourceYear, target)
	ret <- updateICESSurveyFile(file_var, "data_var", text, header, sts, stsDate, useSourceAge = sourceAge, useSourceYear = sourceYear, target)

	return(ret)
}

#' Start the pre-processing based on the given configuration file.
#'
#' \code{preprocess} returns TRUE if succeeded or FALSE if failed.
#' @param configFile The path to the configuration file.
#'
#' @importFrom config get
#' @export
preprocess <- function(configFile) {

	# Get config
	if(!file.exists(configFile))
		return(FALSE)

        configs <- config::get(file = configFile, use_parent = FALSE)

	# Right now, only process survey update:
	types <- list("survey")

	# Composing target location
	target <- list(url=configs[["redustools.remote"]])

	for(i in 1:length(types)) {

		type <- types[[i]]

		print(paste("Processing", type, "updates..."))

		updateFileName <- configs[[types[[i]]]]

		# Set iterations for multiple update requests
		iter <- 1
		while(1) {
			query <- paste0(type, ".update.", iter)
			singleConf <- configs[grepl(query, names(configs))]

			# No more configs for this type, break
			if(length(singleConf) == 0) break

			do.fun <- eval(parse(text=paste0("preprocess.", type)))

			result <- do.fun(query, target, updateFileName, singleConf)

			if(result)
				print(paste("SUCCESS! #", query))
			else
				print(paste("ERROR! #", query))

			# Or just increment the iter
			iter <- iter + 1
		}
	}
	return(TRUE)
}

#redusConfigs <- config::get(file = "redus/redus.yaml", use_parent = FALSE)

#preprocess(redusConfigs)

