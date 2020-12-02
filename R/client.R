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
#' @param text If not NULL then text data in this variable will be put into the survey file.
#' @param srcHead Survey title to be (partially) matched.
#' @param stsName Survey time series name that will served as the source.
#' @param stsDate The timestamp of the survey time series source.
#' @param useSourceAge Use similar age structure to the existing/source survey file.
#' @param useSourceYear Use similar year structure to the existing/source survey file.
#' @param target Target mode (local or remote).
#'
#' @export
updateICESSurveyFile <- function(surveyFile, text, srcHead, stsName, stsDate = NULL, useSourceAge = FALSE, useSourceYear = FALSE, target)
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
		newSurveyLines <- c(surveyLines[1:start], newLines[["data"]], surveyLines[(end + 1):length(surveyLines)])
	} else {
		# Update number of cruises
		surveyLines[2] <- as.numeric(trimws(surveyLines[2])) + 1

		# Get data
		if(is.null(text))
			newLines <- getAssessmentData(stsName, stsDate, ageVec, yearVec, type="STS", format="SAM", useHeader = FALSE, target)
		else
			newLines <- text

		# Combine data
		newSurveyLines <- c(surveyLines[1:length(surveyLines)], srcHead, newLines[["data"]])
	}

	# Save new file
	if(file.exists(surveyFile) && !file.exists(paste0(surveyFile, ".original")))
		file.copy(surveyFile, paste0(surveyFile, ".original"))
	write(newSurveyLines, file=surveyFile)

	# Now process variance for this survey
	surveyID <- head(unlist(strsplit(srcHead, "\\W+", perl = T)), 1)
	surveyFileVar <- paste0(tools::file_path_sans_ext(surveyFile), "_var_", surveyID, ".dat")

	if(file.exists(surveyFileVar) && !file.exists(paste0(surveyFileVar, ".original")))
		file.copy(surveyFileVar, paste0(surveyFileVar, ".original"))

	write(newLines[["data_var"]], file=surveyFileVar)

	return(TRUE)
}

# Do pre-process on survey data
preprocess.survey <- function(query, target, file, config) {

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
	ret <- updateICESSurveyFile(file, text, header, sts, stsDate, useSourceAge = sourceAge, useSourceYear = sourceYear, target)

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

##' Function to read ices survey format. Copied from:
##' https://github.com/fishfollower/SAM/blob/master/stockassessment/R/reading.R
##' The only difference is that this function doesn't remove negative numbers
##' @param filen the file
read.surveys.with.negative<-function(filen){

	is.whole.positive.number <- function(x, tol = .Machine$double.eps^0.5){
		(abs(x - round(x)) < tol)&(x>=0)
	}

	# Function to read ices survey file 
	lin<-readLines(filen,warn=FALSE)[-c(1:2)]
	empty<-which(lapply(lapply(strsplit(lin, split='[[:space:]]+'), 
				paste, collapse=''), nchar)==0)
	if(length(empty)>0){
		lin<-lin[-empty]
	}
	lin<-sub("^\\s+", "",lin)
	idx1<-grep('^[A-Z#]', lin, ignore.case=TRUE)
	idx2<-c(idx1[-1]-1,length(lin))
	names<-lin[idx1]
	years<-matrix(as.numeric(unlist(strsplit(lin[idx1+1], '[[:space:]]+'))), ncol=2, byrow=TRUE)
	times<-matrix(as.numeric(unlist(strsplit(lin[idx1+2], '[[:space:]]+'))), ncol=4, byrow=TRUE)[,3:4,drop=FALSE]
	ages<-matrix(as.numeric(unlist(lapply(strsplit(lin[idx1+3], '[[:space:]]+'), function(x)x[1:2]))), ncol=2, byrow=TRUE)
	for(i in 1:length(names)){
		# Check years 
		if(!is.whole.positive.number(years[i,1])){
			stop(paste("In file",filen, ": Minimum year is expected to be a positive integer number for fleet number",i))
		}
		if(!is.whole.positive.number(years[i,2])){
			stop(paste("In file",filen, ": Maximum year is expected to be a positive integer number for fleet number",i))
		}
		if(years[i,1]>years[i,2]){
			stop(paste("In file",filen, ": Maximum year is expected to be greater than minimum year for fleet number",i))
		}
		# Check ages 
		##if(!is.whole.positive.number(ages[i,1])){
		##  stop(paste("In file",filen, ": Minimum age is expected to be a positive integer number for fleet number",i))
		##}
		##if(!is.whole.positive.number(ages[i,2])){
		##  stop(paste("In file",filen, ": Maximum age is expected to be a positive integer number for fleet number",i))
		##}    
		if(ages[i,1]>ages[i,2]){
			stop(paste("In file",filen, ": Maximum age is expected to be greater than minimum age for fleet number",i))
		}
		# Check times
		if((times[i,1]<0)|(times[i,1]>1)){
			stop(paste("In file",filen, ": Minimum survey time is expected to be within [0,1] for fleet number",i))
		} 
		if((times[i,2]<0)|(times[i,2]>1)){
			stop(paste("In file",filen, ": Maximum survey time is expected to be within [0,1] for fleet number",i))
		} 
		if(times[i,2]<times[i,1]){
			stop(paste("In file",filen, ": Maximum survey time is expected greater than minimum survey time for fleet number",i))
		}
	}

	as.num <- function(x, na.strings = "NA") {
		stopifnot(is.character(x))
		na = x %in% na.strings
		x[na] = 0
		x = as.numeric(x)
		x[na] = NA_real_
		x
	}

	onemat<-function(i){
		lin.local<-gsub('^[[:blank:]]*','',lin[(idx1[i]+4):idx2[i]])
		nr<-idx2[i]-idx1[i]-3
		ret<-matrix(as.num(unlist((strsplit(lin.local,'[[:space:]]+')))),nrow=nr, byrow=TRUE)[,,drop=FALSE]   #[,1:(2+ages[i,2]-ages[i,1]),drop=FALSE]
		if(nrow(ret)!=(years[i,2]-years[i,1]+1)){
			stop(paste("In file",filen, ": Year range specified does not match number of rows for survey fleet number",i))
		} 
		if((ncol(ret)-1)<(ages[i,2]-ages[i,1]+1)){
			stop(paste("In file",filen, ": Fewer columns than indicated by age range for survey fleet number",i))
		} 
		if(!is.numeric(ret)){
			stop(paste("In file",filen, ": Non numeric data values detected for survey fleet number",i))
		}
		ret<-as.matrix(ret[,-1]/ret[,1])
		rownames(ret)<-years[i,1]:years[i,2]
		ret<-ret[,1:length(ages[i,1]:ages[i,2]),drop=FALSE]
		colnames(ret)<-ages[i,1]:ages[i,2]
		attr(ret,'time')<-times[i,]
		ret
	}
	obs<-lapply(1:length(names),onemat)  
	names(obs)<-names
	obs
}


#redusConfigs <- config::get(file = "redus/redus.yaml", use_parent = FALSE)

#preprocess(redusConfigs)

