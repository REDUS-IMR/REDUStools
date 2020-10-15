getAssessmentData.core <- function(src, date, ageVec, yearVec, type, format, useHeader)
{

	print("Generating assessment data")

	if(type == "STS") {
		srcData <- loadSTS(src, date)
	} else {
		stop("Not implemented!")
	}

	if(format == "SAM") {
		result <- formatSAM(srcData,  ageVec, yearVec, useHeader)
	} else {
		stop("Not implemented!")
	}

	return(list(data=result[[1]], data_var=result[[2]]))
}

#' @importFrom data.table copy
formatSAM <- function(data, ageVec, yearVec, useHeader = TRUE){

        mySTS <- data$meta$stsName

	if(!is.null(ageVec) && length(ageVec) == 2 &&
			ageVec[2] <= max(data$data$age, na.rm=T) && 
			ageVec[1] >= min(data$data$age, na.rm=T)) {
		plusAge <- ageVec[2]
		minAge <- ageVec[1]
	} else {
		plusAge <- as.numeric(data$meta$plusAge)
		minAge <- as.numeric(data$meta$minAge)
	}

	maxAge <- max(data$data$age, na.rm=T)

	if(!is.null(yearVec) && length(yearVec) == 2 &&
		yearVec[2] <= max(data$data$year)&&
		yearVec[1] >= min(data$data$year)) {
                endYear <- yearVec[2]
                startYear <- yearVec[1]
	} else {
		startYear <- min(data$data$year)
		endYear <- max(data$data$year)
	}

	type <- data$meta$dataType

	# Get numbers
	tempfTBL <- stsTable(data, "Abundance", raw=TRUE)
	tempTBL <- data.table(as.matrix(tempfTBL))
	colnames(tempTBL) <- attr(tempfTBL, "col.vars")[[1]]
	tempTBL[, year:=(attr(tempfTBL, "row.vars")[[1]])]

	# and variance
	tempTBLVAR <- lapply(attr(tempfTBL, "variance"), data.table)
	## Filter out last column and row if they represent NAs
	tempTBLVAR <- lapply(tempTBLVAR, function(xy) { if(substr(tail(colnames(xy), 1), 1, 1) == "V") {
								xy <- xy[1:(nrow(xy) - 1), 1:(ncol(xy) - 1)];
								xy
								}
							else {xy}
							})
	## Copy age to row
	tempTBLVAR <- lapply(tempTBLVAR, function(xy) {nn <- copy(colnames(xy)); xy[, age:= nn]})
	## Filter age
	tempTBLVAR <- lapply(tempTBLVAR, function(xy) {
					xy <- xy[age %in% intersect(age, as.character(c(minAge:plusAge))),];
					xy <- xy[, setdiff(colnames(xy), as.character(c(minAge:plusAge))) := NULL];
					xy
			})
	## Filter year
	tempTBLVAR <- tempTBLVAR[intersect(names(tempTBLVAR), as.character(c(startYear:endYear)))]

	# Parse config
	#FileOutputName <- paste(mySTS, type,"SAM.dat", sep=".")

	# Filter out table
	rowUnset <- setdiff(tempTBL$year, as.character(c(startYear:endYear)))
	if(length(rowUnset) > 0 ) {
		tempTBL <- tempTBL[!(year %in% rowUnset),]
	}
	tempTBL[, setdiff(colnames(tempTBL), as.character(c(minAge:maxAge))) := NULL]

	# Getting the plus age columns (7+ or else)
	plusAges <- setdiff(colnames(tempTBL), as.character(c(1:(plusAge-1))))
	tempTBL[, as.character(plusAge) := rowSums(.SD, na.rm = TRUE), .SDcols = plusAges]

	# We only interested in getting year 1 to plusAge ...
	ageStart <- minAge
	ageEnd <- plusAge

	keep <- as.character(ageStart:ageEnd)

	# ..and filter out unwanted age columns using keep
	tempTBL <- tempTBL[, ..keep]

	# Round the abundance-per-year values to 3 decimal places
	tempTBL <- tempTBL[, round(.SD, digits = 3)]
	tempTBLVAR <- lapply(tempTBLVAR, function(xy) xy[, round(.SD, digits = 3)])

	# Add effort value in the first column
	effortNumber <- 1
	tempTBL <- cbind(a = effortNumber, tempTBL)
	tempTBLVAR <- lapply(tempTBLVAR, function(xy) cbind(a = effortNumber, xy))
	
	### Prepare header

	# Age-span
	FirstLastAge <- paste(ageStart, ageEnd, sep = '\t')

	# Year-span
	FirstLastYear <- paste(startYear, endYear, sep = '\t')

	# Effort code would be problematic, here we use a simple text matching (e.g., Winter = '0.75 1')
	# Also, name of the fleet will follow it
	if(grep("winter", mySTS, ignore.case = TRUE)){
		EffortLine <- paste(1, 1, 0.75, 1, sep = "\t")
		FleetName <- 'Q4'
	}else{
		EffortLine <- paste(1, 1, 1, 1, sep = "\t")
		FleetName <- 'All-year'
	}

	# Combine lines for header
	textHead <- paste(FirstLastYear, EffortLine, FirstLastAge, sep = "\n")

	### Write everything

	# Placeholder
	FileOutput <- tempfile()
	FileOutputVar <- tempfile()

	if(useHeader) {
		# Add title
		textHead <- paste(FleetName, textHead, sep = "\n")

		# Write title
		suppressWarnings(write(strtrim(mySTS, 80), file= FileOutput, append=FALSE))

		# Write number of fleet (Now we only have one fleet)
		NoFleet <- 100
		suppressWarnings(write(NoFleet + 1, file= FileOutput, append=TRUE))
	}

	# Write header per fleet
	suppressWarnings(write(textHead, file= FileOutput, append=TRUE))

	# Table per-fleet
	suppressWarnings(write.table(tempTBL, file=FileOutput, append=TRUE, sep="\t", dec=".", col.names = FALSE, row.names=FALSE))

	# Variance always have the header text
	suppressWarnings(write(strtrim(mySTS, 80), file= FileOutputVar, append=FALSE))
	suppressWarnings(write(100 + length(tempTBLVAR), file= FileOutputVar, append=TRUE))
	suppressWarnings(
		lapply(names(tempTBLVAR),
			function (yr) {
				write(paste(paste0("\nVAR", yr), FirstLastAge, EffortLine, FirstLastAge, sep = "\n"), file= FileOutputVar, append=TRUE);
				write.table(tempTBLVAR[[yr]], file=FileOutputVar, append=TRUE, sep="\t", dec=".", col.names = FALSE, row.names=FALSE)
			}
		)
	)

	ret <- list()
	ret[["data"]] <- readLines(FileOutput)
	ret[["data_var"]] <- readLines(FileOutputVar)

	return(ret)
}

#' @importFrom data.table setcolorder set
#' @importFrom stats na.omit
#' @importFrom utils head tail
formatGadget <- function(data){

	# Remove NAs in age or length
	data <- stats::na.omit(data, cols=c("age","LenGrp"))

	# Create mean values because we still have .ids
	stage1 <- data[, lapply(.SD, mean, na.rm=TRUE), by = c("year","LenGrp","age"), .SDcols =! ".id"]

	# Convert NAs to zero
	for (i in seq_along(stage1)) data.table::set(stage1, i=which(is.na(stage1[[i]])), j=i, value=0)

	# Summing
	stage2 <- stage1[, lapply(.SD, sum, na.rm=TRUE), by = c("year","LenGrp"), .SDcols =! c("age")]
	stage3 <- stage1[, lapply(.SD, sum, na.rm=TRUE), by = c("year"), .SDcols =! c("age", "LenGrp")]

	# Get MaxMin length
	maxMinLen <- sort(unique(stage1[["LenGrp"]]))
	maxMinLen <- c(utils::head(maxMinLen,1), utils::tail(maxMinLen,1))

	# Creating additional columns
	stage1 <- stage1[, c("step", "area"):= list(1,"allareas")]
	stage2 <- stage2[, c("step", "area", "age") := list(1,"allareas","allages")]
	stage3 <- stage3[, c("step", "area", "age", "LenGrp") := list(1,"allareas","allages", paste("len", maxMinLen[1], as.numeric(maxMinLen[2])+4, sep="-"))]

	# Length group label
	tmpLabel <- processLenGrpHeader(stage1, prefix="len", sep="")

	# Process labels
	stage1 <- stage1[, LenGrp:=as.character(LenGrp)]
	set(stage1, j="age", value=paste0("age",stage1[["age"]]))
	set(stage1, j="LenGrp", value=tmpLabel[stage1[["LenGrp"]]])

	stage2 <- stage2[,LenGrp:=as.character(LenGrp)]
	set(stage2, j="LenGrp", value=tmpLabel[stage2[["LenGrp"]]])
	
	ret <- list(stage1,stage2,stage3)	

	# Set column order
	ret <- lapply(ret, function(x){data.table::setcolorder(x, c("year", "step", "area","age", "LenGrp", "Ab.Sum", "Weight.Sum"))})

	return(ret)

}

#createAssessmentFiles <- function (processResults, stsDir){

	# Create output directories
    # lapply(resultNames, function(x) dir.create(paste0(stsDir, "/", x, "/assessment"), recursive = TRUE, showWarnings = FALSE))

	# Make the SAM formatted output file
	#formatSAM(tempTBLAbundance, configSTS, type="abundance")
	#formatSAM(tempTBLWeight, configSTS, type="weight")

	# Make Gadget output file
#}

