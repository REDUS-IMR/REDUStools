#' Load the available STS names from IMR NMD.
#'
#' \code{getSTSList} returns an list of names of the available survey time series.
#'
#' @return A list contains all the names of the available STS will be returned.
#'
#' @importFrom Rstox getNMDinfo
#' @export
getSTSList <- function(){
  STS <- names(getNMDinfo("sts"))
  return(STS)
}

#' Load the processed REDUS time series database.
#'
#' \code{loadSTS} returns an Rstox processed REDUS time series database.
#'
#' @param mySTS STS name.
#' @param ts timestamp. Must be in "YYMMDD.HHMMSS" or "YYMMDD" character format. Default is NULL.
#' @return On a valid input a REDUS database object will be returned or NULL otherwise.
#' @examples
#' \dontrun{
#' loadSTS("Barents Sea Northeast Arctic cod bottom trawl index in winter", "20181215")
#' }
#'
#' @export
loadSTS <-function(mySTS, ts = NULL){

	stsDir <- paste0(getTimeSeriesDir("survey"), mySTS)

	if(!is.null(ts)) {
		stsFile <- list.files(stsDir, pattern = paste0("output.rds.", ts, "+"))[1]
		if(is.na(stsFile)) {
			stsFile <- "output.rds"
		}
	} else {
		stsFile <- "output.rds"
	}

	dataFile <- paste0(stsDir, "/", stsFile)
	if(file.exists(dataFile)){
		stsData <- readRDS(dataFile)
		return(stsData)
	}else{
		return(NULL)
	}
}

#' Process a length grup header.
#'
#' \code{processLenGrpHeader} returns a length group header translation.
#'
#' For example an original length group header of \code{c('5', '10', '15')} is transformed
#' into \code{c('5'='5-9', '10'='10-15', '15'='15+')}.
#'
#' @param tblResult REDUS STS data table.
#' @param target Column name of the length groups category.
#' @param prefix Prefix for the headers.
#' @param rangeSeparator Range separator character.
#' @param plusAgeChar Plus age character.
#' @param sep Separator character.
#' @return On a valid input a character object (list) will  be returned.
#' @examples
#' \dontrun{
#' stsData <- loadSTS("Barents Sea Northeast Arctic cod bottom trawl index in winter")
#' headerTranslation <- processLenGrpHeader(stsData$data2)
#' }
#'
#' @export
processLenGrpHeader <- function(tblResult, target="LenGrp", prefix="", rangeSeparator="-", plusAgeChar="+", sep=" "){

	# Sort the unique label values
	lenGrpLabel <- sort(unique(tblResult[[target]]))
	lenGrpInt <- c("")

	# Handle decimal lengths (e.g., on shrimp)
	interval <- 1
	if(lenGrpLabel[2]%%1>0)
	   interval <- 0.01

	# For each entry, generate the interval by considering on the next entry
	for(i in  1:length(lenGrpLabel)-1){
	    lenGrpInt[i] <- paste0(prefix, lenGrpLabel[i], rangeSeparator, lenGrpLabel[i+1]-interval, separator=sep)
	}
	# Plus age
	lenGrpInt[i+1] <- paste0(prefix, lenGrpLabel[i], plusAgeChar)

	return(structure(lenGrpInt, names=lenGrpLabel))
}

#' Generate an STS plot using ggplot2.
#'
#' \code{stsPlot} returns an STS plot based on its parameters.
#'
#' @param stsData REDUS STS data from \code{loadSTS}.
#' @param type text, valid values are \code{"Abundance"} and \code{"Weight"}.
#' @return On a valid input a ggplot2 object will be returned.
#' @examples
#' \dontrun{
#' stsPlot(loadSTS("Barents Sea Northeast Arctic cod bottom trawl index in winter"), "Abundance")
#' }
#'
#' @importFrom ggplot2 ggplot stat_summary_bin scale_color_hue mean_cl_boot ggtitle aes_string ylab xlab guide_legend guides scale_y_continuous
#' @importFrom scales log2_trans trans_breaks trans_format math_format
#' @export
stsPlot <- function(stsData, type="Abundance"){
	# If NULL, data is yet generated
	if(is.null(stsData))
		return("Unfortunately, the data have not been generated!")
	
	# Extract config and result
	config <- stsData$meta
	result <- stsData$data

	# Get numberscale
	numberscale <- as.numeric(config$numberScale)

	# Set the correct legend label, maxunit and for Length Group extract the Length Group interval label
	if(config$groupType == "age"){
	  legendTitle <- "Age group"
	  maxUnit <- as.numeric(config$maxAge)
	  }
	else if(config$groupType == "LenGrp"){
	  legendTitle <- "Length Group"
	  tmpLabel <- processLenGrpHeader(result)
	  maxUnit <- 10000
	}

	# Whether it is abundance or weight
	if(type == "Abundance"){
		varY <- "Ab.Sum"
		labelY <- "Abundance"
	}else if(type == "Weight"){
		varY <- "Weight.Sum/Ab.Sum"
		labelY <- "Weight"
	}else{
		print("Invalid type")
		return(NULL)
	}

	# Make charts
	outChart <- ggplot2::ggplot(result[result[[config$groupType]] < maxUnit + 1], ggplot2::aes_string(x="year", y=varY, group=config$groupType, colour=sprintf("factor(%s)",config$groupType))) +
		    ggplot2::stat_summary_bin(geom="ribbon", fun.data=ggplot2::mean_cl_boot, fun.args=(conf.int=0.95), alpha=0.3, colour=NA) +
		    ggplot2::stat_summary_bin(geom="line", fun.y=mean) +
		    ggplot2::stat_summary_bin(geom="point", fun.y=mean) +
		    ggplot2::ggtitle(config$STS) +
		    ggplot2::ylab(labelY) +
		    ggplot2::xlab("Year") +
		    ggplot2::guides(colour = ggplot2::guide_legend(legendTitle)) +
		    ggplot2::scale_y_continuous(trans = scales::log2_trans(), breaks = scales::trans_breaks("log2", function(x) 2^x), labels = scales::trans_format("log2", scales::math_format(2^.x)))

	#In case of Length Group, override the labels to show interval
	if(config$groupType == "LenGrp"){
		    outChart <- outChart + ggplot2::scale_color_hue(breaks=names(tmpLabel), labels = tmpLabel)
	}

	return(outChart)

}

#' Generate an STS data table.
#'
#' \code{stsTable} prints out a HTML table based on its parameters.
#'
#' @param stsData REDUS STS data from \code{loadSTS}.
#' @param type text, valid values are \code{"Abundance"} and \code{"Weight"}.
#' @param raw boolean
#' @examples
#' \dontrun{
#' stsTable(loadSTS("Barents Sea Northeast Arctic cod bottom trawl index in winter"), "Abundance")
#' }
#'
#' @importFrom stats ftable xtabs
#' @importFrom memisc show_html
#' @importFrom data.table setkeyv key
#' @export
stsTable <- function(stsData, type="Abundance", raw=FALSE){
        # If NULL, data is yet generated
        if(is.null(stsData))
                return("Unfortunately, the data have not been generated!")

        # Extract config and result
        config <- stsData$meta
	result <- stsData$data

	# Get numberscale
	numberscale <- as.numeric(config$numberScale)

	# Set the correct legend label, numberscale and for Length Group extract the Length Group interval label
	if(config$groupType == "age"){
	  legendTitle <- "Age group"
	  maxUnit <- config$maxAge
	  }
	else if(config$groupType == "LenGrp"){
	  legendTitle <- "Length Group"
	  tmpLabel <- processLenGrpHeader(result)
	  maxUnit <- 10000
	}

	# Set keys as year and age
	keycols<-c("year",config$groupType)
	setkeyv(result, keycols)
	
	# Create mean values for every year age combination
	result <- result[, lapply(.SD, mean, na.rm=TRUE), by = key(result), .SDcols = !".id"]
	
	# Whether it is abundance or weight
        if(type == "Abundance"){
		# Divide mean by numberscale and categorize the results based on age/length group
                xtabsEq <- sprintf("Ab.Sum/%d ~ year + %s", numberscale, config$groupType)
		multLabel <-  paste("( x", format(numberscale, big.mark=" "), ")")
        }else if(type == "Weight"){
		# Divide weight by abundance and categorize the results based on age/length group
		xtabsEq <- sprintf("Weight.Sum/Ab.Sum ~ year + %s", config$groupType)
		multLabel <- "(in grams)"
        }else{
                print("Invalid type")
                return(NULL)
        }

	# Produce the table
	outTable <- stats::ftable(stats::xtabs(xtabsEq, result))

	# Re-define column and row name so we can get a pretty output
	legendTitle <- paste(legendTitle, multLabel)
	names(attr(outTable,"col.vars"))<- legendTitle
	names(attr(outTable,"row.vars"))<-"Year"

	# If Length Group, rename the length column names to include interval
	source <- attr(outTable,"col.vars")[[legendTitle]]

	# Special case for Length group, override the col.vars
	if(config$groupType == "LenGrp"){
	   attr(outTable,"col.vars")[[legendTitle]] <- as.vector(tmpLabel[source])
	}
	if(raw)
		return(outTable)
	else
		memisc::show_html(outTable, digits=2)
}

#' Generate an html output consists of plots and table.
#'
#' \code{doGenerate} generates a html page (using knitr), which consists of plots 
#' and tables and also returns the metadata an survey time series specified in its argument.
#'
#' @param mySTS text.
#' @param ts timestamp. Must be in "YYMMDD.HHMMSS" or "YYMMDD" character format. Default is NULL.
#' @return If the input valid and the time series contains a build fingerprint, a 
#'   list will be returned. Otherwise it's NULL.
#' @examples
#' \dontrun{
#' doGenerate("Barents Sea Northeast Arctic cod bottom trawl index in winter")
#' }
#'
#' @importFrom utils download.file
#' @importFrom knitr knit2html
#' @export
doGenerate <- function(mySTS, ts = NULL){
  # Fetch STS data
  stsData <- loadSTS(mySTS, ts)

  # Template text for knitr	
  rawText<-c(
   paste("##", mySTS),
   "### Abundance",
   "```{r plot-abundance, fig.width=10, fig.height=8, warning=FALSE, echo=FALSE}",    
   "stsPlot(stsData)",
   "```",
   "```{r table-abundance, results='asis', warning=FALSE, echo=FALSE}",
   "stsTable(stsData)",
   "```",
   "### Weight",
   "```{r plot-weight, fig.width=10, fig.height=8, warning=FALSE, echo=FALSE}",
   "stsPlot(stsData, \"Weight\")",
   "```",
   "```{r table-weight, results='asis', warning=FALSE, echo=FALSE}",
   "stsTable(stsData, \"Weight\")",
   "```"
  )
  writeLines(rawText, "output.Rmd")
  # Downlaod CSS, if not exists
  if(!file.exists("output.css"))
  	utils::download.file(url="https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/paper/bootstrap.min.css", destfile="output.css", method="curl")
  # Generate html
  knitr::knit2html("output.Rmd", output="output.html",  stylesheet = "output.css");
  # If there is a fingerprint is attached, return it
  if(!is.null(stsData) && !is.null(stsData$fp))
  	return(stsData$fp)
  else
	return(NULL)
}

