#' Generate status matrix for the survey timeseries processing.
#'
#' \code{genStatusHTML} generates and returns HTML status matrix.
#'
#' @param timeSeriesStatus return from loadTimeSeriesStatus().
#' @param series specific series. Default "".
#' @param year specific year. Default ""
#' @param item specific item. Default ""
#' 
#' @importFrom knitr kable
#' @importFrom utils tail
#' @export
#' @examples
#' genStatusHTML(loadTimeSeriesStatus())
genStatusHTML <- function(timeSeriesStatus, series="", year="", item=""){

	# From: https://stackoverflow.com/questions/6029743/merge-or-combine-by-rownames
	# Adapted to max two way comparison
	mbind<-function(...){
		Reduce( function(x,y){
			if(length(row.names(x)) < length(row.names(y))){
				add <- setdiff(row.names(y), row.names(x))
				nrownames <- c(row.names(x), add)
				#x <- rbind(x, rep(c(NA), length(add)))
				for( i in 1:length(add) )
					x <- rbind(x, rep(c(NA), ncol(x)))
				row.names(x) <- nrownames
			}		
				cbind(x,y[match(row.names(x),row.names(y)),])
			}, list(...) )
	}

	# Aggregate status matrices
	aggregateStatus <- function(statusLogical){
		as.matrix(lapply(statusLogical,function(x){all(unlist(x))}))
	}

	# Get the last 7 days data 
	lastStatus <- utils::tail(names(timeSeriesStatus), 7)

	if(series==""){
		first <- lapply(timeSeriesStatus[lastStatus], function(x){aggregateStatus(x[["rawStatusLogical"]])})
		id <- "statusTop"
	}else{
		if(year==""){
			first <- lapply(timeSeriesStatus[lastStatus], function(x){aggregateStatus(x[["rawStatusLogical"]][[series]])})
			id <- paste0("status",gsub(" ","",series))
		}else{
			if(item==""){
				first <- lapply(timeSeriesStatus[lastStatus], function(x){aggregateStatus(x[["rawStatusLogical"]][[series]][[year]])})
				id <- paste0("status",gsub(" ","",series), year)
			}else{
				first <- lapply(timeSeriesStatus[lastStatus], function(x){x[["rawStatus"]][[series]][[year]][[item]][[2]]})
				id <- paste0("status",gsub(" ","",series), year, item)
			}
		}
	}
	
	statusMatrix <- do.call(mbind, lapply(first, function(x){(x)}))

	if(item!="")
		rownames(statusMatrix) <- c(item)

	statusMatrix <- as.data.frame(statusMatrix)

	is.na(statusMatrix) <- statusMatrix == "NULL"

	colnames(statusMatrix) <- as.Date(as.integer(lastStatus), origin = "1970-01-01")

	return(as.character(knitr::kable(statusMatrix, format="html", table.attr = paste0("id=\"", id, "\" class=\"table table-hover\""))))
} 


#' Load time series status data.
#'
#' \code{loadTimeSeriesStatus} returns the status of the timeseries processing.
#' @param type The type of data source requested. Defaults to "REDUS".
#' @export
#' @examples
#' loadTimeSeriesStatus()
loadTimeSeriesStatus <- function(type="REDUS"){

	if(type=="REDUS")
		timeseriesDir <- getTimeSeriesDir("survey")
	else
		timeseriesDir <- "/home/a5414/STOX2SAM-stockversion/data"

	timeSeriesStatusFile <- file.path(timeseriesDir, "status.rds")

	if(file.exists(timeSeriesStatusFile)){
		timeSeriesStatus <- readRDS(timeSeriesStatusFile)
	}else{
		timeSeriesStatus <- list()
	}
	return(timeSeriesStatus)
}

#' Get error information of a specific item in the matrix.
#'
#' \code{loadTimeSeriesStatus} returns the status of the timeseries processing.
#' @param path The path to the item.
#' @param timestamp The timestamp of the iter.
#' @export
getErrorInfo <- function(path, timestamp){

	status <- loadTimeSeriesStatus()
	
	recordtime <- as.character(ceiling(as.numeric(difftime(timestamp, "1970-01-01", units = c("days")))))

	detailedStatus <- status[[recordtime]][["rawStatus"]]
	
	paths <- unlist(strsplit(path, "[$]"))

	detailedStatus <- detailedStatus[[paths[[1]]]][[paths[[2]]]][[paths[[3]]]]

	return(detailedStatus[[2]])
}

