# Keep CRAN check happy
globalVariables(c("year", ".SD", ".x", "LenGrp", "..keep", ":="))

# Get configuration
getConfig <- function() {
   configFile <- system.file('extdata', 'config.yaml', package='REDUStools')
   config <- config::get(file = configFile, use_parent = FALSE)
   return(config)
}

#' Get the time series directory path
#'
#' \code{getTimeSeriesDir} returns the path of the time series working directory,
#' which the type can be "survey" or "catch".
#'
#' @param type The requested type of survey time series.
#' @return The absolute path to the survey time type series requested.
#'
#' @examples
#' getTimeSeriesDir("survey")
#'
#' @export
getTimeSeriesDir <- function(type) {
	config <- getConfig()
	return(paste0(config[["root.dir"]], "/", config[[paste0(type, ".dir")]], "/timeseries/"))
}

#' Get REDUS work directory (as defined in the option file)
#'
#' @export
getREDUSRootDir <- function() {
   config <- getConfig()
   return(paste0(config[["root.dir"]], "/"))
}

# Get template directory
getTemplateDir <- function() {
   tmpltDir <- system.file('extdata', 'template', package='REDUStools')
   return(tmpltDir)
}
