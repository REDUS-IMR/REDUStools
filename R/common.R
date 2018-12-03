# Keep CRAN check happy
globalVariables(c("year", ".SD", ".x", "LenGrp", "..keep", ":="))

# Get configuration
getConfig <- function() {
   configFile <- system.file('extdata', 'config.yaml', package='REDUStools')
   config <- config::get(file = configFile, use_parent = FALSE)
   return(config)
}

# Get Time series directory
getTimeSeriesDir <- function(type) {
	config <- getConfig()
	return(paste0(config[["root.dir"]], "/", config[[paste0(type, ".dir")]], "/timeseries/"))
}

# Get REDUS work directory
getREDUSRootDir <- function() {
   config <- getConfig()
   return(paste0(config[["root.dir"]], "/"))
}

# Get template directory
getTemplateDir <- function() {
   tmpltDir <- system.file('extdata', 'template', package='REDUStools')
   return(tmpltDir)
}
