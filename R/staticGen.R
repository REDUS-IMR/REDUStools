#' @importFrom ggplot2 ggsave
genPNGplot <- function(stsData, stsName, staticOutDir){

	if(length(stsData$data)==0)
	   return(NULL)

	# Generate plots
	outChart <- list()
	outChart$Ab <- stsPlot(stsData, "Abundance")
	outChart$Wt <- stsPlot(stsData, "Weight")

        print(staticOutDir)

	# Save the charts
	ggplot2::ggsave(filename=paste0(staticOutDir, "/", stsName, ".Abundance.png"), plot=outChart$Ab, type="cairo-png")
	ggplot2::ggsave(filename=paste0(staticOutDir, "/", stsName, ".Weight.png"), plot=outChart$Wt, type="cairo-png")

	return(outChart)
}

#' @importFrom knitr knit2pdf
#' @importFrom utils write.table
genTables <- function(stsData, stsName, staticOutDir){

	if(length(stsData$data)==0)
	   return(NULL)

        # Generate plots
	outTable <- list()
        outTable$Ab <- stsTable(stsData, "Abundance", raw=TRUE)
        outTable$Wt <- stsTable(stsData, "Weight", raw=TRUE)

	# Use knitr to generate pdf file using latex
	tblTempfile <- tempfile()
        templateDir <- getTemplateDir()
        
        file.copy(paste0(templateDir, "/framed.sty"), dirname(tblTempfile))
	knitr::knit2pdf(paste0(templateDir, "/genTable.Rnw"), output=paste0(tblTempfile, ".tex"))

	file.copy(paste0(tblTempfile,".pdf"), paste0(staticOutDir, "/", stsName, ".result.pdf"), overwrite = TRUE)
	file.copy(paste0(tblTempfile,".tex"), paste0(staticOutDir, "/", stsName, ".result.tex"), overwrite = TRUE)

	# Create CSVs
	utils::write.table(as.data.frame(as.matrix(outTable$Ab)), sep=",", row.names=TRUE, col.names=TRUE, file=paste0(staticOutDir, "/", stsName, ".Abundance.csv"))
	utils::write.table(as.data.frame(as.matrix(outTable$Wt)), sep=",", row.names=TRUE, col.names=TRUE, file=paste0(staticOutDir, "/", stsName, ".Weight.csv"))

	return(outTable)
}

#' @importFrom knitr knit2html
#' @importFrom data.table data.table
genHTML <- function(results, outTable, stsName, staticOutDir){

	if(length(outTable)==0)
		return(NULL)

	tempFP <- format(results$fp)
	stsFP <- data.table::data.table(param=names(tempFP),value=tempFP)
	knitr::knit2html(paste0(getTemplateDir(), "/genHTML.Rmd"), output=paste0(staticOutDir, "/index.html"))
}

createStaticOutput <- function(sts, outDir) {
	## Create static outputs

        # Load STS
	processResults <- loadSTS(sts)

	# Create output directory
        stsOut <- paste0(outDir, "/", sts, "/output")
	dir.create(stsOut, recursive = TRUE, showWarnings = FALSE)

	outChart <- genPNGplot(processResults, sts, stsOut)
	outTable <- genTables(processResults, sts, stsOut)

	genHTML(processResults, outTable, sts, stsOut)
}


