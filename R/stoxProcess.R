#' Process survey time series using Rstox.
#'
#' \code{processRstoxSTS} returns a detailed status of the actions performed in survey time series process.
#' @param masterScript Location of the REDUS master script configuration.
#'
#' @export
#'
#' @import desc
#' @import Rstox
#' @importFrom XML xmlParse xmlNamespaceDefinitions getDefaultNamespace getNodeSet xmlValue xmlAttrs xmlName xmlValue<- saveXML newXMLDoc xmlApply
#' @importFrom rJava .jgetEx
#' @importFrom data.table data.table rbindlist tstrsplit CJ is.data.table
#' @importFrom filelock lock unlock
#' @importFrom utils packageVersion
#' @importFrom parallel detectCores
processRstoxSTS <- function(masterScript) {

  # Force Biotic data into v3
  forceBioticV3 <- function(projectName) {
    # Read project path
    projectPaths <- getProjectPaths(projectName)
    # Getting the XML files
    bioticDataPath <- paste0(projectPaths$projectPath, "/input/biotic")
    files <- list.files(bioticDataPath, full.names =  TRUE, recursive = TRUE)
    # Replace the namespaces
    lapply(files, function(x) system(paste0("sed -i'.bak' 's/\"http:\\/\\/www.imr.no\\/formats\\/nmdbiotic\\/v3.1\"/\"http:\\/\\/www.imr.no\\/formats\\/nmdbiotic\\/v3\"/g' '", x, "'")))
  }

  # Apply overrides to xml nodes
  applyXMLOverrides <- function(projectName, mySTS, masterFileName, rver = "1.2", forceFileFix = FALSE) {
    # Construct the used namespace
    selectedNS = paste("http://www.imr.no/formats/stox/v", rver, sep = "")

    # Read project.xml:
    projectPaths <- getProjectPaths(projectName)

    # Parse the XML
    projectFileName <- projectPaths$projectXML
    projectXML <- xmlParse(projectFileName)

    # Extract all available namespace(s)
    nsDefs <- xmlNamespaceDefinitions(projectXML)
    ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

    # If selectedNS is not available, fallback to the first default namespace
    if (!length(grep(paste0("^", selectedNS, "$"), ns))) {
      selectedNS <- ns[[1]]
    }

    # Parse the Master script
    masterScript <- xmlParse(masterFileName)
    masterNS <- getDefaultNamespace(masterScript)[[1]]$uri

    # Get the override xml nodes
    masterOverrides <- getNodeSet(masterScript, paste("//ns:parameters[@sts='", mySTS, "']/ns:override/*", sep = ""), namespaces = c(ns = masterNS))

    # Apply overrides
    isChanged <- FALSE
    for (newParam in masterOverrides) {
      combStr <- sprintf("[@%s='%s']", names(xmlAttrs(newParam, namespaces = c(ns = masterNS))), xmlAttrs(newParam, namespaces = c(ns = masterNS)))
      xpathExp <- paste("//ns:", xmlName(newParam), paste(combStr, collapse = ''), sep = "")
      r <- getNodeSet(projectXML, xpathExp, namespaces = c(ns = selectedNS))
      if (!is.null(r) && length(r) > 0) {
        message("Override: //", xpathExp, " with ", xmlValue(newParam))
        xmlValue(r[[1]]) <- xmlValue(newParam)
        isChanged <- TRUE
      }
    }

    # Get the filename fixes (if any)
    masterFileFix <- getNodeSet(masterScript, "//ns:fileFix", namespaces = c(ns = masterNS))

    # Fixing filename errors
    for (newParam in masterFileFix) {
      xpathExp <- paste("//ns:parameter[contains(@name,\"FileName\") and contains(text(),\"", xmlAttrs(newParam, namespaces = c(ns = masterNS))["from"], "\")]", sep = "")
      r <- getNodeSet(projectXML, xpathExp, namespaces = c(ns = selectedNS))
      if (!is.null(r) && length(r) > 0) {
        message("Change input filename: ", xmlAttrs(newParam, namespaces = c(ns = masterNS))["from"], " to ", xmlAttrs(newParam, namespaces = c(ns = masterNS))["to"])
        xmlValue(r[[1]]) <- gsub(xmlAttrs(newParam, namespaces = c(ns = masterNS))["from"], xmlAttrs(newParam, namespaces = c(ns = masterNS))["to"], xmlValue(r[[1]]), fixed = TRUE)
        isChanged <- TRUE
      }
    }

    # If force filefix
    if (forceFileFix) {
      # Get all the filename parameters
      xpathExp <- paste("//ns:process[@name=\"ReadBioticXML\"]//ns:parameter[contains(@name,\"FileName\")]", sep = "")
      r <- getNodeSet(projectXML, xpathExp, namespaces = c(ns = selectedNS))
      # List all the inputs 
      bioticFiles <- list.files(paste0(projectPaths$projectPath, "/input/biotic"), pattern = "biotic.*[.]xml")
      bioticFiles <- paste0("input/biotic/", bioticFiles)
      for (fileNo in 1:length(bioticFiles)) {
        xpathExp <- paste("//ns:process[@name=\"ReadBioticXML\"]//ns:parameter[contains(@name,\"FileName", fileNo, "\")]", sep = "")
        subNode <- getNodeSet(projectXML, xpathExp, namespaces = c(ns = selectedNS))
        if (length(subNode) > 0) {
          message("Change filename", fileNo, " to ", bioticFiles[[fileNo]])
          xmlValue(subNode[[1]]) <- bioticFiles[[fileNo]]
          isChanged <- TRUE
        }
        else { print("Need to add new node") }
        }
    }

    # Save the modified XML file
    if (isChanged) {
      message("Saving new ", projectFileName)

      # Prepare backup for the original process.xml
      file.copy(projectFileName, paste0(projectFileName, ".original"), overwrite = FALSE)

      # Prepare backup for the original process.xml
      file.copy(projectFileName, paste0(projectFileName, ".old"), overwrite = TRUE)

      # We need to do as below when saving to make sure that the XML structure is intact
      cat(iconv(saveXML(projectXML, encoding = "UTF-8", indent = TRUE), to = "UTF-8"), file = projectFileName)

      message("Done!")
    } else {
      message("Not modify ", projectFileName)
    }
  }


  getMasterConfigIndividual <- function(xmlset) {
    masterNS <- getDefaultNamespace(newXMLDoc(xmlset))[[1]]$uri
    masterConfig <- getNodeSet(newXMLDoc(xmlset), "//ns:configuration/*", namespaces = c(ns = masterNS))
    configSTS <- lapply(masterConfig, xmlValue)
    configSTSName <- sapply(masterConfig, xmlName)
    names(configSTS) <- configSTSName
    return(configSTS)
  }


  getMasterConfigs <- function(masterFileName, section) {
    masterScript <- xmlParse(masterFileName)
    masterNS <- getDefaultNamespace(masterScript)[[1]]$uri
    print(masterNS)
    masterConfig <- getNodeSet(masterScript, paste0("//ns:", section), namespaces = c(ns = masterNS))
    tmp <- xmlApply(masterConfig, xmlAttrs, namespaces = c(ns = masterNS))
    availSTS <- lapply(masterConfig, getMasterConfigIndividual)
    names(availSTS) <- sapply(tmp, function(x) x[["sts"]])
    return(availSTS)
  }

  combineOutput <- function(data) {
    # Combine all the bootstrap runs in one data table:
    DT <- rbindlist(data, idcol = TRUE)

    # Change IDs to numeric values
    if (!is.numeric(DT[, .id])) {
      DT[, .id := tstrsplit(.id, "run")[2]]
    }

    return(DT)
  }

  processReport <- function(DT, base, var = "Abundance", grp1 = "age", grp2 = NULL) {

    # If grp1 is missing, replace it with all zeros:
    if (length(grp1) == 0 || length(DT[[grp1]]) == 0) {
      grp1 <- "temp"
      DT[[grp1]] <- integer(nrow(DT))
    }

    # Set key and group by
    if (!is.null(grp2)) {
      setkeyv(DT, cols = c(".id", "Stratum", grp1, grp2))
      byGrp <- c(grp1, grp2, ".id")
    }
    else {
      setkeyv(DT, cols = c(".id", "Stratum", grp1))
      byGrp <- c(grp1, ".id")
    }

    # Filter and sum by stratum:
    strata <- unique(base$Stratum[base$includeintotal %in% TRUE])

    # Sum the abundance or the product of abundance and weight (and possibly others in the future):
    varInd <- abbrMatch(var[1], c("Abundance", "IndividualWeightGram"), ignore.case=TRUE)

    # Declare the variables used in the DT[] expression below:
    . <- NULL
    Ab.Sum <- NULL
    Abundance <- NULL
    Stratum <- NULL
    IndividualWeightGram <- NULL
    if (varInd$ind == 1) {
      tmp <- DT[Stratum %in% strata, .(Ab.Sum = sum(Abundance, na.rm = TRUE)), by = byGrp]
    }
    else if (varInd$ind == 2) {
      tmp <- DT[Stratum %in% strata, .(Weight.Sum = sum(Abundance * IndividualWeightGram, na.rm = TRUE)), by = byGrp]
    }
    else {
      warning(paste0("'var' does not match the available values (", getPlottingUnit()$defaults$Rstox_var, ")"))
    }

    tmp1 <- as.data.frame(tmp)
    unique_grp1 <- unique(tmp1[, grp1])

    if (!is.null(grp2)) {
      unique_grp2 <- unique(tmp1[, grp2])
      setkeyv(tmp, cols = c(".id", grp1, grp2))
      tmp <- tmp[CJ(unique(tmp$.id), unique_grp1, unique_grp2), allow.cartesian = TRUE]
    } else {
      setkeyv(tmp, cols = c(".id", grp1))
      tmp <- tmp[CJ(unique(tmp$.id), unique_grp1), allow.cartesian = TRUE]
    }

    return(tmp)
  }

  # Empty an STS, but retain the .git for historical indicator
  wipeSeries <- function(STS) {

    stsDirs <- dir(getProjectPaths()$projectRoot, full.names = TRUE, pattern = paste0(STS, "_"))

    toDelete <- list.files(stsDirs, recursive = TRUE, full.names = TRUE, all.files = TRUE)

    excludes <- grep("^(?=.*/[.]git?)", toDelete, perl = TRUE)

    if (length(excludes))
      toDelete <- toDelete[-excludes]

    print(toDelete)

    file.remove(toDelete)
  }

  saveSeries <- function(STS, data, data2, configSTS, masterConf) {

    # Extract fingerprints
    getFP <- function() {
      out <- list()

      rstoxDesc <- description$new(file = system.file("DESCRIPTION", package = "Rstox"))
      REDUSDesc <- description$new(file = system.file("DESCRIPTION", package = "REDUStools"))
      
      out$buildTime <- format(Sys.time(), "%a %b %d %X %Y")
      out$masterRev <- as.character(REDUSDesc$get("GithubSHA1"))
      out$rstoxRev <- as.character(rstoxDesc$get("GithubSHA1"))
      out$rstoxVer <- as.character(packageVersion("Rstox"))
      out$RVer <- version$version.string
      return(out)
    }

    # Create fingerprint
    FP <- getFP()
    print(FP)
    # If not required to save table, exit early 
    if (masterConf$saveOutputTable != "true")
      return(FP)

    # Create output structure
    tmpOut <- list(meta = configSTS, fp = FP, data = data, data2 = data2)

    # Create STS directory
    outSTS <- paste0(REDUStools::getTimeSeriesDir("survey"), STS)
    dir.create(outSTS, recursive = TRUE, showWarnings = FALSE)

    # Save it (make two files, one with timestamp)
    latestOut <- paste0(outSTS, "/output.rds")
    latestOutTS <- paste0(latestOut, ".", format(Sys.time(), "%Y%m%d.%H%M%S"))
    saveRDS(tmpOut, latestOutTS)
    file.copy(latestOutTS, latestOut, overwrite = TRUE)

    # Return FP
    return(FP)
  }

  loadSeries <- function(STS, configSTS, force) {

    # Construct STS directory
    outSTS <- paste0(getProjectPaths()$projectRoot, "/", STS)

    # see if file exists and if forceReprocess != TRUE, if FALSE do re-process
    if (file.exists(paste0(outSTS, "/output.rds")) && !force) {
      message("Found STS, now loading data...")
      # loadRDS
      tmp <- readRDS(paste0(outSTS, "/output.rds"))
      # compare, but remove "force" elements

      configSTS$overwriteNMD <- FALSE
      configSTS$forceReProcess <- FALSE

      tmp$meta$overwriteNMD <- FALSE
      tmp$meta$forceReProcess <- FALSE

      message("Comparing the saved config...")
      if (identical(configSTS, tmp$meta)) {
        message("Configs are identical!")
        return(tmp)
      } else {
        message("Configs are different, re-processing data")
        return(TRUE)
      }
    } else {
      message("Re-processing data")
      return(FALSE)
    }
  }

  processSTS <- function(configSTS, masterConf) {
    # For abrupt exit
    gotoExit <- FALSE

    # Placeholder for all returns
    mainReturn <- list()

    # Start time
    mainReturn$startTime <- Sys.time()

    # Placeholder for fingerprint
    mainReturn$fp <- list()

    # Record the status for all the important steps
    statusRecord <- list()

    # Time series name, must be from the list given by NMD
    mySTS <- configSTS$stsName

    # Whether to always download data from NMD, overwriting the old
    overwriteNMD <- eval(toupper(configSTS$overwriteNMD) == "TRUE")

    # Whether to always re-process data, old process's results is overwritten
    forceReProcess <- eval(toupper(configSTS$forceReProcess) == "TRUE")

    # Years to be skipped from the process
    skipYear <- as.numeric(unlist(strsplit(configSTS$skipYear, ",")))

    # Starting year
    startYear <- as.numeric(configSTS$startYear)
    if (is.na(startYear)) startYear <- 0

    # End year
    endYear <- as.numeric(configSTS$endYear)
    if (is.na(endYear)) endYear <- Inf

    # Desired level for report output, usually bootstrapImpute
    levelRequested <- configSTS$levelRequested

    # Parameters for bootstrap
    ## bootstrap seed
    bootstrapSeed <- as.numeric(configSTS$bootstrapSeed)
    ## bootstrapImpute seed
    bootstrapImputeSeed <- as.numeric(configSTS$bootstrapImputeSeed)
    ## how many bootstrap iterations
    bootstrapIter <- as.numeric(configSTS$bootstrapIter)
    ## how many cores use
    coresUse <- as.numeric(configSTS$coresUse)
    if(is.null(coresUse)) coresUse <- detectCores()

    # Plus age parameter
    plusAge <- as.numeric(configSTS$plusAge)

    # Output filename
    FileOutput <- configSTS$FileOutput

    # Get timeseries infromation from NMD
    STS <- getNMDinfo("sts")

    # Select a series according to the mySTS parameter
    mySeriesInfo <- STS[[mySTS]]

    # Getting the list of year from the series
    yearList <- as.numeric(mySeriesInfo[, 'sampleTime'])

    # Filter out year list using startYear, endYear, skipYears parameters
    processYearList <- yearList[yearList >= startYear & yearList <= endYear & !yearList %in% skipYear]

    # See whether we have the STS saved
    tempSeries <- loadSeries(mySTS, configSTS, forceReProcess)

    if (!is.atomic(tempSeries) && is.data.table(tempSeries$data) && !forceReProcess) {
      # Return fingerprint instead
      mainReturn$fp <- tempSeries$fp
      mainReturn$tempTBL <- tempSeries$data
      mainReturn$tempTBLLK <- tempSeries$data2
      return(mainReturn)
    } else {
      # Empty placeholder for results
      tempTBL <- data.table()
      tempTBLLK <- data.table()
      # No previously saved data means config can be altered since the latest processing, force re-process
      forceReProcess <- TRUE
    }

    # Getting the series (##statusRecord)
    # Check whether the data is downloaded correctly, update statusRecord accordingly
    tryCatch({
      if (overwriteNMD)
        wipeSeries(mySTS)
      getNMDdata(mySTS, server = "http://tomcat7.imr.no:8080/apis/nmdapi", ver = list(biotic = 3, API = list(biotic = 3)), zipSTS = FALSE, ow = overwriteNMD)
    }, error = function(e) {
      statusRecord$global$getNMDdata <<- list(FALSE, as.character(e))
      mainReturn$statusRecord <<- statusRecord
      mainReturn$tempTBL <<- tempTBL
      mainReturn$tempTBLLK <<- tempTBLLK
      gotoExit <<- TRUE
    })
    if (gotoExit == TRUE) {
      return(mainReturn)
    } else {
      statusRecord$global$getNMDdata <- list(TRUE, NA)
    }

    # NMD API v2 doesn't spit sorted data, sort it now
    processYearList <- processYearList[order(processYearList)]

    # Do processing, starting from baseline, bootstrap, imputation, and report building
    # Unable to process year 1993, error happened during getBaseline processing
    for (processYear in processYearList) {
      # For skipping
      gotoNext <- FALSE

      textYear <- as.character(processYear)

      # Placeholder for statusRecord
      statusRecord[[textYear]] <- list()

      # changes indicator
      isProjectUpdated <- FALSE

      # Generate name of a project
      projectName <- paste(mySTS, processYear, sep = "_")

      message(projectName)

      # If set in the master script, override values in the project XML ((##statusRecord)
      if (toupper(configSTS$applyOverrides) == "TRUE") {
        tryCatch({
          applyXMLOverrides(projectName, mySTS, masterScript, forceFileFix = FALSE)
        }, error = function(e) {
          statusRecord[[textYear]]$applyXMLOverrides <<- list(FALSE, as.character(e))
        })
        if (length(statusRecord[[textYear]]$applyXMLOverrides) == 0) {
          statusRecord[[textYear]]$applyXMLOverrides <- list(TRUE, NA)
        }
      } else {
        # If apply overrides is set to FALSE, mark it as done without error in the status
        statusRecord[[textYear]]$applyXMLOverrides <- list(TRUE, NA)
      }

      # If we must rename the XML Biotic v3.1 to v3
      if (!is.null(configSTS$forceBioticV3) && toupper(configSTS$forceBioticV3) == "TRUE") {
        forceBioticV3(projectName)
      }

      # Read the project data
      if (!forceReProcess)
        projectEnv <- loadProjectData(projectName)

      # Link the data to the projects due to a bug in the zipped files:
      #projectEnv <- updateProject(projectName)

      # Run the bootstrapping to generate estimates of the variability in the data (cv):
      if (forceReProcess || is.null(projectEnv[['bootstrap']]$base.SuperIndAbundance)) {
        # Get the output and inputs of the baseline (##statusRecord)
        projectBaseline <- tryCatch({
          getBaseline(projectName)
        }, error = function(e) {
          statusRecord[[textYear]]$getBaseline <<- list(FALSE, as.character(e))
          gotoNext <<- TRUE
        }, finally = {
          if (!is.null(e <- .jgetEx())) {
            print("Java exception was raised")
            statusRecord[[textYear]]$getBaseline <<- list(FALSE, as.character(e))
            gotoNext <<- TRUE
          }
        })

        if (!is.list(projectBaseline) || length(projectBaseline$outputData[['SuperIndAbundance']]) == 0) {
          statusRecord[[textYear]]$getBaseline <- list(FALSE, "Result length is zero")
          gotoNext <- TRUE
        }

        if (gotoNext == TRUE)
          next
        else
          statusRecord[[textYear]]$getBaseline <- list(TRUE, NA)

        # Run bootstrap (##statusRecord)
        temp <- tryCatch({
          runBootstrap(projectName, nboot = bootstrapIter, cores = coresUse, seed = bootstrapSeed, acousticMethod = PSU ~ Stratum, bioticMethod = EDSU ~ Stratum)
        }, error = function(e) {
          statusRecord[[textYear]]$runBootstrap <<- list(FALSE, as.character(e))
          gotoNext <<- TRUE
        }, finally = {
          if (!is.null(e <- .jgetEx())) {
            print("Java exception was raised")
            statusRecord[[textYear]]$runBootstrap <<- list(FALSE, as.character(e))
            gotoNext <<- TRUE
          }
        })
        if (sum(unlist(lapply(getProjectData(projectName, var = "bootstrap")$SuperIndAbundance, nrow))) == 0) {
          statusRecord[[textYear]]$runBootstrap <- list(FALSE, "Result length is zero")
          gotoNext <- TRUE
        }

        if (gotoNext == TRUE)
          next
        else
          statusRecord[[textYear]]$runBootstrap <- list(TRUE, NA)

        isProjectUpdated <- TRUE
      }

      print("Bootstrap Impute")

      # Fill in missing data (missing length, weight and so on) based on the age information:
      if (forceReProcess || is.null(projectEnv[['bootstrapImpute']]$base.SuperIndAbundance)) {
        temp <- tryCatch({
          imputeByAge(projectName, cores = coresUse, seed = bootstrapImputeSeed)
        }, error = function(e) {
          statusRecord[[textYear]]$imputeByAge <<- list(FALSE, as.character(e))
          gotoNext <<- TRUE
        }, finally = {
          if (!is.null(e <- .jgetEx())) {
            print("Java exception was raised")
            statusRecord[[textYear]]$imputeByAge <<- list(FALSE, as.character(e))
            gotoNext <<- TRUE
          }
        })

        if (sum(unlist(lapply(getProjectData(projectName, var = "bootstrapImpute")$SuperIndAbundance, nrow))) == 0) {
          statusRecord[[textYear]]$imputeByAge <- list(FALSE, "Result length is zero")
          gotoNext <- TRUE
        }

        if (gotoNext == TRUE)
          next
        else
          statusRecord[[textYear]]$imputeByAge <- list(TRUE, NA)

        isProjectUpdated <- TRUE
      }

      print("Save bootstrap data")

      # Save the bootstrap data:
      if (isProjectUpdated) {
        temp <- tryCatch({
          saveProjectData(projectName)
        }, error = function(e) {
          statusRecord[[textYear]]$saveProjectData <<- list(FALSE, as.character(e))
          gotoNext <<- TRUE
        })

        if (gotoNext == TRUE)
          next
        else
          statusRecord[[textYear]]$saveProjectData <- list(TRUE, NA)

        projectEnv <- tryCatch({
          loadProjectData(projectName)
        }, error = function(e) {
          statusRecord[[textYear]]$loadProjectData <<- list(FALSE, as.character(e))
          gotoNext <<- TRUE
        })

        if (gotoNext == TRUE)
          next
        else
          statusRecord[[textYear]]$loadProjectData <- list(TRUE, NA)
        }

      print("Generate reports")

      # Generate reports:
      temp <- tryCatch({
        reportAbundance(projectName)
      }, error = function(e) {
        statusRecord[[textYear]]$getReports <<- list(FALSE, as.character(e))
        gotoNext <<- TRUE
      })

      if (gotoNext == TRUE)
        next
      else
        statusRecord[[textYear]]$getReports <- list(TRUE, NA)

      # If baseline data is requested, get base data from projectBaseline,
      # whereas boostrap and bootstrapImpute data are available in projectEnv
      if (levelRequested == "baseline") {
        toProcessData <- list(projectBaseline$outputData[['SuperIndAbundance']])
        baseData <- projectBaseline$outputData[['SuperIndAbundance']]
      } else {
        toProcessData <- projectEnv[[levelRequested]]$SuperIndAbundance
        baseData <- projectEnv[[levelRequested]]$base.SuperIndAbundance
      }

      print("Processing the raw outputs")

      # Process the Raw Output (includes saving it to be processed later)
      DT <- combineOutput(toProcessData)

      # Process report (Abundance per group)
      tempAbundanceAge <- processReport(DT, baseData, var = "Abundance", grp1 = configSTS$groupType)

      # Process report (Weight per group)
      tempWeightAge <- processReport(DT, baseData, var = "IndividualWeightGram", grp1 = configSTS$groupType)

      # Add year information to the processed data
      tempAge <- tryCatch({
        cbind("year" = processYear, merge(tempAbundanceAge, tempWeightAge))
      }, error = function(e) {
        statusRecord[[textYear]]$getReports <<- list(FALSE, as.character(e))
        gotoNext <<- TRUE
      })

      if (gotoNext == TRUE)
        next

      # Append data
      tempTBL <- rbindlist(list(tempTBL, tempAge), use.names = TRUE, fill = TRUE)

      #--- Length Key data (for gadget)
      # Process report (ALK)
      tempALK <- processReport(DT, baseData, var = "Abundance", grp1 = "LenGrp", grp2 = "age")

      # Process report (WLK)
      tempWLK <- processReport(DT, baseData, var = "IndividualWeightGram", grp1 = "LenGrp", grp2 = "age")

      # Add year information to the processed data
      tempLK <- cbind("year" = processYear, merge(tempALK, tempWLK))

      # Append data
      tempTBLLK <- rbindlist(list(tempTBLLK, tempLK), use.names = TRUE, fill = TRUE)
      #--- End Length Key data (for gadget)

      # Close the current project to save memory:
      closeProject(projectName)

      rm(tempALK, tempWLK, tempLK, tempAge, tempWeightAge, tempAbundanceAge, DT, temp, baseData)
      rm(projectEnv)
      gc()

    }

    # Save the series output table and metadata and status
    mainReturn$fp <- tryCatch({
      saveSeries(mySTS, tempTBL, tempTBLLK, configSTS, masterConf)
    }, error = function(e) {
      statusRecord$global$saveSeries <<- list(FALSE, as.character(e))
    })

    if (length(statusRecord[[textYear]]$saveSeries) == 0) {
      statusRecord$global$saveSeries <- list(TRUE, NA)
    }

    # Put the end time
    mainReturn$endTime <- Sys.time()

    # Put all return values
    mainReturn$tempTBL <- tempTBL
    mainReturn$tempTBLLK <- tempTBLLK
    mainReturn$statusRecord <- statusRecord
    return(mainReturn)
  }


  genStatusReport <- function(x) {

    fillStatus <- function(x) {

      statusTemplate <- list(applyXMLOverrides = NA, getBaseline = NA, runBootstrap = NA, imputeByAge = NA, saveProjectData = NA, loadProjectData = NA, getReports = NA)

      for (i in 1:length(statusTemplate)) {
        name <- names(statusTemplate[i])
        if (length(x[[name]]) < 1) x[[name]] <- statusTemplate[[i]]
        }
      return(x)
    }

    doRecursiveProc <- function(y) {
      if (is.list(y[[1]])) {
        ret <- lapply(y, doRecursiveProc)
        #print(names(ret[1]))
        if (names(ret[1]) != "getNMDdata")
          ret <- fillStatus(ret)
        } else {
          ret <- all(as.logical(unlist(y, recursive = F, use.names = F)), na.rm = T)
        }
      return(ret)
    }

    raw <- lapply(x$statusRecord, doRecursiveProc)
  }

  aggregateStatus <- function(statusLogical) {
    as.matrix(lapply(statusLogical, function(x) { all(unlist(x)) }))
  }

  saveRunStatus <- function(masterConf) {

    rawStatus <- masterConf$rawStatus
    rawStatusLogical <- masterConf$rawStatusLogical
    timestamp <- masterConf$timestamp

    timeseriesDir <- REDUStools::getTimeSeriesDir("survey")
    dir.create(timeseriesDir)
    timeSeriesStatusFile <- file.path(timeseriesDir, "/status.rds")

    #lock
    file.lock <- lock(paste0(timeSeriesStatusFile, ".lock"))

    #load
    if (file.exists(timeSeriesStatusFile)) {
      timeSeriesStatus <- readRDS(timeSeriesStatusFile)
    } else {
      timeSeriesStatus <- list()
    }

    #append
    timeSeriesStatus[[timestamp]] <- list()
    timeSeriesStatus[[timestamp]][["rawStatusLogical"]] <- rawStatusLogical
    timeSeriesStatus[[timestamp]][["rawStatus"]] <- rawStatus

    #save
    saveRDS(timeSeriesStatus, file = timeSeriesStatusFile)

    #unlock
    unlock(file.lock)
  }

  # Parse parameters from master script
  # (TODO: Look at the possibility to integrate XSD, as this will remove the need of variable casting)
  masterConf <- getMasterConfigs(masterScript, "configuration")[[1]]

  # Set to maindir (containing scripts, templates, etc.)
  #mainDir <- masterConf$scriptDir
  #setwd(mainDir)

  # Parse all STS parameters
  mainParams <- getMasterConfigs(masterScript, "parameters")

  # Process the STS
  processResults <- lapply(mainParams, processSTS, masterConf)
  resultNames <- names(processResults)

  # Create Status Matrix
  masterConf$rawStatusLogical <- lapply(processResults, genStatusReport)
  masterConf$rawStatus <- lapply(processResults, function(x) { x$statusRecord })

  # Save the status Matrix for the current date
  masterConf$timestamp <- as.character(as.numeric(as.Date(processResults[[1]]$startTime)))

  # /data/timeseries/status.rds
  # /data/timeseries/datadiff.rds

  if (masterConf$saveRunStatus == "true")
    saveRunStatus(masterConf)

  #if(masterConf$saveOutputStatics == "true")
  #  createStaticOutput(mainParams, processResults, resultNames, REDUStools::getTimeSeriesDir("survey"))

  #if(masterConf$generateAssessmentFiles == "true")
  #	createAssessmentFiles(mainParams, processResults, resultNames, masterConf$stsOutDir)

  
  return(masterConf$rawStatusLogical)
}
