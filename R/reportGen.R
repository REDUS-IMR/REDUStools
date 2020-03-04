#' Automatic report generation of plots and table from a specific directory.
#'
#' \code{doGenReport} generates a html page using knitr from all CSV, PNG, and TXT files inside a directory.
#'
#' @param srcdir directory containing the files.
#' @param verbose whether to print all included files in the R console. Default to FALSE. 
#' @examples
#' \dontrun{
#' doGenReport("./projects")
#' }
#'
#' @importFrom markdown markdownToHTML
#' @importFrom knitr kable
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils read.csv
#' @export
doGenReport <- function(srcdir, verbose = FALSE) {

    allProj <- list.dirs(srcdir, recursive = FALSE)

    text <- c()
    toc <- c("# Table of Contents")

    for(p in allProj) {
        # Title
        text <- c(text, paste0("<h1 id=\"", basename(p), "\">Artifacts for ", toupper(basename(p)), " :</h1>"))

        # TOC
        toc <- c(toc, paste0('1. [', toupper(basename(p)), '](', paste0("#", basename(p)), ')'))

        # Set target directory
        targetName <- c("data", "model", "output", "report")
        items <- list.dirs(p, recursive = FALSE)

        for (i in items) {
            # Only process targetname
            if( !any(basename(i) %in% targetName) ) next

            text <- c(text, paste0("<h2 id=\"", paste0(basename(p), "-", basename(i)), "\">", basename(i), "</h2>"))

            toc <- c(toc, paste0('   1. [', toupper(basename(i)), '](', paste0("#", paste0(basename(p), "-", basename(i)), ')')))

            files <- list.files(i, pattern = c(".png|.csv|.txt"),  full.names = TRUE)

            if(verbose)
                print(files)

            for(f in files) {
                fname <- tools::file_path_sans_ext(basename(f))
                text <- c(text, paste0("<h3 id=\"",  paste0(basename(p), "-", basename(i), "-", fname), "\">", fname, "</h3>"))

                toc <- c(toc, paste0('      1. [', fname, '](', paste0("#", paste0(basename(p), "-", basename(i), "-", fname), ')')))

                if(tools::file_ext(f) == "csv") {
                    dt <- read.csv(f, check.names = FALSE)
                    text <- c(text, knitr::kable(dt, format = "markdown", caption = fname))
                } else if(tools::file_ext(f) == "png") {
                    text <- c(text, paste0('![', fname, '](', f, ')'))
                } else if(tools::file_ext(f) == "txt") {
                    text <- c(text, paste0('```\n', paste(readLines(f), collapse="\n"), '\n```'))
                }
            }
        }

    }

    fileConn <- file(paste0(srcdir, "/output.html"))
    writeLines(markdown::markdownToHTML(text=c(toc, "\n\n---\n", text)), fileConn)
    close(fileConn)

}
