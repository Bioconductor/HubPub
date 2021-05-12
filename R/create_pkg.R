#' Create a Bioconductor Hub package
#'
#' This function creates the skeleton of a package that follow the guidelines
#' for Bioconductor type packages. It is expected of the user to go through and
#' make any necessary changes or improvements once the package begins to take
#' shape. For examples, the DESCRIPTION contains very basic requirements, but
#' the developer should go back and fill in the 'Title:' and 'Description:'
#' fields.
#'
#' @param package A `character(1)` with the path of the package to be created.
#' @param type A `character(1)` to indicate what type of hub package is to be
#' created. Either `AnnotationHub` or `ExperimentHub` are acceptable.
#' @param use_git A `logical(1)` indicating whether to set up `git` using
#' `usethis::use_git()`. Default is set to TRUE.
#'
#' @importFrom available available_on_bioc valid_package_name
#' @importFrom BiocManager available 
#'
#' @return Path to package location
#'
#' @examples
#' fl <- tempdir()
#' create_pkg(file.path(fl, "tstPkg"), "AnnotationHub")
#'
#' @export
create_pkg <- function(package,
    type = c("AnnotationHub", "ExperimentHub"),
    use_git = TRUE)
{
    current_dir <- getwd()
    old_interactive <- options(rlang_interactive = FALSE)
    on.exit({
        setwd(current_dir)
        options(old_interactive)
    })

    pth <- path.expand(package)
    pkg <- basename(pth)
    type <- match.arg(type)
    stopifnot(
        !file.exists(pth),
        length(pkg) == 1 && is.character(pkg),
        length(BiocManager::available(pkg)) == 0L,
        available_on_bioc(pkg),
        valid_package_name(pkg)
    )

    if (type == "AnnotationHub") {
        import <- "AnnotationHubData"
    }
    else
        import <- "ExperimentHub"

    bioc_fields <- list(Version = "0.99.0",
        biocViews = type,
        License = "Artistic-2.0",
        Date = Sys.Date(),
        Imports = import,
        BugReports = paste0("https://support.bioconductor.org/t/", pkg)
    )

    usethis::create_package(pth, fields = bioc_fields)
    usethis::proj_set(pth)

    if(use_git) {
        usethis::use_git()
    }

    usethis::use_template("pkg-package.R",
        save_as = paste0("/R/",pkg,"-package.R"),
        data = list(package = pkg),
        package = "HubPub")

    biocthis::use_bioc_news_md(open = FALSE)
    usethis::use_directory("man")

    usethis::use_directory("inst/scripts")
    usethis::use_template("make-data.R",
        save_as = "/inst/scripts/make-data.R",
        package = "HubPub")
    usethis::use_template("make-metadata.R",
        save_as = "/inst/scripts/make-metadata.R",
        package = "HubPub")

    if (type == "ExperimentHub")
        usethis::use_template("zzz.R",
            save_as = "/R/zzz.R",
            package = "HubPub")

    usethis::use_directory("inst/extdata")
    metadata_path <- file.path(pth, "inst", "extdata", "metadata.csv")
    writeLines(paste(names(hub_metadata()), collapse = ","), metadata_path)

    usethis::use_testthat()
    usethis::use_template("test_metadata.R",
        save_as = "/tests/testthat/test_metadata.R",
        data = list(type = type, package = pkg),
        package = "HubPub")

    pth
}
