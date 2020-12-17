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
#' `usethis::use_git()`. Default is set to FALSE.
#' 
#' @importFrom available available_on_bioc valid_package_name
#' 
#' @export
#' 
#' @examples
#' fl <- tempdir()
#' create_pkg(paste0(fl, "/tstPkg"), "AnnotationHub", TRUE)
create_pkg <- function(package,
    type = c("AnnotationHub", "ExperimentHub"),
    use_git = FALSE)
{
    current_dir <- getwd()
    on.exit(setwd(current_dir))

    pth <- path.expand(package)
    pkg <- basename(pth)
    stopifnot(
        !file.exists(pth),
        length(pkg) == 1 && is.character(pkg),
        length(BiocManager::available(pkg)) == 0L,
        available_on_bioc(pkg),
        valid_package_name(pkg)
    )

    usethis::create_package(pth)
    usethis::proj_set(pth)

    if(use_git) {
        usethis::use_git()
    }

    biocthis::use_bioc_description(biocViews = type)

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
    x <- as.list(c(title = "Title", description = "Description",
        biocversion = "BiocVersion", genome = "Genome",
        sourcetype = "SourceType", sourceurl = "SourceUrl",
        sourceversion = "SourceVersion", species = "Species",
        taxonomyid = "TaxonomyId", coordinate1based = "Coordinate_1_based",
        dataprovider = "DataProvider", maintainer = "Maintainer",
        rdataclass = "RDataClass", dispatchclass = "DispatchClass",
        locationprefix = "Location_Prefix", rdatapath = "RDataPath",
        tags = "Tags"))
    usethis::use_template("metadata.csv",
        save_as = "/inst/extdata/metadata.csv",
        data = x,
        package = "HubPub")
    #df <- data.frame(matrix(0, nrow = 0, ncol = 17, dimnames = list(NULL, x)))
    #fl <- file.path(pth, "inst", "extdata", "metadata.csv")
    #write.csv(df, file = fl, row.names = FALSE)

    usethis::use_testthat()
    usethis::use_template("test_metadata.R",
        save_as = "/tests/testthat/test_metadata.R",
        data = list(type = type, package = pkg),
        package = "HubPub")
    invisible(pth)
}
