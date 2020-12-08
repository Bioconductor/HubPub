#' Add a hub resource
#' 
#' This function adds a hub resource to the AH or EH package metadata.csv file.
#' It can be used while creating a new hub package or for adding data to an 
#' existing package.
#' 
#' @param package A `character(1)` with the name of an existing hub package or 
#' the path to a newly created (not yet submitted/accepted) hub package.
#' @param fields A named character list with the data to be added to the 
#' resource. The required entries are:
#' \itemize{
#'   \item title
#'   \item description
#'   \item biocversion
#'   \item genome
#'   \item soucetype
#'   \item sourceurl
#'   \item sourceversion
#'   \item species
#'   \item taxonomyid
#'   \item coordinate1based
#'   \item dataprovider
#'   \item maintainer
#'   \item rdataclass
#'   \item dispatchclass
#'   \item locationprefix
#'   \item rdatapath
#'   \item tags
#' All these entries are needed in order to add the resource properly to the 
#' metadata.csv file.
#' 
#' @export
#' 
#' @examples
#' tst <- list(title = "ENCODE", description = "a test entry", biocverison = "3.9", genome = NA, sourcetype = "JSON", sourceurl = "https://www.encodeproject.org", sourceversion = NA, species = NA, taxonomyid = NA, coordinate1based = NA, dataprovider = "ENCODE Project", maintainer = "tst person <tst@email.com>", rdataclass = "data.table", dispatchclass = "Rda", locationprefix = NA, rdatapath = "ENCODExplorerData/encode_df_lite.rda", tags = "ENCODE")
#' hub_add_resource("~/Documents/tstPkg", fields = tst)
hub_add_resource <- function(package, fields)
{
    fl <- system.file("inst", "templates", "metadata.csv",
        package = "HubPub")
    tmpl <- readLines(fl)

    ## read in the metadata.csv file
    if (available_on_bioc(package))
        dat_path <- file.path(package, "inst", "extdata", "metadata.csv")
    else
        dat_path <- system.file("extdata", "metadata.csv", package = package)

    #metadata <- read.csv(file = dat_path)

    dat <- strsplit(whisker.render(tmpl, data = fields), ",")
    #metadata[dim(metadata)[1]+1,] <- dat[[1]]
    write.table(dat, file = dat_path, row.names = FALSE, sep = ",", append = TRUE)
    #Test*HubMetadata(package) change for specific hub
}
