#' Add a hub resource
#'
#' This function adds a hub resource to the AH or EH package metadata.csv file.
#' It can be used while creating a new hub package or for adding data to an
#' existing package.
#'
#' @param package A `character(1)` with the name of an existing hub package or
#' the path to a newly created (not yet submitted/accepted) hub package.
#'
#' @param fields A named list with the data to be added to the
#' resource. Elements and content of the list are described in `?metadata`.
#'
#' @importFrom dplyr bind_rows
#'
#' @examples
#' ## create a mock package with metadata.csv; these steps are usually
#' ## done by `create_pkg()` ...
#' pkgdir <- tempfile()
#' metadata_path <- file.path(pkgdir, "inst", "extdata", "metadata.csv")
#' dir.create(dirname(metadata_path), recursive = TRUE)
#' writeLines(paste(names(metadata()), collapse = ","), metadata_path)
#'
#'
#' ## create a metadata record
#' metadata <- metadata(
#'     Title = "ENCODE",
#'     Description = "a test entry",
#'     BiocVersion = "4.1",
#'     Genome = NA_character_,
#'     SourceType = "JSON",
#'     SourceUrl = "https://www.encodeproject.org",
#'     SourceVersion = "x.y.z",
#'     Species = NA_character_,
#'     TaxonomyId = NA_integer_,
#'     Coordinate_1_based = NA,
#'     DataProvider = "ENCODE Project",
#'     Maintainer = "tst person <tst@email.com>",
#'     RDataClass = "data.table",
#'     DispatchClass = "Rda",
#'     Location_Prefix = NA_character_,
#'     RDataPath = "ENCODExplorerData/encode_df_lite.rda",
#'     Tags = c("ENCODE", "Homo sapiens")
#' )
#'
#' ## add the record to the metadata
#' add_resource(pkgdir, metadata)
#'
#' @export
add_resource <- function(package, fields)
{
    .metadata_validate(fields)
    fields[["Tags"]] <- list(fields[["Tags"]])

    ## read in the metadata.csv file
    if (available_on_bioc(package))
        dat_path <- file.path(package, "inst", "extdata", "metadata.csv")
    else
        dat_path <- system.file("extdata", "metadata.csv", package = package)

    metadata <- .import_metadata(dat_path)
    metadata <- bind_rows(metadata, fields)
    .export_metadata(metadata, dat_path)

    #Test*HubMetadata(package) change for specific hub
    dat_path
}
