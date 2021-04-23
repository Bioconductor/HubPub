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
#' @param metafile A `character(1)` with the name of the metadata csv file.
#'     The default file name is 'metadata.csv'. 
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
#'     TaxonomyId = as.integer(9606),
#'     Coordinate_1_based = NA,
#'     DataProvider = "ENCODE Project",
#'     Maintainer = "tst person <tst@email.com>",
#'     RDataClass = "data.table",
#'     DispatchClass = "Rda",
#'     Location_Prefix = "s3://annotationhub/",
#'     RDataPath = "ENCODExplorerData/encode_df_lite.rda",
#'     Tags = "ENCODE:Homo sapiens"
#' )
#'
#' ## add the record to the metadata
#' add_resource(pkgdir, metadata)
#'
#' @export
add_resource <- function(package, fields, metafile = "metadata.csv")
{
    .metadata_validate(fields)
    fields[["Tags"]] <- list(fields[["Tags"]])

    ## read in the metadata.csv file
    metafile <- metafile
    if (available_on_bioc(package)) 
        dat_path <- file.path(package, "inst", "extdata", metafile)
    else
        dat_path <- system.file("extdata", metafile, package = package)

    metadata <- .import_metadata(dat_path)
    metadata <- bind_rows(metadata, fields)
    .export_metadata(metadata, dat_path)

    ## validate *HubMetadata(package)
    descrip <- read.dcf(file.path(package, "DESCRIPTION"))
    biocviews <- descrip[,"biocViews"]
    terms <- strsplit(biocviews, ",[[:space:]]*")[[1]]
    tags <- strsplit(fields$Tags[[1]], ":")[[1]]

    if ("AnnotationHub" %in% terms)
        AnnotationHubData::AnnotationHubMetadata(
            Title = fields$Title,
            Description = fields$Description,
            BiocVersion = fields$BiocVersion,
            Genome = fields$Genome,
            SourceType = fields$SourceType,
            SourceUrl = fields$SourceUrl,
            SourceVersion = fields$SourceVersion,
            Species = fields$Species,
            TaxonomyId = fields$TaxonomyId,
            Coordinate_1_based = fields$Coordinate_1_based,
            DataProvider = fields$DataProvider,
            Maintainer = fields$Maintainer,
            RDataClass = fields$RDataClass,
            DispatchClass = fields$DispatchClass,
            Location_Prefix = fields$Location_Prefix,
            RDataPath = fields$RDataPath,
            Tags = tags,
            RDataDateAdded = Sys.time(),
            Recipe = NA_character_)   
    else if ("ExperimentHub" %in% terms)
        ExperimentHubData::ExperimentHubMetadata(
            Title = fields$Title,
            Description = fields$Description,
            BiocVersion = fields$BiocVersion,
            Genome = fields$Genome,
            SourceType = fields$SourceType,
            SourceUrl = fields$SourceUrl,
            SourceVersion = fields$SourceVersion,
            Species = fields$Species,
            TaxonomyId = fields$TaxonomyId,
            Coordinate_1_based = fields$Coordinate_1_based,
            DataProvider = fields$DataProvider,
            Maintainer = fields$Maintainer,
            RDataClass = fields$RDataClass,
            DispatchClass = fields$DispatchClass,
            Location_Prefix = fields$Location_Prefix,
            RDataPath = fields$RDataPath,
            Tags = tags,
            RDataDateAdded = Sys.time())
    else
        stop("The package needs to include a valid Hub term in the biocViews")

    dat_path
}
