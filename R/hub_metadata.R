.METADATA <- list(
    Title = character(1),
    Description = character(1),
    BiocVersion = package_version("0.0"),
    Genome = character(1),
    SourceType = character(1),
    SourceUrl = character(1),
    SourceVersion = character(1),
    Species = character(1),
    TaxonomyId = integer(1),
    Coordinate_1_based = NA,
    DataProvider = character(1),
    Maintainer = character(1),
    RDataClass = character(1),
    DispatchClass = character(1),
    Location_Prefix = character(1),
    RDataPath = character(1),
    Tags = character()
)


.metadata_validate <-
    function(value)
{
    ## all names of user-supplied value in names of template
    stopifnot(
        setequal(names(value), names(.METADATA))
    )

    ## make sure value is in same order as template
    value <- value[names(.METADATA)]

    ## check value to class of template
    ok <- mapply(function(v, t) inherits(v, class(t)), value, .METADATA)
    if (!all(ok))
        stop(
            "'class(value)' differs from class of template for fields:\n    ",
            paste(names(.METADATA)[!ok], collapse = "\n    ")
        )

    TRUE
}

#' Create and validate metadata
#'
#' This functions makes a list of values that can be used to add as a
#' resource to a 'metadata.csv' file in a Hub package. The type of
#' each argument indicates the expected value, e.g., `Title =
#' character(1)` indicates that it is expected to be a character
#' vector of length 1. See individual parameters for more information.
#'
#' @param Title `character(1)` Title for the resource with version or genome 
#'     build as appropriate.
#'
#' @param Description `character(1)` Description of the resource. May include 
#'     details such as data type, format, study origin, sequencing 
#'     technology, treated vs control, number of samples etc.
#'
#' @param BiocVersion The two-digit version of Bioconductor the
#'     resource is being introduced into. Could be a character vector
#'     `"4.1"` or an object created from `package_version()`, e.g.,
#'     `package_version("4.1")`.
#'
#' @param Genome `character(1)` Name of genome build.
#'
#' @param SourceType `character(1)` Form of originial data, e.g., BED, FASTA, 
#'     etc. `getValidSourceTypes()` list currently acceptable values. If nothing
#'     seems appropriate for your data reach out to maintainer@bioconductor.org.
#'
#' @param SourceUrl `character(1)` URL of originial resource(s).
#'
#' @param SourceVersion `character(1)`. A description of the version of
#'     the resource in the original source. Since source version may
#'     not follow R / Bioconductor versioning practices, this field
#'     is not restricted to a `package_version()` format.
#'
#' @param Species `character(1)` Species name. For help on valid species see 
#'     `getSpeciesList`, `validSpecies`, or `suggestSpecies`.
#'
#' @param TaxonomyId `integer(1)` NCBI code. There are checks for valid 
#'     taxonomyID given the Species which produce warnings. See
#'     GenomeInfoDb::loadTaxonomyDb() for full validation table.
#'
#' @param Coordinate_1_based `logical(1)` are the genomic coordinates in
#'     the resource 0-based, or 1-based? Use NA if genomic coordinates
#'     are not present in the resource.
#'
#' @param DataProvider `character(1)` Provider of original data, e.g., NCBI, 
#'     UniProt etc.
#'
#' @param Maintainer `character(1)` Maintainer name and email address, `A 
#'     Maintainer <URL: a. maintainer@email.com>`.
#'
#' @param RDataClass `character(1)` Class of derived R object, e.g., GRanges. 
#'     Length must match the length of `RDataPath`.
#'
#' @param DispatchClass `character(1)` Determines how data are loaded into R. 
#'     The value for this field should be `Rda` if the data were serialized with
#'     `save()` and `Rds` if serialized with `saveRDS`. The filename should have
#'     the appropriate `rda` or `rds` extension.
#'
#'     A number of dispatch classes are pre-defined in 
#'     AnnotationHub/R/AnnotationHubResource-class.R with the suffix `Resource`.
#'     For example, if you have sqlite files, the AnnotationHubResource-class.R
#'     defines SQLiteFileResource so the DispatchClass would be SQLiteFile. 
#'     Contact maintainer@bioconductor.org if you are not sure which class to 
#'     use. The function `AnnotationHub::DispatchClassList()` will output a 
#'     matrix of currently implemented DispatchClass and brief description of 
#'     utility. If a predefine class does not seem appropriate contact 
#'     maintainer@bioconductor.org.
#'
#' @param Location_Prefix `character(1)` URL location of AWS S3 bucket or web 
#'     site where resource is located.
#'
#' @param RDataPath `character(1)` File path to where object is stored in AWS S3
#'     bucket or on the web. This field should be the remainder of the path to 
#'     the resource. The `Location_Prefix` will be prepended to `RDataPath` for
#'     the full path to the resource. If the resource is stored in 
#'     Bioconductor's AWS S3 buckets, it should start with the name of the 
#'     package associated with the metadata and should not start with a leading 
#'     slash. It should include the resource file name. For strongly associated 
#'     files, like a bam file and its index file, the two files should be 
#'     seperates with a colon `:`. This will link a single hub id with multiple
#'     files.
#'
#' @param Tags `character()` Zero or more tags describing the data, colon 
#'     `:` separated.
#'
#' @return None
#' @examples
#' hub_metadata()
#'
#' tst <- hub_metadata(
#'     Title = "ENCODE",
#'     Description = "a test entry",
#'     BiocVersion = package_version("3.9"),
#'     Genome = NA_character_,
#'     SourceType = "JSON",
#'     SourceUrl = "https://www.encodeproject.org",
#'     SourceVersion = package_version("0.0"),
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
#' @export
hub_metadata <- function(
    Title = character(1),
    Description = character(1),
    BiocVersion = package_version("0.0"),
    Genome = character(1),
    SourceType = character(1),
    SourceUrl = character(1),
    SourceVersion = character(1),
    Species = character(1),
    TaxonomyId = integer(1),
    Coordinate_1_based = NA,
    DataProvider = character(1),
    Maintainer = character(1),
    RDataClass = character(1),
    DispatchClass = character(1),
    Location_Prefix = character(1),
    RDataPath = character(1),
    Tags = character())
{
    BiocVersion <- package_version(BiocVersion)
    SourceVersion <- tryCatch({
        as.character(SourceVersion)
    }, error = function(err) {
        stop("'SourceVersion' cannot be coerced to character(1)")
    })
    metadata <- list(
        Title = Title,
        Description = Description,
        BiocVersion = BiocVersion,
        Genome = Genome,
        SourceType = SourceType,
        SourceUrl = SourceUrl,
        SourceVersion = SourceVersion,
        Species = Species,
        TaxonomyId = TaxonomyId,
        Coordinate_1_based = Coordinate_1_based,
        DataProvider = DataProvider,
        Maintainer = Maintainer,
        RDataClass = RDataClass,
        DispatchClass = DispatchClass,
        Location_Prefix = Location_Prefix,
        RDataPath = RDataPath,
        Tags = Tags
    )
    .metadata_validate(metadata)
    metadata
}

## column classes for read.csv
.metadata_colclasses <-
    function()
{
    classes <- lapply(.METADATA, class)
    classes$BiocVersion <- "character"

    unlist(classes)
}        

#' @importFrom utils read.csv write.csv
.import_metadata <-
    function(path)
{
    metadata <- read.csv(path, colClasses = .metadata_colclasses())

    metadata$BiocVersion <- package_version(metadata$BiocVersion)
    metadata$Tags <- strsplit(metadata$Tags, ",[ ]*")

    metadata
}

.export_metadata <-
    function(metadata, path)
{
    metadata$BiocVersion <- as.character(metadata$BiocVersion)
    metadata$Tags <- vapply(
        metadata$Tags, paste, character(1), collapse = ", "
    )
    write.csv(metadata, file = path, row.names = FALSE)
}
