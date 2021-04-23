context("add_resource.R")

test_that("`add_resource()` works",
{
    fl <- tempdir()
    create_pkg(paste0(fl, "/tstResourcePkg"), "AnnotationHub")

    meta_path <- file.path(fl, "tstResourcePkg", "inst", "extdata", "metadata.csv")
    meta <- read.csv(meta_path)
    expect_identical(dim(meta), c(0L, 17L))
    expect_identical(colnames(meta)[1], "Title")

    metadata <- metadata(
        Title = "ENCODE",
        Description = "a test entry",
        BiocVersion = "4.1",
        Genome = NA_character_,
        SourceType = "JSON",
        SourceUrl = "https://www.encodeproject.org",
        SourceVersion = "x.y.z",
        Species = NA_character_,
        TaxonomyId = NA_integer_,
        Coordinate_1_based = NA,
        DataProvider = "ENCODE Project",
        Maintainer = "tst person <tst@email.com>",
        RDataClass = "data.table",
        DispatchClass = "Rda",
        Location_Prefix = "s3://annotationhub/",
        RDataPath = "ENCODExplorerData/encode_df_lite.rda",
        Tags = "ENCODE:Homo sapiens"
    )
    add_resource(paste0(fl, "/tstResourcePkg"), metadata)
    meta2 <- read.csv(meta_path)
    expect_identical(dim(meta2), c(1L, 17L))
    expect_identical(meta2[[1]], "ENCODE")
    expect_true(is.na(meta2[[4]]))

    expect_error(add_resource())
    expect_error(add_resource("metadata"))
})
