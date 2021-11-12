context("metadata validity")

test_that("metadata is valid",
{
    if(!requireNamespace("{{{type}}}Data", quietly = TRUE))
        BiocManager::install("{{{type}}}Data")

    path <- find.package("{{{packge}}}")
    metadata <- system.file("extdata", "metadata.csv", package = "{{{package}}}")
    expect_true({{{type}}}Data::make{{{type}}}Metadata(path, metadata))
})
