context("metadata validity")

test_that("metadata is valid",
{
    metadata <- system.file("extdata", "metadata.csv", package = "{{{package}}}")
    expect_true(test{{{type}}}Metadata("{{{package}}}", metadata))
})
