context("create_pkg")

test_that("`create_pkg()` works",
{
    fl <- tempdir()
    create_pkg(paste0(fl, "/tstAHPkg"), "AnnotationHub")

    dsc <- read.dcf(file.path(fl, "tstAHPkg", "DESCRIPTION"))
    expect_identical(unname(dsc[,"Package"]), "tstAHPkg")
    expect_identical(unname(dsc[,"Version"]), "0.99.0")
    expect_identical(unname(dsc[,"biocViews"]), "AnnotationHub")
    expect_identical(unname(dsc[,"Imports"]), "AnnotationHub")

    sugs <- unname(dsc[,"Suggests"])
    sugs_split <- unlist(strsplit(sugs, "\n"))
    expect_identical(sugs_split[2], "AnnotationHubData")

    create_pkg(paste0(fl, "/tstEHPkg"), "ExperimentHub")

    dsc2 <- read.dcf(file.path(fl, "tstEHPkg", "DESCRIPTION"))
    expect_identical(unname(dsc2[,"Package"]), "tstEHPkg")
    expect_identical(unname(dsc2[,"Version"]), "0.99.0")
    expect_identical(unname(dsc2[,"biocViews"]), "ExperimentHub")
    expect_identical(unname(dsc2[,"Imports"]), "ExperimentHub")

    sugs2 <- unname(dsc2[,"Suggests"])
    sugs2_split <- unlist(strsplit(sugs2, "\n"))
    expect_identical(sugs2_split[2], "ExperimentHubData")

    expect_error(create_pkg())
    expect_error(create_pkg(paste0(fl, "/tstAHPkg")))
})
