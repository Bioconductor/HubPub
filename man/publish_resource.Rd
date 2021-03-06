% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish_resource.R
\name{publish_resource}
\alias{publish_resource}
\title{A function that publishes resource to the hub S3 bucket}
\usage{
publish_resource(path, object, dry.run = TRUE)
}
\arguments{
\item{path}{A \code{character(1)} path to the file or the name of the directory
to be added to the bucket. If adding a directory, be sure there are no
nested directories and only files within it.}

\item{object}{A \code{character(1)} to indicate how the file should be named on
the bucket.}

\item{dry.run}{A boolean to indicate if the resource should in fact be
published. The defalut is TRUE, meaning the resource won't be published.}
}
\value{
None
}
\description{
This function uses functionality from the aws.s3 package to put files or
directories on the Bioconductor's test hub S3 bucket. The user should have
already contacted the hubs maintainers at hubs@bioconductor.org to get
the necessary credentials to access the bucket. These credentials should be
delcared in the system environment prior to running this function.
}
\examples{
pkgdir <- tempfile()
fl1 <- file.path(pkgdir, "mtcars1.csv")
dir.create(dirname(fl1), recursive = TRUE)
write.csv(mtcars, file = file.path(fl1))
fl2 <- file.path(pkgdir, "mtcars2.csv")
write.csv(mtcars, file = file.path(fl2))
publish_resource(pkgdir, "test_dir")

fl3 <- file.path(pkgdir, "mtcars3.csv")
write.csv(mtcars, file = file.path(fl3))
publish_resource(fl3, "test_dir")
}
