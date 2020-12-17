#' A function that publishes resource to the hub S3 bucket
#'
#' This function uses functionality from the aws.s3 package to put files or 
#' directorieso on the Bioconductor's test hub S3 bucket. The user should 
#' have already contacted the hubs maintainers at hubs@bioconductor.org to get 
#' the necessary credentials to access the bucket. These credentials should be 
#' delcared in the system environment prior to running this function.
#'
#' @param path A `character(1)` path to the file or directory to be added to 
#'     the bucket.
#' @param object A `character(1)` to indicate how the file should be named on 
#'     the bucket.
#'
#' @importFrom aws.s3 put_object put_folder
#' @importFrom fs is_file
#'
#' @export
#'
#' @examples
#' fl <- tempdir()
#'
publish_resource <- function(path, object)
{
    vars <- c("AWS_DEFAULT_OUTPUT", "AWS_DEFAULT_REGION", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
    tryCatch(stopifnot(all(nzchar(Sys.getenv(vars)))),
        error = stop("Not all system environments are set, do so and rerun function.")
    )

    if (is_file(path))
        put_object(path, object, bucket = "AnnotationContributor", acl = "public-read") 
    else
        put_folder(path, bucket = "AnnotationContributor") 
}
