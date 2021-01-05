#' A function that publishes resource to the hub S3 bucket
#'
#' This function uses functionality from the aws.s3 package to put files or 
#' empty directories on the Bioconductor's test hub S3 bucket. The user should 
#' have already contacted the hubs maintainers at hubs@bioconductor.org to get 
#' the necessary credentials to access the bucket. These credentials should be 
#' delcared in the system environment prior to running this function.
#'
#' @param path A `character(1)` path to the file or the name of the empty 
#'     directory to be added to the bucket.
#' @param object A `character(1)` to indicate how the file should be named on 
#'     the bucket.
#'
#' @importFrom aws.s3 put_object put_folder
#' @importFrom fs is_file
#'
#' @export
#'
#' @examples
#' tmp_dir <- tempdir()
#' tmp_fl1 <- tempfile()
#' utils::write.csv(mtcars, file = tmp_fl1)
#' tmp_fl2 <- tempfile()
#' utils::write.csv(mtcars, file = tmp_fl2)
#' publish_resource(tmp_dir, "test_dir/")
#'
#' tmp_fl3 <- tempfile()
#' utils::write.csv(mtcars, file = tmp_fl3)
#' publish_resouce(tmp_fl3, "test_dir/temp_file3.csv")
publish_resource <- function(path, object)
{
    vars <- c("AWS_DEFAULT_OUTPUT", "AWS_DEFAULT_REGION", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
    if (!all(nzchar(Sys.getenv(vars))))
        stop("Not all system environments are set, do so and rerun function.")

    if (is_file(path))
        put_object(file = path, 
            object = object,
            bucket = "annotation-contributor",
            acl = "public-read"
        ) 
    else {
            files <- list.files(path, full.names = TRUE)
            sapply(files, function(f) {
                put_object(file = f,
                    object = paste0(object, basename(f)),
                    bucket = "annotation-contributor",
                    acl = "public-read"
                )
            })
    }
}
