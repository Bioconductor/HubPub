#' A function that publishes resource to the hub S3 bucket
#'
#' This function uses functionality from the aws.s3 package to put files or 
#' directories on the Bioconductor's test hub S3 bucket. The user should have 
#' already contacted the hubs maintainers at hubs@bioconductor.org to get 
#' the necessary credentials to access the bucket. These credentials should be 
#' delcared in the system environment prior to running this function.
#'
#' @param path A `character(1)` path to the file or the name of the directory 
#'     to be added to the bucket. If adding a directory, be sure there are no 
#'     nested directories and only files within it.
#' @param object A `character(1)` to indicate how the file should be named on 
#'     the bucket.
#' @param dry.run A boolean to indicate if the resource should in fact be 
#'     published. The defalut is TRUE, meaning the resource won't be published.
#'
#' @importFrom aws.s3 put_object put_folder
#' @importFrom fs is_file
#' @importFrom utils write.csv
#'
#' @return None 
#'
#' @export
#'
#' @examples
#' pkgdir <- tempfile()
#' fl1 <- file.path(pkgdir, "mtcars1.csv")
#' dir.create(dirname(fl1), recursive = TRUE)
#' write.csv(mtcars, file = file.path(fl1))
#' fl2 <- file.path(pkgdir, "mtcars2.csv")
#' write.csv(mtcars, file = file.path(fl2))
#' publish_resource(pkgdir, "test_dir")
#'
#' fl3 <- file.path(pkgdir, "mtcars3.csv")
#' write.csv(mtcars, file = file.path(fl3))
#' publish_resource(fl3, "test_dir")
publish_resource <- function(path, object, dry.run = TRUE)
{
    vars <- c("AWS_DEFAULT_OUTPUT", "AWS_DEFAULT_REGION", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
    if (!all(nzchar(Sys.getenv(vars)))) {
        warning("Not all system environment variables are set, do so and rerun function.")
        dry.run = TRUE
    }
    
    if (!is_file(path))
        objects <- list.files(path, full.names = TRUE)
    else 
        objects <- path
    sapply(objects, function(f) {
        object = paste0(object, "/", basename(f))
        if (dry.run) {
            to <- paste0("s3://annotation-contributor/", object)
            message("copy '",f,"' to '", to,"'")
        } else {
            put_object(
                file = f,
                object = object,
                bucket = "annotation-contributor",
                acl = "public-read"
            )
        }
    })
}
