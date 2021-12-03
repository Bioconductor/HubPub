#' @importFrom AzureStor storage_endpoint, storage_container
#' @noRd
.sas_credentials <- function(token)
{
    sas <- token
    url <- "https://bioconductorhubs.blob.core.windows.net"
    ep <- storage_endpoint(url, sas = sas)
    container <- storage_container(ep, "staginghub")

    creds <- list("token" = sas, "url" = url, "ep" = ep, "container" = container)
    return(creds)
}
#' Functions to publish resource(s) to Microsoft Azure Genomic Data Lake
#' 
#' @name publish_resource
#' @rdname publish_resource
#' 
#' These functions utilize the AzureStor package to upload local or remote data 
#' to the Bioconductor's temporary data lakes directory. The user should have 
#' already contacted the hubs maintainers at hubs@bioconductor.org to get the 
#' necessary SAS token or SAS URL to be able to upload data. 
#'
#' @param token A `character(1)` SAS token provided by the Bioconductor hubs 
#'     maintainers. 
#' @param path A `character(1)` path to the data to be uploaded.  
#' @param pkgName A `character(1)` name of the package the data will be 
#'     associated with.
#' @param dry.run A boolean to indicate if the resource should in fact be 
#'     published. The defalut is TRUE, meaning the resource won't be published.
#'
#' @importFrom AzureStor storage_multiupload
#'
#' @return None 
#'
#' @export
#'
#' @examples
publish_local_resource <- function(token, path, pkgName, dry.run = TRUE)
{
    creds <- .sas_credentials(token)

    files <- dir(path, recursive = TRUE)
    src <- dir(path, recursive = TRUE, full.names = TRUE)
    dest <- paste0(pkgName, "/", files)

    if (dry.run) {
        message("copy '", files,"' to '", creds$container,"'")
    } else {
        storage_multiupload(creds$container, src = src, dest = dest)     
    }
}

#' @rdname publish_resource
#' 
#' @param 
#' 
#' @importFrom httr GET, content
#' @importFrom AzureStor multicopy_url_to_storage
#' 
#' @return None
#' 
#' @export
#' 
#' @examples
publish_remote_resource <- function(token, remotePath, dry.run = TRUE)
{
    creds <- .sas_credentials(token)

    response <- GET(remotePath)

    src <- sapply(content(response)$tree, function(elt) elt$url)
    names <- sapply(content(response)$tree, function(elt) elt$path)

    keep <- grepl("^data/", names)
    src <- src[keep]
    names <- names[keep]

    dest <- paste0(pkgName, gsub("data/", "", names))

    if (dry.run) {
        message("copy '", names,"' to '", creds$container,"'")
    } else {
        multicopy_url_to_storage(creds$container, src = src, dest = dest, 
            max_concurrent_transfers = 3)
    }
}
