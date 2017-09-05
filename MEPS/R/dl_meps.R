#' Download public use files from MEPS website
#' 
#' Helper function to download public use file from MEPS website. Can save .ssp file to a permanent directory.
#'
#' @param fname name of public use file (standard format)
#' @param dir name of directory to store .ssp file. Defaults to tempdir() if none specified
#'
#' @return full path of data .ssp for import via read.xport

dl_meps <- function(fname, dir = tempdir()) {
    url <- sprintf("https://meps.ahrq.gov/mepsweb/data_files/pufs/%sssp.zip", fname)
    download.file(url, temp <- tempfile())
    uz <- unzip(temp, exdir = dir)
    unlink(temp)
    return(uz)
}
