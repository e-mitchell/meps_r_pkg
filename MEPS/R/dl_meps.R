#' Download public use file from MEPS website
#'
#' This function downloads MEPS public use files (PUFs) from the MEPS website,
#' and saves them to a local directory (if specified)
#'
#' @param fname Name of public use file. Must be in standard format (e.g.
#'   'h160g'). Can use the get_puf_names() function to look up file name by year
#'   and type (requires internet connection).
#'
#' @param ext Extension of file type. Options include "dta" (Stata data), "v9" (SAS V9 dataset), "xlsx" (Excel), "ssp" (SAS transport), and "dat" (ASCII). dta, v9, and xlsx are only available for data years 2018 and later.
#'
#' @param dir (optional) Name of directory for file to be stored. If blank, will download to temporary folder
#'
#' @return Location and name of downloaded data file
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' ## Download MEPS 2019 Dental visits file from MEPS website
#'
#' # Stata format (.dta) in "C:/MEPS"
#' meps_file <- dl_meps(fname = "h213b", ext = "dta", dir = "C:/MEPS")
#' haven::read_dta(meps_file) %>% head
#'
#' # SAS V9 file in temporary directory
#' meps_file2 <- dl_meps(fname = "h213b", ext = "v9")
#' haven::read_sas(meps_file2) %>% head



dl_meps <- function(fname, ext = "dta", dir = tempdir()) {

  base_url <- "https://meps.ahrq.gov/mepsweb/data_files/pufs"

  # Remove 'f1' and 'f2' from foldername (relevant to CLNK/RXLK files)
  foldername <- fname %>%
    gsub("f1", "", .) %>%
    gsub("f2", "", .)

  url1 <- stringr::str_glue("{base_url}/{fname}{ext}.zip")
  if(!httr::http_error(url1)) url <- url1

  # For some xlsx, Stata, and SASV9 files, zip is stored under 'foldername'
  url2 <- stringr::str_glue("{base_url}/{foldername}/{fname}{ext}.zip")
  if(!httr::http_error(url2)) url <- url2

  ext <- gsub(".", "", ext, fixed = T) %>% tolower
  ext <- replace(ext, ext == "sas", "v9")

  if(httr::http_error(url))
    stop(stringr::str_glue("Cannot open URL 'url': File not found"))

  utils::download.file(url, temp <- tempfile())
  uz <- utils::unzip(temp, exdir = dir)
  unlink(temp)
  return(uz)
}

