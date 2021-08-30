#' Import MEPS public use files into R as data frame or tibble
#'
#' This function reads in MEPS public use files (PUFs) from the MEPS website, and imports them into R. Larger files (e.g.
#' full-year-consolidated files) can take several seconds to load. Either
#' standardized file name (e.g. 'h209') or both year and file type must be
#' specified.
#'
#' @param file name of public use file. Must be in standard format (e.g.
#'   'h160g'). Can use the get_puf_names() function to look up file name by year
#'   and type (requires internet connection).
#'
#' @param year (required if 'file' is missing) data year, between 1996 and most
#'   current file release.
#'
#' @param type (required if 'file' is missing) file type of desired MEPS file.
#'   Options are: 'PIT' (Point-in-time file); 'FYC' (Full-year consolidated);
#'   'Conditions' (Conditions file); 'Jobs' (Jobs file); 'PRPL'
#'   (Person-Round-Plan); 'PMED' (Prescription Medicines Events); 'DV' (Dental
#'   Visits); 'OM' (Other medical events); 'IP' or 'HS' (Hospital Inpatient
#'   Stays); 'ER' (Emergency Room Visits); 'OP' (Outpatient Visits); 'OB' or
#'   'MV' (Office-based medical visits); 'HH' (Home health); 'CLNK'
#'   (conditions-event link file); 'RXLK' (PMED - events link file);
#'
#' @param dir [deprecated]
#'
#' @return MEPS data as a data frame or tibble.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' ## Load MEPS 2015 outpatient file from MEPS website
#'
#' # Use file name
#' OP2015 <- read_MEPS(file = "h178f")
#'
#' # Use year and file type
#' OP2015 <- read_MEPS(year = 2015, type = "OP")
#'

read_MEPS <- function(file, year, type, dir) {

  # dir is now deprecated -----------------------------------------------------
  if(!missing(dir))
    stop("dir is deprecated. Files can only be read from MEPS website")

  # QC checks on var inputs ---------------------------------------------------

  # ERROR: Check that either file or year and type are specified
  if (missing(file) & (missing(year) | missing(type)))
    stop("Must specify either file name or both year and type.")

  # WARN: If file and year and type are all specified, note that file is used
  if(!missing(file) & !(missing(year) & missing(type)))
    warning("Both file name and year or type have been specified. Using file name.")


  # Set fname and remove extension if specified -------------------------------

  if(missing(file)) {

    fname_web <- get_puf_names(year = year, type = type, web = T) %>% as.character

  } else {

    file <- tolower(file) # force lower case

    file_split <- stringr::str_split(file, "\\.")[[1]]

    fname_web  <- file <- file_split[1]
  }

  # Load files from WEB -------------------------------------------------------
  # Try downloading files in this order:
  #  1. dta if available (2018 and later, mostly)
  #  2. ssp otherwise (1996-2017, mostly)

  # 1. Check for Stata (.dta) file first

  meps_file <- try(dl_meps(fname_web, ext = "dta"), silent = T)

  if(class(meps_file) != "try-error") {
    return(haven::read_dta(meps_file))
  }

  # 2. Else, use SAS transport file (.ssp)

  meps_file <- try(dl_meps(fname_web, ext = "ssp"), silent = T)
  return(foreign::read.xport(meps_file))

} # END read_MEPS




# HELPER FUNCTION --------------------------------------------------------------
# Read MEPS file into R, based on file type extension


read_MEPS_file <- function(dir, fname, ext) {

  if(ext == "dta")
    return(haven::read_dta(stringr::str_glue("{dir}/{fname}STATA.dta")))

  if(ext == "sas")
    return(haven::read_sas(stringr::str_glue("{dir}/{fname}.sas7bdat")))

  if(ext == "xlsx")
    return(readxl::read_excel(stringr::str_glue("{dir}/{fname}.xlsx")))

  if(ext == "ssp")
    return(foreign::read.xport(stringr::str_glue("{dir}/{fname}.ssp")))

  if(ext == "dat") {

    foldername <- fname %>% gsub("f[0-9]+","",.)

    url <- sprintf("https://meps.ahrq.gov/data_stats/download_data/pufs/%s/%sru.txt",
                   foldername, fname)

    meps_file <- stringr::str_glue("{dir}/{fname}.dat")

    # Load using R programming statements if available
    if(!httr::http_error(url)) {

      # Set as global, so meps_path won't be overwritten by R programming statements
      meps_path <<- meps_file
      source(url)
      meps_dat <- get(fname)

    } else {

      # Otherwise, scrape ASCII info from Stata files
      warning("No R programming statements found. Scraping Stata programming statements instead.")

      dat_info <- get_ascii_info(fname)
      meps_dat <- readr::read_fwf(
        meps_file,
        col_positions =
          readr::fwf_positions(
            start = dat_info[["start"]],
            end   = dat_info[["end"]],
            col_names = dat_info[["names"]]),
        col_types = dat_info[["types"]],
        na=c("","NA","."))
    }

    return(meps_dat)
  }

}



