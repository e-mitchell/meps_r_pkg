#' Import MEPS public use files into R as data frame or tibble
#'
#' This function reads in MEPS public use files (PUFs) from the MEPS website,
#' and imports them into R. Larger files (e.g.full-year-consolidated files) can
#' take several seconds to load. Internet connection required.
#'
#' @param file name of public use file. Must be in standard format (e.g.
#'   'h160g'). If 'file' is specified, it overrides any additional parameters.
#'
#' @param ... other parameters passed to the get_puf_names() function. Must
#' specify one of the following (see examples below): \cr \cr

#' \itemize{
#'   \item 'year' and 'type' (for most annual files)
#'   \item 'panel' with 'type' = "LONG" (for Longitudinal files)
#'   \item 'type' only if type = BRR or PL
#'  }
#'
#'
#' @return MEPS data as a data frame or tibble.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' # Use file name
#' OP2020 <- read_MEPS(file = "h220f")
#'
#' # Use year and file type
#' OP2020 <- read_MEPS(year = 2020, type = "OP")
#'
#' # Get Longitudinal file
#' LONG <- read_MEPS(type = "LONG", panel = 23)
#'
#' # Get 4-year Longitudinal file (available for Panels 23 and 24)
#' LONG_4yr <- read_MEPS(type = "LONG", panel = 23, long_type = "4-year")
#'
#' # Special cases: BRR file and Pooled Linkage
#' BRR <- read_MEPS(type = "BRR")
#' PL  <- read_MEPS(type = "PL")



read_MEPS <- function(file,...) {

  toomany_error = "Too many files requested. Only 1 file can be imported at a time."


  # QC length of var inputs ---------------------------------------------------

  if(!missing(file)) {
    if(length(file) > 1)
      stop(toomany_error)
  }


  # Set fname and remove extension if specified -------------------------------

  if(missing(file)) {

    fname_web <- get_puf_names(...)

    if(length(unlist(fname_web)) > 1)
      stop(toomany_error)

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



