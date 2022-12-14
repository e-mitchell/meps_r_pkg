#' Import MEPS public use files into R as data frame or tibble
#'
#' This function reads in MEPS public use files (PUFs) from the MEPS website,
#' and imports them into R. Larger files (e.g.full-year-consolidated files) can
#' take several seconds to load. Either standardized file name (e.g. 'h209') or
#' both year and file type must be specified. Internet connection required.
#'
#' @param file name of public use file. Must be in standard format (e.g.
#'   'h160g'). Can use the get_puf_names() function to look up file name by year
#'   and type.
#'
#' @param year (required if 'file' is missing, except when type = "BRR" or
#' "Pooled Linkage") data year, between 1996 and most
#'   current file release. Ignored if type = "BRR" or "Pooled Linkage".
#'
#' @param type (required if 'file' is missing) file type of desired MEPS file.Options are: \cr \cr
#' Main files:
#' \itemize{
#'   \item "PIT" (Point-in-time file)
#'   \item "FYC" (Full-year consolidated file)
#'   \item "COND", "Conditions" (Medical conditions file)
#'   \item "Jobs" (Jobs file)
#'   \item "PRPL" (Person-round-plan file)
#'   \item "LONG", "Longitudinal"
#'  } \cr
#'
#'  Event files:
#'  \itemize{
#'   \item "RX", "PMED" (Prescribed medicines)
#'   \item "Dental", "DV", "DN" (Dental visits)
#'   \item "Other_Medical", "OM" (Other medical expenses)
#'   \item "Inpatient", "IP","HS" (Hospital inpatient stays)
#'   \item "Emergency_Room", "ER" (Emergency room visits)
#'   \item "Outpatient", "OP" (Outpatient visits)
#'   \item "Office_based", "OB", "MV (Office-based medical provider visits)
#'   \item "Home_Health", "HH" (Home health)
#'   \item "CLNK" (Condition-event linkage file)
#'   \item "RXLK" (PMED-event linkage file)
#' } \cr
#'
#' Other files:
#' \itemize{
#'   \item "Multum" (Multum Lexicon addendum files, 1996-2013)
#'   \item "PSAQ" (Preventative care SAQ, 2014)
#'   \item "MOS" (Medical Organizations Survey, 2015-2016)
#'   \item "FS" (Food Security file, 2016-2017)
#'   \item "BRR" (Balanced Repeated Replicates file)
#'   \item "PL", "Pooled Linkage" (Pooled Linkage file for common variance)
#' }
#'
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


  # QC length of var inputs ---------------------------------------------------

  if(!missing(file)) {
    if(length(file) > 1)
      stop("Too many files requested. Only 1 file can be imported at a time.")
  }

  if(!missing(year)) {
    if(length(year) > 1)
      stop("Too many years requested. Only 1 year can be imported at a time.")
  }

  if(!missing(type)) {
    if(length(type) > 1)
      stop("Too many files requested. Only 1 file can be imported at a time.")
  }


  # Check if special case (BRR or Pooled Linkage) -----------------------------

  special_type = F

  if(!missing(type)) {
    special_type = (toupper(type) %in%
    c("BRR", "POOLED LINKAGE", "PL", "POOLED VARIANCE"))
  }

  # QC checks on var inputs ---------------------------------------------------

  if(!special_type) {

    # ERROR: Check that either file or year and type are specified
    if (missing(file) & (missing(year) | missing(type)))
      stop("Must specify either file name or both year and type.")

    # WARN: If file and year and type are all specified, note that file is used
    if(!missing(file) & !(missing(year) & missing(type)))
      warning("Both file name and year or type have been specified. Using file name.")

  } else {

    # For special types (BRR, Pooled linkage, year is ignored)
    if(!missing(year))
      warning("Year is ignored for BRR and Pooled Linkage files.")

  }

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



