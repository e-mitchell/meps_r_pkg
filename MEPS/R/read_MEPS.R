#' Import MEPS public use files into R as data frame or tibble
#'
#' This function reads in MEPS public use files (PUFs), either from a local
#' directory or the MEPS website, and imports them into R. Larger files (e.g.
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
#' @param dir (optional) local directory containing MEPS files (e.g. .ssp, .dat,
#'   .xlsx). If left blank, files will be downloaded from MEPS website(requires
#'   internet connection).
#'
#' @param ext (optional) specifies file type to be uploaded (e.g. dat, ssp,
#'   xlsx). If left blank, the recommended file type will be used (generally,
#'   'ssp' for pre-2018, 'dta' for 2018 and later)
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
#'
#' ## Load from local directory
#'
#' # Use file name (extension optional)
#' OP2015 <- read_MEPS(file = "h178f.ssp", dir = "C:/MEPS")
#'
#' # Use year and file type
#' OP2015 <- read_MEPS(year = 2015, type = "OP", dir = "C:/MEPS")



read_MEPS <- function(file, year, type, dir, ext = "best") {

  # QC checks on var inputs ---------------------------------------------------

  # ERROR: Check that either file or year and type are specified
  if (missing(file) & (missing(year) | missing(type)))
    stop("Must specify either file name or both year and type.")

  # WARN: If file and year and type are all specified, note that file is used
  if(!missing(file) & !(missing(year) & missing(type)))
    warning("Both file name and year or type have been specified. Using file name.")


  # Set fname and extension ---------------------------------------------------

  if(missing(file)) {

    fname_local <- get_puf_names(year = year, type = type, web = F) %>% as.character
    fname_web   <- get_puf_names(year = year, type = type, web = T) %>% as.character

  } else {

    file <- tolower(file) # force lower case

    file_split <- stringr::str_split(file, "\\.")[[1]]

    file      <- file_split[1]
    file_ext  <- file_split[2]

    # WARN: If 'file' has extension AND 'ext' is specified, use 'ext'
    if(!is.na(file_ext)) {
      if(ext == "best"){
        ext <- file_ext
      }else{
        warning("File extension has been specified in both the 'file' and 'ext' arguments. Using 'ext'.")
      }
    }

    fname_local <- fname_web <- file

  }

  # Standardize 'ext' format --------------------------------------------------
  ext <- gsub("\\.", "", ext) %>% tolower
  ext <- dplyr::case_when(
    ext %in% c("excel", "xls") ~ "xlsx",
    ext == "ascii" ~ "dat",
    ext == "stata" ~ "dta",
    ext == "sas7bdat" ~ "sas",
    ext == "v9" ~ "sas",
    TRUE ~ ext
  )

  # WARN: If 'ext' not allowed
  if(!ext %in% c("best", "dta", "sas", "xlsx", "dat", "ssp")) {
    ext <- "best"
    warning("Specified extension not allowed. Using best extension for requested file.")
  }

  # Load files from LOCAL folder or WEB (if 'dir' is missing) -----------------
  # Try downloading files in this order:
  #  1. dta
  #  2. sas
  #  3. xlsx
  #  4. ssp
  #  5. dat (need R/Stata programming statements -- web only)

  web <- missing(dir)


  # LOCAL load ----------------------------------------------------------------

  if(!web) {

    # If ext is specified, return requested file.ext
    if(ext != 'best')
      return(read_MEPS_file(dir, fname = fname_local, ext = ext))

    # if ext == best, look for preferred files first
    for(try_ext in c("dta", "sas", "xlsx", "ssp")) {

      meps_dat <- try(
        read_MEPS_file(
          dir,
          fname = fname_local,
          ext = try_ext),
        silent = T)

      if(class(meps_dat)[1] != "try-error")
        return(meps_dat)
    }

    stop(stringr::str_glue("No readable file found in {dir}"))

  } # END if(!web)


  # WEB load ------------------------------------------------------------------

  if(web) {

    # if ext is specified, return requested file.ext
    if(ext != 'best') {
      meps_file <- dl_meps(fname_web, ext)
      return(read_MEPS_file(dirname(meps_file), fname = fname_web, ext = ext))
    }

    # if ext == best, look for preferred files first
    for(try_ext in c("dta", "sas", "xlsx", "ssp", "dat")) {

      meps_file <- try(dl_meps(fname_web, try_ext), silent = T)
      if(class(meps_file) == "try-error") next

      meps_dat <- try(
        read_MEPS_file(
          dir = dirname(meps_file),
          fname = fname_web,
          ext = try_ext),
        silent = T)

      if(class(meps_dat)[1] != "try-error")
        return(meps_dat)
    }

    stop(stringr::str_glue("Specified MEPS file not found."))

  } # END if(web)

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



