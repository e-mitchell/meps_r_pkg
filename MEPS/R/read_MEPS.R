#' Read MEPS public use files and import into R as data frame
#'
#' This function reads in MEPS public use files (PUFs) in .ssp (for 1996-2017 PUFs) or .dat (for 2018 and later PUFs) format, either from a local directory or the MEPS website, and imports them into R as a data frame. Larger files (e.g. full-year-consolidated files) can take several seconds to load. Either standardized file name or both year and file type must be specified.
#'
#' @param file name of public use file. Must be in standard format (e.g. 'h160g'). Can use the get_puf_names() function to look up file name by year and type (requires internet connection).
#'
#'
#' @param year (required if 'file' is missing) data year, between 1996 and most current file release.
#' @param type (required if 'file' is missing) file type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRPL' (Person-Round-Plan), 'PMED' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), and 'RXLK' (PMED - events link file)
#' @param dir (optional) local directory containing .ssp or .dat files. If left blank, files will be downloaded from MEPS website(requires internet connection).
#'
#' @return MEPS data as a data frame.
#' @export
#'
#' @examples
#' ## Download MEPS 2015 outpatient file from MEPS website
#'
#' # Use file name directly
#' OP2015 <- read_MEPS(file = 'h178f')
#'
#' # Use year and file type
#' OP2015 <- read_MEPS(year = 2015, type = 'OP')
#'
#' ## Download MEPS 2015 outpatient file from local directory
#'
#' OP2015 <- read_MEPS(year = 2015, type = 'OP', dir = 'C:/MEPS')



read_MEPS <- function(file, year, type, dir, web) {

  # QC checks on var inputs ---------------------------------------------------

  # warn if using 'web' option explicitly -- deprecated
  if(!missing(web)) {
    warning("'web' argument is deprecated. Use 'dir' to specify local directory containing .ssp / .dat files, or leave blank to download from internet.")
  }

  # Check that either file or year and type are specified
  if (missing(file) & (missing(year) | missing(type)))
    stop("Must specify either file name or both year and type.")

  # If file and year and type are all specified, note that file is used
  if(!missing(file) & !(missing(year) & missing(type)))
    warning("Both file name and year or type have been specified. Using file name.")

  # Set fname and extension (ssp or dat) --------------------------------------

  if(!missing(file)) {

    file <- tolower(file) # force lower case

    file_split <- str_split(file, "\\.")[[1]]

    file <- file_split[1]
    ext  <- file_split[2] # Will be NA if missing

    fname_local <- fname_web <- file

    # Setting year based on file name (over-writing specified year)
    # If file is specified, but 'get_puf_names' hasn't been updated,
    # set year to max year in 'get_puf_names'
    pnames <- get_puf_names()
    if(file %in% unlist(pnames)) {
      year_row <- which(pnames == file, arr.ind = T)[1,1]
      year <- pnames$YEAR[year_row]
    } else {
      year <- max(pnames$YEAR)
    }

    # Check specified file extension (if given)
    if(!is.na(ext)) {

      if(ext == "ssp" & year >= 2018)
        stop("SAS transport files (.ssp) files are not compatible with R for PUFs from 2018 and later. Use the ASCII file format (.dat) instead.")

      if(ext == "dat" & year < 2018)
        stop("ASCII files (.dat) are not recommended for 1996-2017 PUFs. Please use SAS transport (.ssp) instead.")

      if(!ext %in% c("ssp", "dat"))
        stop("File extension not recognized.")

    }

  } else {
    fname_local <- get_puf_names(year = year, type = type, web = F) %>% as.character
    fname_web   <- get_puf_names(year = year, type = type, web = T) %>% as.character
  }

  # Set extension (override if specified)
  dat_file <- (year >= 2018 | fname_local == "h201")
  ext <- ifelse(dat_file, 'dat', 'ssp') # override specified extension


  # Download file from web if needed ------------------------------------------

  web <- missing(dir)

  local_path <- paste0(fname_local, ".", ext)

  if(!web) {
    if (!(local_path %in% tolower(list.files(dir)))) {
      warning(sprintf("%s.%s not found in specified directory. Downloading from MEPS website instead.", fname_local, ext))
      web = T
    }
  }

  if(web) {
    # download from website and save to temporary folder
    meps_file <- dl_meps(fname_web, ext)

  } else {
    # set local path
    meps_file <- sprintf("%s/%s", dir, local_path)
  }


  # Read into R ---------------------------------------------------------------

  # ASCII file
  if(ext == "dat") {

    foldername <- fname_local %>% gsub("f[0-9]+","",.)

    url <- sprintf("https://meps.ahrq.gov/data_stats/download_data/pufs/%s/%sru.txt",
                   foldername, fname_local)

    # Load using R programming statements if available
    if(!http_error(url)) {

      # Set as global, so meps_path won't be overwritten by R programming statements
      meps_path <<- meps_file
      source(url)
      meps_dat <- get(fname_local)

    } else {

    # Otherwise, scrape ASCII info from Stata files
      dat_info <- get_ascii_info(fname_local)
      meps_dat <- read_fwf(
        meps_file,
        col_positions =
          fwf_positions(
            start = dat_info[["start"]],
            end   = dat_info[["end"]],
            col_names = dat_info[["names"]]),
        col_types = dat_info[["types"]],
        na=c("","NA","."))
    }

  }

  if(ext == "ssp") {
    meps_dat <- read.xport(meps_file)
  }

  # Return --------------------------------------------------------------------

  return(meps_dat)

}







