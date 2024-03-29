#' Get MEPS Public Use File Names
#'
#' This is a lookup function that returns a single requested file name or list of names for specified MEPS data file. If no arguments are given, the full list of annual MEPS Public-Use-Files will be returned.
#' @param year (optional) Data year, between 1996 and most current PUF release. If omitted, files from all years will be returned. For Longitudinal files, specify panel instead of year.
#' @param panel (optional) Panel of study. Valid only for Longitudinal files.
#' @param long_type (optional) Type of Longitudinal file. Default is "2-year" file. Panels 23 and 24 were extended to 4 years of data collection, so these panels have the option to specify "3-year" or "4-year" files as well.
#' @param type (optional) Type of desired MEPS file. Options are: \cr \cr
#' Main files:
#' \itemize{
#'   \item "PIT" (Point-in-time file)
#'   \item "FYC" (Full-year consolidated file)
#'   \item "COND", "Conditions" (Medical conditions file)
#'   \item "Jobs" (Jobs file)
#'   \item "PRPL" (Person-round-plan file)
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
#'   \item "LONG", "Longitudinal"
#' }
#'
#' @param web if TRUE, returns names of .zip files from web, otherwise, returns names of .ssp files after download
#' @export
#' @examples
#' ## Get file name for full-year-consolidated (FYC) file from 2018
#' get_puf_names(2018,'FYC')
#'
#' ## Get file names for all PUFs in 2020
#' get_puf_names(2020)
#'
#' ## Get file names for PMED event files, all years
#' get_puf_names(type = 'PMED')
#'
#' ## Get Longitudinal file names for all Panels
#' get_puf_names(type = "LONG")
#'
#' ## Return all files, all years
#' get_puf_names()

get_puf_names <- function(year, type, panel, long_type = "2-year", web = T) {

    puf_names = load_puf_names_file()


    # If special case (BRR, Pooled Linkage, or LONG), return ------------------

    if(!missing(type)) {
      max_year = max(puf_names$Year)
      TYPE = toupper(type)

      if("BRR" %in% TYPE) {

        if(length(TYPE) > 1){
          stop("Multiple 'Types' not allowed with 'BRR' file.")
        } else {
          return(get_brr_name(max_year))
        }

      }

      if(any(TYPE %in% c("POOLED LINKAGE", "PL", "POOLED VARIANCE"))) {
        if(length(TYPE) > 1){
          stop("Multiple 'Types' not allowed with 'Pooled Linkage' file.")
        } else {
          return(get_pl_name(max_year))
        }
      }

      # Need to specify "Panel", not "Year", in get_long_name
      if(any(TYPE %in% c("LONG", "LONGITUDINAL"))) {

        if(length(TYPE) > 1){

          stop("Multiple 'Types' not allowed with 'Pooled Linkage' file.")

        } else if(!missing(year)) {

          stop("For Longitudinal files, specify Panel number instead of Year.")

        } else {

          return(get_long_name(panel, long_type))

        }
      }

    } # end if(!missing(type))




    # Expand event file names -------------------------------------------------

    meps_names <- puf_names %>%
      dplyr::rename(PMED = PMED.Events)

    event_letters <- list(
      Dental         = "b",
      Other_Medical  = "c",
      Inpatient      = "d",
      Emergency_Room = "e",
      Outpatient     = "f",
      Office_Based   = "g",
      Home_Health    = "h")

    for(evnt in names(event_letters)){
      letter = event_letters[[evnt]]
      value = meps_names$Events %>% gsub("\\*",letter,.)
      meps_names[,evnt] = value
    }

    meps_names <- meps_names %>% dplyr::select(-Events)
    cols <- meps_names %>%
      dplyr::select(-Year, -dplyr::matches("Panel")) %>%
      colnames

    # Force colnames to be uppercase (to match toupper(type))
    colnames(meps_names) <- toupper(colnames(meps_names))


    # Add alternate file abbreviations, when 'type' is called -----------------
    meps_names_expanded <- meps_names %>%
      dplyr::mutate(
        RX = PMED,
        DV = DENTAL,
        DN = DENTAL,
        OM = OTHER_MEDICAL,
        IP = INPATIENT,
        HS = INPATIENT,
        ER = EMERGENCY_ROOM,
        OP = OUTPATIENT,
        OB = OFFICE_BASED,
        MV = OFFICE_BASED,
        HH = HOME_HEALTH,
        PRP = PRPL,
        COND = CONDITIONS,
        CONDITION = CONDITIONS
      )

    allowed_types <- meps_names_expanded %>%
      dplyr::select(-YEAR, -PANELS) %>%
      colnames


    # Check for data input errors ---------------------------------------------

    if (!missing(type)) {

      # Force type to be uppercase to match colnames
      type = toupper(type)

      if (any(!type %in% allowed_types)) {
        stop(sprintf(
          "Type must be one of the following: %s",
          paste(allowed_types, collapse = ", ")))
      }
    }

    if (!missing(year)) {
        if (any(!year %in% meps_names$YEAR))
            stop(sprintf("Year must be a number between %s and %s", min(meps_names$YEAR), max(meps_names$YEAR)))
    }


    # Return MEPS names based on specified, year, type ------------------------

    # - Print note that "Special" types not shown (Longitudinal, BRR, PooledLinkage)

    out_message <- "The Longitudinal, Balanced Repeated Replicates (BRR), and Pooled Linkage files are not shown. To get these file names, use the 'type = ' option (e.g., 'type = \"LONG\"')"

    if (missing(year) & missing(type)) {

      # If year and type are missing, output entire matrix of file names
      out <- meps_names
      message(out_message)

    } else if (missing(year) & !missing(type)) {

      # If only type is specified, output all years for that type
      out <- meps_names_expanded %>%
        dplyr::select(YEAR, dplyr::all_of(type))


    } else if (missing(type) & !missing(year)) {

      # If only year is specified, output all types for that year
      out <- meps_names %>%
        dplyr::filter(YEAR %in% year) %>%
        dplyr::select(-dplyr::matches("Panel"))

      message(out_message)

    } else {

      # If year and type are specified
      out <- meps_names_expanded %>%
        dplyr::filter(YEAR %in% year) %>%
        dplyr::select(YEAR, dplyr::all_of(type))

      # Remove 'YEAR' column if only 1 year and 1 type is specified
      if(length(year) == 1 & length(type) == 1) {
        out <- out %>% dplyr::select(-YEAR)
      }

    }

    if (web)
        return(out)

    # Convert from download names (in meps_names) to .ssp file names ------------

    meps_mat <- as.matrix(out)

    hc_list <- c(
      "h10a", "h10if1", "h10if2", "h26bf1", "h19",
      sprintf("h16%sf1", letters[2:8]), sprintf("h10%sf1", letters[2:8]))

    meps_mat[meps_mat %in% hc_list] <- sub("h", "hc", meps_mat[meps_mat %in% hc_list])

    meps_mat[meps_mat == "h05"] = "hc005xf"
    meps_mat[meps_mat == "h06r"] = "hc006r"
    meps_mat[meps_mat == "h07"] = "hc007"
    meps_mat[meps_mat == "h09"] = "hc009xf"
    meps_mat[meps_mat == "h13"] = "hc013xf"

    out <- as.data.frame(meps_mat, stringsAsFactors = F)

    return(out)
}




# HELPER FUNCTIONS ------------------------------------------------------------

# Load PUF names master file from GitHub (if available), or cached file
load_puf_names_file <- function() {

  # If no internet connection, use cached file
  if(!curl::has_internet()) {
    return(puf_names_cached)
  }

  # Try to load latest PUF names from GitHub
  meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

  puf_names_current <- try(utils::read.csv(meps_file, stringsAsFactors = F), silent = T)

  # If connection to GitHub isn't working (e.g. firewall won't let you connect)
  #  then use local cached data
  if(class(puf_names_current) == "try-error") {
    warning("Loading cached PUF names instead.")
    return(puf_names_cached)
  }

  puf_names <- puf_names_current %>%
    dplyr::mutate(Year = suppressWarnings(as.numeric(Year))) %>%
    dplyr::filter(!is.na(Year))

  return(puf_names)

}

# For special file types (BRR, Pooled linkage, Longitudinal) ------------------

load_long_names_file <- function() {

  # If no internet connection, use cached file
  if(!curl::has_internet()) {
    return(long_names_cached)
  }

  # Try to load latest PUF names from GitHub
  meps_long_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_longitudinal_file_names.csv"

  long_names_current <- try(utils::read.csv(meps_long_file, stringsAsFactors = F), silent = T)

  # If connection to GitHub isn't working (e.g. firewall won't let you connect)
  #  then use local cached data
  if(class(long_names_current) == "try-error") {
    warning("Loading cached PUF names instead.")
    return(long_names_cached)
  }

  long_names <- long_names_current %>%
    dplyr::mutate(Panel = suppressWarnings(as.numeric(Panel))) %>%
    dplyr::filter(!is.na(Panel))

  return(long_names)
}


get_long_name <- function(panel, long_type = "2-year") {


  meps_names = load_long_names_file()

  if(missing(panel)) {
    return(meps_names)
  }


  # Display message about 2-year files if using panel 23 or 24
  if(any(panel %in% c(23, 24))) {

    message("Returning 2-year Longitudinal file. For 3-year or 4-year files, specify option 'long_type = \"3-year\" or \"4-year\" ")

  }

  if(long_type %in% c("3-year", "4-year") & !any(panel %in% c(23, 24))) {
    stop("3-year and 4-year longitudinal files are only available for Panels 23 or 24.")
  }


  meps_names %>%
    dplyr::filter(Panel %in% panel) %>%
    dplyr::filter(Number_of_Years == long_type) %>%
    dplyr::select(File_Name) %>%
    dplyr::rename(Longitudinal = File_Name)
}



get_brr_name <- function(max_year = 2099) {
  year <- max_year

  while(year > 2015) { # we know the year is after 2015

    yr = substr(year, 3, 4)

    # Check if SAS programming statements exist, to get name of BRR file
    chk_url <- stringr::str_glue("https://meps.ahrq.gov/data_stats/download_data/pufs/h036brr/h36brr{yr}su.txt")

    if(!httr::http_error(chk_url)) {
      return(stringr::str_glue("h36brr{yr}"))
    } else {
      year = year - 1
    }

  }

}


get_pl_name <- function(max_year = 2099) {
  year <- max_year

  while(year > 2015) { # we know the year is after 2015

    yr = substr(year, 3, 4)

    # Check if SAS programming statements exist, to get name of pooled linkage file
    chk_url <- stringr::str_glue("https://meps.ahrq.gov/data_stats/download_data/pufs/h036/h36u{yr}su.txt")

    if(!httr::http_error(chk_url)) {
      return(stringr::str_glue("h36u{yr}"))
    } else {
      year = year - 1
    }

  }
}








