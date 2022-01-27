#' Get MEPS Public Use File Names
#'
#' This is a lookup function that returns a single requested file name or list of names for specified MEPS data file. Internet access is required, since the function reads from the HHS-AHRQ GitHub page.
#' @param year (optional) Data year, between 1996 and most current PUF release. If omitted, files from all years will be returned
#' @param type (optional) Type of desired MEPS file. Options are: \cr \cr
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
#' }
#'
#' @param web if TRUE, returns names of .zip files from web, otherwise, returns names of .ssp files after download
#' @export
#' @examples
#' ## Get file name for full-year-consolidated (FYC) file from 2005
#' get_puf_names(2005,'FYC')
#'
#' ## Get file names for all PUFs in 2014
#' get_puf_names(2014)
#'
#' ## Get file names for PMED event files, all years
#' get_puf_names(type='PMED')
#'
#' ## Return all files, all years
#' get_puf_names()
#'
#' ## Compare names of .ssp files with those on website links
#' get_puf_names(year = 1996, type = 'DV')
#' get_puf_names(year = 1996, type = 'DV', web = FALSE)

get_puf_names <- function(year, type, web = T) {

    # Load latest PUF names from GitHub ---------------------------------------

    meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

    puf_names_current <- utils::read.csv(meps_file, stringsAsFactors = F)

    puf_names <- puf_names_current %>%
      dplyr::mutate(Year = suppressWarnings(as.numeric(Year))) %>%
      dplyr::filter(!is.na(Year))


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
      dplyr::select(-Year, -dplyr::ends_with("Panel")) %>%
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
        CONDITION = CONDITIONS,
        LONG = LONGITUDINAL
      )

    allowed_types <- meps_names_expanded %>%
      dplyr::select(-YEAR, -OLD.PANEL, -NEW.PANEL) %>%
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

    if (missing(year) & missing(type)) {
        out <- meps_names

    } else if (missing(year) & !missing(type)) {
        out <- meps_names_expanded %>%
          dplyr::select(YEAR, dplyr::all_of(type))

    } else if (missing(type) & !missing(year)) {
        out <- meps_names %>%
          dplyr::filter(YEAR %in% year) %>%
          dplyr::select(-dplyr::ends_with("Panel"))

    } else {
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
