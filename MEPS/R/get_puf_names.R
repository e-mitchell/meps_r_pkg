#' Get MEPS Public Use File Names
#'
#' This is a lookup function that returns a single requested file name or list of names for specified MEPS data file. Internet access is required, since the function reads from the HHS-AHRQ GitHub page.
#' @param year (optional) Data year, between 1996 and most current PUF release. If omitted, files from all years will be returned
#' @param type (optional) File type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRPL' (Person-Round-Plan), 'PMED' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), 'RXLK' (PMED - events link file), and 'PMED.Multum' (Multum Lexicon addendum files for 1996-2013)
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
#' get_puf_names(year = 1996, type = 'DV', web=F)

get_puf_names <- function(year, type, web = T) {

    # Load latest PUF names from GitHub ---------------------------------------

    meps_file = "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_file_names.csv"

    puf_names_current <- read.csv(meps_file, stringsAsFactors = F)

    puf_names <- puf_names_current %>%
      mutate(Year = suppressWarnings(as.numeric(Year))) %>%
      filter(!is.na(Year))

    # Expand event file names -------------------------------------------------

    meps_names <- puf_names %>%
      rename(PMED = PMED.Events) %>%
      mutate(RX = PMED)

    event_letters <- list(DV="b",OM="c",IP="d",ER="e",OP="f",OB="g",HH="h")

    for(evnt in names(event_letters)){
      letter = event_letters[[evnt]]
      value = meps_names$Events %>% gsub("\\*",letter,.)
      meps_names[,evnt] = value
    }

    meps_names <- meps_names %>% select(-Events)

    # Check for data input errors ---------------------------------------------

    if (!missing(type)) {

      # If type = RX / PRP, re-name to PMED / PRPL
        # if (type == "RX") {
        #   type <- "PMED"
        #   warning("Getting 'PMED' file instead")
        # }

        if (type == "PRP") {
          type <- "PRPL"
          warning("Getting 'PRPL' file instead")
        }


      if (!type %in% colnames(meps_names)) {
        cols <- meps_names %>% select(-Year, -ends_with("Panel")) %>% colnames
        stop(sprintf("Type must be one of the following: %s", paste(cols, collapse = ", ")))
      }
    }

    if (!missing(year)) {
        if (!year %in% meps_names$Year)
            stop(sprintf("Year must be between %s and %s", min(meps_names$Year), max(meps_names$Year)))
    }

    # Return MEPS names based on specified, year, type ------------------------

    if (missing(year) & missing(type)) {
        out <- meps_names

    } else if (missing(year) & !missing(type)) {
        out <- meps_names %>% select(Year, type)

    } else if (missing(type) & !missing(year)) {
        out <- meps_names %>% filter(Year == year) %>% select(-ends_with("Panel"))

    } else {
        out <- meps_names %>% filter(Year == year) %>% select(type)
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
