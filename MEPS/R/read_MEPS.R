#' Read MEPS public use files and import into R as data frame
#'
#' This function reads in MEPS public use files in .ssp format, either from a local directory or the MEPS website, and imports them into R as a data frame. Larger files (e.g. full-year-consolidated files) can take several seconds to load. Either standardized file name or both year and file type must be specified.
#'
#' @param file name of public use file. Must be in standard format (e.g. 'h160g'). Can use the get_puf_names() function to convert year and file type to standard format.
#' @param year (required if 'file' is missing) data year, between 1996 and most current file release.
#' @param type (required if 'file' is missing) file type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRPL' (Person-Round-Plan), 'PMED' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), and 'RXLK' (PMED - events link file)
#' @param dir (optional) local directory containing .ssp files. If left blank, files will be downloaded from MEPS website(requires internet connection).
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
#' # First download to local directory using download_ssp
#'
#' download_ssp('h178f', dir = 'C:/MEPS')
#'
#' OP2015 <- read_MEPS(year = 2015, type = 'OP', dir = 'C:/MEPS')


read_MEPS <- function(file, year, type, dir, web) {

    if(!missing(web)) {
      warning("'web' argument is deprecated. Use 'dir' to specify local directory containing .ssp files, or leave blank to download from internet.")
    }

  # If directory is not specified, load from web
    web = missing(dir)

    if (missing(file) & (missing(year) | missing(type)))
        stop("Must specify either file or year and type.")

    if (!missing(file)) {
        fname_local <- fname_web <- file
    } else {
        fname_local <- get_puf_names(year = year, type = type, web = F) %>% as.character
        fname_web   <- get_puf_names(year = year, type = type, web = T) %>% as.character
    }

  # Load from local directory if available
    if (!web) {

        if (!fname_local %>% endsWith(".ssp"))
          fname_local <- paste0(fname_local, ".ssp", "")

        # If not in local directory, warn and download from MEPS website
        if (!(fname_local %in% tolower(list.files(dir)))) {
          warning(sprintf("%s not found in specified directory. Downloading from MEPS website instead.", fname_local))

        } else {
          return(read.xport(sprintf("%s/%s", dir, fname_local)))
        }

    }

    return(read.xport(dl_meps(fname_web)))
}
