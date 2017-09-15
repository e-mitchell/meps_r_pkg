#' Read MEPS public use files and import into R as data frame
#'
#' This function reads in MEPS public use files in .ssp format, either from a local directory or the MEPS website, and imports them into R as a data frame. Larger files (e.g. full-year-consolidated files) can take several seconds to load. Either standardized file name or both year and file type must be specified.
#'
#' @param file (optional) name of public use file. Must be in standard format (e.g. 'h160g'). Can use the get_puf_names() function to convert year and file type to standard format.
#' @param year (optional) data year, between 1996 and most current file release.
#' @param type (optional) file type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRP' (Person-Round-Plan), 'RX' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), and 'RXLK' (RX - events link file)
#' @param dir directory containing .ssp files
#' @param web if TRUE, downloads data directly from MEPS website (requires internet connection)
#'
#' @return MEPS data as a data frame.
#' @export
#'
#' @examples
#' ## Download MEPS 2014 full-year-consolidated file from MEPS website
#' 
#' # Use file name directly
#' FYC2014 <- read_MEPS(file='h171',web=T)
#' 
#' # Use year and file type
#' FYC2014 <- read_MEPS(year=2014,type='FYC',web=T)
#' 
#' ## Download MEPS 2014 FYC file from local directory
#' 
#' # First download to local directory using download_ssp
#' 
#' download_ssp('h171',dir='mydata')
#' 
#' FYC2014 <- read_MEPS(year=2014,type='FYC',dir='mydata')


read_MEPS <- function(file, year, type, dir = ".", web = F) {
    
    if (missing(file) & (missing(year) | missing(type))) 
        stop("Must specify either file or year and type")
    
    if (!missing(file)) {
        fname <- file
    } else {
        fname <- get_puf_names(year = year, type = type, web = web) %>% as.character
    }
    
    if (!web) {
        if (!fname %>% endsWith(".ssp")) 
            fname <- paste0(fname, ".ssp", "")
        
        if (!(fname %in% tolower(list.files(dir)))) 
            stop(sprintf("%s not found. Use 'dir = ' to specify directory or 'web=T' to download from the MEPS website (requires internet connection).", 
                fname))
        
        return(read.xport(sprintf("%s/%s", dir, fname)))
    }
    
    return(read.xport(dl_meps(fname)))
}
