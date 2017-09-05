#' Get MEPS Public Use File Names
#'
#' This is a lookup function that returns a single requested file name or list of names for specified MEPS data file
#' @param year (optional) Data year, between 1996 and most current PUF release. If omitted, files from all years will be returned
#' @param type (optional) File type of desired MEPS file. Options are 'PIT' (Point-in-time file), 'FYC' (Full-year consolidated), 'Conditions' (Conditions file), 'Jobs' (Jobs file), 'PRP' (Person-Round-Plan), 'RX' (Prescription Medicines Events), 'DV' (Dental Visits), 'OM' (Other medical events), 'IP' (Inpatient Stays), 'ER' (Emergency Room Visits), 'OP' (Outpatient Visits), 'OB' (Office-based visits), 'HH' (Home health), 'CLNK' (conditions-event link file), and 'RXLK' (RX - events link file)
#' @export
#' @examples 
#' ## Get file name for full-year-consolidated (FYC) file from 2005 
#' get_puf_names(2005,'FYC')
#' 
#' ## Get file names for all PUFs in 2014
#' get_puf_names(2014)
#' 
#' ## Get file names for RX event files, all years
#' get_puf_names(type='RX')
#' 
#' ## Return all files, all years 
#' get_puf_names()
#' meps_names # can also just view meps_names

get_puf_names <- function(year, type) {
    
    # If missing year and type
    if (missing(year) & missing(type)) {
        warning("Returning meps_names data")
        return(meps_names)
    }
    
    if (!missing(type)) {
        if (!type %in% colnames(meps_names)) {
            cols = meps_names %>% select(-Year, -ends_with("Panel")) %>% colnames
            stop(sprintf("Type must be one of the following: %s", paste(cols, collapse = ", ")))
        }
    }
    
    if (!missing(year)) {
        if (!year %in% meps_names$Year) 
            stop(sprintf("Year must be between %s and %s", min_year, max_year))
    }
    
    
    # All years for specified type
    if (missing(year) & !missing(type)) {
        
        return(meps_names %>% select(Year, type))
    }
    
    # All files for specified year
    if (missing(type) & !missing(year)) {
        min_year = min(meps_names$Year)
        max_year = max(meps_names$Year)
        
        
        
        return(meps_names %>% filter(Year == year) %>% select(-ends_with("Panel")))
    }
    
    # Single year, single type
    return(meps_names %>% filter(Year == year) %>% select(type))
}
