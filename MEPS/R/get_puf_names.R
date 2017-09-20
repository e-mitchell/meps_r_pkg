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
#' 
#' ## Compare names of .ssp files with those on website links
#' get_puf_names(year = 1996, type = 'DV')
#' get_puf_names(year = 1996, type = 'DV', web=F)

get_puf_names <- function(year, type, web = F) {
    
    # Check for data input errors -----------------------------------------------
    
    if (!missing(type)) {
        if (!type %in% colnames(meps_names)) {
            cols <- meps_names %>% select(-Year, -ends_with("Panel")) %>% colnames
            stop(sprintf("Type must be one of the following: %s", paste(cols, collapse = ", ")))
        }
    }
    
    if (!missing(year)) {
        if (!year %in% meps_names$Year) 
            stop(sprintf("Year must be between %s and %s", min(meps_names$Year), max(meps_names$Year)))
    }
    
    # Return MEPS names based on specified, year, type --------------------------
    
    if (missing(year) & missing(type)) {
        warning("Returning meps_names data")
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
    
    hc_list <- c("h10a", "h10if1", "h10if2", "h26bf1", "h19", sprintf("h16%sf1", letters[2:8]), 
        sprintf("h10%sf1", letters[2:8]))
    
    hc0_list <- c("h06r", "h07")
    
    meps_mat <- as.matrix(out)
    meps_mat[meps_mat %in% hc_list] <- sub("h", "hc", meps_mat[meps_mat %in% hc_list])
    meps_mat[meps_mat %in% hc0_list] <- sub("h", "hc0", meps_mat[meps_mat %in% hc0_list])
    out <- as.data.frame(meps_mat, stringsAsFactors = F)
    
    return(out)
}
