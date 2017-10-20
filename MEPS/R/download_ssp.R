#' Download MEPS public use file 
#' 
#' This function downloads SAS transport files (.ssp) from the MEPS website based on specified file and saves them in the specified folder. Requires internet connection.
#' @param file name of PUF file. Must be in standard format (e.g. 'h160g'). Can use the get_puf_names() function to convert year and file type to standard format.
#' @param dir local directory to store .ssp files. Defaults to 'meps_data' folder in current working directory. Will create folder if none exists.
#' @param force if TRUE, overwrites existing files. Otherwise, the function checks if file already exists in the specified directory.
#' @export
#' @examples 
#' # Download .ssp file for 2015 full-year-consolidated data
#' 
#' download_ssp('h181')
#' 
#' # Download .ssp file for 2013 office-based event file, and store in 'mydata' folder
#' 
#' fname = get_puf_names(year=2013,type='OB')
#' download_ssp(fname,dir='mydata')

download_ssp <- function(file, dir = "meps_data", force = F, silent = F) {
    file = as.character(file)
    if (file %>% endsWith(".ssp")) 
        file <- gsub(".ssp", "", file)
    
    if (!force) {
        # Check if file already exists
        file.ssp <- paste0(file, ".ssp")
        file.alt <- file.ssp %>% sub("h", "hc", .)
        if (file == "h06r") 
            file.alt = "hc006r.ssp"
        if (file == "h07") 
            file.alt = "hc007.ssp"
        
        all_files = tolower(list.files(dir))
        if (any(c(file.ssp, file.alt) %in% all_files)) {
            if (!silent) 
                message(sprintf("File %s already loaded. Use 'force=T' to force download", file))
            return()
        }
    }
    
    if (!dir.exists(dir)) 
        message(sprintf("Creating directory '%s'", dir))
    
    dl_meps(file, dir = dir)
}

