

# Helper function to download public use file from MEPS website.  Can
# save .ssp file to a permanent directory (dir).

dl_meps <- function(fname, dir = tempdir()) {
    url <- sprintf("https://meps.ahrq.gov/mepsweb/data_files/pufs/%sssp.zip", 
        fname)
    download.file(url, temp <- tempfile())
    uz <- unzip(temp, exdir = dir)
    unlink(temp)
    return(uz)
}

# Extract data year based on the two-digit number in PERWT**F or
# WTDPER** (WTDPER** was converted to PERWT**F in 1999). The weight
# variable must be in the dataset.

get_year <- function(df) {
    cols <- names(df)
    wtvar <- grep("PERWT|WTDPER", cols, value = T)
    yr <- regmatches(wtvar, gregexpr("[0-9]+", wtvar)) %>% unlist
    return(yr)
}

# Extract event type based on most common two-character prefix for
# variable names on MEPS event datasets.

get_evnt_key <- function(df) {
    cols <- names(df)
    subs <- substr(cols, 1, 2)
    key <- names(which.max(table(subs)))
    if (!key %in% c("RX", "DV", "OM", "IP", "ER", "OP", "OB", "HH")) {
        warning("Event key is not a recognized event type. Check that data is an event dataset with original variable names and try again.")
        return(NULL)
    }
    return(key)
}

