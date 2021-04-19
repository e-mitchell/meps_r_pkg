


# Helper function to download public use file from MEPS website. Can save file to a permanent directory
# (dir).

dl_meps <- function(fname, ext = "ssp", dir = tempdir()) {

  ext <- gsub(".", "", ext, fixed = T) %>% tolower
  ext <- replace(ext, ext == "sas", "v9")

  url <- sprintf("https://meps.ahrq.gov/data_files/pufs/%s%s.zip", fname, ext)

  if(httr::http_error(url))
    stop(stringr::str_glue("Cannot open URL 'url': File not found"))

  utils::download.file(url, temp <- tempfile())
  uz <- utils::unzip(temp, exdir = dir)
  unlink(temp)
  return(uz)
}

# Extract data year based on the two-digit number in PERWT**F or WTDPER** (WTDPER** was converted to PERWT**F in
# 1999).  The weight variable must be in the dataset.

get_year <- function(df) {
    cols <- names(df)
    var <- grep("PERWT|WTDPER", cols, value = T)
    if (length(var) == 0)
        var <- grep("XP[0-9][0-9]", cols, value = T)
    yr <- regmatches(var, gregexpr("[0-9][0-9]", var)) %>% unlist
    return(yr)
}

# Extract event type based on most common two-character prefix for variable names on MEPS event datasets.

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

