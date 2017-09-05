#' Extract event type from MEPS event dataset
#' 
#' Extracts event type based on two-character prefix for variable names on MEPS event datasets.
#'
#' @param df MEPS event dataset
#'
#' @return Two-character string of event key. If key not recognized, returns NULL.
#' @export 
#'
#' @examples
#' dat <- read_MEPS(file='h102b',web=T)
#' get_evnt_key(dat)
#' 
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
