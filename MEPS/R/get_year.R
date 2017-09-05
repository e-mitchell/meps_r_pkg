#' Extract data year from dataset
#' 
#' Extracts data year based on the two-digit number in PERWT**F or WTDPER** (WTDPER** was converted to PERWT**F in 1999). The weight variable must be in the dataset.
#'
#' @param df MEPS dataset
#'
#' @return two-digit character key for year
#' @export
#'
#' @examples
#' 
#' # Get data year from full year file
#' FYC <- read_MEPS(file='h147')
#' get_year(FYC)
#' 
#' # From event file
#' RX <- read_MEPS(file='h10a')
#' get_year(RX)

get_year <- function(df) {
    cols <- names(df)
    wtvar <- grep("PERWT|WTDPER", cols, value = T)
    yr <- regmatches(wtvar, gregexpr("[0-9]+", wtvar)) %>% unlist
    return(yr)
}
