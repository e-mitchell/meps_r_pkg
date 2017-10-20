#' Sum doctor + facility payments for hospital event
#'
#' Creates total expenditure variable for each source of payment (e.g. Out of pocket, Medicaid, Medicare) by adding doctor and facility expenditures. Applies only to hospital event data: Inpatient, Outpatient, and Emergency Room.
#'
#' @param df MEPS event data (hospital only)
#'
#' @return data frame with summarized expenditures. Can contain original variable names (with event-type prefix) or standardized names with event prefix removed.
#' @export
#'
#' @examples
#' 
#' ER <- read_MEPS(type='ER',year=2014,web=T)
#' head(ER)
#' 
#' # Add total SOPs
#' ER2 <- add_total_sops(ER)
#' head(ER2)
#' 
#' # Add total SOPs after removing 'ER' prefix
#' ER_bare <- ER %>% rm_evnt_key('ER')
#' ER3 <- add_total_sops(ER_bare)
#' head(ER3)

add_total_sops <- function(df) {
    sop_prefix <- c("SF", "MR", "MD", "PV", "VA", "TR", "OF", "SL", "WC", "OT", "OR", "OU")
    yr <- get_year(df)
    ev <- suppressWarnings(get_evnt_key(df))
    
    dark_years <- yr %in% c("96", "97", "98", "99")
    
    if (dark_years) 
        sop_prefix <- replace(sop_prefix, sop_prefix == "TR", "CH")
    
    sops <- paste0(sop_prefix, yr, "X")
    doc_sops <- paste0(ev, "D", sops) %>% sort
    fac_sops <- paste0(ev, "F", sops) %>% sort
    new_sops <- paste0(ev, sops) %>% sort
    
    match1 <- all(substring(doc_sops, 2) == substring(fac_sops, 2))  # without event key
    match2 <- all(substring(doc_sops, 4) == substring(fac_sops, 4))  # with event key
    if (!(match1 | match2)) 
        stop("Facility and doctor sources of payment do not match. Check that all source of payment variables are included.")
    
    if (all(c(doc_sops, fac_sops) %in% colnames(df))) {
        df[, new_sops] <- df[, doc_sops] + df[, fac_sops]
    }
    
    # Add 'TR' for early year, 'XP' for 'EXP'
    TRvar <- paste0(ev, "TR", yr, "X")
    CHvar <- paste0(ev, "CH", yr, "X")
    XPvar <- paste0(ev, "XP", yr, "X")
    EXPvar <- paste0(ev, "EXP", yr, "X")
    
    if (dark_years) 
        df[, TRvar] = df[, CHvar]
    
    if (!XPvar %in% colnames(df)) 
        df[, XPvar] = df[, EXPvar]
    
    return(df)
}
