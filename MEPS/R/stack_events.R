#' Stack event files
#'
#' Stack MEPS event files. If needed, will remove event key prefixes to standardize variable names and will add total source of payment variables to hospital event files. By default, keeps only variables that are shared by the datasets (plus LINKIDX / EVNTIDX for ID variables)
#'
#' @param ... MEPS event datasets to stack.
#' @param keep.vars Vector of variables in addition to shared variables to keep in the stacked dataset.
#' @param keep.all If TRUE, keep all variables from all datasets. Can slow down computation.
#'
#' @return data frame of stacked event files with standardized variable names. Name of dataset is included as 'data' variable in stacked data set.
#' @export
#'
#' @examples
#'
#' # Get event datasets
#' RX <- read_MEPS(year=2013,type='RX',web=T)
#' OB <- read_MEPS(year=2013,type='OB',web=T)
#' OP <- read_MEPS(year=2013,type='OP',web=T)
#' ER <- read_MEPS(year=2013,type='ER',web=T)
#'
#' aa <- stack_events(RX,OB,OP,ER); head(aa);
#'
#' # Force 'SEEDOC' and 'SEETLKPV' into stacked dataset
#' bb <- stack_events(RX,OB,OP,ER,keep.vars=c('SEEDOC','SEETLKPV')); head(bb);
#'
stack_events <- function(..., keep.vars = NULL, keep.all = F) {

    arg_names <- as.list(match.call())[-1]

    df_list <- list(...)
    names(df_list) <- arg_names[1:length(df_list)]

    message("Removing event keys...")
    df_list <- lapply(df_list, function(x) suppressWarnings(rm_evnt_key(x)))

    message("Adding total SOPs...")
    df_list <- lapply(df_list, function(x) suppressWarnings(add_total_sops(x)))

    if (keep.all) {
        if (!is.null(keep.vars))
            warning("keep.vars specified with keep.all=TRUE. Keeping all variables from all datasets.")
        out <- suppressWarnings(dplyr::bind_rows(df_list, .id = "data"))
        return(out)
    }

    all_cols <- lapply(df_list, colnames)
    matched_cols <- Reduce(intersect, all_cols)
    keep.vars <- unique(c("LINKIDX", "EVNTIDX", matched_cols, keep.vars))

    df_list <- lapply(
        df_list,
        function(x)
            x %>% dplyr::select(dplyr::one_of(keep.vars)))

    out <- suppressWarnings(dplyr::bind_rows(df_list, .id = "data"))

    return(out)
}
