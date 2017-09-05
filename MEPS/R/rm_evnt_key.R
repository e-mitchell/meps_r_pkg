#' Rename Variables of Event Data
#'
#' This function renames columns of the event files to remove the event key prefix
#' (e.g. 'OBXP13X' => 'XP13X'), to facilitate stacking event files.
#' @param df data frame of MEPS event file (e.g. h160a for RX events from 2013)
#' @param evnt_key optional argument specifying event key to be removed from column names. Defaults to most common
#' 2-letter prefix of current variable names
#' @export
#' @examples 
#' # Read in office-based medical events file from 2013 ('h160g')
#' OB2013 = downloadMEPS('h160g')
#' OBnew = rm_evnt_key(OB2013)
#' head(OBnew)

rm_evnt_key <- function(df, evnt_key) {
    cols = names(df)
    if (missing(evnt_key)) {
        evnt_key = names(sort(table(substr(cols, 1, 2)), decreasing = T)[1])
        message(sprintf("evnt_key is missing. Using '%s'.", evnt_key))
    }
    
    key_cols = cols[startsWith(cols, evnt_key)]
    cols[startsWith(cols, evnt_key)] = sub(evnt_key, "", key_cols)
    cols[cols == sprintf("FF%sTYPE", evnt_key)] = "FFTYPE"
    
    names(df) <- cols
    return(df)
}
