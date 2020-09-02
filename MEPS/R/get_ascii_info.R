#' Get ASCII info from Stata programming statements, for reading .dat files into R
#'
#' This function pulls information needed to load the MEPS PUFS in ASCII .dat file format into R. This program was built as a work-around for MEPS 2018 PUFs. Due to design changes in the survey instrument, the SAS transport files (.ssp) are output using the CPORT engine starting in 2018. Since this file type is not compatible with R, the ASCII (.dat) file must be used instead. This function pulls the variable names, types, and positions from the Stata programming statements. It is intended as a temporary work-around, until the R programming statements are released. Internet connection is required.
#'
#' @param filename name of public use file. Must be in standard format (e.g. 'h160g'). Can use the get_puf_names() function to look up file name by year and type.
#'
#' @param stata_file (optional) complete path to Stata programming statements (.txt file) when saved to a local folder. If not specified, Stata programming statements will be pulled from MEPS website.
#'
#' @return list(start, end, names, types). List of ASCII file information, included variable start and end positions, names, and types
#' @export
#'
#' @examples
#' ## Import 2018 dental file (H206b)
#'
#' # Pull ASCII file info from MEPS website.
#' dn_info <- get_ascii_info('h206b')
#'
#' # Import data using read_fwf function (must save .dat file to local directory first)
#' meps_dat <- read_fwf("C:/MEPS/h206b.dat",
#'   col_positions =
#'     fwf_positions(
#'       start = dn_info[["start"]],
#'       end   = dn_info[["end"]],
#'       col_names = dn_info[["names"]]),
#'   col_types = dn_info[["types"]])
#'

get_ascii_info <- function(filename, stata_file) {
  # Pull variable positions from 'stata commands' file

  # Use web version if local version is not specified
  if(missing(stata_file)) {
    foldername <- filename %>% gsub("f[0-9]+","",.)
    stata_commands <- readLines(sprintf(
      "https://meps.ahrq.gov/data_stats/download_data/pufs/%s/%sstu.txt",
      foldername, filename))
  } else {
    stata_commands <- readLines(stata_file)
  }

  dta_name <- case_when(
    filename == "h01" ~ "hc001",
    filename == "h05" ~ "hc005xf",
    filename == "h06r"~ "hc006r",
    filename == "h07" ~ "hc007",
    filename == "h09" ~ "hc009xf",
    filename == "h13" ~ "hc013xf",
    TRUE ~ filename)

  hc_dta  <- dta_name %>% sub("h","hc",.)
  hc0_dta <- dta_name %>% sub("h","hc0",.)

  find_end <- function(str) {
    # Just checking first half of stata commands (bottom half is formatting)
    # - Mac fails for unrecognized characters
    len = length(stata_commands)
    which(tolower(stata_commands[1:(len/2)]) == sprintf("using %s.dat;", str))
  }

  infix_start <- which(stata_commands == "infix")

  infix_end_h <- find_end(dta_name)
  infix_end_hc <- find_end(hc_dta)
  infix_end_hc0 <- find_end(hc0_dta)
  infix_end <- c(infix_end_h, infix_end_hc, infix_end_hc0)[1]

  infix_data  <- stata_commands[(infix_start+1):(infix_end-1)]

  infix_df <- infix_data %>%
    str_trim %>%
    gsub("-\\s+","-",.) %>%
    tibble::as_tibble() %>%
    separate(
      value, into = c("var_type", "var_name", "start", "end"),
      sep = "\\s+|-",
      fill = "left") %>%
    mutate(var_type = replace_na(var_type, "double")) # Stata assumes by default

  # Create 'start', 'end', 'name', and 'type' vectors
  pos_start <- infix_df %>% pull(start) %>% as.numeric
  pos_end   <- infix_df %>% pull(end) %>% as.numeric
  cnames <- infix_df %>% pull(var_name)
  ctypes <- infix_df %>%
    mutate(typeR = case_when(
      var_type %in% c("str") ~ "c", # character
      var_type %in% c("long", "int", "byte", "double") ~ "n", # numeric
      TRUE ~ "ERROR"
    )) %>%
    pull(typeR) %>%
    setNames(cnames)

  #if(any(ctypes == 'ERROR')) stop("BAD CTYPES -- need to investigate please")

  return(
    list("start" = pos_start,
         "end"   = pos_end,
         "names" = cnames,
         "types" = ctypes))
}
