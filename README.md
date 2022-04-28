# meps_r_pkg

meps_r_pkg is an R library designed to facilitate loading and manipulation of public use files (PUFs) from the Medical Expenditure Panel Survey Household Component (MEPS-HC):
 * `get_puf_names()` returns a current list of the names of MEPS public use files.
 * `read_MEPS()` loads MEPS public use files (PUFs) directly from the MEPS website. Internet connection required.
 * `dl_meps()` downloads MEPS public use files (PUFs) from the MEPS website and saves to local directory. Internet connection required.

## Installation

``` r
# First, install and load the devtools package
install.packages("devtools")

# Next, install from github and load
devtools::install_github("e-mitchell/meps_r_pkg/MEPS")
library(MEPS)
```


## Usage
``` r
# Show PUF names for 2019 data files
get_puf_names(year = 2019)
#>   YEAR  PIT  FYC CONDITIONS  PMED JOBS PRPL LONGITUDINAL    CLNK
#> 1 2019 h205 h216       h214 h213a h211 h215         h217 h213if1
#>      RXLK MULTUM PSAQ MOS FS DENTAL OTHER_MEDICAL INPATIENT
#> 1 h213if2      -    -   -  -  h213b         h213c     h213d
#>   EMERGENCY_ROOM OUTPATIENT OFFICE_BASED HOME_HEALTH
#> 1          h213e      h213f        h213g       h213h

# Show PUF names for the Conditions files, 2015-2019
get_puf_names(type = "COND", year = 2015:2019)
#>   YEAR COND
#> 1 2015 h180
#> 2 2016 h190
#> 3 2017 h199
#> 4 2018 h207
#> 5 2019 h214

# Download Conditions files from website and save to 'C:/MEPS'
dl_meps("h214", ext = "dta", dir = "C:/MEPS") # Stata format (.dta)
dl_meps("h214", ext = "v9",  dir = "C:/MEPS") # SAS V9 format (.sas7bdat)

# Load event-level datasets for 2019
MV <- read_MEPS(year = 2019, type = "MV") # office-based medical visits
OP <- read_MEPS(year = 2019, type = "OP") # outpatient visits
ER <- read_MEPS(year = 2019, type = "ER") # emergency room
IP <- read_MEPS(year = 2019, type = "IP") # inpatient

