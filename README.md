# meps_r_pkg

meps_r_pkg is an R library designed to facilitate loading and manipulation of public use files (PUFs) from the Medical Expenditure Panel Survey Household Component (MEPS-HC):
 * `get_puf_names()` returns a current list of the names of MEPS public use files. Internet connection is needed.
 * `read_MEPS()` loads MEPS public use files (PUFs) directly from the MEPS website (default)

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
# Show PUF names for 2016
get_puf_names(year = 2016)
#>   Year  PIT  FYC Conditions  PMED Jobs PRPL Longitudinal    CLNK    RXLK
#> 1 2016 h177 h192       h190 h188a h185 h191         h193 h188if1 h188if2
#>   Multum  MOS    RX    DV    OM    IP    ER    OP    OB    MV    HH
#> 1      - h187 h188a h188b h188c h188d h188e h188f h188g h188g h188h

# Load event-level datasets for 2016
MV <- read_MEPS(year = 2016, type = "MV") # office-based medical visits
OP <- read_MEPS(year = 2016, type = "OP") # outpatient visits
ER <- read_MEPS(year = 2016, type = "ER") # emergency room
IP <- read_MEPS(year = 2016, type = "IP") # inpatient
