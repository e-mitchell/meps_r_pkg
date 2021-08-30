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

combined_events <- stack_events(MV, OP, ER, IP)

head(combined_events)
#>   data      EVNTIDX  DUID PID DUPERSID EVENTRN FFEEIDX PANEL MPCDATA FFTYPE SF16X MR16X MD16X  PV16X
#> 1   MV 100011010021 10001 101 10001101       1      -1    21       2     -1    10     0     0 196.00
#> 2   MV 100011010031 10001 101 10001101       1      -1    21       2     -1    10     0     0  45.00
#> 3   MV 100011010041 10001 101 10001101       1      -1    21       2     -1    10     0     0 193.00
#> 4   MV 100011010051 10001 101 10001101       1      -1    21       2     -1    10     0     0  59.72
#> 5   MV 100011010061 10001 101 10001101       1      -1    21       2     -1     0     0     0  71.15
#> 6   MV 100011010071 10001 101 10001101       2      -1    21       2     -1    10     0     0 196.00
#>   VA16X TR16X OF16X SL16X WC16X OR16X OU16X OT16X  XP16X TC16X IMPFLAG PERWT16F VARSTR VARPSU
#> 1     0     0     0     0     0     0     0     0 206.00   206       4 12999.55   1021      1
#> 2     0     0     0     0     0     0     0     0  55.00    46       4 12999.55   1021      1
#> 3     0     0     0     0     0     0     0     0 203.00   203       4 12999.55   1021      1
#> 4     0     0     0     0     0     0     0     0  69.72   125       4 12999.55   1021      1
#> 5     0     0     0     0     0     0     0     0  71.15   100       3 12999.55   1021      1
#> 6     0     0     0     0     0     0     0     0 206.00   206       4 12999.55   1021      1
```
