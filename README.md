# historic_birds

I added a project specific `.Rprofile` that calls the script `sourcer.R` if it exists and by default sets `stringsAsFactors = FALSE`. The `sourcer.R` script is used to call any utility functions in `utility_script.R` and loads a number of packages (or download and load them if necesary). These packages are:

- lubridate
- dplyr
- reshape2
