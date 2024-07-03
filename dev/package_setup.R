# April 18, 2024

library(devtools)

# set up project
create_package("C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/waves")

# add license
use_gpl3_license()

# set up git repo
use_git()

# set up github
use_github()

# add packages (from CRAN)
use_package("data.table")
use_package("dplyr")
use_package("ggplot2")
#use_package("gtools")
use_package("flextable")
use_package("lubridate")
use_package("officer")
use_package("purrr")
use_package("RColorBrewer")
#use_package("rlang")
use_package("stringr")
use_package("tidyr")
#use_package("tidyselect")
use_package("grDevices")

# add packages (not on CRAN)
use_dev_package("adcp", remote = "dempsey-CMAR/acdp")

# readme
use_readme_rmd()

# github actions
use_github_action_check_standard()

# code factor
# https://www.codefactor.io/dashboard

# tests
use_testthat(3)

