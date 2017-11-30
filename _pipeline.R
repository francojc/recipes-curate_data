
# ABOUT -------------------------------------------------------------------

# Description: <The aim of this project>
# Usage: <How to run this master script>
# Author: <Your Name>
# Date: <current date>

# SETUP -------------------------------------------------------------------

# Global project options or packages

# Load `pacman`. If not installed, install then load.
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
  library("pacman", character.only = TRUE)
}

# RUN ---------------------------------------------------------------------

# 1. Acquire data

source(file = "code/acquire_data.R")

# 2. Curate data

source(file = "code/curate_data.R")

# 3. Transform data

# source(file = "code/transform_data.R")

# 4. Analyze data

# source(file = "code/analyze_data.R")

# 5. Generate reports

# source(file = "code/generate_reports.R")

# LOG ---------------------------------------------------------------------

# Log information on the current configuration of the system
sink(file = "log/session_info.log")
sessionInfo()
sink()

# CLEAN UP ----------------------------------------------------------------

# Remove all current environment variables
rm(list = ls())

