
# ABOUT -------------------------------------------------------------------

# Description: This script collects data from the web and stores it on disk
# Usage: Only an internet connection is required. Data collected is stored
# in the `data/original/` directory. 
# Author: Jerid Francom
# Date: October 1, 2017

# SETUP -------------------------------------------------------------------

# Script-specific options or packages
pacman::p_load(tidyverse, gutenbergr, rvest, stringr)

# Load custom functions for this project
source(file = "functions/acquire_functions.R")

# RUN ---------------------------------------------------------------------

# Download ACTIV-ES Corpus (ACTIV-ES) -------------------------------------

# Download plain-text version
get_zip_data(url = "https://github.com/francojc/activ-es/raw/master/activ-es-v.02/corpus/plain.zip", target_dir = "data/original/actives/plain")

# Download Switchboard Dialog Act Corpus (SDAC) ---------------------------

# Resource information: https://catalog.ldc.upenn.edu/LDC97S62
# Download corpus annotations
get_compressed_data(url = "https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz", target_dir = "data/original/sdac/")

# Download Brown Corpus (BROWN) -------------------------------------------

# BROWN found at: http://www.nltk.org/nltk_data/

# Download tei version (xml)
get_zip_data(url = "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/brown_tei.zip", target_dir = "data/original/brown/")

# LOG ---------------------------------------------------------------------

# Any descriptives that will be helpful to understand the results of this
# script and how it contributes to the aims of the project

# CLEAN UP ----------------------------------------------------------------

# Remove all current environment variables
rm(list = ls())
