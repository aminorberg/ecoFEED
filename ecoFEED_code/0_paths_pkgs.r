rm(list = ls(all = TRUE)) ; gc()

# set the working directory, i.e. the path to the 'ecoFEED' folder
# path below is an example
setwd("/Users/JaneDoe/Documents/ecoFEED/")

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(forcats)
library(readxl)

filePath1 <- "ecoFEED_data/data_current"
filePath2 <- "ecoFEED_mod_data/data_current"
filePath3 <- "ecoFEED_data/data_indicators"
filePath4 <- "ecoFEED_mod_data/data_indicators"
