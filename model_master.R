############################################################
#### MODEL.MASTER
### Master code for running the SEIHCRD model for Indiana (or
### any other state with appropriate modification). This is based
### on an epidemiological SEIR model, but three populations are
### added:  Hospitalized, Critically hospitalized, and Deceased.
### More info can be found in the accompanying documentation.
############################################################
## Ryan Hastings, 26 March 2020
############################################################
rm(list=ls()) # clear out variables

# this would be just library(tidyverse) but for some reason that
# doesn't work on my machine
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(haven)
library(readxl)

# run code to configure model
source("model_configuration.R")

# run code to initialize model
source("model_initialization.R")

# run model core
source("model_dynamic_core.R")

# output results
source("model_out.R")