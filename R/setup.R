
rm(list=ls())


# Load packages -----
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(forcats)
library(janitor)
library(ISOweek)
library(growthrates)
library(glue)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(forecast)
library(sandwich)
library(rgeos)
library(sf)
library(officer)
library(gt)
library(webshot)
library(knitr)
library(flextable)
library(distill)
library(english)

# Create project paths -----
path.root  <- here::here()
path.R     <- file.path(path.root,'R')
path.Rmd   <- file.path(path.root,'Rmd')
path.data  <- file.path(path.root,'data')
path.templates <- file.path(path.root,'templates')
path.local     <- file.path(path.root,'local')

# Create local folder or all types of outputs -----
dir.create(path.local, showWarnings = FALSE, recursive = TRUE)


