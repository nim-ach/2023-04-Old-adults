# Prepare workspace -------------------------------------------------------

## Load packages
library(readxl)
library(data.table)

## Import excel file
elderly <- read_excel("data-raw/Datos Adultos Mayores v1.xlsx", skip = 6)
