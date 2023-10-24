# Preliminaries

rm(list = ls()) # removing all objects to start with a clean slate
library("utils")
library("tidyverse")
library("readr")
library("openxlsx")
library("readxl")
library("haven")
setwd("C:/Users/HP/source/repos/r-datascience")

df <- read_excel("data/fredgraph.xls",skip = 11)
