library("readxl")
library("writexl")
library("tidyverse")
library("ggplot2")
library("ggpmisc")
library("forcats")
library("patchwork")
library("wesanderson")
library("MetBrewer") 
library("cowplot")

QV_QUINA_SCRAPER <- 
  read_excel("QV_Raw_Data.xlsx", sheet = 1)

QV_REAFFUTAGE <- 
  read_excel("QV_Raw_Data.xlsx", sheet = 2)

QV_QS_RG <- 
  read_excel("QV_Raw_Data.xlsx", sheet = 3)



