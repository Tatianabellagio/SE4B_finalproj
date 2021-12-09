## code to prepare `buttR_neuro` dataset goes here
library(tidyverse)
buttR_neuro = read.csv("analysis/data/raw_data/Nowicki_C_SPP_Males_data_ESM.csv",stringsAsFactors = TRUE,
                       colClasses=c("count"="numeric")) %>%
  filter(region_mammal == "meAMY.BNST")

usethis::use_data(buttR_neuro, overwrite = TRUE)
