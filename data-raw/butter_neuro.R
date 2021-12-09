## code to prepare `butter_neuro` dataset goes here
library(tidyverse)
butter_neuro = read_csv("analysis/data/raw_data/Nowicki_C_SPP_Males_data_ESM.csv") %>%
  filter(region_mammal == "meAMY.BNST")

usethis::use_data(butter_neuro, overwrite = TRUE)
