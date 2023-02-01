library(tidyverse)
library(tidylog)
library(lubridate)

Microarthropods<-read_csv2(file = "data/FUNDER_2023_Microarthropods_composition_CollembolaMD_rawdata.csv") %>%
  mutate(siteID = recode(siteID,
                       # old name (replace) = valid name (do not change)
                       'Gud' = "Gudmedalen",
                       'Lav' = "Lavisdalen",
                       'Ram' = "Rambera",
                       'Ulv' = "Ulvehaugen",
                       'Skj' = "Skjelingahaugen",
                       'Alr' = "Alrust",
                       'Arh' = "Arhelleren",
                       'Fau' = "Fauske",
                       'Hog' = "Hogsete",
                       'Ovs' = "Ovstedalen",
                       'Vik' = "Vikesland",
                       'Ves' = "Veskre"),
       blockID=paste0(substr(siteID,1,3),blockID),
       plotID=paste0(blockID,treatment)) %>%
  select(siteID, blockID, treatment, plotID, extraction_height:observer,Mite_fungivorous:Comments)

write_csv(Microarthropods, file = "cleaned data/FUNDER_clean_microarthropod_composition_2023.csv")
