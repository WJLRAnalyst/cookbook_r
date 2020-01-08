library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)

vehicles <- read.csv("vehicles.csv", stringsAsFactors = FALSE)

head(vehicles)
tail(vehicles)

# labels <- read.table("varnames.txt",sep="-")
# labell <- rbind(strsplit(readLines("varnames.txt")," - "))

labels <- readLines("varnames.txt") %>% 
  strsplit(" - ") %>% 
  do.call(rbind,.)

"year" %in% names(vehicles)
vehicles$year %>% unique() %>% length() # 31
min(vehicles$year)
max(vehicles$year)

(vehicles$fuelType)
table(vehicles$fuelType1)
table(vehicles$fuelType2)

nrow(vehicles)

ff <- vehicles %>% 
  select(fuelType,fuelType1,fuelType2) %>% 
  filter(fuelType2 != "") %>% 
  unique()
  