# https://www.fueleconomy.gov/feg/ws/index.shtml#vehicle

library(dplyr)
# library(plyr)
library(ggplot2)
library(reshape2)
library(plotly) # TO DO - google conflicts with ggplot2

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

summary(vehicles$fuelType)
table(vehicles$fuelType1)
table(vehicles$fuelType2)

nrow(vehicles) # 34287

#### fuel types ####
ff <- vehicles %>%
  select(fuelType,fuelType1,fuelType2) %>% 
  # filter(fuelType2 != "") %>% 
  unique()

#### transmission ####
table(vehicles$trany)
vehicles$trany[vehicles$trany == ""] <- NA 

# only care whether auto or manual - helpfully, the first characters of the string in each case are consistent
vehicles <- vehicles %>% 
  mutate(trany2 = ifelse(substr(trany,1,4)=="Auto", "Auto", "Manual"))

vehicles$trany2 <- as.factor(vehicles$trany2)

table(vehicles$trany2)
with(vehicles,table(trany2,year))

#### charge ####
# NB - in raw data, unique(vehicles$iCharger) == c("","i"). R coerces "T" to TRUE, so sChrger is imported as char and tCharger as bool  

table(vehicles$sCharger)
table(vehicles$tCharger,useNA = "always")
with(vehicles,table(sCharger,year))
with(vehicles,table(tCharger,year,useNA="always"))

vehicles$sCharger <- ifelse(vehicles$sCharger =="S", TRUE,FALSE)
vehicles$tCharger[vehicles$tCharger == ""] <- FALSE

#### MPG ####

MPG_trends <- vehicles %>% 
  group_by(year) %>% 
  summarise(mCity = mean(city08),
            mHigh = mean(highway08),
            mComb = mean(comb08))

plot_ly(MPG_trends, x=~year,y=~mCity) 

ggplot(MPG_trends,aes(year,mComb)) +
  geom_point() + 
  geom_smooth() +
  xlab("year") + ylab("mean MPG")

#### does this picture persist when only gasoline powered cars are considered?
gasCars <- vehicles %>% 
  filter(grepl("Gasoline",as.character(fuelType1),fixed=TRUE),
         fuelType2 == "",
         atvType != "Hybrid")

gasMPG_trends <- gasCars %>% 
  group_by(year) %>% 
  summarise(mCity_gas = mean(city08),
            mHigh_gas = mean(highway08),
            mComb_gas = mean(comb08))

ggplot(gasMPG_trends,aes(year,mComb_gas)) +
  geom_point() + 
  geom_smooth() +
  xlab("year") + ylab("mean MPG")

#### engine size (and relationship to MPG) ####
vehicles$displ <- as.numeric(vehicles$displ)
gasCars$displ <- as.numeric(gasCars$displ)

ggplot(gasCars,aes(displ,comb08)) +
  geom_point() + 
  geom_smooth() +
  xlab("Engine size (displacement)") + ylab("MPG")


displByYear <- vehicles %>% 
  group_by(year) %>% 
  summarise(mDispl = mean(displ))

gas_displByYear <- gasCars %>% 
  group_by(year) %>% 
  summarise(mDispl = mean(displ))

plot_ly(data = displByYear,x=~year,y=~mDispl)
plot_ly(data = gas_displByYear,x=~year,y=~mDispl)

#############################3





comb <- gas_displByYear %>% 
  inner_join(gasMPG_trends,by="year") %>% 
  select(year,mDispl,mComb_gas) 

plot_ly(data=comb,x=~year,y=~mDispl) %>% 
  add_trace(y=~comb$mComb)










