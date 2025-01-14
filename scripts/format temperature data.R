# read in and format temp data to calculate thermocline depth
if (!require(rLakeAnalyzer, quietly = TRUE)) install.packages('rLakeAnalyzer')
suppressWarnings(library(rLakeAnalyzer))

if (!require(tidyverse, quietly = TRUE)) install.packages('tidyverse')
suppressWarnings(library(tidyverse))

### read in and format the tchain data from EDI ###

# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.436.2


# Package ID: knb-lter-ntl.436.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Temperature Chain Data 2013 - 2019.
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Michael Pace - University of Virginia 
# Data set creator:  Grace Wilkinson - University of Wisconsin-Madison 
# Metadata Provider:  Dat Ha - University of Virginia 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:  Dat Ha -  University of Virginia  - dh3dv@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/436/2/fbdee3e3df51d802a1e9d31216e07d3b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Lake",     
                 "DoY",     
                 "RoundDoY",     
                 "date",     
                 "time",     
                 "wtr_00.5",     
                 "wtr_01.0",     
                 "wtr_01.5",     
                 "wtr_02.0",     
                 "wtr_02.5",     
                 "wtr_03.0",     
                 "wtr_03.5",     
                 "wtr_04.0",     
                 "wtr_04.5",     
                 "wtr_05.0",     
                 "wtr_06.0"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
if (class(dt1$DoY)=="factor") dt1$DoY <-as.numeric(levels(dt1$DoY))[as.integer(dt1$DoY) ]               
if (class(dt1$DoY)=="character") dt1$DoY <-as.numeric(dt1$DoY)
if (class(dt1$RoundDoY)=="factor") dt1$RoundDoY <-as.numeric(levels(dt1$RoundDoY))[as.integer(dt1$RoundDoY) ]               
if (class(dt1$RoundDoY)=="character") dt1$RoundDoY <-as.numeric(dt1$RoundDoY)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$wtr_00.5)=="factor") dt1$wtr_00.5 <-as.numeric(levels(dt1$wtr_00.5))[as.integer(dt1$wtr_00.5) ]               
if (class(dt1$wtr_00.5)=="character") dt1$wtr_00.5 <-as.numeric(dt1$wtr_00.5)
if (class(dt1$wtr_01.0)=="factor") dt1$wtr_01.0 <-as.numeric(levels(dt1$wtr_01.0))[as.integer(dt1$wtr_01.0) ]               
if (class(dt1$wtr_01.0)=="character") dt1$wtr_01.0 <-as.numeric(dt1$wtr_01.0)
if (class(dt1$wtr_01.5)=="factor") dt1$wtr_01.5 <-as.numeric(levels(dt1$wtr_01.5))[as.integer(dt1$wtr_01.5) ]               
if (class(dt1$wtr_01.5)=="character") dt1$wtr_01.5 <-as.numeric(dt1$wtr_01.5)
if (class(dt1$wtr_02.0)=="factor") dt1$wtr_02.0 <-as.numeric(levels(dt1$wtr_02.0))[as.integer(dt1$wtr_02.0) ]               
if (class(dt1$wtr_02.0)=="character") dt1$wtr_02.0 <-as.numeric(dt1$wtr_02.0)
if (class(dt1$wtr_02.5)=="factor") dt1$wtr_02.5 <-as.numeric(levels(dt1$wtr_02.5))[as.integer(dt1$wtr_02.5) ]               
if (class(dt1$wtr_02.5)=="character") dt1$wtr_02.5 <-as.numeric(dt1$wtr_02.5)
if (class(dt1$wtr_03.0)=="factor") dt1$wtr_03.0 <-as.numeric(levels(dt1$wtr_03.0))[as.integer(dt1$wtr_03.0) ]               
if (class(dt1$wtr_03.0)=="character") dt1$wtr_03.0 <-as.numeric(dt1$wtr_03.0)
if (class(dt1$wtr_03.5)=="factor") dt1$wtr_03.5 <-as.numeric(levels(dt1$wtr_03.5))[as.integer(dt1$wtr_03.5) ]               
if (class(dt1$wtr_03.5)=="character") dt1$wtr_03.5 <-as.numeric(dt1$wtr_03.5)
if (class(dt1$wtr_04.0)=="factor") dt1$wtr_04.0 <-as.numeric(levels(dt1$wtr_04.0))[as.integer(dt1$wtr_04.0) ]               
if (class(dt1$wtr_04.0)=="character") dt1$wtr_04.0 <-as.numeric(dt1$wtr_04.0)
if (class(dt1$wtr_04.5)=="factor") dt1$wtr_04.5 <-as.numeric(levels(dt1$wtr_04.5))[as.integer(dt1$wtr_04.5) ]               
if (class(dt1$wtr_04.5)=="character") dt1$wtr_04.5 <-as.numeric(dt1$wtr_04.5)
if (class(dt1$wtr_05.0)=="factor") dt1$wtr_05.0 <-as.numeric(levels(dt1$wtr_05.0))[as.integer(dt1$wtr_05.0) ]               
if (class(dt1$wtr_05.0)=="character") dt1$wtr_05.0 <-as.numeric(dt1$wtr_05.0)
if (class(dt1$wtr_06.0)=="factor") dt1$wtr_06.0 <-as.numeric(levels(dt1$wtr_06.0))[as.integer(dt1$wtr_06.0) ]               
if (class(dt1$wtr_06.0)=="character") dt1$wtr_06.0 <-as.numeric(dt1$wtr_06.0)

# Convert Missing Values to NA for non-dates

dt1$wtr_00.5 <- ifelse((trimws(as.character(dt1$wtr_00.5))==trimws("NA")),NA,dt1$wtr_00.5)               
suppressWarnings(dt1$wtr_00.5 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_00.5))==as.character(as.numeric("NA"))),NA,dt1$wtr_00.5))
dt1$wtr_01.0 <- ifelse((trimws(as.character(dt1$wtr_01.0))==trimws("NA")),NA,dt1$wtr_01.0)               
suppressWarnings(dt1$wtr_01.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_01.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_01.0))
dt1$wtr_01.5 <- ifelse((trimws(as.character(dt1$wtr_01.5))==trimws("NA")),NA,dt1$wtr_01.5)               
suppressWarnings(dt1$wtr_01.5 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_01.5))==as.character(as.numeric("NA"))),NA,dt1$wtr_01.5))
dt1$wtr_02.0 <- ifelse((trimws(as.character(dt1$wtr_02.0))==trimws("NA")),NA,dt1$wtr_02.0)               
suppressWarnings(dt1$wtr_02.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_02.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_02.0))
dt1$wtr_02.5 <- ifelse((trimws(as.character(dt1$wtr_02.5))==trimws("NA")),NA,dt1$wtr_02.5)               
suppressWarnings(dt1$wtr_02.5 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_02.5))==as.character(as.numeric("NA"))),NA,dt1$wtr_02.5))
dt1$wtr_03.0 <- ifelse((trimws(as.character(dt1$wtr_03.0))==trimws("NA")),NA,dt1$wtr_03.0)               
suppressWarnings(dt1$wtr_03.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_03.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_03.0))
dt1$wtr_03.5 <- ifelse((trimws(as.character(dt1$wtr_03.5))==trimws("NA")),NA,dt1$wtr_03.5)               
suppressWarnings(dt1$wtr_03.5 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_03.5))==as.character(as.numeric("NA"))),NA,dt1$wtr_03.5))
dt1$wtr_04.0 <- ifelse((trimws(as.character(dt1$wtr_04.0))==trimws("NA")),NA,dt1$wtr_04.0)               
suppressWarnings(dt1$wtr_04.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_04.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_04.0))
dt1$wtr_04.5 <- ifelse((trimws(as.character(dt1$wtr_04.5))==trimws("NA")),NA,dt1$wtr_04.5)               
suppressWarnings(dt1$wtr_04.5 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_04.5))==as.character(as.numeric("NA"))),NA,dt1$wtr_04.5))
dt1$wtr_05.0 <- ifelse((trimws(as.character(dt1$wtr_05.0))==trimws("NA")),NA,dt1$wtr_05.0)               
suppressWarnings(dt1$wtr_05.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_05.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_05.0))
dt1$wtr_06.0 <- ifelse((trimws(as.character(dt1$wtr_06.0))==trimws("NA")),NA,dt1$wtr_06.0)               
suppressWarnings(dt1$wtr_06.0 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wtr_06.0))==as.character(as.numeric("NA"))),NA,dt1$wtr_06.0))


tchain = dt1


# assess what lake_years are available
tchain = tchain %>% mutate(lake_year = paste(Lake, Year, sep = "_"))

# filter to just 2018 and 2019
tchain.18.19 = tchain %>% filter(Year %in% c(2018, 2019))

# create datetime, doy, and hour columns column
tchain.18.19 = tchain.18.19 %>% mutate(datetime = as.POSIXct(paste(date, time, sep = " "), format = "%Y-%m-%d %H:%M:%S", tz = "America/Chicago"))
tchain.18.19 = tchain.18.19 %>% mutate(doy = RoundDoY, hour = hour(datetime))

# take the hourly average
tchain.hourly = tchain.18.19 %>% group_by(Lake, Year, doy, hour) %>% 
  summarize(across(starts_with("wtr_"), \(x) mean(x, na.rm = TRUE))) %>% ungroup

tchain.hourly <- tchain.hourly %>%
  mutate(
    # Combine year, day, and hour columns into a single string
    datetime = as.POSIXct(paste(Year, doy, sprintf("%02d", hour), sep = " "), format = "%Y %j %H", tz = "America/Chicago"),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  ) %>%
  select(-doy, -hour) %>%
  select(datetime, everything())

# make datetime a datetime
tchain.hourly = tchain.hourly %>% mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))

# save tchain
write.table(tchain.hourly, "./data/formatted data/hourly tchain/2018 and 2019 hourly tchain.csv", row.names = FALSE, sep = "\t")

save(tchain.hourly, file = "./data/formatted data/hourly tchain/2018 and 2019 hourly tchain.RData")



ggplot(tchain.hourly, aes(x = datetime, y = wtr_00.5, color = Lake))+
  geom_line(alpha = 0.5)+
  facet_wrap(Year~Lake, scales = "free")




### read in and format the routines temp data ###


