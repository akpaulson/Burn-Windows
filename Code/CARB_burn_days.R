#### Description #####

# This code will sort through the California air resources board agricultural
# burn day notices since 1998. Goal is a long format daily designation of burn or
# no burn. Not sure if I want it with each air basin as a column or totally long 
# format.  

#### Packages ####
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)
library(lubridate)
library(sf)
library(sp)
library(ggplot2)


#### Load each year's CARB burn day data ####

#Data on agricultural burn day notices from the California Air Resources Board
# were downloaded from the the Historical Agricultural Burn Decisions dataset
# found here: https://ww3.arb.ca.gov/smp/histor/histor.htm. I downloaded the 
# data from 1998 - 2021 (though April 2021).  It's not in a great format for 
# R processing, so let's see if we can get it into a better format: 


#Was trying to automate this for all years at once, but different years/months 
  # within years have
  # different formats (and burn days aren't always filled in!). There are also 
  # different numbers of air basins in different years. Rather than hard coding 
  # all of this in a giant loop, I think it will be faster to go year by year : (

#1998
md1998 <- "RawData/CARB_ag_burn_days/md1998.xls"

tab_names <- excel_sheets(path = md1998)

all_sheets <- lapply(1:length(tab_names), 
                   function(i) read_excel(path = md1998, 
                                          sheet = tab_names[i], 
                                          col_names = TRUE,
                                          range = "A6:AH31", 
                                          col_types = "text") %>% 
                     mutate(month = tab_names[i]) %>% 
                     select(-c(`...2`, `...3`)))
md1998_df <- bind_rows(all_sheets) %>% 
  mutate(year = "1998") %>% 
  mutate(month = str_replace_all(month, "98", ""))
md1998_df[is.na(md1998_df)] <- "B"

#1999
md1999 <- "RawData/CARB_ag_burn_days/md1999.xls"

tab_names <- excel_sheets(path = md1999)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md1999, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH31", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md1999_df <- bind_rows(all_sheets) %>% 
  mutate(year = "1999") %>% 
  mutate(month = str_replace_all(month, "99", ""))
md1999_df[is.na(md1999_df)] <- "B"

#2000
md2000 <- "RawData/CARB_ag_burn_days/md2000.xls"

tab_names <- excel_sheets(path = md2000)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2000, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH31", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md2000_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2000") %>% 
  mutate(month = str_replace_all(month, "00", ""))
md2000_df[is.na(md2000_df)] <- "B"


#2001
md2001 <- "RawData/CARB_ag_burn_days/md2001.xls"

tab_names <- excel_sheets(path = md2001)

all_sheets_through_aug <- lapply(1:8, 
                     function(i) read_excel(path = md2001, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH31", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

all_sheets_sep_dec <- lapply(9:12, 
                                 function(i) read_excel(path = md2001, 
                                                        sheet = tab_names[i], 
                                                        col_names = TRUE,
                                                        range = "A6:AH35", 
                                                        col_types = "text") %>% 
                                   mutate(month = tab_names[i]) %>% 
                                   select(-c(`...2`, `...3`)))

md2001_df <- bind_rows(all_sheets_through_aug, all_sheets_sep_dec) %>% 
  mutate(year = "2001") %>% 
  mutate(month = str_replace_all(month, "01", ""))
md2001_df[is.na(md2001_df)] <- "B"


#2002
md2002 <- "RawData/CARB_ag_burn_days/md2002.xls"

tab_names <- excel_sheets(path = md2002)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2002, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH35", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md2002_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2002") %>% 
  mutate(month = str_replace_all(month, "02", ""))
md2002_df[is.na(md2002_df)] <- "B"


#2003
md2003 <- "RawData/CARB_ag_burn_days/md2003.xls"

tab_names <- excel_sheets(path = md2003)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2003, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH37", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md2003_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2003") %>% 
  mutate(month = str_replace_all(month, "03", "")) %>% 
  #There are different numbers of rows in each sheet : (
  filter(!`AIR BASIN` %in% c("*Elevation Other Than 3,000 Feet (1,000's of Feet)",
                            "Bay Area Fall Tule Burn Allocation (100's of Acres)"))
md2003_df[is.na(md2003_df)] <- "B"


#2004
md2004 <- "RawData/CARB_ag_burn_days/md2004.xls"

tab_names <- excel_sheets(path = md2004)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2004, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH37", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md2004_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2004")
md2004_df[is.na(md2004_df)] <- "B"


#2005
md2005 <- "RawData/CARB_ag_burn_days/md2005.xls"

tab_names <- excel_sheets(path = md2005)

all_sheets <- lapply(1:(length(tab_names)-1), 
                     function(i) read_excel(path = md2005, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A6:AH37", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
#December format changes: 
dec_2005 <- read_excel(path = md2005, 
                       sheet = "Dec", 
                       col_names = TRUE,
                       range = "A5:AH36", 
                       col_types = "text") %>% 
  mutate(month = "Dec") %>% 
  select(-c(`...2`, `...3`))

md2005_df <- bind_rows(all_sheets) %>% 
  rbind(., dec_2005) %>% 
  mutate(year = "2005")
md2005_df[is.na(md2005_df)] <- "B"


#2006
#several (4) months in 2006 had day 31 listed as day 1, leading to repetition in 
  # column names. I hand-corrected this. February is also messed up. 
md2006 <- "RawData/CARB_ag_burn_days/md2006.xls"

tab_names <- excel_sheets(path = md2006)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2006, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`))) 

#February is messed up, remove: 
all_sheets = all_sheets[-2]

feb_2006  <- read_excel(path = md2006, 
                       sheet = "Feb", 
                       col_names = TRUE,
                       range = "A6:AH37", 
                       col_types = "text") %>% 
  mutate(month = "Feb") %>% 
  select(-c(`...2`, `...3`))


md2006_df <- bind_rows(all_sheets) %>% 
  rbind(., feb_2006) %>% 
  mutate(year = "2006") %>% 
  filter(!`AIR BASIN` %in% c("*Elevation Other Than 3,000 Feet (1,000's of Feet)", 
                             "Bay Area Fall / Spring Tule Burn Allocation (Acres)"))

#2007
md2007 <- "RawData/CARB_ag_burn_days/md2007.xls"

tab_names <- excel_sheets(path = md2007)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2007, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH34", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))
md2007_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2007") %>% 
  select(-c(`...32`))


#2008
md2008 <- "RawData/CARB_ag_burn_days/md2008.xls"

tab_names <- excel_sheets(path = md2008)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2008, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2008_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2008")%>% 
  select(-c(`...31`, `...32`)) %>% 
  filter(!`1` == "Implimented starting February 2008")


#2009
md2009 <- "RawData/CARB_ag_burn_days/md2009.xls"

tab_names <- excel_sheets(path = md2009)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2009, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2009_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2009")%>% 
  select(-c(`...32`))


#2010
md2010 <- "RawData/CARB_ag_burn_days/md2010.xls"

tab_names <- excel_sheets(path = md2010)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2010, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2010_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2010")%>% 
  select(-c(`...32`))

#2011
md2011 <- "RawData/CARB_ag_burn_days/md2011.xls"

tab_names <- excel_sheets(path = md2011)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2011, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2011_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2011")%>% 
  select(-c(`...32`))

#2012

md2012 <- "RawData/CARB_ag_burn_days/md2012.xls"

tab_names <- excel_sheets(path = md2012)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2012, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2012_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2012")%>% 
  select(-c(`...32`))

#2013

md2013 <- "RawData/CARB_ag_burn_days/md2013.xls"

tab_names <- excel_sheets(path = md2013)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2013, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2013_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2013")%>% 
  select(-c(`...32`))

#2014
md2014 <- "RawData/CARB_ag_burn_days/md2014.xls"

tab_names <- excel_sheets(path = md2014)

all_sheets <- lapply(1:(length(tab_names)-1), 
                     function(i) read_excel(path = md2014, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2014_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2014")%>% 
  select(-c(`...32`))

#2015
md2015 <- "RawData/CARB_ag_burn_days/md2015.xls"

tab_names <- excel_sheets(path = md2015)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2015, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2015_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2015") %>% 
  select(-`Burn %`)

#2016
md2016 <- "RawData/CARB_ag_burn_days/md2016.xls"

tab_names <- excel_sheets(path = md2016)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2016, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2016_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2016") %>% 
  select(-`Burn %`)


#2017
md2017 <- "RawData/CARB_ag_burn_days/md2017.xlsx"

tab_names <- excel_sheets(path = md2017)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2017, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2017_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2017") %>% 
  select(-`Burn %`) %>% 
  filter(!`AIR BASIN` %in% c("Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                             "*Elevation Other Than 3,000 Feet (1,000's of Feet)"))



#2018
md2018 <- "RawData/CARB_ag_burn_days/md2018.xlsx"

tab_names <- excel_sheets(path = md2018)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2018, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2018_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2018") %>% 
  select(-`Burn %`) %>% 
  filter(!`AIR BASIN` %in% c("Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                             "*Elevation Other Than 3,000 Feet (1,000's of Feet)"))


#2019
md2019 <- "RawData/CARB_ag_burn_days/md2019.xlsx"

tab_names <- excel_sheets(path = md2019)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2019, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2019_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2019") %>% 
  select(-`Burn %`) %>% 
  filter(!`AIR BASIN` %in% c("Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                             "*Elevation Other Than 3,000 Feet (1,000's of Feet)"))


#2020

md2020 <- "RawData/CARB_ag_burn_days/md2020.xlsx"

tab_names <- excel_sheets(path = md2020)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2020, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2020_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2020") %>% 
  select(-`Burn %`) %>% 
  filter(!`AIR BASIN` %in% c("Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                             "*Elevation Other Than 3,000 Feet (1,000's of Feet)"))


#2021
md2021 <- "RawData/CARB_ag_burn_days/md2021.xlsx"

tab_names <- excel_sheets(path = md2021)

all_sheets <- lapply(1:length(tab_names), 
                     function(i) read_excel(path = md2021, 
                                            sheet = tab_names[i], 
                                            col_names = TRUE,
                                            range = "A5:AH36", 
                                            col_types = "text") %>% 
                       mutate(month = tab_names[i]) %>% 
                       select(-c(`...2`, `...3`)))

md2021_df <- bind_rows(all_sheets) %>% 
  mutate(year = "2021") %>% 
  select(-`Burn %`) %>% 
  filter(!`AIR BASIN` %in% c("Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                             "*Elevation Other Than 3,000 Feet (1,000's of Feet)")) %>% 
  filter(!month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


#### Join all CARB burn day data and organize ####

#Join together all of the data from each year: 

carb <- bind_rows(md1998_df, md1999_df, md2000_df, md2001_df, md2002_df, 
                  md2003_df, md2004_df, md2005_df, md2006_df, md2007_df, 
                  md2008_df, md2009_df, md2010_df, md2011_df, md2012_df, 
                  md2013_df, md2014_df, md2015_df, md2016_df, md2017_df, 
                  md2018_df, md2019_df, md2020_df, md2021_df) %>% 
  #Pivot longer: 
  pivot_longer(., cols = -c(`AIR BASIN`, month, year), 
               names_to = "day", 
               values_to = "burn_day") %>% 
  rename(air_basin = `AIR BASIN`) %>% 
  #get rid of days that don't exist: 
  filter(!(month == "Feb" & day %in% c("30", "31"))) %>% 
  filter(!(month == "Feb" & day %in% c("29") & !year %in% c("2000", "2004", "2008", "2012", "2016", "2020"))) %>% 
  filter(!(month %in% c("Apr", "April") & day %in% c("31"))) %>% 
  filter(!(month == "Jun" & day %in% c("31"))) %>% 
  filter(!(month == "Sep" & day %in% c("31"))) %>% 
  mutate(month = if_else(month %in% c("Nov 1-11", "Nov 12-30"), "Nov", month)) %>% 
  filter(!(month == "Nov" & day %in% c("31"))) %>% 
  mutate(month = if_else(month %in% c("April"), "Apr", month)) %>% 
  mutate(month = if_else(month %in% c("March"), "Mar", month)) %>% 
  #Get date column in shape and lubridate: 
  unite(col = "date", c(month, day, year), sep = " ", remove = FALSE) %>% 
  mutate(date = mdy(date)) %>% 
  #remove late April 2021 dates: 
  filter(!date > "2021-04-27") %>% 
  #There were two files for november in 2014, get rid of the bad ones: 
  filter(!(is.na(burn_day) & date <= "2014-11-30" & date >= "2014-11-01")) %>% 
  #Get rid of air basins that are designated with normal burn day designations: 
  filter(!air_basin %in% c("San Joaquin Valley Authorized VOC / PM10 / Nox", 
                           "San Joaquin Valley Authorized VOC / PM10", 
                           "San Joaquin Valley Authorized VOC / PM10 / NOx", 
                           "San Joaquin Valley Authorized Agriculture VOC / PM10", 
                           "Bay Area Fall / Spring Tule Burn Allocation (Acres)", 
                           "*Elevation Other Than 3,000 Feet (1,000's of Feet)", 
                           "Sacramento Valley Low", "SACRAMENTO VALLEY LOW")) %>% 
  mutate(air_basin = if_else(air_basin %in% c("Great Basin Valleys North (Test effective May 19th)", 
                                              "Great Basin Valleys North(Alpine, Mono)", 
                                              "Great Basin Valleys North"), 
                             "Great Basin Valleys North", air_basin)) %>% 
  mutate(air_basin = if_else(air_basin %in% c("Great Basin Valleys South (Test effective May 19th)",
                                              "Great Basin Valleys South(Inyo)",
                                              "Great Basin Valleys South"), 
                             "Great Basin Valleys South", air_basin))


unique(carb$burn_day)
unique(carb$air_basin)
#Some of the CARB air basins have names that are written differently at different
  #times...I don't know if it's worth it to change them all here or to wait
  # to see which ones I need based on the RAWS stations.  I will change the 
  # Great Basin Valleys since the Markleeville RAWS is there 



#### Save CARB burn day designation for all air basins ####

write.csv(carb, "InProcessData/carb.csv", row.names = FALSE)







#### Get CARB air basin for each RAWS station ####


#Bring in CARB shapefile

air_basin_shp <- st_read("RawData/CARB_ag_burn_days/CaAirBasin.shp")
plot(st_geometry(air_basin_shp))

air_basin_poly = st_union(air_basin_shp)
plot(st_geometry(air_basin_poly))

# Have list of quality controlled RAWS stations from CEFA (https://cefa.dri.edu/raws/)
  # This includes lat/long and elevation data for each station. I will try to 
  # use this to pull lat/long (rather than the hand enterred lat/long I was
  # using before): 

raws_stations <- read_excel("RawData/RAWSfw13list.xlsx", sheet = "QC RAWS")

#turn into sf object using lat/long
raws_stations <- st_as_sf(raws_stations, coords = c("LonDegrees", "LatDegrees"), 
           remove = FALSE,
           crs = 4326) %>% 
  #Match CRS to air basin shapefile: 
  st_transform(., crs = st_crs(air_basin_shp)) %>% 
  # Select only RAWS stations in CA: 
  # get vector indicating whether management area intersects fire: 
  mutate(raws_in_ca = as.vector(st_intersects(., air_basin_poly, sparse = FALSE))) %>%
  #filter FACTS to those that intersect 2020 fire perimeters of interest
  filter(raws_in_ca %in% c("TRUE")) %>% 
  dplyr::select(-raws_in_ca)

plot(st_geometry(raws_stations))

# #Bring in my RAWS notes file (I put lat/long in there)
# 
# raws_notes <- read_excel("RawData/district_raws_info.xlsx") %>% 
#   filter(!is.na(RAWS_Name)) %>% 
#   #filter to Klamath and lake tahoe for now: 
#   filter(forest %in% c("Klamath National Forest", "Lake Tahoe Basin")) %>% 
#   #remove periods from lat/long
#   mutate(Lat = str_replace_all(Lat, "\\.", "")) %>% 
#   mutate(Long = str_replace_all(Long, "\\.", "")) %>% 
#   #separate the degrees, minutes and seconds: 
#   separate(Lat, paste("lat",c("d","m","s"), sep="_") ) %>%
#   separate(Long, paste("long",c("d","m","s"), sep="_" ) ) %>%
#   #change to numeric
#   mutate(across(.cols = c(lat_d, lat_m, lat_s, long_d, long_m, long_s), as.numeric)) %>% 
#   mutate(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
#             long_dec= -1 * (long_d + long_m/60 + long_s/60^2)) %>% 
#   select(-c(lat_d, lat_m, lat_s, long_d, long_m, long_s)) %>% 
#   st_as_sf(., coords = c("long_dec", "lat_dec"), 
#            remove = FALSE,
#            crs = 4326) 


#Check it: 
ggplot() +
  geom_sf(data = air_basin_poly) +
  geom_sf(data = raws_stations)


# Join information from RAWS (lat/long/elev) to info from CARB: 
raws_stations_extract <- raws_stations %>% 
  st_join(., air_basin_shp, left = TRUE) %>% 
  as.data.frame() %>% 
  rename(air_basin = NAME, 
         RAWS_ID = StationID, 
         RAWS_Name = Name) %>% 
  select(RAWS_ID, RAWS_Name, Elevation, LatDegrees, LonDegrees, air_basin) 


write.csv(raws_stations_extract, "InProcessData/raws_carb_stations.csv", row.names = FALSE)
