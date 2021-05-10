#### Description ####

#This code imports all of the CLEANED RAWS data exported after basic QA/QC in 
# Fire Family Plus version 5.0 

# Process for getting the RAWS data / Cleaning it: 

# 1. Download from https://fam.nwcg.gov/fam-web/
    # Hourly data since 2014, earlier than that it is only daily obs
# 2. Download both earlier daily observations and gap-filled observations from 
      # The program for Climate, Ecosystems and Fire Applications 
      # (https://cefa.dri.edu/raws/)
# 3. Conduct basic QA / QC with Fire Family Plus version 5.0. QA/QC included: 
  # a) Import data in this order: FAMWEB, Daily CEFA, daily gap-filled CEFA, do not
    # overwrite data at any step. 
  # b) Change fuel model to "timber" fuel model so that fuel indices are 
    # calculated correctly (e.g. ERC)
  # c) Look for large gaps in the RAWS data using the Climatology/Stats Graph - Count
      # I kept track of large gaps, but I haven't done anything additional with
      # this information. Because the proportions I calculate are based on the 
      # number of possible days that we have data for (not number of total possible
      # days), I think it should be okay to ignore them for now. 
  # d) Looking at the "all" observations, note when the hourly observations began.
  # e) Look at all of the data and conduct QA/QC. This typically means removing
      # obviously outlying temperature, precipitation, or wind speed observations. 
      # To do so, I would compare observations to nearby hourly / daily observations. 
      # Temps >115 or <-5, gusts or wind speeds >100 mph, and hourly precip >5 in
      # were typically (but not always) removed. I tried to crosscheck with 
      # "O" observations and nearby observations (nearby in date an time). There
      # is probably more QA/QC that can be done, but this should be sufficient
      # for our purposes. 
# 4. Export Hourly Listing Record from FFP
# 5. Change snow flag to 0 for all observations and recalculate/export hourly
  # listing for fuel moistures, etc. without snow flag considered. 
# 6. Import data here and put in format for burn window analyses. 


#### Packages #### 

library(readr)
library(data.table)
library(dplyr)
library(lubridate)


#### Load RAWS Data ####

# Structure of the RAWS output should be the same for each RAWS station, hopefully. 
# Pull out the header row associated with all RAWS stations (hopefully consistent): 
hdr <- read.table("RAWS/station_data/Klamath/hourly_listing/blueridge_040203_hourly.txt", 
                  skip = 27, nrow = 1, as.is = TRUE)



#### Blue ridge ####

# Read in tables (skip 30 lines to skip header info., 
  # fill = TRUE accounts for missing columns)

#Blue Ridge: 
raws_040203 <- read.table("RAWS/station_data/Klamath/hourly_listing/blueridge_040203_hourly.txt", 
                   skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040203, 
         RAWS_station = "Blue Ridge", 
         snow_flag = "yes", 
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2001/05/22"))


raws_040203_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/blueridge_040203_hourly_nosnowflag.txt", 
                   skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040203, 
         RAWS_station = "Blue Ridge", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2001/05/22"))


#### Sawyer's Bar: ####
raws_040222 <- read.table("RAWS/station_data/Klamath/hourly_listing/sawyersbar_040222_hourly.txt", 
                   skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040222, 
         RAWS_station = "Sawyers Bar", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/04/10"))

raws_040222_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/sawyersbar_040222_hourly_nosnowflag.txt", 
                   skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040222, 
         RAWS_station = "Sawyers Bar", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/04/10"))





#### Somes Bar ####

raws_040231_hord <- read.table("RAWS/station_data/Klamath/hourly_listing/somesbar_040231_hourly.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040231, 
         RAWS_station = "Somes Bar", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2004/03/16")) 


raws_040231_nosf_hord <- read.table("RAWS/station_data/Klamath/hourly_listing/somesbar_040231_hourly_nosnowflag.txt", 
                                    skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040231, 
         RAWS_station = "Somes Bar", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2004/03/16"))


raws_040231_ssrrd <- read.table("RAWS/station_data/Klamath/hourly_listing/somesbar_040231_hourly.txt", 
                                skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040231, 
         RAWS_station = "Somes Bar", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2004/03/16")) 


raws_040231_nosf_ssrrd <- read.table("RAWS/station_data/Klamath/hourly_listing/somesbar_040231_hourly_nosnowflag.txt", 
                                     skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040231, 
         RAWS_station = "Somes Bar", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2004/03/16"))





#### Collins Baldy ####
raws_040237_hord <- read.table("RAWS/station_data/Klamath/hourly_listing/collinsbaldy_040237_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040237, 
         RAWS_station = "Collins Baldy", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2002/07/10")) 


raws_040237_nosf_hord <- read.table("RAWS/station_data/Klamath/hourly_listing/collinsbaldy_040237_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040237, 
         RAWS_station = "Collins Baldy", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/04/10"))


raws_040237_ssrrd <- read.table("RAWS/station_data/Klamath/hourly_listing/collinsbaldy_040237_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040237, 
         RAWS_station = "Collins Baldy", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2002/07/10")) 


raws_040237_nosf_ssrrd <- read.table("RAWS/station_data/Klamath/hourly_listing/collinsbaldy_040237_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Salmon River",
         RAWS_station_nmbr = 040237, 
         RAWS_station = "Collins Baldy", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/04/10"))


#### Oak Knoll ####
raws_040218 <- read.table("RAWS/station_data/Klamath/hourly_listing/oakknoll_040218_hourly.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040218, 
         RAWS_station = "Oak Knoll", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/06/10")) 


raws_040218_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/oakknoll_040218_hourly_nosnowflag.txt", 
                                    skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Oak Knoll",
         RAWS_station_nmbr = 040218, 
         RAWS_station = "Oak Knoll", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/06/10"))




#### Slater Butte ####
raws_040225 <- read.table("RAWS/station_data/Klamath/hourly_listing/040225_SlaterButte_hourly.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Happy Camp",
         RAWS_station_nmbr = 040225, 
         RAWS_station = "Slater Butte", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/09/15")) %>% 
  #I think some of the gust speeds are messed up again. I contacted CEFA on 5/8/21. 
      # Just removing Gspd >100 from database here: 
  mutate(GSpd = if_else(GSpd > 100, NA_real_, GSpd))


raws_040225_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/040225_SlaterButte_hourly_nosnowflag.txt", 
                                    skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Happy Camp",
         RAWS_station_nmbr = 040225, 
         RAWS_station = "Slater Butte", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/09/15")) %>% 
  #I think some of the gust speeds are messed up again. I contacted CEFA on 5/8/21. 
  # Just removing Gspd >100 from database here: 
  mutate(GSpd = if_else(GSpd > 100, NA_real_, GSpd))



#### Dutch Indy ####

raws_040246 <- read.table("RAWS/station_data/Klamath/hourly_listing/dutchindy_040246_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Happy Camp",
         RAWS_station_nmbr = 040246, 
         RAWS_station = "Dutch Indy", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2010/01/01")) 


raws_040246_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/dutchindy_040246_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Happy Oak Ranger District  - Happy Camp",
         RAWS_station_nmbr = 040246, 
         RAWS_station = "Dutch Indy", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2010/01/01")) 



#### Juanita ####

raws_040240 <- read.table("RAWS/station_data/Klamath/hourly_listing/juanita_040240_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Goosenest Ranger District",
         RAWS_station_nmbr = 040240, 
         RAWS_station = "Juanita", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/06/26")) 


raws_040240_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/juanita_040240_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Goosenest Ranger District",
         RAWS_station_nmbr = 040240, 
         RAWS_station = "Juanita", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/06/26")) 



#### Van Bremmer ####

raws_040243 <- read.table("RAWS/station_data/Klamath/hourly_listing/vanbremmer_040243_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Goosenest Ranger District",
         RAWS_station_nmbr = 040243, 
         RAWS_station = "Van Bremmer", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/12/01")) 


raws_040243_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/vanbremmer_040243_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Goosenest Ranger District",
         RAWS_station_nmbr = 040243, 
         RAWS_station = "Van Bremmer", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1999/12/01")) 


#### Callahan 2 ####

raws_040245 <- read.table("RAWS/station_data/Klamath/hourly_listing/callahan2_040245_hourly.txt", 
                                skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Scott River",
         RAWS_station_nmbr = 040245, 
         RAWS_station = "Callahan 2", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/02/19")) 


raws_040245_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/callahan2_040245_hourly_nosnowflag.txt", 
                                     skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Scott River",
         RAWS_station_nmbr = 040245, 
         RAWS_station = "Callahan 2", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("2000/02/19"))





#### Quartz Hill ####

raws_040239 <- read.table("RAWS/station_data/Klamath/hourly_listing/quartzhill_040239_hourly.txt", 
                          skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Scott River",
         RAWS_station_nmbr = 040239, 
         RAWS_station = "Quartz Hill", 
         snow_flag = "yes",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1998/07/17")) 


raws_040239_nosf <- read.table("RAWS/station_data/Klamath/hourly_listing/quartzhill_040239_hourly_nosnowflag.txt", 
                               skip = 30, col.names = hdr, as.is = TRUE, fill = TRUE) %>% 
  mutate(Forest = "Klamath National Forest", 
         District = "Salmon Scott River Ranger District - Scott River",
         RAWS_station_nmbr = 040239, 
         RAWS_station = "Quartz Hill", 
         snow_flag = "no",
         #change date to date format: 
         DATE = mdy(DATE)) %>% 
  #reorganize columns: 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, everything()) %>% 
  #select only dates with hourly observations available: 
  filter(DATE >= as.Date("1998/07/17"))





#### Join all of the data together ####

raws <- rbind(raws_040203, raws_040203_nosf, 
              raws_040222, raws_040222_nosf, 
              raws_040231_hord, raws_040231_nosf_hord, 
              raws_040231_ssrrd, raws_040231_nosf_ssrrd, 
              raws_040237_hord, raws_040237_nosf_hord, 
              raws_040237_ssrrd, raws_040237_nosf_ssrrd, 
              raws_040218, raws_040218_nosf, 
              raws_040225, raws_040225_nosf, 
              raws_040246, raws_040246_nosf, 
              raws_040240, raws_040240_nosf, 
              raws_040243, raws_040243_nosf, 
              raws_040245, raws_040245_nosf, 
              raws_040239, raws_040239_nosf)

unique(raws$RAWS_station)

#save raw RAWS data: 
write.csv(raws, "InProcessData/raws.csv", row.names = FALSE)