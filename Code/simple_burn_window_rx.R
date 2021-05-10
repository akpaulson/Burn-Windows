#### Description ####

#Bring together all of the RAWS data, carb burn day designation, and 
# national preparedness designations. Prep data for simple burn window analyses. 

#### Packages ####

library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)



#### Data ####


#RAWS data: Process data to get 1/0 if in prescription (simple BW)

raws <- read.csv("InProcessData/raws_klamath.csv", stringsAsFactors = FALSE)


#Pull out columns of interest (RH, FM10, wind gusts for simple burn window)
raws_rx <- raws %>% 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, DATE, 
         Time, RH, GSpd, FM10, SFlag) %>% 
  mutate(DATE = ymd(DATE)) %>% 
  rename(date = DATE) %>% 
  group_by(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, date) %>% 
  #Calc min RH, min fuel moisture 10, and max gust speed for each day: 
  summarize(min_rh = min(RH), 
            min_fm10 = min(FM10), 
            max_gspd = max(GSpd), 
            .groups = "drop") %>% 
  #Now turn into binary designation if in Rx for simple burn window or now
  mutate(bin_rh = if_else(min_rh >= 20 & min_rh <= 50, 1, 0), 
         bin_fm10 = if_else(min_fm10 >= 7 & min_fm10 <= 20, 1, 0), 
         bin_gspd = if_else(max_gspd < 25, 1, 0), 
         inRx = if_else(bin_rh == 1 & bin_fm10 == 1 & bin_gspd ==1, 1, 0))


# California Air Resources Board Burn Day Data: 

carb <- read.csv("InProcessData/carb.csv", stringsAsFactors = FALSE) %>% 
  mutate(date = ymd(date)) %>% 
  select(-month, -year, -day) %>% 
  mutate(air_basin = tolower(air_basin))

raws_carb_stations <- read.csv("InProcessData/raws_carb_stations.csv", 
                               stringsAsFactors = FALSE) %>% 
  rename(RAWS_station_nmbr = RAWS_ID, 
         RAWS_station = RAWS_Name) %>% 
  mutate(air_basin = tolower(air_basin)) %>% 
  mutate(RAWS_station = str_to_title(RAWS_station)) %>% 
  # some of the RAWS stations are listed differently here than in my dataset: 
  mutate(RAWS_station = if_else(RAWS_station == "Collins Baldy Lo", "Collins Baldy", 
                                RAWS_station)) %>% 
  mutate(RAWS_station = if_else(RAWS_station == "Blue Ridge (Knf)", "Blue Ridge", 
                                RAWS_station)) %>% 
  mutate(RAWS_station = if_else(RAWS_station == "Callahan #2", "Callahan 2", 
                                RAWS_station))


# Preparedness Level Data (through 2019, email out to Vaillant to get 2020/2021 data): 

nops <- read_excel("RawData/PL Levels_Randy.xlsx", 
                   sheet = "Original data", 
                   na = "n/a", 
                   col_types = c("date", rep("text", 8))) %>% 
  select(-c(`...2`, `...3`, `...7`, `...8`, `...9`)) %>% 
  rename(date = `...1`, 
         national_pl = `National PL`, 
         nops_pl = `NOPS PL`) %>% 
  mutate(date = ymd(date)) %>% 
  #For now, remove SOPS (only working with Northern stations at the moment...update later)
  select(-`SOPS PL`) %>% 
  #Calculate bin_pl according to Striplin criteria...if NOPS is less than 3 = 1, 
  # if NOPS is NA, fill in with National PL (<3 = 1)
  mutate(bin_national_pl = if_else(national_pl <3, 1, 0)) %>% 
  mutate(bin_nops_pl = if_else(nops_pl <3, 1, 0)) %>% 
  mutate(bin_nops_nat_pl = if_else(!is.na(bin_nops_pl), bin_nops_pl, bin_national_pl)) %>% 
  filter(!is.na(bin_nops_nat_pl)) %>% 
  select(date, bin_nops_nat_pl)




# Simple burn window data from Striplin Paper

striplin <- read_excel("RawData/040920masterFFP5SnowFlag=0.xlsx", sheet = "Master", 
                     na = c("", "NA")) %>% 
  mutate(Date = ymd(Date)) %>% 
  #Add in missing columns
  mutate(Forest = "Lake Tahoe Basin", 
         District = "Lake Tahoe Basin", 
         RAWS_station = "Baron/Markleeville", 
         snow_flag = "no", 
         RAWS_station_nmbr = NA, 
         Elevation = NA, 
         LatDegrees = NA, 
         LonDegrees = NA, 
         air_basin = NA) %>% 
  #rename columns to match the raws/carb dataset
  rename(date = Date, 
         min_rh = Rh, 
         min_fm10 = mBarFM, 
         max_gspd = Gs, 
         bin_rh = binRh, 
         bin_fm10 = BinFm, 
         bin_gspd = binWs, 
         burn_day = BD) %>% 
  #remove extraneous rows (extraneous for now)
  select(-c(Year, Month, Day, DayOfYr, Strk, StMax, NopsPL, NatPL, BinNaPl, PL, binBW)) %>% 
  #reorder rows: 
  select(c("Forest", "District", "RAWS_station_nmbr", "RAWS_station", 
          "snow_flag", "date", "min_rh", "min_fm10", "max_gspd", "bin_rh", 
          "bin_fm10", "bin_gspd", "inRx", "Elevation", "LatDegrees", "LonDegrees", 
          "air_basin", "burn_day", "binBD"), everything())
  




#### Join raws and carb data ####

simple_bw <- raws_rx %>% 
  left_join(raws_carb_stations) %>% 
  left_join(., carb, by = c("date", "air_basin"))

unique(simple_bw$RAWS_station[which(is.na(simple_bw$air_basin))])

unique(simple_bw$burn_day)

#There are a lot of different burn day designations, need to classify these as
  # burn days or not. 

simple_bw_rx <- simple_bw %>% 
  mutate(binBD = if_else(burn_day %in% c("G", "B", 
                                         "S", "Ab", "B^"), 1, 
                         if_else(burn_day %in% c("NB", "M", "F", 
                                                 "X", NA), 0, 
                                 NA_real_)))

#Notes: 
  # Not sure what "X" means
  # classifying Ab (amended decision, amended to a permissive burn day) as burn day
  # classifying B^ (burn day but restrictions requested) as a burn day
  # classifying marginal (M) and fair (F) as non-burn-days - as with Striplin

#Double checked and no NAs, so it looks like I've assigned designation correctly
  # at least for these Klamath sites. 


#### Add Striplin data and join simple burn window Rx data with preparedness level data ####

simple_bw_rx_b <- simple_bw_rx %>% 
  #add in the striplin data from his paper - Baron/Markleeville
  bind_rows(., striplin) %>%
  #Join in the planning level data: 
  left_join(., nops, by = "date") %>% 
  #remove NAs - these are 2020 and 2021 data - can't do BW for them yet. 
  filter(!is.na(bin_nops_nat_pl)) %>% 
  # Do we have a burn window?
  mutate(bin_bw = if_else(inRx == 1 & binBD == 1 & bin_nops_nat_pl == 1, 1, 0))

unique(simple_bw_rx_b$RAWS_station)



# Save Clean data that indicates which days in Rx for RAWS, NOPS, CARB: 

write.csv(simple_bw_rx_b, "CleanData/simple_burn_window_rx.csv", row.names = FALSE)




#### Prep data for simple burn windows - Figure 2 ####  


# Figure shows the frequency of the burn window and different components of 
#the burn window (in prescription, burn day, and preparedness level). 
# Frequency is based on the entire study period (1999 through 2019), showing 
# the proportion of a given date over that time period that was appropriate
# for prescribed fire. 



bw_bin <- simple_bw_rx_b %>% 
  mutate(day_of_yr = format(as.Date(date), "%d-%b")) %>% 
  select(-c(date, RAWS_station_nmbr, min_rh, min_fm10, max_gspd, bin_rh, bin_fm10, bin_gspd, 
            Elevation, LatDegrees, LonDegrees, air_basin, burn_day)) %>% 
  group_by(day_of_yr, Forest, District, RAWS_station, snow_flag) %>% 
  #count number of days within time period that were "1" for each variable
  summarize_all(list(count_yrs = ~sum(!is.na(.)), 
                     sum_rx = ~sum(., na.rm = TRUE))) %>% 
  # Calculate proportion of days in the period that were "1", all divided by
  # 21 except for Feb 29
  mutate(prop_inRx = inRx_sum_rx / inRx_count_yrs, 
         prop_BD = binBD_sum_rx / binBD_count_yrs, 
         prop_PL = bin_nops_nat_pl_sum_rx / bin_nops_nat_pl_count_yrs, 
         prop_BW = bin_bw_sum_rx / bin_bw_count_yrs) %>% 
  #Try to fix dates: 
 # mutate(DayOfYr_hyphen = str_replace_all(DayOfYr, " ", "-")) %>% 
  mutate(date = as.Date(paste("20-", day_of_yr, sep = ""), format = "%y-%d-%b")) %>% 
  arrange(date) 



#Try to plot: 
a <- bw_bin %>% 
  filter(Forest == "Klamath National Forest", 
         District == "Salmon Scott River Ranger District - Salmon River", 
         RAWS_station == "Blue Ridge", 
         snow_flag == "no")

ggplot(a, aes(x = date, y = prop_PL, group = 1)) +
  geom_line() +
  geom_line(aes(y = prop_BD), color = "#2c7bb6") +
  geom_line(aes(y = prop_inRx), color = "#d73027") +
  geom_area(aes(y = prop_BW), fill = "gray") +
  scale_x_date(date_labels = "%d %b", # change date labelling
               breaks = a$date[seq(1, length(a$date), by = 7)], 
               #only 7th day displayed
               expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Proportion of days 1999-2019")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x = element_blank())

#Save data for starting shiny app: 
figure2_data <- bw_bin %>% 
  select(Forest, District, RAWS_station, day_of_yr, snow_flag, date, prop_inRx, prop_BD, prop_PL, prop_BW)
write.csv(figure2_data, "ShinyApp_BurnWindow/BurnWindows/all_raws_fig2.csv", row.names = FALSE)




#### Prep Data for Striplin Figure 3 #####

# Figure 3 shows the percentage of all days in each month that were burn windows

# Show the mean +/- se of 
# burn window days in each month across the study period, removing days where 
  # there was an NA in one of the variables for designating the burn window

monthly_bw2 <- simple_bw_rx_b %>% 
  select(date, Forest, District, RAWS_station, snow_flag, bin_bw) %>% 
  group_by(year = year(date), month = month(date, label = TRUE, abbr = TRUE), 
           Forest, District, RAWS_station, snow_flag) %>% 
  summarize(total_days = sum(!is.na(bin_bw)), 
                     bw_days = sum(bin_bw, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(percent = bw_days / total_days * 100) %>% 
  group_by(month, Forest, District, RAWS_station, snow_flag) %>% 
  summarize(mean_percent = mean(percent), 
            se_percent = sd(percent)/sqrt(n()), 
            .groups = "drop")


dat <- monthly_bw2 %>% 
  filter(Forest == "Klamath National Forest", 
         District == "Salmon Scott River Ranger District - Salmon River", 
         RAWS_station == "Blue Ridge", 
         snow_flag == "no") %>% 
  #This is to try to control the y-axis limits for a given station:
  mutate(max = max(mean_percent)+max(se_percent)+7)

ggplot(dat, aes(x = month, y = mean_percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  geom_text(aes(label = round(mean_percent, 0), vjust = -4.5)) +
  scale_y_continuous(limits = c(0, (dat$max), expand = c(0,0))) +
  geom_errorbar(aes(ymin = mean_percent - se_percent, 
                    ymax = mean_percent + se_percent), width = 0.2) +
  labs(y = "Percent days (%)") +
  theme_classic() +
  theme(axis.title.x = element_blank())


#Save data for starting shiny app: 
write.csv(monthly_bw2, "ShinyApp_BurnWindow/BurnWindows/all_raws_fig3_data.csv", row.names = FALSE)






#### Striplin Figure 4 #####

# This figure shows the mean number of burn windows (2-3 day, 4-5 day, 
# and 6 + days) each month from 1999 to 2019.  It's based on the 
# column "StMax" in the master dataset that counts how many days
# in a row a burn window occurred.  The 6-day + burn windows are
# not broken down into smaller burn window periods (e.g. 2-3 days), 
# so the burn window periods are exclusive.  Single day burn
# windows are not accounted for in this figure.  

#Randy had created his streak column in Excel. Need to recreate this in R. 

streak <- simple_bw_rx_b %>% 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, date, bin_bw) %>% 
  arrange(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, date) %>% 
  group_by(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag) %>% 
  #Calculate streaks of 1s and 0s: 
  mutate(streak = sequence(rle(bin_bw)$lengths)) %>% 
  #if not a burn window, change streak to 0. 
  mutate(streak = ifelse(bin_bw == 0, 0, streak)) %>% 
  #Next, need column STMax that tells us the max streak value in streak, everything
    #else should be NA
  mutate(streak_max = if_else(streak > 0 & lead(streak, n = 1L) == 0, streak, NA_real_))

streak2 <- streak %>% 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, 
         date, streak_max) %>% 
  filter(!is.na(streak_max)) %>% 
  filter(streak_max > 1) %>% 
  mutate(streak_max_chr = case_when(
    streak_max == 2 ~ "2 to 3 days", 
    streak_max == 3 ~ "2 to 3 days", 
    streak_max == 4 ~ "4 to 5 days", 
    streak_max == 5 ~ "4 to 5 days", 
    streak_max >= 6 ~ "6 days or more", 
    TRUE ~ NA_character_)) %>% 
  group_by(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, 
           year = year(date), month = month(date, label = TRUE, abbr = TRUE),
           streak_max_chr) %>% 
  summarize(count = n(), 
            .groups = "drop") %>% # Get initial number of burn windows in each 
  # month/year for each burn year length 
  complete(month, year, 
           nesting(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag, 
                   streak_max_chr), fill = list(count = 0)) %>% 
  group_by(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag,
           month, streak_max_chr) %>% 
  summarize(mean_bw = mean(count), 
            se_bw = sd(count) / sqrt(n()),
            .groups = "drop")

dat <- streak2 %>% 
  filter(Forest == "Klamath National Forest", 
         District == "Salmon Scott River Ranger District - Salmon River", 
         RAWS_station == "Blue Ridge", 
         snow_flag == "no")

ggplot(dat, aes(x = month, y = mean_bw, 
                    ymin = mean_bw - se_bw, 
                    ymax = mean_bw + se_bw, 
                    fill = streak_max_chr)) +
  geom_bar(stat = "identity",  
           color = "black", 
           position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("lightgray", "gray60", "black")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Mean number of muliple-day \n burn windows") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank())

#Save data for starting shiny app: 


write.csv(streak2, "ShinyApp_BurnWindow/BurnWindows/all_raws_fig4_data.csv", row.names = FALSE)





#### Prep Data for Striplin Figure 5 #####

# This figure shows the number of burn days for each month across all 
# years of the study. This uses the binBW column plus the date 
# columns

bws <- simple_bw_rx_b %>% 
  select(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag,
         date, bin_bw) %>% 
  filter(bin_bw == 1) %>% 
  group_by(Forest, District, RAWS_station_nmbr, RAWS_station, snow_flag,
           year = year(date), 
           month = month(date, label = TRUE, abbr = TRUE)) %>% 
  summarize(bw_count = sum(bin_bw), 
            .groups = "drop") %>% 
  complete(month, nesting(Forest, District, RAWS_station_nmbr, 
                                RAWS_station, snow_flag, year), 
           fill = list(bw_count = 0))
#Almost there, but it is backfilling (so Blue ridge shouldn't start until May 2001, 
  #but it is backfilling to January?) Maybe that's fine because they'll show 0 anyway?

dat <- bws %>% 
  filter(Forest == "Klamath National Forest", 
         District == "Salmon Scott River Ranger District - Salmon River", 
         RAWS_station == "Blue Ridge", 
         snow_flag == "no")



ggplot(dat, aes(x = year, y = bw_count)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~month, nrow = 4, ncol = 3) +
  scale_x_continuous(breaks = seq(1999, 2019, by = 2),
                     expand = c(0,0)) +
  labs(x = "Year", y = "Number of Days") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,  vjust=0.5, hjust=1), 
        axis.text = element_text(size = 10))


#Save data for starting shiny app: 
write.csv(bws, "ShinyApp_BurnWindow/BurnWindows/all_raws_fig5_data.csv", row.names = FALSE)

