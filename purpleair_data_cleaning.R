# Load libraries
library(tidyverse)
library(lubridate)


# Load data
file_path <- c("C:/Users/ethan.walker/Box/Ethan Walker UM/R/wildfire_data_analysis/")

um_data_initial <- read_csv(paste0(file_path, "input/um_purpleair_data.csv"))


# Initial data cleaning
um_data_clean1 <- um_data_initial %>% 
   # format date and PM vars
   mutate(datetime = ymd_hms(created_at),
          pm1_cf = as.numeric(`PM1.0_CF1_ug/m3`),
          pm2.5_cf = as.numeric(`PM2.5_CF1_ug/m3`),
          pm2.5_atm = as.numeric(`PM2.5_ATM_ug/m3`),
          pm10_cf = as.numeric(`PM10.0_CF1_ug/m3`)) %>% 
   select(entry_id, datetime, pm2.5_cf, pm2.5_atm, pm1_cf, pm10_cf,
          UptimeMinutes:`Humidity_%`) %>% 
   # separate datetime into vars for date and time
   separate(datetime, into = c("date", "time"), sep = " ", remove = FALSE) %>% 
   # separate time into vars for hour/min/sec
   separate(time, into = c("hour", "minute", "second"), sep = ":", remove = FALSE) %>% 
   # format date var
   mutate(date = ymd(date)) %>% 
   rename_all(tolower) %>% 
   arrange(datetime)


# Check for duplicate lines in data by using `uptimeminutes` var
um_data_duplicate_entry_check <- um_data_clean1 %>% 
   arrange(datetime) %>% 
   mutate(duplicate_entry = if_else(uptimeminutes == lag(uptimeminutes), 1, 0),
          duplicate_entry = if_else(lead(duplicate_entry) == 1, 1, duplicate_entry)) %>% 
   # filter out instances with duplicate entries
   filter(duplicate_entry == 1) %>% 
   arrange(datetime)
view(um_data_duplicate_entry_check)


# Remove duplicate lines by filtering for distinct datetimes
um_data_clean2 <- um_data_clean1 %>% 
   distinct(datetime, .keep_all = TRUE)


# Check for gaps > 2 min in time sequence data by using `uptimeminutes` var
um_data_gap_check <- um_data_clean2 %>% 
   arrange(datetime) %>% 
   mutate(datetime_gap = if_else(uptimeminutes - lag(uptimeminutes) > 2, 1, 0),
          datetime_gap = if_else(lead(datetime_gap) == 1, 1, datetime_gap)) %>% 
   filter(datetime_gap == 1)
view(um_data_gap_check)


# Filter for lines that have higher percent differences in CF vs ATM PM2.5
um_data_pm_diff_check <- um_data_clean2 %>% 
   mutate(pm2.5_diff = pm2.5_cf - pm2.5_atm,
          pm2.5_perc_diff = pm2.5_diff/pm2.5_cf*100,
          # Specify percent difference cutoff below
          pm2.5_high_perc_diff = if_else(pm2.5_perc_diff > 25, 1, 0)) %>% 
   filter(pm2.5_high_perc_diff == 1)
view(um_data_pm_diff_check)


# Try to fix issue of high perc diff resulting from low PM values
# Round small values (<1ug/m3) in the raw data so you don't end up with small means
um_data_clean3 <- um_data_clean2 %>% 
   mutate(pm2.5_atm_new = if_else(pm2.5_atm < 0.5, 0, pm2.5_atm),
          pm2.5_atm_new = if_else(pm2.5_atm >= 0.5 & pm2.5_atm < 1, 1, pm2.5_atm_new),
          pm2.5_cf_new = if_else(pm2.5_cf < 0.5, 0, pm2.5_cf),
          pm2.5_cf_new = if_else(pm2.5_cf >= 0.5 & pm2.5_cf < 1, 1, pm2.5_cf_new)) %>% 
   select(-pm2.5_cf, -pm2.5_atm)
   


# Basic summary stats for raw, 2-min data
um_data_pm_summary <- um_data_clean3 %>% 
   summarize("Mean PM2.5" = mean(pm2.5_atm_new, na.rm = TRUE),
             "N" = n(),
             "SD PM2.5" = sd(pm2.5_atm_new, na.rm = TRUE),
             "Min PM2.5" = min(pm2.5_atm_new, na.rm = TRUE),
             "Median PM2.5" = median(pm2.5_atm_new, na.rm = TRUE),
             "Max PM2.5" = max(pm2.5_atm_new, na.rm = TRUE))
um_data_pm_summary

# Basic summary stats for raw, 2-min data by month
um_data_pm_month_summary <- um_data_clean3 %>% 
   mutate(pm_month = lubridate::month(date, label = TRUE)) %>% 
   group_by(pm_month) %>% 
   summarize("Mean PM2.5" = mean(pm2.5_atm_new, na.rm = TRUE),
             "N" = n(),
             "SD PM2.5" = sd(pm2.5_atm_new, na.rm = TRUE),
             "Min PM2.5" = min(pm2.5_atm_new, na.rm = TRUE),
             "Median PM2.5" = median(pm2.5_atm_new, na.rm = TRUE),
             "Max PM2.5" = max(pm2.5_atm_new, na.rm = TRUE))
um_data_pm_month_summary


# Calculate means for daily and hourly data
um_data_pm_means <- um_data_clean3 %>% 
   arrange(datetime) %>% 
   group_by(date, hour) %>% 
   mutate(pm2.5_atm_mean_hour = mean(pm2.5_atm_new, na.rm = TRUE),
          pm2.5_cf_mean_hour = mean(pm2.5_cf_new, na.rm = TRUE),
          temp_f_mean_hour = mean(temperature_f, na.rm = TRUE),
          humidity_mean_hour = mean(`humidity_%`, na.rm = TRUE)) %>% 
   group_by(date) %>% 
   mutate(pm2.5_atm_mean_daily = mean(pm2.5_atm_new, na.rm = TRUE),
          pm2.5_cf_mean_daily = mean(pm2.5_cf_new, na.rm = TRUE),
          temp_f_mean_daily = mean(temperature_f, na.rm = TRUE),
          humidity_mean_daily = mean(`humidity_%`, na.rm = TRUE)) %>% 
   ungroup() 


# Select distinct obersvation for each date/hour and pull relevant vars
um_data_pm_hourly_means <- um_data_pm_means %>% 
   distinct(date, hour, .keep_all = TRUE) %>% 
   select(date, hour, pm2.5_atm_mean_hour, pm2.5_cf_mean_hour,
          temp_f_mean_hour, humidity_mean_hour) %>% 
   arrange(date, hour)


# Select distinct obersvation for each date and pull relevant vars
um_data_pm_daily_means <- um_data_pm_means %>% 
   distinct(date, .keep_all = TRUE) %>% 
   select(date, pm2.5_atm_mean_daily, pm2.5_cf_mean_daily,
          temp_f_mean_daily, humidity_mean_daily) %>% 
   arrange(date)



# save data as CSV and RDS for future use
write_rds(um_data_pm_hourly_means, paste0(file_path, "Output/um_data_pm_hourly_means.rds"))
write_csv(um_data_pm_hourly_means, paste0(file_path, "Output/um_data_pm_hourly_means.csv"), na = " ")

write_rds(um_data_pm_daily_means, paste0(file_path, "Output/um_data_pm_daily_means.rds"))
write_csv(um_data_pm_daily_means, paste0(file_path, "Output/um_data_pm_daily_means.csv"), na = " ")
