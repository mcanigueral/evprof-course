library(dplyr)
library(lubridate)
library(tidyr)

# Each section created with Ctrl+Shift+R

# Reading data from JSON ------------------------------------------------------------

acn_json <- jsonlite::fromJSON("data/acndata_sessions_jpl.json")
acn_sessions <- acn_json[["_items"]]

# Data frame
acn_sessions
# Tibble
as_tibble(acn_sessions)


# Convert to datetime
acn_sessions$connectionTime[1]
acn_sessions[["connectionTime"]][1]
acn_sessions[1, "connectionTime"]

# From lubridate
dmy_hms(acn_sessions$connectionTime[1])
dmy_hms(acn_sessions$connectionTime[1], tz = "America/Los_Angeles")


# Charging point ID
unique(acn_sessions$siteID)
unique(acn_sessions$spaceID)

# Tidy data set
acn_sessions_tb <- acn_sessions %>%
  as_tibble() %>%
  separate(connectionTime, sep=',', into = c('Weekday', 'connectionDateTime')) %>%
  mutate(
    connectionDateTime = dmy_hms(connectionDateTime) %>% with_tz("America/Los_Angeles"),
    disconnectionDateTime = dmy_hms(disconnectTime) %>% with_tz("America/Los_Angeles"),
    chargingEndDateTime = dmy_hms(doneChargingTime) %>% with_tz("America/Los_Angeles")
  ) %>%
  rename(
    id = `_id`,
    Energy = kWhDelivered
  ) %>%
  select(-c(clusterID, userInputs, disconnectTime, doneChargingTime, timezone)) %>%
  select(spaceID, everything())




# Another data set --------------------------------------------------------

sessions_profiles <- readr::read_csv("data/sessions_profiles.csv")
sessions_profiles


# Types of profiles
unique(sessions_profiles$Profile)


# Data set preparation
start_time_1 <- sessions_profiles$ConnectionStartDateTime[1]
start_time_1

# - timezone conversions (`readr` saves everything in UTC)
start_time_1
with_tz(start_time_1, tzone = "Europe/Amsterdam")

# - Date times rounded to 15 minutes
start_time_1
round_date(start_time_1, unit = "15 minutes")

# - Rest of variables: ConnectionEndDateTime, ChargingStartDateTime, ChargingHours, Power
#     - ConnectionEndDateTime
start_time_1
connection_hours_1 <- sessions_profiles$ConnectionHours[1]

start_time_1 +
  hours(connection_hours_1 %/% 1) +
  minutes(round((connection_hours_1 %% 1)*60))

#     - ChargingHours
end_time_1 <- sessions_profiles$ChargingEndDateTime[1]
end_time_1 - start_time_1
as.numeric(end_time_1 - start_time_1)
as.numeric(end_time_1 - start_time_1, units = "hours")


# Everything together
# Create a new data set with the following variables:
#   Profile, ConnectionStartDateTime, ConnectionEndDateTime,
#   ChargingStartDateTime, ChargingEndDateTime,
#   ConnectionHours, ChargingHours,
#   Power, Energy
sessions_profiles2 <- sessions_profiles %>%
  mutate(

  )




# Example of filtering some rows
sessions_profiles2 <- sessions_profiles2 %>%
  filter(
    ConnectionHours > 0,
    ChargingHours > 0,
    Energy > 0
  )

# More examples
sessions_profiles2 %>%
  filter(
    !(Profile %in% c("Shortstay", "Visit"))
  )



# Data set summary (nicer with tibbles than data.frames)
sessions_profiles2 %>% summary()

# Own summary
sessions_summary <- sessions_profiles2 %>%
  mutate(
    date = floor_date(ConnectionStartDateTime, "day")
  ) %>%
  group_by(date, Profile) %>%
  summarise(
    n = n(),
    max_kWh = max(Energy),
    max_kW = max(Power),
    avg_conn = mean(ConnectionHours)
  )


# Calculate the total energy consumed by week


# Calculate the total energy consumed by week and user profile






# Fill gaps ---------------------------------------------------------------

profiles_n_sessions <- sessions_summary %>%
  select(date, Profile, n) %>%
  pivot_wider(names_from = Profile, values_from = n)

profiles_n_sessions

# Fill the gaps
profiles_n_sessions %>%
  replace_na(list(Dinner = 0, Home = 0, Pillow = 200, Worktime = 1000))
# replace(is.na(.), 0)
# mutate_all(replace_na, 0)
# mutate_if(is.numeric, replace_na, 0)
# mutate_all(`*`, 100)
# mutate_all(`+`, 100)




