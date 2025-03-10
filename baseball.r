```{r} library(baseballr) library(dplyr) library(ggplot2) library(reshape2) library(zoo)
```
```{r}
# updated functions, old function does not work
csv_from_url <- function(...){ data.table::fread(...)
} #-------------------------------------------------------------------- -----------------
make_baseballr_data <- function(df, type, timestamp){
out <- df %>% tidyr::as_tibble()
class(out) <- c("baseballr_data","tbl_df","tbl","data.table","data.frame")
attr(out,"baseballr_timestamp") <- timestamp attr(out,"baseballr_type") <- type return(out)
}
# @title
statcast_search <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
playerid = NULL,
player_type = "batter", ...) {
# Check for other user errors.
if (start_date <= "2015-03-01") { # March 1, 2015 was the first date of Spring Training.
message("Some metrics such as Exit Velocity and Batted Ball Events have only been compiled since 2015.")
}
if (start_date < "2008-03-25") { # March 25, 2008 was the first date of the 2008 season.
stop("The data are limited to the 2008 MLB season and after.")
return(NULL) }
if (start_date == Sys.Date()) {
 message("The data are collected daily at 3 a.m. Some of today's games may not be included.")
}
if (start_date > as.Date(end_date)) {
stop("The start date is later than the end date.")
return(NULL) }
playerid_var <- ifelse(player_type == "pitcher", "pitchers_lookup%5B%5D",
"batters_lookup%5B%5D")
vars <- tibble::tribble( ~var, ~value,
"all", "true",
"hfPT", "",
"hfAB", "",
"hfBBT", "",
"hfPR", "",
"hfZ", "",
"stadium", "",
"hfBBL", "",
"hfNewZones", "",
"hfGT", "R%7CPO%7CS%7C&hfC",
"hfSea", paste0(lubridate::year(start_date), "%7C"), "hfSit", "",
"hfOuts", "",
"opponent", "", "pitcher_throws", "", "batter_stands", "", "hfSA", "",
"player_type", player_type, "hfInfield", "",
"team", "",
"position", "",
"hfOutfield", "",
"hfRO", "",
"home_road", "",
playerid_var, ifelse(is.null(playerid), "",
as.character(playerid)),
"game_date_gt", as.character(start_date), "game_date_lt", as.character(end_date), "hfFlag", "",
"hfPull", "",
"metric_1", "",

 "hfInn", "",
"min_pitches", "0",
"min_results", "0",
"group_by", "name",
"sort_col", "pitches", "player_event_sort", "h_launch_speed", "sort_order", "desc",
"min_abs", "0",
"type", "details") %>%
dplyr::mutate(pairs = paste0(.data$var, "=", .data$value))
if (is.null(playerid)) {
# message("No playerid specified. Collecting data for all
batters/pitchers.") vars <- vars %>%
dplyr::filter(!grepl("lookup", .data$var)) }
url_vars <- paste0(vars$pairs, collapse = "&")
url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?", url_vars)
# message(url)
# Do a try/catch to show errors that the user may encounter while downloading.
tryCatch( {
suppressMessages( suppressWarnings(
payload <- csv_from_url(url, encoding ="UTF-8") )
) },
error = function(cond) { message(cond)
stop("No payload acquired")
},
# this will never run?? warning = function(cond) {
message(cond) }
)
# returns 0 rows on failure but > 1 columns if (nrow(payload) > 1) {

 names(payload) <- c("pitch_type", "game_date", "release_speed", "release_pos_x",
"pitcher", "events", "spin_rate_deprecated", "game_type", "stand", "hit_location",
"pfx_x", "pfx_z", "on_1b", "outs_when_up", "tfs_deprecated", "sv_id", "vx0",
"release_pos_z", "player_name", "batter",
"description", "spin_dir", "break_angle_deprecated", "break_length_deprecated", "zone", "des",
"p_throws", "home_team", "away_team", "type", "bb_type", "balls", "strikes", "game_year", "plate_x", "plate_z", "on_3b", "on_2b", "inning", "inning_topbot", "hc_x", "hc_y", "tfs_zulu_deprecated", "fielder_2", "umpire",
"vy0", "vz0", "ax", "ay", "az", "sz_top", "sz_bot", "hit_distance_sc",
"launch_speed", "launch_angle", "effective_speed", "release_spin_rate",
"release_extension", "game_pk", "pitcher_1",
"fielder_2_1",
"fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7",
"fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
"estimated_woba_using_speedangle", "woba_value", "woba_denom",
"babip_value", "iso_value", "launch_speed_angle", "at_bat_number",
"pitch_number", "pitch_name", "home_score", "away_score", "bat_score",
"fld_score", "post_away_score", "post_home_score", "post_bat_score",
"post_fld_score", "if_fielding_alignment",
"of_fielding_alignment",
"spin_axis", "delta_home_win_exp", "delta_run_exp", "bat_speed", "swing_length")
payload <- process_statcast_payload(payload) %>% make_baseballr_data("MLB Baseball Savant Statcast Search data
from baseballsavant.mlb.com",Sys.time()) return(payload)

 } else {
warning("No valid data found")
# (somewhere within the statcast_search function before the payload is searched for)
colos <- c("pitch_type", "game_date",
"release_speed", "release_pos_x", "release_pos_z", "player_name", "batter", "pitcher",
"events", "description", "spin_dir", "spin_rate_deprecated", "break_angle_deprecated", "break_length_deprecated", "zone", "des", "game_type", "stand", "p_throws",
"home_team", "away_team", "type", "hit_location", "bb_type", "balls", "strikes", "game_year", "pfx_x",
"pfx_z", "plate_x", "plate_z",
"on_3b", "on_2b", "on_1b", "outs_when_up", "inning", "inning_topbot", "hc_x",
"hc_y", "tfs_deprecated", "tfs_zulu_deprecated", "fielder_2", "umpire", "sv_id",
"vx0", "vy0", "vz0", "ax",
"ay", "az", "sz_top", "sz_bot",
"hit_distance_sc", "launch_speed", "launch_angle", "effective_speed", "release_spin_rate", "release_extension", "game_pk", "pitcher_1", "fielder_2_1", "fielder_3", "fielder_4", "fielder_5", "fielder_6", "fielder_7", "fielder_8", "fielder_9", "release_pos_y", "estimated_ba_using_speedangle",
"estimated_woba_using_speedangle",
"woba_value", "woba_denom", "babip_value",
"iso_value", "launch_speed_angle", "at_bat_number", "pitch_number", "pitch_name", "home_score", "away_score", "bat_score", "fld_score", "post_away_score", "post_home_score", "post_bat_score", "post_fld_score",
"if_fielding_alignment",
"of_fielding_alignment", "spin_axis",
"delta_home_win_exp", "delta_run_exp") colNumber <- ncol(payload)
if(length(colos) != colNumber){
newCols <- paste("newStat", 1:(length(colos) - colNumber)) colos <- c(colos, newCols)

 message("New stats detected! baseballr will be updated soon to properly identify these stats")
}
# payload is acquired somewhere in here
# when the payload columns need to be named: names(payload) <- colos
payload <- payload %>%
make_baseballr_data("MLB Baseball Savant Statcast Search data from
baseballsavant.mlb.com",Sys.time()) return(payload)
} }
statcast_search.default <- function(start_date = Sys.Date() - 1, end_date = Sys.Date(),
playerid = NULL,
player_type = "batter", ...) {
message(paste0(start_date, " is not a date. Attempting to coerce..."))
start_Date <- as.Date(start_date)
tryCatch( {
end_Date <- as.Date(end_date) },
warning = function(cond) {
message(paste0(end_date, " was not coercible into a date. Using
today."))
end_Date <- Sys.Date()
message("Original warning message:")
message(cond) }
)
statcast_search(start_Date, end_Date,
playerid, player_type, ...)
}
statcast_search_batters <- function(start_date, end_date, batterid = NULL, ...) {

 statcast_search(start_date, end_date, playerid = batterid, player_type = "batter", ...)
}
statcast_search_pitchers <- function(start_date, end_date, pitcherid = NULL, ...) {
statcast_search(start_date, end_date, playerid = pitcherid, player_type = "pitcher", ...)
} ```
```{r}
# Example of using the function to fetch Jacob deGrom's data deGrom_id <- playerid_lookup("deGrom", "Jacob")$mlbam_id
degrom_data <- statcast_search("2015-01-01", "2019-12-31", playerid = deGrom_id, player_type = "pitcher")
```
```{r}
# Analysis of deGrom's pitching data degrom_data %>%
filter(!is.na(pitch_type), !is.na(release_speed)) %>% group_by(year = lubridate::year(game_date), pitch_type) %>% summarise(
Average_Speed = mean(release_speed, na.rm = TRUE), Total_Strikeouts = sum(events == "strikeout", na.rm = TRUE), .groups = 'drop' # This will drop all grouping after
summarisation ) %>%
ggplot(aes(x = pitch_type, y = Average_Speed, fill = pitch_type)) + geom_bar(stat = "identity") +
facet_wrap(~year) +
labs(title = "Jacob deGrom's Pitch Speed by Type Across Years", x =
"Pitch Type", y = "Average Speed (mph)") + theme_minimal()
```
```{r} library(dplyr) library(baseballr)
fetch_deGrom_data <- function(start_year, end_year) {

 deGrom_id <- playerid_lookup("deGrom", "Jacob")$key_mlbam[1] # Fetch deGrom's player ID
all_data <- lapply(start_year:end_year, function(year) { # Statcast search for each year statcast_search(start_date = paste0(year, "-01-01"),
end_date = paste0(year, "-12-31"), playerid = deGrom_id,
player_type = "pitcher")
}) %>%
bind_rows() %>%
filter(launch_speed > 0) # Filter out pitches with zero batted
ball speed
return(all_data) }
deGrom_data <- fetch_deGrom_data(2016, 2021) ```
#SUMMARY
```{r} summary(deGrom_data) str(deGrom_data)
```
```{r}
data <- deGrom_data %>%
filter(!is.na(pitch_type) & pitch_type != "" & !is.na(release_speed))
```
```{r}
deGrom_velocity <- data %>%
group_by(year = year(game_date), pitch_type) %>%
summarise(average_velocity = mean(release_speed, na.rm = TRUE), .groups = 'drop')
ggplot(deGrom_velocity, aes(x = year, y = average_velocity, color = pitch_type)) +
geom_line() +
geom_point() +
labs(title = "Average Pitch Velocity by Year", x = "Year", y =
"Velocity (mph)") + theme_minimal() +

 scale_color_brewer(palette = "Set1")
```
#STRIKE OUT TRENDS ```{r} library(lubridate) library(dplyr) library(ggplot2)
# Ensure 'game_date' is in date format data <- data %>%
mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"), year = year(game_date))
# Now perform your summarization summary_data <- data %>%
group_by(year, pitch_type) %>% summarise(
Average_Speed = mean(release_speed, na.rm = TRUE),
.groups = 'drop' # This controls the creation of an extra grouped layer
)
ggplot(summary_data, aes(x = pitch_type, y = Average_Speed, color = pitch_type)) +
geom_line() +
geom_point() +
facet_wrap(~year) +
labs(title = "Jacob deGrom's Pitch Speed by Type Across Years", x =
"Pitch Type", y = "Average Speed (mph)") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 120, hjust = 1, vjust = 1,
size = 12)) # Rotate and adjust the size of x-axis labels ```
```{r}
# Print the names of all columns in the dataset print(names(deGrom_data))
```
```{r}

 library(baseballr)
# Look up Jacob deGrom's player ID
degrom_lookup <- playerid_lookup("deGrom", "Jacob") deGrom_id <- degrom_lookup$mlbam_id
# Print IDs to ensure they are loaded print(deGrom_id)
```
```{r}
# Function to fetch data for a specific pitcher over the years 2016-2021
get_pitcher_data <- function(player_id, start_year, end_year) {
do.call(rbind, lapply(start_year:end_year, function(year) { statcast_search(start_date = paste0(year, "-01-01"), end_date =
paste0(year, "-12-31"),
playerid = player_id, player_type = "pitcher")
})) }
# Fetching data for Jacob deGrom
degrom_data <- get_pitcher_data(deGrom_id, 2016, 2021) ```
```{r}
degrom_data$outs_recorded <- degrom_data$events %in% c("strikeout", "field_out", "force_out", "double_play", "grounded_into_double_play", "strikeout_double_play", "sac_bunt", "sac_fly", "fielders_choice_out")
outs_total <- sum(degrom_data$outs_recorded)
# Convert total outs to innings pitched (3 outs per inning) innings_pitched <- outs_total / 3
```
```{r}
# Function to calculate ERA custom_era_calculation <- function(data) {
earned_runs <- sum(data$events %in% c("home_run", "scored_by_hit"), na.rm = TRUE)
innings_pitched <- sum(data$outs_recorded, na.rm = TRUE) / 3 if (innings_pitched == 0) return(NA) # Avoid division by zero

 (earned_runs / innings_pitched) * 9 }
```
```{r}
custom_whip_calculation <- function(data) {
walks_hits <- sum(data$events %in% c("walk", "single", "double", "triple", "home_run"), na.rm = TRUE)
innings_pitched <- sum(data$innings_pitched, na.rm = TRUE)
if (innings_pitched == 0) return(NA) # Avoid division by zero walks_hits / innings_pitched
} ```
```{r}
analyze_data <- function(data) {
data %>%
group_by(year = lubridate::year(game_date)) %>% summarise(
Average_Velocity = mean(release_speed, na.rm = TRUE), Total_Strikeouts = sum(events == "strikeout", na.rm = TRUE), ERA = custom_era_calculation(cur_data()),
WHIP = custom_whip_calculation(cur_data())
) }
```
```{r}
degrom_stats <- analyze_data(degrom_data) print(degrom_stats)
```
```{r} library(ggplot2)
# Convert 'year' to numeric
degrom_stats$year <- as.numeric(as.character(degrom_stats$year))
# Check and convert 'ERA' and 'WHIP' to numeric
degrom_stats$ERA <- as.numeric(as.character(degrom_stats$ERA)) degrom_stats$WHIP <- as.numeric(as.character(degrom_stats$WHIP))

 # Plotting ERA and WHIP over time ggplot(degrom_stats, aes(x = year)) +
geom_line(aes(y = ERA, color = "ERA"), size = 1) + # Specify color within aes() for legends
geom_line(aes(y = WHIP, color = "WHIP"), size = 1) + # Specify color within aes() for legends
scale_color_manual(values = c("ERA" = "blue", "WHIP" = "red")) + labs(
title = "Jacob deGrom: ERA and WHIP Trends (2016-2020)", y = "Value",
x = "Year",
color = "Metric" # Proper label for legend
)+
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) #
Improves readability of x-axis labels ```
```{r} library(baseballr) library(dplyr)
degrom_lookup <- playerid_lookup("deGrom", "Jacob") deGrom_id <- degrom_lookup$mlbam_id
get_pitcher_data <- function(player_id, start_year, end_year) { do.call(rbind, lapply(start_year:end_year, function(year) {
statcast_search(start_date = paste0(year, "-01-01"), end_date = paste0(year, "-12-31"),
playerid = player_id, player_type = "pitcher")
})) }
# Fetching data for Jacob deGrom
degrom_data <- get_pitcher_data(deGrom_id, 2016, 2021)
```
```{r}
# Convert game_date to Date type
degrom_data$game_date <- as.Date(degrom_data$game_date)

 #Summary
summary_stats <- degrom_data%>%
group_by(pitch_type) %>% summarise(
Average_Speed = mean(release_speed, na.rm = TRUE), Strikeouts = sum(events == "strikeout", na.rm = TRUE), Total_Pitches = n()
) ```
```{r}
# Plotting average pitch speed by pitch type
ggplot(summary_stats, aes(x = pitch_type, y = Average_Speed, fill = pitch_type)) +
geom_bar(stat = "identity") +
labs(title = "Average Pitch Speed by Pitch Type", x = "Pitch Type", y = "Average Speed (mph)")
# Strikeout rate by pitch type
summary_stats$Strikeout_Rate <- summary_stats$Strikeouts / summary_stats$Total_Pitches
ggplot(summary_stats, aes(x = pitch_type, y = Strikeout_Rate, fill = pitch_type)) +
geom_bar(stat = "identity") +
labs(title = "Strikeout Rate by Pitch Type", x = "Pitch Type", y = "Strikeout Rate")
```
```{r}
#Pitch Velocity Over Time
#This visualization will help observe changes in Jacob deGrom's pitch velocity across games.
ggplot(degrom_data, aes(x = game_date, y = release_speed, color = pitch_type)) +
geom_line() +
labs(title = "Pitch Velocity Over Time", x = "Game Date", y = "Velocity (mph)") +
theme_minimal() ```
```{r}
#Distribution of Pitch Types

 #See the frequency of different pitch types used by deGrom.
ggplot(degrom_data, aes(x = pitch_type, fill = pitch_type)) + geom_bar() +
labs(title = "Distribution of Pitch Types", x = "Pitch Type", y =
"Count") + theme_minimal()
```
```{r} library(dplyr) library(ggplot2)
significant_outcomes <- c("strikeout", "home_run", "single", "double", "triple", "walk")
# Filter data for significant outcomes and plot degrom_data %>%
filter(events %in% significant_outcomes) %>% group_by(pitch_type, events) %>%
summarise(count = n(), .groups = 'drop') %>%
ggplot(aes(x = pitch_type, y = count, fill = events)) + geom_bar(stat = "identity", position = position_stack(reverse =
TRUE)) +
scale_fill_brewer(palette = "Set1") + # Using a color palette that
is easy to distinguish
labs(title = "Outcome by Pitch Type", x = "Pitch Type", y =
"Count") +
guides(fill = guide_legend(title = "Event Type", title.position =
"top", title.hjust = 0.5, label.hjust = 0.5)) +
theme_minimal() +
theme(legend.position = "right", legend.title.align = 0.5) #
Adjusting legend position and alignment ```
```{r} degrom_data %>%
mutate(strike = ifelse(description %in% c("called_strike", "swinging_strike"), "Strike", "Ball")) %>%
ggplot(aes(x = factor(zone), fill = strike)) + geom_bar(position = "fill") +
scale_y_continuous(labels = scales::percent_format()) +

 labs(title = "Pitch Effectiveness by Zone", x = "Zone", y = "Percentage") +
theme_minimal() ```
```{r}
# Check structure of both columns str(degrom_data$release_speed) str(degrom_data$spin_rate_deprecated)
# Count NA values in the spin_rate_deprecated sum(is.na(degrom_data$spin_rate_deprecated))
summary(degrom_data$spin_rate_deprecated) ```
```{r}
degrom_data <- degrom_data %>%
filter(pitch_type != "")
# Visualize the distribution of release speeds by pitch type ggplot(degrom_data, aes(x = pitch_type, y = release_speed, fill = pitch_type)) +
geom_boxplot() +
labs(title = "Distribution of Release Speeds by Pitch Type", x = "Pitch Type", y = "Release Speed (mph)") +
theme_minimal()
# Examine outcomes by pitch type degrom_data %>%
group_by(pitch_type, events) %>%
summarise(count = n(), .groups = 'drop') %>%
ggplot(aes(x = pitch_type, y = count, fill = events)) + geom_bar(stat = "identity", position = position_stack()) + labs(title = "Outcomes by Pitch Type", x = "Pitch Type", y =
"Count") ```
```{r}
# calculate the total number of pitches total_pitches_per_year <- df %>%
group_by(game_year) %>% summarize(total_pitches = n())

# calculate the proportions at which each description occurs description_proportions <- df %>%
group_by(game_year, description) %>% # we do a count
summarize(count = n()) %>%
ungroup() %>%
# and observe over each year left_join(total_pitches_per_year, by = "game_year") %>% mutate(proportion = count / total_pitches) %>% select(game_year, description, proportion)
# and filter out anything less than a rate of 0.05, as it is too small for meaningful conclusions description_proportions_filtered <- description_proportions %>%
filter(proportion >= 0.05)
# Calculate proportions for "pitch_type"
# and follow the same process as with desctiptions pitch_type_proportions <- df %>%
group_by(game_year, pitch_type) %>%
summarize(count = n()) %>%
ungroup() %>%
left_join(total_pitches_per_year, by = "game_year") %>% mutate(proportion = count / total_pitches) %>% select(game_year, pitch_type, proportion)
# Plot for "description" proportions ggplot(description_proportions_filtered, aes(x = game_year, y = proportion, color = description)) +
geom_line() +
labs(title = "Proportions of Pitch Results Over 5 Years",
x = "game_year",
y = "Proportion") + theme_minimal()
# Plot for "pitch_type" proportions ggplot(pitch_type_proportions, aes(x = game_year, y = proportion, color = pitch_type)) +
geom_line() +
labs(title = "Proportions of Pitch Types Over 5 Years",
x = "game_year",
y = "Proportion") + theme_minimal()