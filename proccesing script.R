# Getting and loading the data
data_link <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(data_link, destfile = "weather.csv.bz2")

weather_data <- read.csv(bzfile("weather.csv.bz2"))

# Necessary packages
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tidytext)

#session information


# Data processing----
data_subset <- weather_data %>% select(EVTYPE,FATALITIES,INJURIES, 
                                       PROPDMG,PROPDMGEXP,
                                       CROPDMG,CROPDMGEXP) %>%
    mutate(event_type = str_to_lower(EVTYPE)) %>%
    mutate(event_type = case_when(
        str_detect(event_type, "thunder|tstm|microburst|tunder|thuder|thuner|thundeerstorm winds|tropical|gustnado|downburst") ~ "thunder/tropical storm",
        str_detect(event_type, "tornado|torndao|landspout") ~ "tornado",
        str_detect(event_type, "heat") ~ "heat",
        str_detect(event_type, "flood|flooding|floods|floodin|urban/sml stream fld|ice jam") ~ "flood",
        str_detect(event_type, "lightning|lighting|ligntning") ~ "lightning",
        str_detect(event_type, "ice storm|glaze|ice/strong winds|freezing rain|ice roads|ice floes|ice") ~ "ice storm",
        str_detect(event_type, "frost|early frost|frost\freeze|freeze") ~ "frost/freeze",
        str_detect(event_type, "blizzard/winter storm|high wind/blizzard") ~ "blizzard",
        str_detect(event_type, "winter storms|winter storm high winds|winter weather") ~ "winter storm",
        str_detect(event_type, "wind|winds") ~ "high wind",
        str_detect(event_type, "small hail") ~ "hail",
        str_detect(event_type, "hurricane") ~ "hurricane",
        str_detect(event_type, "snow") ~ "heavy snow",
        str_detect(event_type, "fog") ~ "fog",
        str_detect(event_type, "rip currents") ~ "rip current",
        str_detect(event_type, "tropical storm") ~ "tropical storm",
        str_detect(event_type, "heavy rain|heavy shower|heavy precipitation") ~ "heavy rain",
        str_detect(event_type, "storm surge/tide|coastal surge") ~ "storm surge",
        str_detect(event_type, "hail") ~ "hail",
        str_detect(event_type, "rain|gusty wind/hvy rain|hvy rain|hvy rainrecord rainfall|rainstorm") ~ "heavy rain",
        str_detect(event_type, "cold") ~ "extreme cold",
        str_detect(event_type, "landslides") ~ "landslide",
        str_detect(event_type, "strong winds") ~ "strong wind",
        str_detect(event_type, "excessive wetness") ~ "drought", 
        str_detect(event_type, "extreme windchill|extreme wind chill|cool and wet") ~ "extreme cold",
        str_detect(event_type, "fire") ~ "wildfire",
        str_detect(event_type, "swells|high seas|surf") ~ "high surf",
        str_detect(event_type, "erosion|coastal storm|high tides") ~ "coastal flood",
        str_detect(event_type, "urban|dam break|high water") ~ "flash flood",
        str_detect(event_type, "apache county|marine accident|severe turbulence|unseasonably warm") ~ "other",
        str_detect(event_type, "wintry|mix|icy roads") ~ "winter weather",
        TRUE ~ event_type),
        property_exponent = str_trim(str_to_lower(PROPDMGEXP)),
        crop_exponent =  str_trim(str_to_lower(CROPDMGEXP))) %>%
    mutate(property_exponent = case_when(
        property_exponent ==  "k" ~  10^3,
        property_exponent == "m" ~  10^6,
        property_exponent == "b" ~  10^9,
        property_exponent == "h" ~  10^2,
        property_exponent == "8" ~  10^8,
        property_exponent =="7" ~  10^7,
        property_exponent =="6" ~  10^6,
        property_exponent =="5" ~  10^5,
        property_exponent =="4" ~  10^4,
        property_exponent =="3" ~  10^3,
        property_exponent == "2" ~  10^2,
        property_exponent == "1" ~  10^1,
        property_exponent == "0" ~  10^0,
        property_exponent == "-" ~  1,
        property_exponent == "?" ~  1,
        property_exponent == "+" ~  1,
        property_exponent == ""  ~  1,
        TRUE ~ 1 ),
        crop_exponent = case_when(
            crop_exponent == "k" ~  10^3,
            crop_exponent == "m" ~  10^6,
            crop_exponent == "b" ~  10^9,
            crop_exponent == "h" ~  10^2,
            crop_exponent == "8" ~  10^8,
            crop_exponent == "7" ~  10^7,
            crop_exponent == "6" ~  10^6,
            crop_exponent == "5" ~  10^5,
            crop_exponent == "4" ~  10^4,
            crop_exponent == "3" ~  10^3,
            crop_exponent == "2" ~  10^2,
            crop_exponent == "1" ~  10^1,
            crop_exponent == "0" ~  10^0,
            crop_exponent == "-" ~  1,
            crop_exponent == "?" ~  1,
            crop_exponent == "+" ~  1,
            crop_exponent == ""  ~  1,
            TRUE ~ 1 ))



# Health impact ----
health_impact <- data_subset %>% group_by(event_type) %>%
    summarise(num_fatal = sum(FATALITIES),
              num_injuries = sum(INJURIES),
              num_fatal_injuries = num_fatal + num_injuries) %>%
    filter(num_fatal_injuries > 0 ) %>%
    arrange(desc(num_fatal_injuries))

# Health impact plot

plot_data <- health_impact %>% slice(1:10) %>%
             pivot_longer(cols = c(num_fatal, num_injuries),
                     names_to = "impact_type",
                     values_to = "count")


health_plot <- ggplot(data = plot_data,
                      aes(x = count,
                          y = reorder_within(event_type, count, impact_type),
                          fill = impact_type)) +
               geom_col() +
               labs(title = "Public Health Impact by Severe Weather Event",
                    x = "Total Number", 
                    y = NULL, fill = "Impact type") +
               scale_fill_manual(values = c("num_injuries" = "#1b9e77",
                                          "num_fatal" = "#d95f02")) + 
               theme_minimal() +
               scale_x_continuous(labels = scales::comma) +
               facet_wrap(~ impact_type, scales = "free",
                          labeller = as_labeller(c("num_injuries" = "Injuries",
                                                   "num_fatal" = "Fatalites"))) +
               scale_y_reordered() +
               theme(plot.margin = margin(5, 10, 5, 20, "pt"),
                 legend.position = "none")


# Economic impact ----
economic_impact <- data_subset %>% group_by(event_type) %>% 
    summarise( property_damage =sum(property_exponent * PROPDMG, na.rm = TRUE) ,
               crop_damage = sum(crop_exponent * CROPDMG, na.rm = TRUE),
               total_damage = property_damage + crop_damage) %>%
    arrange(desc(total_damage))


#economic impact plot

plot2_data <- economic_impact %>% slice(1:15) %>%
    pivot_longer(cols = c(property_damage, crop_damage),
                 names_to = "impact_type",
                 values_to = "losses")

economic_plot <- ggplot(data = plot2_data,
       aes(x = losses/10^9,
           y = reorder_within(event_type, losses, impact_type),
           fill = impact_type)) +
    geom_col() +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma) +
    facet_wrap(~ impact_type, scales = "free",
               labeller = as_labeller(c("crop_damage" = "Crop Damage",
                        "property_damage" = "Property Damage"))) +
    scale_y_reordered() +
    theme(plot.margin = margin(5, 10, 5, 20, "pt"),
          legend.position = "none") +
    labs(title = "Ecnomic Impact by Severe Weather Event",
         x = "Total Number (In Billions)", 
         y = NULL, fill = "Impact type") +
    scale_fill_manual(values = c("property_damage" = "#1b9e77",
                                 "crop_damage" = "#d95f02"))









