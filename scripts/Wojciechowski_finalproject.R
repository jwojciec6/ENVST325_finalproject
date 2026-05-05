library(lubridate)
library(ggplot2)
library(tidyverse)
library(stats)

atlanta      <- read_csv("data/atlanta.csv")
boston       <- read_csv("data/boston.csv")
charlotte    <- read_csv("data/charlotte.csv")
chicago      <- read_csv("data/chicago.csv")
detroit      <- read_csv("data/detroit.csv")
la           <- read_csv("data/la.csv")
miami        <- read_csv("data/miami.csv")
minneapolis  <- read_csv("data/minneapolis.csv")
montgomery   <- read_csv("data/montgomery.csv")
newjersey    <- read_csv("data/newjersey.csv")
newyork      <- read_csv("data/newyork.csv")
philly       <- read_csv("data/philly.csv")
phoenix      <- read_csv("data/phoenix.csv")
saltlakecity <- read_csv("data/saltlakecity.csv")
seattle      <- read_csv("data/seattle.csv")
stlouis      <- read_csv("data/stlouis.csv")

#clean up data keeping important columns of data and adding TAVG column
#divide by 10 on temp and precipitation for units of mm and C respectively
clean_city <- function(df) {
  df %>%
    select(DATE, NAME, PRCP, TMAX, TMIN) %>%
    mutate(
      DATE = ymd(DATE),
      TMAX = TMAX / 10,
      TMIN = TMIN / 10,
      PRCP = PRCP / 10,
      TAVG = (TMAX + TMIN) / 2
    )
}

atlanta      <- clean_city(atlanta)
boston       <- clean_city(boston)
charlotte    <- clean_city(charlotte)
chicago      <- clean_city(chicago)
detroit      <- clean_city(detroit)
la           <- clean_city(la)
miami        <- clean_city(miami)
minneapolis  <- clean_city(minneapolis)
montgomery   <- clean_city(montgomery)
newjersey    <- clean_city(newjersey)
newyork      <- clean_city(newyork)
philly       <- clean_city(philly)
phoenix      <- clean_city(phoenix)
saltlakecity <- clean_city(saltlakecity)
seattle      <- clean_city(seattle)
stlouis      <- clean_city(stlouis)

#Combine all cities into one data frame

all_cities <- bind_rows(
  atlanta      %>% mutate(city = "Atlanta",       region = "Southeast"),
  boston       %>% mutate(city = "Boston",        region = "Northeast"),
  charlotte    %>% mutate(city = "Charlotte",     region = "Southeast"),
  chicago      %>% mutate(city = "Chicago",       region = "Midwest"),
  detroit      %>% mutate(city = "Detroit",       region = "Midwest"),
  la           %>% mutate(city = "Los Angeles",   region = "West"),
  miami        %>% mutate(city = "Miami",         region = "Southeast"),
  minneapolis  %>% mutate(city = "Minneapolis",   region = "Midwest"),
  montgomery   %>% mutate(city = "Montgomery",    region = "Southeast"),
  newjersey    %>% mutate(city = "New Jersey",    region = "Northeast"),
  newyork      %>% mutate(city = "New York",      region = "Northeast"),
  philly       %>% mutate(city = "Philadelphia",  region = "Northeast"),
  phoenix      %>% mutate(city = "Phoenix",       region = "West"),
  saltlakecity %>% mutate(city = "Salt Lake City",region = "West"),
  seattle      %>% mutate(city = "Seattle",       region = "West"),
  stlouis      %>% mutate(city = "St. Louis",     region = "Midwest")
) %>%
  mutate(
    region = factor(region, levels = c("Northeast", "Southeast", "Midwest", "West")),
    city   = factor(city),
    year = year(DATE),
    month = month(DATE)
  ) %>%
#additional cleaning of bad data Chicago had invalid readings until this year
  filter(!(city == "Chicago" & year(DATE) < 1959)) %>%
  filter(year >= 1950)

# extreme thresholds calculations

tmax_thresh <- all_cities %>%
  group_by(city) %>%
  summarise(tmax_p90 = quantile(TMAX, 0.90, na.rm = TRUE), .groups = "drop")

prcp_thresh <- all_cities %>%
  filter(PRCP > 0) %>%
  group_by(city) %>%
  summarise(prcp_p95 = quantile(PRCP, 0.95, na.rm = TRUE), .groups = "drop")

tmin_thresh <- all_cities %>%
  group_by(city) %>%
  summarise(tmin_p10 = quantile(TMIN, 0.10, na.rm = TRUE), .groups = "drop")

daily <- all_cities %>%
  left_join(tmax_thresh, by = "city") %>%
  left_join(prcp_thresh, by = "city") %>%
  left_join(tmin_thresh, by = "city") %>%
  mutate(
    hot_day    = !is.na(TMAX) & TMAX > tmax_p90,
    cold_day   = !is.na(TMIN) & TMIN < tmin_p10,
    heavy_prcp = !is.na(PRCP) & PRCP > prcp_p95 & PRCP > 0
  )

#monthly statistic calculations

monthly <- daily %>%
  group_by(city, region, year, month) %>%
  summarise(
    n_days          = n(),
    tavg_monthly    = mean(TAVG,  na.rm = TRUE),
    tmax_monthly    = mean(TMAX,  na.rm = TRUE),
    tmin_monthly    = mean(TMIN,  na.rm = TRUE),
    tmax_record     = max(TMAX,   na.rm = TRUE),
    tmin_record     = min(TMIN,   na.rm = TRUE),
    prcp_monthly    = sum(PRCP,   na.rm = TRUE),
    prcp_max_day    = max(PRCP,   na.rm = TRUE),
    n_hot_days      = sum(hot_day,    na.rm = TRUE),
    n_cold_days     = sum(cold_day,   na.rm = TRUE),
    n_heavy_prcp    = sum(heavy_prcp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 25)

regions <- levels(monthly$region)

# climatology table construction
# Temperature (°C): mean, SD, max, min for TAVG, TMAX, and TMIN.
# Precipitation (mm): mean, SD, max, min of monthly totals.
# Change over time: linear trend (per decade) for TAVG and PRCP,
# computed via lm() across all years for that city × month.

fit_slope <- function(y, x) {
  if (sum(!is.na(y)) < 10) return(NA_real_)
  coef(lm(y ~ x))[["x"]] * 10
}

climatology <- monthly %>%
  group_by(city, region, month) %>%
  summarise(
    n_years = n(),
    # Average temperature
    tavg_mean = mean(tavg_monthly, na.rm = TRUE),
    tavg_sd   = sd(tavg_monthly,   na.rm = TRUE),
    tavg_max  = max(tavg_monthly,  na.rm = TRUE),
    tavg_min  = min(tavg_monthly,  na.rm = TRUE),
    # Mean daily max temperature
    tmax_mean = mean(tmax_monthly, na.rm = TRUE),
    tmax_sd   = sd(tmax_monthly,   na.rm = TRUE),
    tmax_max  = max(tmax_record,   na.rm = TRUE),   # all-time hottest day
    tmax_min  = min(tmax_monthly,  na.rm = TRUE),
    # Mean daily min temperature
    tmin_mean = mean(tmin_monthly, na.rm = TRUE),
    tmin_sd   = sd(tmin_monthly,   na.rm = TRUE),
    tmin_max  = max(tmin_monthly,  na.rm = TRUE),
    tmin_min  = min(tmin_record,   na.rm = TRUE),   # all-time coldest day
    # Precipitation
    prcp_mean = mean(prcp_monthly, na.rm = TRUE),
    prcp_sd   = sd(prcp_monthly,   na.rm = TRUE),
    prcp_max  = max(prcp_monthly,  na.rm = TRUE),
    prcp_min  = min(prcp_monthly,  na.rm = TRUE),
    # Change over time
    tavg_trend_per_decade = fit_slope(tavg_monthly, year),
    tmax_trend_per_decade = fit_slope(tmax_monthly, year),
    tmin_trend_per_decade = fit_slope(tmin_monthly, year),
    prcp_trend_per_decade = fit_slope(prcp_monthly, year),
    .groups = "drop"
  ) %>%
  mutate(
    month_label = factor(month.abb[month], levels = month.abb),
    across(where(is.numeric), ~ round(.x, 2))
  ) %>%
  arrange(region, city, month)

dir.create("output", showWarnings = FALSE)
write_csv(climatology, "output/climatology_all_cities.csv")

#monthly trends heatmaps for each city

fit_trend <- function(y, x) {
  if (sum(!is.na(y)) < 10) return(c(slope = NA_real_, pval = NA_real_))
  m <- lm(y ~ x)
  c(slope = coef(m)[["x"]] * 10,
    pval  = summary(m)$coefficients["x", "Pr(>|t|)"])
}

# Temperature heatmaps
month_trends_temp <- monthly %>%
  group_by(city, region, month) %>%
  summarise(
    slope = fit_trend(tavg_monthly, year)[["slope"]],
    pval  = fit_trend(tavg_monthly, year)[["pval"]],
    .groups = "drop"
  ) %>%
  mutate(
    month_label = factor(month.abb[month], levels = month.abb),
    significant = !is.na(pval) & pval < 0.05
  )

for (r in regions) {
  p <- ggplot(filter(month_trends_temp, region == r),
              aes(x = month_label, y = city, fill = slope)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(significant, "*", "")),
              color = "white", size = 4.5, vjust = 0.8) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                         midpoint = 0, name = "°C / decade", na.value = "grey80") +
    labs(
      title    = paste(r, "— Temperature Trend by Month"),
      subtitle = "°C per decade  |  * = p < 0.05  |  Red = warming, Blue = cooling",
      x = "Month", y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9))
  print(p)
}

# Precipitation heatmaps
month_trends_prcp <- monthly %>%
  filter(!(city %in% c("Detroit", "New York"))) %>%
  group_by(city, region, month) %>%
  summarise(
    slope = fit_trend(prcp_monthly, year)[["slope"]],
    pval  = fit_trend(prcp_monthly, year)[["pval"]],
    .groups = "drop"
  ) %>%
  mutate(
    month_label = factor(month.abb[month], levels = month.abb),
    significant = !is.na(pval) & pval < 0.05
  )

for (r in regions) {
  p <- ggplot(filter(month_trends_prcp, region == r),
              aes(x = month_label, y = city, fill = slope)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(significant, "*", "")),
              color = "white", size = 4.5, vjust = 0.8) +
    scale_fill_gradient2(low = "#d7191c", mid = "white", high = "#1a9641",
                         midpoint = 0, name = "mm / decade", na.value = "grey80") +
    labs(
      title    = paste(r, "— Precipitation Trend by Month"),
      subtitle = "mm per decade  |  * = p < 0.05  |  Green = wetter, Red = drier",
      x = "Month", y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9))
  print(p)
}




 








