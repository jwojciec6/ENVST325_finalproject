library(lubridate)
library(ggplot2)
library(tidyverse)
library(stats)

#read in each cities data 
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

#clean up data keeping important columns of data
#divide by 10 on temp and precipitation for units of mm and C respectively
clean_city <- function(df) {
  df %>%
    select(DATE, NAME, PRCP, TMAX, TMIN) %>%
    mutate(
      DATE = ymd(DATE),
      TMAX = TMAX / 10,
      TMIN = TMIN / 10,
      PRCP = PRCP / 10,
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

#Combine all cities into one data frame add region tags

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

prcp_thresh <- all_cities %>%
  filter(PRCP > 0) %>%
  group_by(city) %>%
  summarise(prcp_p95 = quantile(PRCP, 0.95, na.rm = TRUE), .groups = "drop")


#calculate daily extreme thresholds.
daily <- all_cities %>%
  left_join(prcp_thresh, by = "city") %>%
  mutate(
    heavy_prcp = !is.na(PRCP) & PRCP > prcp_p95 & PRCP > 0
  )

#monthly statistic calculations for percipitation

monthly <- daily %>%
  group_by(city, region, year, month) %>%
  summarise(
    n_days          = n(),
    prcp_monthly    = sum(PRCP,   na.rm = TRUE),
    prcp_max_day    = max(PRCP,   na.rm = TRUE),
    n_heavy_prcp    = sum(heavy_prcp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 25)

regions <- levels(monthly$region)


#monthly trends heatmaps for each city

fit_trend <- function(y, x) {
  if (sum(!is.na(y)) < 10) return(c(slope = NA_real_, pval = NA_real_))
  m <- lm(y ~ x)
  c(slope = coef(m)[["x"]] * 10,
    pval  = summary(m)$coefficients["x", "Pr(>|t|)"])
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
#create heatmaps for ach region
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


# Exponential fit on daily precipitation extremes 

#create period labels
period_labels <- c("1960–1980", "2000–2020")
period_colors <- c("1960–1980" = "#2166ac", "2000–2020" = "#d6604d")

extreme_prcp <- daily %>%
  filter(
    PRCP > prcp_p95,
    year %in% c(1960:1980, 2000:2020)
  ) %>%
  mutate(
    period = case_when(
      year <= 1980 ~ "1960–1980",
      year >= 2000 ~ "2000–2020"
    ),
    period = factor(period, levels = period_labels)
  ) %>%
  filter(!is.na(PRCP), PRCP > 0)

#compute exponential parameters including lamda, higher lamda = less extreme events
exp_fits <- extreme_prcp %>%
  group_by(city, region, period) %>%
  summarise(
    n      = n(),
    lambda = 1 / mean(PRCP, na.rm = TRUE),
    mean_x = mean(PRCP, na.rm = TRUE),
    .groups = "drop"
  )

#set x bounds based on most extreme event for each city
x_grid <- extreme_prcp %>%
  group_by(city) %>%
  summarise(x_max = max(PRCP, na.rm = TRUE), .groups = "drop")

#build exponential curves for each city
exp_curves <- exp_fits %>%
  left_join(x_grid, by = "city") %>%
  rowwise() %>%
  mutate(
    x   = list(seq(0.01, x_max, length.out = 400)),
    pdf = list(dexp(seq(0.01, x_max, length.out = 400), rate = lambda))
  ) %>%
  unnest(c(x, pdf)) %>%
  select(city, region, period, lambda, mean_x, n, x, pdf)

#build annotation labels to avoid overlap
ann <- exp_fits %>%
  group_by(city, region) %>%
  mutate(
    y_pos = max(lambda) * c(1.0, 0.75),
    label = paste0(period, "  μ = ", round(mean_x, 1), " mm  n = ", n)
  ) %>%
  ungroup()

#build graphs for each region.
for (r in regions) {
  
  curves_r <- filter(exp_curves,   region == r)
  raw_r    <- filter(extreme_prcp, region == r)
  ann_r    <- filter(ann,          region == r)
  
  p <- ggplot() +
    geom_histogram(
      data     = raw_r,
      aes(x = PRCP, y = after_stat(density), fill = period),
      binwidth = 4, alpha = 0.20, position = "identity", color = NA
    ) +
    geom_line(
      data      = curves_r,
      aes(x = x, y = pdf, color = period),
      linewidth = 1.2
    ) +
    geom_vline(
      data     = exp_fits %>% filter(region == r),
      aes(xintercept = mean_x, color = period),
      linetype = "dashed", linewidth = 0.7, alpha = 0.8
    ) +
    geom_text(
      data  = ann_r,
      aes(label = label, color = period, y = y_pos),
      x     = Inf, hjust = 1.05, size = 2.6,
      lineheight = 1.2, show.legend = FALSE
    ) +
    scale_fill_manual(values  = period_colors, name = "Period") +
    scale_color_manual(values = period_colors, name = "Period") +
    facet_wrap(~ city, scales = "free", ncol = 2) +
    labs(
      title    = paste(r, "— Exponential PDF: Daily Precipitation Extremes (p95)"),
      subtitle = "Fitted Exp(λ = 1/μ)  |  Dashed = period mean  |  Blue = 1960–1980, Red = 2000–2020",
      x        = "Daily Precipitation (mm)",
      y        = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
  
  print(p)
}
 








