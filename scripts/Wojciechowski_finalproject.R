library(lubridate)
library(ggplot2)
library(tidyverse)
library(stats)

#read in each cities data 
# set working directory to ENVST325_finalproject before running code.
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
      title    = paste(r, " Precipitation Trend by Month"),
      subtitle = "mm per decade  |  * = p < 0.05  |  Green = wetter, Red = drier",
      x = "Month", y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9))
  print(p)
}


# Exponential fit on daily precipitation extremes 

period_labels <- c("1960–1980", "2000–2020")
period_colors <- c("1960–1980" = "#2166ac", "2000–2020" = "#d6604d")

# Filter daily data to the two comparison periods including zero precipitation days
# select only needed columns to avoid inheriting prcp_p95 from daily
all_prcp <- daily %>%
  select(city, region, year, month, DATE, PRCP) %>%
  filter(
    !is.na(PRCP),
    year %in% c(1960:1980, 2000:2020)
  ) %>%
  mutate(
    period = case_when(
      year <= 1980 ~ "1960–1980",
      year >= 2000 ~ "2000–2020"
    ),
    period = factor(period, levels = period_labels)
  )

# Fit exponential distribution parameters for each city and period
# lambda = 1/mean, higher lambda = steeper decay = less heavy rain
exp_fits <- all_prcp %>%
  group_by(city, region, period) %>%
  summarise(
    n      = n(),
    lambda = 1 / mean(PRCP, na.rm = TRUE),
    mean_x = mean(PRCP, na.rm = TRUE),
    .groups = "drop"
  )

# Build smooth exponential PDF curves for each city and period
# starting at 1mm to avoid the y-axis spike at zero inflating the scale for better visuals
exp_curves <- exp_fits %>%
  rowwise() %>%
  mutate(
    x   = list(seq(1, 30, length.out = 400)),
    pdf = list(dexp(seq(1, 30, length.out = 400), rate = lambda))
  ) %>%
  unnest(c(x, pdf)) %>%
  select(city, region, period, lambda, mean_x, n, x, pdf)

# Compute the p95 precipitation threshold per city across all days in both periods
# named thresh to avoid column name conflict with prcp_p95 already in daily
prcp_thresh_all <- all_prcp %>%
  group_by(city) %>%
  summarise(thresh = quantile(PRCP, 0.95, na.rm = TRUE), .groups = "drop")

# Overall mean and count annotation per city per period
ann <- exp_fits %>%
  group_by(city, region) %>%
  mutate(
    y_pos = c(0.45, 0.40),
    label = paste0(period, "  u = ", round(mean_x, 1), " mm  n = ", n)
  ) %>%
  ungroup()

# p95+ annotation: mean and count of days exceeding the p95 threshold per period
ann_p95 <- all_prcp %>%
  left_join(prcp_thresh_all, by = "city") %>%
  filter(PRCP >= thresh) %>%
  group_by(city, region, period) %>%
  summarise(
    n_p95    = n(),
    mean_p95 = mean(PRCP, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(city, region) %>%
  mutate(
    y_pos_p95 = c(0.33, 0.28),
    label_p95 = paste0(period, "  u95 = ", round(mean_p95, 1), " mm  n = ", n_p95)
  ) %>%
  ungroup()

# Delta annotation: difference in p95+ mean between periods
ann_delta <- ann_p95 %>%
  select(city, region, period, mean_p95) %>%
  pivot_wider(names_from = period, values_from = mean_p95) %>%
  rename(early = `1960–1980`, late = `2000–2020`) %>%
  filter(!is.na(early) & !is.na(late)) %>%
  mutate(
    delta      = late - early,
    pct_change = round((delta / early) * 100, 1),
    label      = paste0("du95 = ", ifelse(delta > 0, "+", ""), round(delta, 1),
                        " mm (", ifelse(pct_change > 0, "+", ""), pct_change, "%)")
  )

#implement exponential fit plots for each region and city
for (r in regions) {
  
  all_r     <- filter(all_prcp,   region == r)
  curves_r  <- filter(exp_curves, region == r)
  ann_r     <- filter(ann,        region == r)
  ann_p95_r <- filter(ann_p95,    region == r)
  delta_r   <- filter(ann_delta,  region == r)
  thresh_r  <- prcp_thresh_all %>% filter(city %in% unique(all_r$city))
  
  thresh_label_r <- thresh_r %>%
    mutate(label_y = 0.48)
  
  p <- ggplot() +
    
    geom_histogram(
      data     = all_r,
      aes(x = PRCP, y = after_stat(density), fill = period),
      binwidth = 3, alpha = 0.22, position = "identity", color = NA
    ) +
    
    geom_line(
      data      = curves_r,
      aes(x = x, y = pdf, color = period),
      linewidth = 1.2
    ) +
    
    geom_vline(
      data     = exp_fits %>% filter(region == r),
      aes(xintercept = mean_x, color = period),
      linetype = "dashed", linewidth = 0.7, alpha = 0.85
    ) +
    
    geom_vline(
      data    = thresh_r,
      aes(xintercept = thresh),
      color   = "grey25", linetype = "solid", linewidth = 0.65, alpha = 0.8
    ) +
    
    geom_text(
      data  = thresh_label_r,
      aes(x = thresh, y = label_y,
          label = paste0("p95 = ", round(thresh, 1), " mm")),
      hjust = -0.1, vjust = 1, color = "grey25", size = 2.4
    ) +
    
    geom_text(
      data  = ann_r,
      aes(label = label, color = period, y = y_pos),
      x     = 29, hjust = 1, size = 2.5,
      lineheight = 1.2, show.legend = FALSE
    ) +
    
    geom_text(
      data  = ann_p95_r,
      aes(label = label_p95, color = period, y = y_pos_p95),
      x     = 29, hjust = 1, size = 2.5,
      lineheight = 1.2, show.legend = FALSE
    ) +
    
    geom_text(
      data  = delta_r,
      aes(label = label),
      x     = 29, y = 0.22, hjust = 1,
      color = "grey20", size = 2.6, fontface = "bold"
    ) +
    
    scale_fill_manual(values  = period_colors, name = "Period") +
    scale_color_manual(values = period_colors, name = "Period") +
    facet_wrap(~ city, ncol = 2) +
    coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 30)) +
    guides(
      fill  = guide_legend(nrow = 1),
      color = guide_legend(nrow = 1)
    ) +
    labs(
      title    = paste(r, "Exponential Fit: All Daily Precipitation"),
      subtitle = "Exp(l = 1/u) fitted to all days  |  Dashed = period mean  |  Grey line = p95  |  u95 = mean above p95",
      x        = "Daily Precipitation (mm)",
      y        = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "horizontal",
      legend.margin    = margin(t = 10),
      panel.grid.minor = element_blank()
    )
  
  print(p)
}


