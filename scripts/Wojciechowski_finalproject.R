library(lubridate)
library(ggplot2)
library(tidyverse)

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
    city   = factor(city)
  )

#additional cleaning of bad data Chicago had invalid readings until this year
all_cities <- all_cities %>%
  filter(!(city == "Chicago" & year(DATE) < 1959))


#Annual averages

annual <- all_cities %>%
  mutate(year = year(DATE)) %>%
  filter(year >= 1950) %>%
  group_by(city, region, year) %>%
  summarise(
    n_days = n(),
    tavg_annual = mean(TAVG, na.rm = TRUE),
    prcp_annual = sum(PRCP,  na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(n_days >= 340)

#plotting temperature and precipitation annual averages by region

regions <- c("Northeast", "Southeast", "Midwest", "West")

for (r in regions) {
  
  region_data <- filter(annual, region == r)
  
  #Temperature overlay
  p_temp <- ggplot(region_data, aes(x = year, y = tavg_annual, color = city)) +
    geom_line(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
    labs(title = paste(r, "— Annual Average Temperature"),
         x = "Year", y = "Avg Temperature (°C)", color = "City") +
    theme_minimal()
  
  #Precipitation overlay exclude Detroit due to bad precipitation data for 20 years
  #exclude New York for large amounts of bad precipitation data
  p_prcp <- ggplot(filter(region_data, city != "Detroit", city != "New York"), aes(x = year, y = prcp_annual, color = city)) +
    geom_line(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
    labs(title = paste(r, "— Annual Total Precipitation"),
         x = "Year", y = "Total Precipitation (mm)", color = "City") +
    theme_minimal()
  
  print(p_temp)
  print(p_prcp)
}







