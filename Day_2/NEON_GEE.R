library(readr)
towers <- read_csv("NEON_towers_MODIS_LST_export.csv")

## make blank columns
towers$year <- NA
towers$month <- NA
towers$day <- NA
towers$index <- NA

## split dates into four columns
towers[, c("year", "month", "day", "index")] <- stringr::str_split_fixed(towers$`system:index`, "_", 4)
towers$date <- as.Date(paste(towers$year, "-", towers$month, "-", towers$day, sep = ""))

## plot
ggplot() +
  geom_line(data = towers, aes(x = date, y = mean, group = plot_id, color = plot_id)) +
  theme_bw()
