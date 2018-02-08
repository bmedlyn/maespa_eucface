
library(HIEv)
setToken()

if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr,padr)



fcp <- downloadTOA5(c("FACE","FCPLOGG"), startDate=date, endDate=date,
                    maxnfiles=200, tryoffline=TRUE, quiet=TRUE)

fcp2 <- suppressWarnings(thicken(fcp, "30 min", by="DateTime", colname="DateTime2")) %>%
  mutate(Ring = get_ring(Source)) %>%
  group_by(DateTime2) %>%
  rename(wind_speed = WindSpeed, air_pressure = IRGA.Pressure) %>%
  dplyr::select(DateTime2, wind_speed, air_pressure) %>%
  dplyr::summarize_all(funs(mean)) %>%
  rename(DateTime = DateTime2)

