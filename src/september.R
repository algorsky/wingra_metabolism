library(tidyverse)
library(LakeMetabolizer)
library(lubridate)
library(reshape2)

setwd("~/Documents/Adrianna/wingra_metabolism/")

wingra <- read.csv("data/wingra_temp_do.csv")|>
  mutate(datetime = ymd_hms(datetime, tz = Sys.timezone()))
#calculate Z mix
z.mix = ts.meta.depths(get.vars(wingra, 'wtr'), seasonal=TRUE) |> 
  select(datetime, top)%>%
  rename(z.mix = top) |> 
  mutate(z.mix = if_else(z.mix <= 0 | is.na(z.mix), 3.4, z.mix))
wingra_df<-merge(wingra, z.mix, by = "datetime")

september<- wingra_df%>%
  filter(month(datetime)==9)
meteorology <- read.csv('data/september/weather.csv')|>
  mutate(datetime = ymd_hms(datetime, tz = Sys.timezone()))



# ADRIANNAS DATA OF LAKE WINGRA
df = merge(september, meteorology, by = 'datetime') %>%
  mutate(wind_speed_ms = wind_speed_ms,
         solar_flux_Wm2 = solar_flux_Wm2) %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         DO_sat = o2.at.sat.base(temp = Temp_C, altitude = 258),
        # z_mix = 1,
         k_600 = k.vachon.base(wnd = wind_speed_ms, lake.area = 1.3 * 10**6),
         k_gas = k600.2.kGAS.base(k600 = k_600, temperature =Temp_C, gas = "O2"),
         PAR = solar_flux_Wm2 * 2.114) #%>%
 # mutate(DO_sat_calc = (100 * DO_mgL)/DO_perc)

#ggplot(df) +
#  geom_line(aes(datetime, DO_sat, col = 'calculated')) + 
 # geom_line(aes(datetime, DO_sat_calc, col = 'downscaled')) + 
  #theme_minimal()

# SW (W/m2) to PAR (umol/m2/s) is 2.114
# gpp = p_max * tanh((P_alpha * lux))/ p_max)
# r = r_20 * theta**(T-20)
# 15 min measurements --> frequency of 96 (60/15 * 24) measurements per day


data = df %>%
 # mutate(min = minute(datetime)) %>% filter(min != 15) %>%
  rename(do.obs = DO_mgL, do.sat = DO_sat, k.gas = k_gas, z.mix = z.mix, irr = PAR, wtr = Temp_C) %>%
  select(datetime, do.obs, do.sat, k.gas, z.mix, irr, wtr)

m.df = reshape2::melt(data, id = 'datetime')
ggplot(m.df) +
  geom_line(aes(datetime, value)) + 
  facet_wrap(~ variable, scales = 'free') +
  theme_minimal()

ggplot(m.df %>% filter(datetime <= "2023-01-03 23:45:00")) +
  geom_line(aes(datetime, value)) + 
  facet_wrap(~ variable, scales = 'free') +
  theme_minimal()

ggplot(data) +
  geom_line(aes(datetime, do.obs - do.sat, col = 'diff')) +
  geom_line(aes(datetime, do.obs, col = 'obs')) +
  geom_line(aes(datetime, do.sat, col = 'sat')) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  theme_minimal()

# BAYESIAN
output = metab(data = data, method = 'bayesian', lake.lat = 43.3,  wtr.name="wtr", irr.name="irr", do.obs.name="do.obs")

str(output)
 
ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()

output_plot = output %>% mutate(date = as.Date(doy, origin = "2022-12-31")) %>%
  mutate(datetime = as.POSIXct(paste0(date," 12:00:00")))

filter_time = "2023-01-03 17:45:00"
ggplot(data%>% filter(datetime <= filter_time)) +
  geom_line(aes(datetime, do.obs)) + 
  geom_line(data = output_plot%>% filter(datetime <= filter_time), aes(datetime, GPP, col = "GPP")) +
  geom_point(data = output_plot%>% filter(datetime <= filter_time), aes(datetime, GPP, col = "GPP")) +
  geom_line(data = output_plot%>% filter(datetime <= filter_time), aes(datetime, R, col = "R")) +
  geom_point(data = output_plot%>% filter(datetime <= filter_time), aes(datetime, R, col = "R")) +
  theme_minimal()

# BOOKKEEPING
output = metab(data = data, method = 'bookkeep', lake.lat = 43.3)

ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()

# OLS
output = metab(data = data, method = 'ols', lake.lat = 43.3)

ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()

# MLE
output = metab(data = data, method = 'mle', lake.lat = 43.3)

ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()

# KALMAN
output = metab(data = data, method = 'kalman', lake.lat = 43.3)

ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()

