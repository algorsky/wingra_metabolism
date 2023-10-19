library(tidyverse)
library(LakeMetabolizer)
library(lubridate)
library(reshape2)

setwd("~/Documents/Adrianna/wingra_metabolism/")

logger <- read.csv("data/january/deep_DO.csv")
meteorology <- read.csv('data/january/weather.csv')

df = merge(logger, meteorology, by = 'datetime') %>%
  mutate(datetime = lubridate::ymd_hms(datetime),
         DO_sat = o2.at.sat.base(temp = Temp_C, altitude = 258),
         z_mix = 1,
         k_600 = k.vachon.base(wnd = wind_speed_ms, lake.area = 1.3 * 10**6),
         k_gas = k600.2.kGAS.base(k600 = k_600, temperature =Temp_C, gas = "O2"),
         PAR = solar_flux_Wm2 * 2.114)

# SW (W/m2) to PAR (umol/m2/s) is 2.114
# gpp = p_max * tanh((P_alpha * lux))/ p_max)
# r = r_20 * theta**(T-20)
# 15 min measurements --> frequency of 96 (60/15 * 24) measurements per day

data = df %>%
  rename(do.obs = DO_mgL, do.sat = DO_sat, k.gas = k_gas, z.mix = z_mix, irr = PAR, wtr = Temp_C) %>%
  select(datetime, do.obs, do.sat, k.gas, z.mix, irr, wtr)

m.df = reshape2::melt(df, id = 'datetime')
ggplot(m.df) +
  geom_line(aes(datetime, value)) + 
  facet_wrap(~ variable, scales = 'free') +
  theme_minimal()

output = metab(data = data, method = 'bayesian')

str(output)
 
ggplot(output) +
  geom_line(aes(doy, GPP, col = "GPP")) +
  geom_line(aes(doy, NEP, col = 'NEP')) +
  geom_line(aes(doy, R, col = "R")) +
  theme_minimal()
