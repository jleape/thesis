library(tidyverse)
library(stringi)
library(sf)
library(readxl)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

# Accessibility
hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
  read_sf() %>%
  rename(hex = ID)

cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS() %>%
  rename(hex = ID)

acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>%
  readRDS() %>%
  mutate(year = as.integer(ano)) %>%
  filter(tt == 90)

acc_90_wide <- acc %>% 
  mutate(key = paste0("acc", year)) %>%
  dplyr::select(-ano, -tt, -year) %>%
  spread(key = key, value = acc) %>%
  rename(hex = origin)

# Distance to center
# center <- tibble(location = "center", lon = -43.182817, lat = -22.906905) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_join(hex_grid) %>% 
#   st_set_geometry(NULL)

dist_center <- file.path(wd, "data", "intermediate", "osrm", "dist_center.csv") %>%
  read.csv(stringsAsFactors = F) %>%
  mutate(dist_center = dist / 1000) %>% 
  dplyr::select(hex, dist_center)

### Empreendimentos and lotteries
project_lotteries <- file.path(wd, "data", "input", "empreendimentos", "empreendimentos.xlsx") %>%
  read_excel() %>%
  mutate(dt_ins = ymd(paste(insc_ano, insc_mes, 1)),
         dt_sor = ymd(paste(sort_ano, sort_mes, 1)),
         dt_ent = ymd(paste(entr_ano, entr_mes, 1))) %>%
  dplyr::select(cod_lv, units = unidades, edital, dt_ins, dt_sor, dt_ent, lat, lon, territorialidade) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(hex_grid) %>% 
  st_set_geometry(NULL) %>%
  left_join(., dist_center, by = "hex") %>%
  left_join(., acc_90_wide, by = "hex")

saveRDS(project_lotteries, file.path(wd, "data", "intermediate", "project_lotteries.Rds"))

projects <- file.path(wd, "data", "input", "empreendimentos", "empreendimentos.xlsx") %>%
  read_excel() %>%
  dplyr::select(cod_lv, nome, address, units = unidades, lat, lon) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(hex_grid) %>%
  st_set_geometry(NULL) %>% 
  unique()

saveRDS(projects, file.path(wd, "data", "intermediate", "projects.Rds"))

lotteries <- project_lotteries %>%
  group_by(edital) %>%
  summarise(dt_ins = max(dt_ins),
            dt_sor = max(dt_sor),
            dt_ent = mean(dt_ent),
            territorialidade = max(territorialidade),
            avg_dist_center = weighted.mean(dist_center, units),
            avg_acc2014 = weighted.mean(acc2014, units),
            avg_acc2015 = weighted.mean(acc2015, units),
            avg_acc2016 = weighted.mean(acc2016, units),
            avg_acc2017 = weighted.mean(acc2017, units)) %>%
  ungroup()

saveRDS(lotteries, file.path(wd, "data", "intermediate", "lotteries.Rds"))

