library(tidyverse)
library(readr)
library(sf)
library(purrr)
#----------------------------------

wd <- "/Users/jonathanleape/Documents/Apps/pmcmv"

empreendimentos_hex <- file.path(wd, "data", "intermediate", "hex", "empreendimentos_hex.Rds") %>%
  readRDS()

empreendimentos_pts <- file.path(wd, "data", "intermediate", "empreendimentos.geojson") %>%
  read_sf()

hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
  read_sf() %>%
  st_transform(4326)

ttm <- file.path(wd, "data", "intermediate", "otp", "ttm.Rds") %>%
  readRDS()

# acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>%
#   readRDS()
# 
# emp_acess <- empreendimentos_hex %>%
#   left_join(acc, by = "ID")

# MAPA ISOCRONAS

context <- file.path(wd, "data", "input", "shapes", "33MUE250GC_SIR.shp") %>%
  read_sf() %>%
  st_transform(4326) %>%
  filter(!(NM_MUNICIP == "RIO DE JANEIRO"))

study_area <- file.path(wd, "data", "input", "shapes", "RJ_municipio.shp") %>%
  read_sf() %>%
  st_transform(4326)

yr <- "2016"
ID <- 3290
nome <- "Mikonos"

mapa_isocronas <- function(ttm, yr, ID, name) {
  
  isochrones_df <- ttm %>%
    filter(ano == yr, origin == ID, !is.na(tt_bin)) %>%
    inner_join(hex_grid, by = c("destination" = "ID")) %>%
    st_as_sf()
  
  isochrones_df %>%
    ggplot() + 
    geom_sf(data = context, fill = "#999999") +
    geom_sf(data = study_area) +
    coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.1, -22.76)) +
    geom_sf(data = isochrones_df, aes(fill = as.factor(tt_bin)), color = NA) +
    scale_fill_brewer(name = "tempo de viagem\n(minutos)",
                      breaks = c("30", "60", "90", "120", "150", "180", ">180"),
                      palette = "Blues",
                      direction = -1) +
    theme_minimal()
  
  paste0("isocronas_", name, "_", yr, ".png") %>%
    ggsave(plot = last_plot(), 
           device = "png", 
           path = file.path(wd, "data", "output", "mapas"))
}

years <- c("2014", "2015", "2016", "2017")

for (yr in years) {
  by(empreendimentos_hex, seq_len(nrow(empreendimentos_hex)), function(row) mapa_isocronas(ttm, yr, row["ID"], row["Nome_sem_Acento"]))  
}
  