library(ggplot2)
library(dplyr)
library(sf)
library(geojsonsf)
library(RColorBrewer) 
library(scales) 
library(ggsn)
library(gridExtra) 
library(viridis) 
library(cowplot)

options(scipen=999) 

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
#----------------------------------

# Cumulative opportunities by hex
accessibility <- function(yr, tt, ttm_yr) {
  
  walk_speed <- 1.4 # m/s
  wait_time <- 600 # seconds
  
  hex_opps_yr <- file.path(wd, "data", "intermediate", "hex", "hex_opps.Rds") %>% 
    readRDS() %>%
    filter(ano == yr) %>%
    dplyr::select(destination = ID,
                  trabalhadores)
  
  total_trabalhadores <- sum(hex_opps_yr$trabalhadores)
  
  acc_yr_tt <- ttm_yr %>% 
    mutate(trip_tt = travel_time) %>%
    dplyr::select(origin, destination, trip_tt) %>%
    inner_join(hex_opps_yr, by = "destination") %>% 
    mutate(trabalhadores = ifelse(is.na(trabalhadores), 0, trabalhadores)) %>%
    filter(trip_tt <= tt) %>%
    group_by(origin) %>%
    summarise(acc = (sum(trabalhadores)/total_trabalhadores)*100) %>%
    ungroup() %>%
    mutate(ano = yr, tt = tt / 60) %>%
    dplyr::select(origin, acc, ano, tt)
  
  paste0("acc_", yr, "_", tt / 60, ".Rds") %>%
    file.path(wd, "data", "intermediate", "hex", .) %>%
    saveRDS(acc_yr_tt, .)
  
  acc_yr_tt
}

map_access <- function(acc, yr, max_tt) {
  
  acc_yr_tt <- acc %>% 
    filter(ano == yr,
           tt == max_tt)
  
  # Layers
  study_area <- file.path(wd, "data", "input", "shapes", "political", "RJ_municipio.shp") %>%
    read_sf() %>%
    st_transform(4326)
  
  context <- file.path(wd, "data", "input", "shapes", "political", "33MUE250GC_SIR.shp") %>%
    read_sf() %>%
    st_transform(4326) %>%
    filter(!(NM_MUNICIP == "RIO DE JANEIRO"))
  
  hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
    read_sf() %>%
    st_transform(4326) %>%
    rename(origin = ID)
  
  hex_acc <- hex_grid %>% 
    left_join(acc_yr_tt, by = "origin") %>%
    mutate(acc = ifelse(is.na(acc), 0, acc)) %>%
    filter(acc > 0)
  
  projects_df <- file.path(wd, "data", "intermediate", "projects.Rds") %>%
    readRDS() 
  
  projects <- file.path(wd, "data", "intermediate", "hex", "hex_points.geojson") %>%
    read_sf() %>%
    st_transform(4326) %>%
    rename(hex = ID) %>% 
    inner_join(projects_df, by = "hex")
  
  # MAPA
  acc_map <- ggplot() +
    geom_sf(data = study_area, fill = "black", colour = NA) +
    geom_sf(data = context, fill = "lightgrey", colour = "grey") +
    geom_sf(data = (hex_acc %>% 
                      filter(acc > 0)), 
            aes(fill = acc), 
            color = NA) +
    scale_fill_gradientn(colours = viridis(256, 
                                           option = "cividis", 
                                           begin = 0, end = 1, 
                                           direction = 1),
                         limits = c(0,100)) + 
    geom_sf(data = projects, color = "red", size = 0.5) + 
    coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
    ggsn::north(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76, 
                location = "bottomright", symbol = 12, scale = 0.15) +
    ggsn::scalebar(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76,
                   location = "bottomleft",
                   dist = 10, dist_unit = "km", 
                   height=0.01,
                   transform = TRUE, model = "WGS84", st.size = 2) +
    ggtitle(paste("", yr)) +
    theme(panel.background = element_rect(fill = "darkgrey"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.title.align = 0.5,
          plot.title = element_text(size = 10),
          axis.text.x = element_blank(), axis.text.y = element_blank()) +
    labs(x = "",y = "")
  
  acc_map
}

legend_access <- function(acc, yr, max_tt) {
  
  acc_yr_tt <- acc %>% 
    filter(ano == yr,
           tt == max_tt)
  
  hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
    read_sf() %>%
    st_transform(4326) %>%
    rename(origin = ID)
  
  hex_acc <- hex_grid %>% 
    left_join(acc_yr_tt, by = "origin") %>%
    mutate(acc = ifelse(is.na(acc), 0, acc)) %>% 
    filter(acc > 0)
  
  legend <- get_legend(ggplot() + 
                         geom_sf(data = hex_acc, 
                                 aes(fill = acc), 
                                 color = NA) +
                         scale_fill_gradientn(colours = viridis(256, 
                                                                option = "cividis", 
                                                                begin = 0, end = 1, 
                                                                direction = 1),
                                              limits = c(0,100), 
                                              name = "% Formal Jobs\nAccessible in 90 min.") +
                         theme(legend.position='bottom',
                               legend.box='horizontal',
                               legend.title=element_text(size=8),
                               legend.text=element_text(size=10),
                               legend.justification = "center") +
                         guides(fill = guide_colourbar(title.position="top", 
                                                       title.hjust = 0.5)))
  
  legend
}

# loop through years and max travel times
acc <- data.frame(matrix(ncol = 4, nrow = 0)) %>%
  setNames(c("origin", "acc", "ano", "tt"))

ttm <- file.path(wd, "data", "intermediate", "otp", "ttm.Rds") %>%
  readRDS()

years <- c("2014", "2015", "2016", "2017")
max_tts <- c(1800, 3600, 5400, 7200, 9000, 10800)

for (yr in years) {
  ttm_yr <- ttm %>%
    filter(ano == yr)
  for (tt in max_tts) {
    acc_yr_tt <- accessibility(yr, tt, ttm_yr)
    acc <- rbind(acc, acc_yr_tt)
    acc_map_yr_tt <- map_access(acc_yr_tt, yr, tt)
  }
  # map_grid(map_list, png_name, png_path)
}
saveRDS(acc, file.path(wd, "data", "intermediate", "hex", "acc.Rds"))

# just map
acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>% 
  readRDS()

acc_legend <- legend_access(acc, "2014", 90)

png_name <- "acc_legend.png"
ggsave(filename = png_name, 
       plot = acc_legend,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 10, #default 15
       height = 5, #default 10.74
       units = "cm")

acc_map14 <- map_access(acc, "2014", 90)
acc_map15 <- map_access(acc, "2015", 90)
acc_map16 <- map_access(acc, "2016", 90)
acc_map17 <- map_access(acc, "2017", 90)

grid90 <- grid.arrange(acc_map14, acc_map15, acc_map16, acc_map17, ncol = 2)

png_name <- "acc_map_90min.png"
ggsave(filename = png_name, 
       plot = grid90,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 12, #default 10.74
       units = "cm")

png_name <- "acc_map_2017.png"
ggsave(filename = png_name, 
       plot = acc_map17,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 12, #default 10.74
       units = "cm")