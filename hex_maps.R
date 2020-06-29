library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(geojsonsf)
library(RColorBrewer)
library(scales)
library(ggsn)
library(gridExtra)
library(viridis)
library(cowplot) # to extract legend

options(scipen=999) #no scientific notation

wd <- "/Users/jonathanleape/Library/Mobile Documents/com~apple~CloudDocs/Old Work/MIT/PMCMV"
#----------------------------------

# Layers ####
register_google(key = GOOGLE_KEY)
base_tile <- get_stamenmap(bbox=c(-43.8, -23.12, -43.15, -22.76), zoom=11, 
                     maptype = 'terrain-background',
                     style = c(feature = "all", element = "labels", visibility = "off"))

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

projects_df <- file.path(wd, "data", "intermediate", "projects.Rds") %>%
  readRDS()

projects <- file.path(wd, "data", "intermediate", "hex", "hex_points.geojson") %>%
  read_sf() %>%
  st_transform(4326) %>%
  rename(hex = ID) %>% 
  inner_join(projects_df, by = "hex") %>% 
  distinct(cod_lv, .keep_all = T)

# Base Map
base_map <- ggplot() +
  geom_sf(data = study_area, fill = "grey", colour = NA) +
  geom_sf(data = context, fill = "lightgrey", colour = "grey") + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggsn::north(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76, 
              location = "bottomright", symbol = 12, scale = 0.15) +
  ggsn::scalebar(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76,
                 location = "bottomleft",
                 dist = 10, dist_unit = "km", 
                 height=0.01,
                 transform = TRUE, model = "WGS84", st.size = 4) + 
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "",y = "")

base_map_dark <- ggplot() +
  geom_sf(data = study_area, fill = "#252525", colour = "grey") +
  geom_sf(data = context, fill = "#252525", colour = "grey") + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  # ggsn::north(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76, 
  #             location = "bottomright", symbol = 12, scale = 0.15) +
  # ggsn::scalebar(x.min = -43.8, x.max = -43.15, y.min = -23.12, y.max = -22.76,
  #                location = "bottomleft",
  #                dist = 10, dist_unit = "km", 
  #                height=0.01,
  #                transform = TRUE, model = "WGS84", st.size = 4) + 
  theme(panel.background = element_rect(fill = "grey"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "",y = "")

# Planning Zone ####
AP_names <- data.frame("AP" = c(1:5),
                       "AP_name" = c("Centro", "Zona Sul", "Zona Norte", "Barra da Tijuca", "Zona Oeste"))


RP <- file.path(wd, "data", "input", "shapes", "political", "RP.shp") %>%
  read_sf() %>%
  st_transform(4326) %>% 
  inner_join(AP_names, by = "AP")

RP_map <- base_map +
  geom_sf(data = RP, aes(fill = AP_name), size = 0.5) +
  # geom_sf_label(data = RP, aes(label = AP_name), label.size = 0.5) +
  geom_sf(data = projects, color = "black", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 1))

png_name <- "RP_map"
ggsave(filename = png_name, 
       plot = RP_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 15, #default 10.74
       units = "cm")

# Job Distribution ####
dist_center <- file.path(wd, "data", "intermediate", "osrm", "dist_center.csv") %>%
  read.csv(stringsAsFactors = F) %>%
  mutate(dist_center = dist / 1000) %>% 
  dplyr::select(hex, dist_center)

jobs <- file.path(wd, "data", "intermediate", "hex", "hex_opps.Rds") %>%
  readRDS() %>% 
  filter(ano == "2017") %>% 
  mutate(jobs = trabalhadores / 1000) %>%
  dplyr::select(hex = ID, jobs) %>% 
  inner_join(dist_center, by = "hex")

jobs_total <- sum(jobs$jobs)

png(file.path(wd, "data", "output", "monocentricity", "monocentricity.png"),
    width = 700, height = 600)

jobs %>% 
  mutate(cent_bin = cut(dist_center, breaks = c(0, 15, 30, 45, 60, 75))) %>%
  group_by(cent_bin) %>%
  summarise(jobs = sum(jobs)) %>% 
  ungroup() %>% 
  mutate(jobs_perc = sprintf("%1.2f%%", 100*(jobs / jobs_total))) %>%
  ggplot() +
  aes(x = cent_bin, weight = jobs) +
  geom_bar(fill = "#0c4c8a") +
  geom_text(aes(y = jobs, label = jobs_perc), vjust=-0.5, color="#0c4c8a", size=7) +
            # position = position_dodge(width=0.9), vjust=-0.25) + 
  theme_minimal() + 
  labs(x = "\nDistance from Center (KM)", 
       y = "Number of Jobs (1000s)\n") + 
  # scale_x_discrete(name ="\nDistance from Center (KM)", 
  #                  limits=c("15", "30", "45", "60", "75")) + 
  theme(text = element_text(size=25))

dev.off()

hex_jobs <- hex_grid %>% 
  inner_join(jobs, by = c("origin" = "hex"))

# MAP
jobs_map <- base_map_dark +
  geom_sf(data = (hex_jobs %>% 
                    filter(jobs > 0,
                           dist_center > 10)), 
          aes(fill = jobs), 
          color = NA) +
  # scale_fill_gradient(low = "#252525", high = "yellow", na.value = "#252525",
  #                      limits = c(0,25),
  #                      "Jobs (1000s)") +
  scale_fill_gradientn(colours = viridis(256,
                                         option = "cividis",
                                         begin = 0, end = 1,
                                         direction = 1),
                       limits = c(0,20),
                       "Jobs (1000s)") +
  geom_sf(data = projects, color = "red", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  # ggtitle("Driving Distance to City Center") + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 1))

png_name <- "jobs_map.png"
ggsave(filename = png_name, 
       plot = jobs_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 15, #default 10.74
       units = "cm")

# Accessibility ####
map_access <- function(acc, yr, max_tt) {
  
  acc_yr_tt <- acc %>% 
    filter(ano == yr,
           tt == max_tt)
  
  hex_acc <- hex_grid %>% 
    left_join(acc_yr_tt, by = "origin") %>%
    mutate(acc = ifelse(is.na(acc), 0, acc)) %>%
    filter(acc > 0)
  
  # MAP
  acc_map <- base_map_dark +
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
    ggtitle(paste("", yr))
  
  acc_map
}

legend_access <- function(acc, yr, max_tt) {
  
  acc_yr_tt <- acc %>% 
    filter(ano == yr,
           tt == max_tt)
  
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

# accessibility maps
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

# Centrality ####
dist_center <- file.path(wd, "data", "intermediate", "osrm", "dist_center.csv") %>%
  read.csv(stringsAsFactors = F) %>%
  mutate(dist_center = dist / 1000) %>% 
  dplyr::select(hex, dist_center)
  
hex_cent <- hex_grid %>% 
  inner_join(dist_center, by = c("origin" = "hex"))

# MAP
cent_map <- base_map_dark +
  geom_sf(data = (hex_cent %>% 
                    filter(dist_center > 0)), 
          aes(fill = dist_center), 
          color = NA) +
  scale_fill_gradientn(colours = viridis(256, 
                                         option = "cividis", 
                                         begin = 0, end = 1, 
                                         direction = -1),
                       limits = c(0,80),
                       "Distance (km)") + 
  geom_sf(data = projects, color = "red", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Driving Distance to City Center") + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 1))

png_name <- "dist_center_map.png"
ggsave(filename = png_name, 
       plot = cent_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 15, #default 10.74
       units = "cm")

# Murder ####
crime_cisp <- file.path(wd, "data", "input", "shapes", "indicators", "CISP","crime_cisp.Rds") %>% 
  readRDS() %>% 
  filter(CISP != 1)

murder_map <- base_map_dark +
  geom_sf(data = crime_cisp, 
          aes(fill = murder_rate), 
          color = NA) +
  scale_fill_gradientn(colours = viridis(256, 
                                         option = "cividis", 
                                         begin = 0, end = 1, 
                                         direction = 1),
                       limits = c(0,60),
                       "Murders per 100k") + 
  geom_sf(data = projects, color = "red", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Murder Rate (2017)") + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 1))

png_name <- "murder_map.png"
ggsave(filename = png_name, 
       plot = murder_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 15, #default 10.74
       units = "cm")

# IDS ####
ids <- file.path(wd, "data", "input", "shapes", "indicators", "IDS.shp") %>% 
  read_sf() %>% 
  st_transform(4326) %>% 
  dplyr::select(index = CDURPDBO24)

ids_map <- base_map_dark +
  geom_sf(data = ids, 
          aes(fill = index), 
          color = NA) +
  scale_fill_gradientn(colours = viridis(256, 
                                         option = "cividis", 
                                         begin = 0, end = 1, 
                                         direction = 1),
                       limits = c(0,1),
                       "Index") + 
  geom_sf(data = projects, color = "red", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Social Development Index (2010)") + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position="top", 
                                title.hjust = 1,
                                barwidth = 10))

png_name <- "ids_map.png"
ggsave(filename = png_name, 
       plot = ids_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 15, #default 10.74
       units = "cm")

# Favela ####
favelas <- file.path(wd, "data", "input", "shapes", "political", "Limite_Favelas_2016.shp") %>% 
  read_sf() %>% 
  st_transform(4326)

favela_map <- base_map_dark +
  geom_sf(data = favelas, 
          fill = "darkblue", 
          color = "blue") + 
  geom_sf(data = projects, color = "red", size = 0.5) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Favela Boundaries (2017)")

png_name <- "favela_map.png"
ggsave(filename = png_name, 
       plot = favela_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 10, #default 10.74
       units = "cm")

# Projects ####
empinc_end <- file.path(wd, "data", "intermediate", "rais", "participantes", "empinc.Rds") %>%
  readRDS() %>% 
  filter(month == ymd('2017-12-01'))

empinc_proj <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>% 
  # edital01.2014 was invalidated and edital26.2015 does not appear to be random
  filter(!edital %in% c("edital01.2014", "edital26.2015"),
         territorialidade == 0,
         ben == 1) %>% 
  left_join(empinc_end, by = c("cpfstd")) %>% 
  mutate(emp = replace_na(emp, 0),
         inc = replace_na(inc, 0),
         hrs_wk = replace_na(hrs_wk, 0)) %>% 
  dplyr::select(cod_lv, emp, inc, hrs_wk) %>% 
  group_by(cod_lv) %>% 
  summarise_all(list(mean)) %>% 
  ungroup()

proj_rais <- projects %>% 
  inner_join(empinc_proj, by = "cod_lv")
  # st_transform(3857)
  
proj_map <- ggmap(base_tile) + 
  geom_sf(data = context, fill = "lightgrey", colour = "grey", 
          alpha = 0.2, inherit.aes = FALSE) + 
  geom_sf(data = proj_rais, 
          aes(color = inc), 
          size = 2,
          inherit.aes = FALSE) + 
  ggsn::north(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76, 
              location = "bottomright", symbol = 12, scale = 0.15) +
  ggsn::scalebar(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76,
                 location = "bottomleft",
                 dist = 10, dist_unit = "km", 
                 height=0.01,
                 transform = TRUE, model = "WGS84", st.size = 2) + 
  scale_colour_gradientn(colours = viridis(256,
                                         option = "viridis",
                                         begin = 0, end = 1,
                                         direction = 1,
                                         alpha = .8),
                       limits = c(0,800)) +
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Monthly Income (Dec. 2017)") + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.justification = "center",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') +
  guides(fill = guide_colourbar(barwidth = 20))

png_name <- "proj_inc_map.png"
ggsave(filename = png_name, 
       plot = proj_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 20, #default 15
       height = 16, #default 10.74
       units = "cm")

base_tile_zoom <- get_stamenmap(bbox=c(-43.72, -22.93, -43.47, -22.85), zoom=11, 
                                maptype = 'toner-lines')

proj_map_zoom <- ggmap(base_tile_zoom) + 
  geom_sf(data = context, fill = "lightgrey", colour = "grey", 
          alpha = 0.2, inherit.aes = FALSE) + 
  geom_sf(data = proj_rais, 
          aes(color = inc), 
          size = 3,
          inherit.aes = FALSE) + 
  scale_colour_gradientn(colours = viridis(256,
                                           option = "viridis",
                                           begin = 0, end = 1,
                                           direction = 1,
                                           alpha = .8),
                         limits = c(0,800)) +
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') +
  guides(fill = guide_colourbar(barwidth = 20))

png_name <- "proj_inc_map_zoom.png"
ggsave(filename = png_name, 
       plot = proj_map_zoom,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 20, #default 15
       height = 10, #default 10.74
       units = "cm")

# Original Addresses ####
participantes <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>%
  # edital01.2014 was invalidated and edital26.2015 appears to be non-random
  filter(!edital %in% c("edital01.2014", "edital26.2015"),
         territorialidade == 0,
         ben_futuro == 0) %>%
  mutate(group = ifelse(ben == 1, "beneficiary", NA),
         group = ifelse(sorteado == 1 & ben == 0, "denier", group),
         group = ifelse(sorteado == 0, "loser", group),
         pre_yr = year(dt_ent) - 1) %>%
  arrange(desc(ben), desc(sorteado)) %>% 
  distinct(cpfstd, .keep_all = TRUE) %>% 
  dplyr::select(cpfstd, group, pre_yr)

cadunico_hex <- file.path(wd, "data", "intermediate", "cadunico_hex.Rds") %>% 
  readRDS()

cep <- file.path(wd, "data", "intermediate", "cep.geojson") %>%
  geojson_sf()

origins <- participantes %>% 
  inner_join(., dplyr::select(cadunico_hex, cadunico_ano, cpfstd, cep),
             by = c("cpfstd", "pre_yr" = "cadunico_ano")) %>%
  inner_join(., cep, by = "cep") %>% 
  filter(group != "loser") %>% 
  st_as_sf()

origin_map <- base_map_dark + 
  geom_sf(data = origins, aes(color = group), size = 0.002, inherit.aes = FALSE) + 
  scale_color_brewer(palette = "Accent") + 
  geom_sf(data = projects, color = "red", size = 1, inherit.aes = FALSE) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  ggtitle("Lottery Winner Origins") + 
  theme(legend.position = "bottom")

png_name <- "origin_map.png"
ggsave(filename = png_name, 
       plot = origin_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 10, #default 10.74
       units = "cm")

# Original Addresses ####
projects_map <- base_map_dark + 
  geom_sf(data = projects, color = "red", size = 2, inherit.aes = FALSE) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.24, -22.64))

png_name <- "projects.png"
ggsave(filename = png_name, 
       plot = projects_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 15, #default 15
       height = 10, #default 10.74
       units = "cm")

# Rio Transit ####
transit_path <- file.path(wd, "data", "input", "shapes", "transit")

# Lines
Bonde_lines <- file.path(transit_path, "lines", "Bonde.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
BRT_lines <- file.path(transit_path, "lines", "BRT.shp") %>% 
  read_sf() %>% 
  st_transform(4326) %>% 
  filter(Flg_Ativa == 1)
BRT_plans <- file.path(transit_path, "lines", "BRT.shp") %>% 
  read_sf() %>% 
  st_transform(4326) %>% 
  filter(Flg_Ativa == 0)
Metro_lines <- file.path(transit_path, "lines", "Metro.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
Teleferico_lines <- file.path(transit_path, "lines", "Teleferico.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
Trem_lines <- file.path(transit_path, "lines", "Trem.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
VLT_lines <- file.path(transit_path, "lines", "VLT.shp") %>% 
  read_sf() %>% 
  st_transform(4326)

# Stations
Bonde_stations <- file.path(transit_path, "stations", "Bonde.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
BRT_stations <- file.path(transit_path, "stations", "BRT.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
Metro_stations <- file.path(transit_path, "stations", "Metro.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
Teleferico_stations <- file.path(transit_path, "stations", "Teleferico.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
Trem_stations <- file.path(transit_path, "stations", "Trem.shp") %>% 
  read_sf() %>% 
  st_transform(4326)
VLT_stations <- file.path(transit_path, "stations", "VLT.shp") %>% 
  read_sf() %>% 
  st_transform(4326)

transit_map <- ggmap(base_tile) + 
  geom_sf(data = context, fill = "lightgrey", colour = NA, 
          alpha = 0.7, inherit.aes = FALSE) + 
  geom_sf(data = Bonde_lines, color = "brown", size = 0.5, inherit.aes = FALSE, show.legend = "line") +
  geom_sf(data = BRT_lines, color = "blue", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  geom_sf(data = BRT_plans, linetype = "dotted", color = "blue", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  geom_sf(data = Metro_lines, color = "dark orange", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  geom_sf(data = Teleferico_lines, color = "dark red", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  geom_sf(data = Trem_lines, color = "green", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  # geom_sf(data = VLT_lines, color = "purple", size = 0.5, inherit.aes = FALSE, show.legend = "line") + 
  geom_sf(data = Bonde_stations, color = "brown", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) +
  geom_sf(data = BRT_stations, color = "blue", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) + 
  geom_sf(data = Metro_stations, color = "dark orange", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) + 
  geom_sf(data = Teleferico_stations, color = "dark red", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) + 
  geom_sf(data = Trem_stations, color = "green", size = 0.5, inherit.aes = FALSE, show.legend = FALSE) + 
  geom_sf(data = projects, color = "red", size = 0.5, inherit.aes = FALSE, show.legend = "point") + 
  ggsn::north(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76, 
              location = "bottomright", symbol = 12, scale = 0.15) +
  ggsn::scalebar(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76,
                 location = "bottomleft",
                 dist = 10, dist_unit = "km", 
                 height=0.01,
                 transform = TRUE, model = "WGS84", st.size = 4) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  # ggtitle("Rio de Janeiro Transit (2020)") + 
  scale_colour_manual(values = c("Streetcar" = "brown", 
                                 "BRT" = "dark blue",
                                 "Metro" = "dark orange",
                                 "Cable Car" = "dark red",
                                 "Train" = "dark green",
                                 "Light Rail" = "#6b1d85")) +
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.justification = "center",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('')

png_name <- "transit_map.png"
ggsave(filename = png_name, 
       plot = transit_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 20, #default 15
       height = 16, #default 10.74
       units = "cm")

# terrain map ####
terrain_map <- ggmap(base_tile) + 
  geom_sf(data = context, fill = "lightgrey", colour = NA, 
          alpha = 0.7, inherit.aes = FALSE) + 
  geom_sf(data = projects, color = "red", size = 0.5, inherit.aes = FALSE, show.legend = "point") + 
  ggsn::north(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76, 
              location = "bottomright", symbol = 12, scale = 0.15) +
  ggsn::scalebar(x.min = -43.75, x.max = -43.15, y.min = -23.1, y.max = -22.76,
                 location = "bottomleft",
                 dist = 10, dist_unit = "km", 
                 height=0.01,
                 transform = TRUE, model = "WGS84", st.size = 4) + 
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.12, -22.76)) +
  theme(legend.position="none",
        legend.box='horizontal',
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.justification = "center",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('')

png_name <- "terrain_map.png"
ggsave(filename = png_name, 
       plot = terrain_map,
       device = "png",
       path = file.path(wd, "data", "output", "maps"),
       width = 20, #default 15
       height = 12, #default 10.74
       units = "cm")