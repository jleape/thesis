library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(stringi)
library(leaflet)
library(sjPlot)
library(lubridate)
theme_set(theme_bw())

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

# Accessibility
hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
  read_sf()

acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>%
  readRDS() %>%
  mutate(year = as.integer(ano)) %>%
  filter(tt == 90,
         year == 2017) %>%
  mutate(acc = acc / 100)

# Project Uptake
# FIX need to pull Sorteados from sorteados table. Ideally get
sorteados <- file.path(loteria_path, "sorteados_wide.Rds") %>%
  readRDS() %>%
  filter(!is.na(cpfstd)) %>%
  gather(colnames(.)[!(colnames(.) %in% c("cpfstd", 
                                          "vezes_aparece", 
                                          "primeiro_sorteio", 
                                          "ultimo_sorteio"))],
         key = "edital", 
         value = "sorteado")

projects <- file.path(wd, "data", "input", "empreendimentos", "empreendimentos.xlsx") %>%
  read_excel() %>%
  dplyr::select(COD_LV, Nome_sem_acento, Endereco, Lat, Lon, 
                Edital_num, Edital_ano,
                Unidades, Sorteados, Territorialidade, Ano_fim) %>%
  # filter(Ano_fim < 2015) %>%
  mutate(lottery = paste0(Edital_ano, '-', Edital_num)) %>%
  group_by(COD_LV) %>%
  mutate(winners = round(sum(Sorteados))) %>%
         # terr = sum(Territorialidade)) %>%
  filter(min_rank(lottery) == 1L) %>%
         # terr == 0) %>%
  ungroup() %>%
  mutate(uptake = Unidades / winners,
         pos2015 = ifelse(Ano_fim >= 2015, 1, 0)) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)  %>%
  st_join(hex_grid, left = TRUE) %>% 
  # st_set_geometry(NULL) %>%
  inner_join(acc, by = c("ID" = "origin")) %>%
  dplyr::select(COD_LV, Nome_sem_acento, Endereco, Ano_fim, pos2015,
                Unidades, winners, uptake, acc)

# Leaflet Map
leaflet(projects) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(stroke = TRUE, 
             color = "grey",
             weight = 1,
             radius = ~Unidades,
             group = "Projects",
             fillColor = ~colorQuantile("YlOrRd", uptake)(uptake),
             fillOpacity = .6,
             popup = paste("Name:", projects$Nome_sem_acento, "<br>",
                           "Lottery Winners:", projects$winners, "<br>",
                           "Housing Units:", projects$Unidades, "<br>",
                           "Uptake:", scales::percent(projects$uptake), "<br>",
                           "Job Access (90 min):", scales::percent(projects$acc))
  )

# Uptake vs. job access
uptake_acc <- lm(uptake ~ acc, projects)
tab_model(uptake_acc, 
          digits = 4)

ggplot(projects) +
  aes(x = acc, y = uptake) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(x = "Job access within 90 min by transit (%)", y = "Uptake among lottery winners (%)", title = "Uptake vs. Job Access", subtitle = "PMCMV Faixa 1 Projects") +
  theme_minimal() + 
  geom_text(x = 25, y = 300, label = lm_eqn(projects), parse = TRUE)

# Uptake vs. territoriality law

uptake_pos2015 <- lm(uptake ~ pos2015, projects)
tab_model(uptake_pos2015, 
          digits = 4)

uptake_acc_pos2015 <- lm(uptake ~ acc + pos2015, projects)
tab_model(uptake_acc_pos2015, 
          digits = 4)

ggplot(projects) +
  aes(x = Ano_fim, y = uptake) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  labs(x = "Year filled", y = "Uptake among lottery winners (%)", title = "Uptake vs. Job Access", subtitle = "PMCMV Faixa 1 Projects") +
  theme_minimal() + 
  geom_text(x = 25, y = 300, label = lm_eqn(projects), parse = TRUE)

# ggplot Map
context <- file.path(wd, "data", "input", "shapes", "political", "33MUE250GC_SIR.shp") %>%
  read_sf() %>%
  st_transform(4326) %>%
  filter(!(NM_MUNICIP == "RIO DE JANEIRO"))

study_area <- file.path(wd, "data", "input", "shapes", "political", "RJ_municipio.shp") %>%
  read_sf() %>%
  st_transform(4326)

projects %>%
  ggplot() + 
  geom_sf(data = study_area) +
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.1, -22.76)) +
  geom_sf(data = projects$geometry, size = 4, shape = 23, fill = "darkred") +
  theme_minimal()

# Isochrones
paste0("isocronas_", name, "_", yr, ".png") %>%
  ggsave(plot = last_plot(), 
         device = "png", 
         path = file.path(wd, "data", "output", "mapas"))
