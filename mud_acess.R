library(tidyverse)
library(stringi)
library(stringr)
library(sf)
library(ggsn)

options(scipen=999) #no scientific notation
options(digits = 5)

wd <- "/Users/jonathanleape/Documents/Apps/pmcmv"

### CADUNICO HEX ####
cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS()

cadunico_hex <- file.path(wd, "data", "intermediate", "participantes_cadunico.Rds") %>% 
  readRDS() %>%
  dplyr::select(cpfstd, edital, inscrito, sorteado, 
         ben, ben_ano, ben_futuro, empreendimento, 
         cadunico_ano, cep) %>%
  filter(!is.na(cep)) %>%
  inner_join(cep_hex, by = "cep") %>%
  unique()

saveRDS(cadunico_hex, file.path(wd, "data", "intermediate", "hex", "cadunico_hex.Rds"))

# EMPREENDIMENTOS HEX ####
hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>%
  read_sf()

FGV_RJ <- data.frame("Nome_sem_Acento" = "FGV", "COD_LV" = "FGV", 
                     "LAT" = -22.9416219, "LONG" = -43.1822259, "ANO_ENTREGA" = NA)

empreendimentos_pts <- file.path(wd, "data", "input", "empreendimentos", "empreendimentos_faixa1_geral_latlong.csv") %>%
  read.csv2(fileEncoding = "WINDOWS-1252", strip.white = TRUE, stringsAsFactors = F) %>%
  rbind(FGV_RJ) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326)

st_write(empreendimentos_pts, 
         dsn = file.path(wd, "data", "intermediate", "empreendimentos.geojson"), 
         driver = "GeoJSON", # "ESRI Shapefile",
         delete_dsn = T)

empreendimentos_hex <- empreendimentos_pts %>%
  st_join(hex_grid) %>% 
  st_set_geometry(NULL)

saveRDS(empreendimentos_hex, file.path(wd, "data", "intermediate", "hex", "empreendimentos_hex.Rds"))
#----------------------------------

ano_antes <- "2014"
ano_depois <- "2017"

### MAPA
cep <- file.path(wd, "data", "intermediate", "cep.Rds") %>%
  readRDS()

cadunico_pts <- cadunico_hex %>%
  filter(cadunico_ano == ano_antes) %>%
  mutate(contador = 1) %>%
  group_by(cep) %>%
  summarise(inscritos = sum(contador)) %>%
  ungroup() %>%
  inner_join(cep, by = "cep") %>%
  st_as_sf() %>%
  st_transform(4326)

rj_municipio <- file.path(wd, "data", "input", "shapes", "RJ_municipio.shp") %>%
  read_sf() %>%
  st_transform(4326)

context <- file.path(wd, "data", "input", "shapes", "33MUE250GC_SIR.shp") %>%
  read_sf() %>%
  st_transform(4326) %>%
  filter(!(NM_MUNICIP == "RIO DE JANEIRO"))

context %>% ggplot() +
  coord_sf()+
  geom_sf(fill = "#999999")+
  geom_sf(data = rj_municipio, fill = "#CCCCCC")+
  geom_sf(data = cadunico_pts, color = "#000000", size = 0.5) +
  geom_sf(data = empreendimentos_pts, color = "#FF0000")+
  scale_color_manual(labels = c("local de moradia\nCADUNICO 2014", "empreendimentos\nPMCMV"))+
  north(x.min = -43.8, x.max = -43.1, y.min = -23.05, y.max = -22.76, 
        location = "bottomright", symbol = 16, scale = 0.2) +
  # scalebar(x.min = -43.8, x.max = -43.15, y.min = -23.07, y.max = -22.76,
  #          dist = 10, dd2km = TRUE, model = "WGS84", st.size = 1.5)+
  coord_sf(xlim = c(-43.8, -43.15), ylim = c(-23.07, -22.76)) +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        legend.position = "right",
        legend.title.align = 0.5,
        legend.text = element_text(size = 10, hjust = 0),
        text = element_text(size = 9),
        plot.caption = element_text(size = 10, hjust = 0.5))+
  labs(x = "", 
       y = "",
       caption = "Em preto local de moradia das famílias no cadunico de 2014;\nem vermelho empreendimento do PMCMV")

ggsave("moradia_cadunico_2014_empreendimentos.png", plot = last_plot(), 
       device = "png", 
       path = file.path(wd, "data", "output", "maps"),
       width = 16, #automatico eh 15
       height = 12, #automatico eh 10.74
       units = "cm")
#-------------------------------


### MUDANÇA DE ACESSIBILIDADE ####
calc_mud_acc <- function(edit, ano_antes, ano_depois, max_tt){
  
  print(edit)
  acc_antes <- acc %>%
    filter(ano == ano_antes, tt == max_tt)
  
  acc_depois <- acc %>%
    filter(ano == ano_depois, tt == max_tt)
  
  hex_antes <- cadunico_hex %>%
    filter(cadunico_ano <= ano_antes, 
           edital == edit,
           inscrito == 1,
           ben_futuro == 0) %>%
    arrange(-cadunico_ano) %>%
    select(cpfstd, edital, sorteado, hex_antes = ID, empreendimento) %>%
    distinct()
  
  cadunico_hex_depois <- cadunico_hex %>%
    filter(cadunico_ano == ano_depois, 
           edital == edit,
           inscrito == 1,
           ben_futuro == 0) %>%
    distinct_at(vars(cpfstd, ID))
  
  mud_acc_edital <- hex_antes %>% # moradia original
    inner_join(acc_antes, by = c("hex_antes" = "origin")) %>%
    rename(acc_antes = acc) %>%
    # moradia depois no cadunico
    left_join(cadunico_hex_depois, by = "cpfstd") %>%
    rename(hex_cad = ID) %>%
    # empreendimento depois
    left_join(empreendimentos_hex, by = "empreendimento") %>%
    rename(hex_emp = ID) %>%
    # moradia correta depois
    mutate(hex_depois = ifelse(sorteado == 1, hex_emp, hex_cad)) %>%
    inner_join(acc_depois, by = c("hex_depois" = "origin")) %>%
    rename(acc_depois = acc) %>%
    dplyr::select(cpfstd, edital, sorteado, empreendimento, 
                  hex_antes, acc_antes, hex_depois, acc_depois) %>% 
    mutate(acc_depois = ifelse(sorteado == 1 & is.na(acc_depois),
                  mean(acc_depois, na.rm = T), acc_depois),
           mud_acc = acc_depois - acc_antes)
  
  saveRDS(mud_acc_edital, file.path(wd, "data", "output", paste0("mud_acc_", edit, ".Rds")))
  mud_acc_edital
}

ano_antes <- "2014"
ano_depois <- "2017"
max_tt <- 90
# edit <- "edital01.2015"
editais <- c("edital01.2015", "edital04.2015", "edital07.2015")

acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>%
  readRDS()
empreendimentos_hex <- file.path(wd, "data", "intermediate", "hex", "empreendimentos_hex.Rds") %>%
  readRDS() %>%
  mutate(empreendimento = toupper(Nome_sem_Acento))

mud_acc <- do.call(rbind, lapply(editais, calc_mud_acc, 
                                 ano_antes = ano_antes, 
                                 ano_depois = ano_depois, 
                                 max_tt = max_tt))
saveRDS(mud_acc, file.path(wd, "data", "output", "mud_acc.Rds"))

# RESUMEN
mud_acc_res <- mud_acc %>%
  group_by(edital, sorteado) %>%
  summarize(acc_antes = mean(acc_antes),
            acc_depois = mean(acc_depois),
            mud_acc = mean(mud_acc))
