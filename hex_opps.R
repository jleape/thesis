library(tidyverse)
library(haven) #para ler stata
library(readxl)
library(sf)
library(geojsonsf)
library(lubridate)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

### ESTABELECICMENTO DA RAIS POR CEP ####
estb_cols <- list(
  "2014" = c("municpio", "indatividadeano", "razosocial", "cepestab",
             "qtdvnculosativos", "qtdvnculosestatutrios"),
  "2015" = c("município", "indatividadeano", "razãosocial", "cepestab",
             "qtdvínculosativos", "qtdvínculosestatutários"),
  "2016" = c("Município", "Ind.Atividade.Ano", "Razão.Social", "CEP.Estab", 
             "Qtd.Vínculos.Ativos", "Qtd.Vínculos.Estatutários"),
  "2017" = c("Município", "Ind.Atividade.Ano", "Razão.Social", "CEP.Estab", 
             "Qtd.Vínculos.Ativos", "Qtd.Vínculos.Estatutários")
)

import_estb <- function(yr, estb_cols){
  
  print(yr)
  cols <- unlist(estb_cols[yr], use.names=FALSE)
  ### Exclude public services and headquarters with distributed job locations ####
  to_remove <- c("SECRETARIA DE ESTADO DE EDUCACAO", 
                 "POLICIA MILITAR DO ESTADO DO RIO DE JANEIRO", 
                 "NOVA RIO SERVICOS GERAIS LTDA", 
                 "MINISTERIO DA SAUDE",
                 "CORPO DE BOMBEIROS MILITAR DO ESTADO DO RIO DE JANEI", 
                 "TRIBUNAL DE JUSTICA DO ESTADO DO RIO DE JANEIRO",
                 "SECRETARIA DE ESTADO DE SAUDE", 
                 "FUNDACAO DE APOIO A ESCOLA TECNICA DO ESTADO DO RIO", 
                 "POLICIA CIVIL DO ESTADO DO RIO DE JANEIRO", 
                 "PROL CENTRAL DE SERVICOS LTDA", 
                 "HOPE RECURSOS HUMANOS S.A.", 
                 "GUARDA MUNICIPAL DA CIDADE DO RIO DE JANEIRO GM RIO", 
                 "CNS NACIONAL DE SERVICOS LIMITADA", 
                 "ANGELS SERVICOS TECNICOS LTDA", 
                 "COMPANHIA ESTADUAL DE AGUAS E ESGOTOS - CEDAE", 
                 "GAP RIO DE JANEIRO", 
                 "FUNDACAO SAUDE DO ESTADO DO RIO DE JANEIRO",
                 "PREFEITURA DA CIDADE DO RIO DE JANEIRO")
  
  estb <- file.path(wd, "data", "input", "rais", paste0("estb", yr, ".Rds")) %>%
    readRDS() %>%
    rename(munic = cols[1],
           indatividadeano = cols[2],
           razao_social = cols[3],
           cep = cols[4],
           qtdvnculosativos = cols[5],
           qtdvnculosestatutrios = cols[6]) %>%
    filter(munic %in% c(3304557, 330455), 
           indatividadeano == 1) %>% 
    mutate(trabalhadores = as.numeric(qtdvnculosativos) + 
             as.numeric(qtdvnculosestatutrios)) %>%
    dplyr::select(cep, trabalhadores, razao_social) %>%  #219.741 empresas, 3.134.395 trabalhadores
    filter(cep != 99999999) %>% #215.634 empresas, 3.086.176 trabalhadores com CEP válido
    filter(!(razao_social %in% to_remove)) #31 empresas e 724.331 empregos excluídos
  
  cep_opps <- estb %>%
    group_by(cep) %>% 
    summarise(trabalhadores = sum(trabalhadores)) %>% 
    filter(trabalhadores > 0) %>%
    arrange(-trabalhadores) %>%
    mutate(cep = as.numeric(cep), ano = yr) %>%
    ungroup()
}

years <- c("2014", "2015", "2016", "2017")
cep_opps <- do.call(rbind, lapply(years, import_estb, estb_cols = estb_cols))
saveRDS(cep_opps, file.path(wd, "data", "intermediate", "rais", "cep_opps.Rds"))

plot_cep_opps <- function(cep_opps, yr){
  cep_opps %>% 
    filter(ano = yr, trabalhadores < 2000) %>% 
    ggplot() + 
    geom_histogram(aes(x = trabalhadores))
}
#----------------------------------

### BASE DE CEPs ####
cep_taina <- read_excel(file.path(wd, "data", "input", "cep", "qualocep_geo_2.xlsx"), 
                        col_types = c("numeric", "numeric", "numeric")) %>%
  rbind(read_excel(file.path(wd, "data", "input", "cep", "qualocep_geo_1.xlsx"), 
                   col_types = c("numeric", "numeric", "numeric"))) %>%
  filter(!is.na(latitude))


cep_miguel <- file.path(wd, "data", "input", "cep", "ceps_brasil_fev18.csv") %>% 
  read_csv() %>%
  filter(cep_ativo == "S") %>% 
  dplyr::select(cep, latitude, longitude)

cep <- rbind(cep_taina, cep_miguel) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

st_write(cep, 
         dsn = file.path(wd, "data", "intermediate", "cep.geojson"), 
         driver = "GeoJSON",
         delete_dsn = T)
#----------------------------------

### GEORREFERENCIANDO OS CEPs ####
cep <- file.path(wd, "data", "intermediate", "cep.geojson") %>%
  geojson_sf()
hex_grid <- file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson") %>% 
  geojson_sf()

cep_hex <- cep %>%
  st_join(hex_grid) %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(ID))

saveRDS(cep_hex, file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds"))
#----------------------------------

# JOIN HEX X CEP_OPPS
cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS()
cep_opps <- file.path(wd, "data", "intermediate", "rais", "cep_opps.Rds") %>%
  readRDS()

hex_opps <- cep_hex %>% 
  inner_join(cep_opps, by = "cep") %>% 
  group_by(ID, ano) %>% 
  summarize(trabalhadores = sum(trabalhadores, na.rm = T)) %>% 
  ungroup()

sum(hex_opps$trabalhadores[hex_opps$ano == "2014"], na.rm = T) #2.229.031

saveRDS(hex_opps, file.path(wd, "data", "intermediate", "hex", "hex_opps.Rds"))
#----------------------------------

# other layers of interest
hex_points <- file.path(wd, "data", "intermediate", "hex", "hex_points.geojson") %>% 
  read_sf() %>% 
  rename(hex = ID)

favelas <- file.path(wd, "data", "input", "shapes", "political", "Limite_Favelas_2016.shp") %>% 
  read_sf() %>% 
  st_transform(4326)

ids <- file.path(wd, "data", "input", "shapes", "indicators", "IDS.shp") %>% 
  read_sf() %>% 
  st_transform(4326)

pop_cisp <- file.path(wd, "data", "input", "shapes", "indicators", "CISP", "CISP_pop.csv") %>% 
  read_csv(na = c("", " ")) %>% 
  filter(year == 2017)

crime_data <- file.path(wd, "data", "input", "shapes", "indicators", "CISP", "CISP_crime.csv") %>% 
  read_csv(na = c("", " ")) %>% 
  filter(vano == 2017, 
         munic == "Rio de Janeiro") %>% 
  dplyr::select(CISP, year = vano,
                murder = hom_doloso, attempted_homicide = tentat_hom, assault_w_injury = lesao_corp_dolosa, 
                rape = estupro, 
                mugging = total_roubos, theft = total_furtos,
                kidnapping = sequestro, extorsion = extorsao, fraud = estelionato, 
                drug_seizure = apreensao_drogas, 
                incident = registro_ocorrencias) %>% 
  group_by(CISP, year) %>% 
  summarise_all(~{sum(.x, na.rm = any(!is.na(.x)))}) %>%
  ungroup()

crime_indicators <- crime_data %>%
  inner_join(., pop_cisp, by = c("CISP", "year")) %>% 
  mutate(murder_rate = murder / pop * 100000,
         rape_rate = rape / pop * 100000, 
         mugging_rate = mugging / pop * 100000, 
         fraud_rate = fraud / pop * 100000, 
         drug_rate = drug_seizure / pop * 100000, 
         incident_rate = incident / pop * 100000) %>% 
  dplyr::select(CISP, year, 
                murder_rate, rape_rate, mugging_rate, 
                fraud_rate, drug_rate, incident_rate)

crime_cisp <- file.path(wd, "data", "input", "shapes", "indicators", "CISP", "Limites_CISP.shp") %>% 
  read_sf() %>% 
  dplyr::select(CISP = DP) %>% 
  st_transform(4326) %>% 
  inner_join(., crime_indicators, by = "CISP")

saveRDS(crime_cisp, file.path(wd, "data", "input", "shapes", "indicators", "CISP","crime_cisp.Rds"))

hex_indicators <- hex_points %>% 
  st_join(favelas) %>%
  mutate(favela = as.numeric(!is.na(OBJECTID))) %>% 
  dplyr::select(hex, favela) %>% 
  mutate(favela = replace_na(favela, 0)) %>% 
  st_join(ids) %>% 
  dplyr::select(hex, favela, ids = CDURPDBO24) %>%
  st_join(crime_cisp) %>% 
  dplyr::select(hex, favela, ids, 
                murder_rate, rape_rate, mugging_rate, drug_rate)

ggplot(data = hex_indicators) +
  geom_sf(aes(color = murder_rate)) +
  scale_color_viridis_c(option = "plasma")

hex_indicators %>% 
  st_set_geometry(NULL) %>% 
  saveRDS(file.path(wd, "data", "intermediate", "hex", "hex_indicators.Rds"))
