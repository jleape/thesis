library(sf)
library(sp)

wd <- "/Users/jonathanleape/Documents/Apps/pmcmv"

### geometry to columns ####
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
# --------------------------

area_path <- file.path(wd, "data", "input", "shapes", "RJ_municipio.shp")
crs <- 32723

### ABRIR ARQUIVO DA CIDADE DO RJ ####
area <- read_sf(area_path) %>%
  st_transform(crs) %>% #WGS 84 / UTM zone 23S
  as("Spatial")
# --------------------------

# definindo a malha hexagonal ####
size <- 500 #distancia entre os centroides (metros)
cell_area <- ((3^(1/3))/2)*(size/1000)^2 #in square km
hex_points <- spsample(area, 
                       type = "hexagonal", 
                       cellsize = size, 
                       offset = c(0.5,0.5)) #offset para nao ser aleatorio
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
# --------------------------

# Transformando a malha e os pontos em SF ####
hex_grid <- hex_grid %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  mutate(hex = 1:nrow(.))

hex_points <- hex_points %>% 
  st_as_sf() %>% 
  st_transform(4326) %>% 
  mutate(hex = 1:nrow(.)) 

# points CSV #### 
hex_points <- hex_points %>%
  sfc_as_cols(names = c("X", "Y")) %>%
  as.data.frame() %>% 
  dplyr::rename(GEOID = hex) %>% 
  dplyr::select(-geometry)

hex_points %>%
  write.csv(file = file.path(wd, "data", "intermediate", "hex", "hex_points.csv"))
# --------------------------
cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS() %>% 
  rename(hex = ID)

cad_hex <- file.path(wd, "data", "intermediate", "cadunico_std.Rds") %>%
  readRDS() %>% 
  ungroup() %>%
  dplyr::select(cep) %>% 
  inner_join(., cep_hex, by = c("cep")) %>% 
  dplyr::select(hex) %>% 
  unique()
  
cad_hex_pts <- hex_points %>% 
  filter(GEOID %in% cad_hex$hex) %>%
  write.csv(file = file.path(wd, "data", "intermediate", "hex", "cad_hex_pts.csv"))

# points e hex geojson ####
st_write(hex_grid, 
         dsn = file.path(wd, "data", "intermediate", "hex", "hex_grid.geojson"), 
         driver = "GeoJSON", # "ESRI Shapefile",
         delete_dsn = T)
st_write(hex_points, 
         dsn = file.path(wd, "data", "intermediate", "hex", "hex_points.geojson"), 
         driver = "GeoJSON", # "ESRI Shapefile",
         delete_dsn = T)
# --------------------------