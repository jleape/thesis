library(osrm)
library(tidyverse)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

# run local OSRM server
# https://hub.docker.com/r/osrm/osrm-backend/
# cd /Users/jonathanleape/Dropbox\ \(MIT\)/PMCMV/osrm-data 
# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-extract -p /opt/car.lua /data/rio-de-janeiro_brazil.osm.pbf
# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-partition /data/rio-de-janeiro_brazil.osrm
# docker run -t -v "${PWD}:/data" osrm/osrm-backend osrm-customize /data/rio-de-janeiro_brazil.osrm
# docker run -t -i -p 5000:5000 -v "${PWD}:/data" osrm/osrm-backend osrm-routed --max-table-size=1000000 --algorithm mld /data/rio-de-janeiro_brazil.osrm

# test
# curl "http://127.0.0.1:5000/route/v1/driving/-22.8764063,-43.4975303;-22.882899,-43.5445784?steps=true"

options(osrm.server = "http://localhost:5000/")

# import hex points
hex_cad <- file.path(wd, "data", "intermediate", "hex", "cad_hex_pts.csv") %>% 
  read.csv %>%
  dplyr::select(hex = GEOID, lon = X, lat = Y)

hex_opps <- file.path(wd, "data", "intermediate", "hex", "hex_opps.Rds") %>% 
  readRDS()

project_lotteries <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>%
  readRDS()

urban_points <- file.path(wd, "data", "intermediate", "hex", "hex_points.csv") %>% 
  read.csv %>%
  dplyr::select(hex = GEOID, lon = X, lat = Y) %>%
  filter(hex %in% hex_cad$hex | hex %in% hex_opps$ID | hex %in% project_lotteries$hex)

center <- file.path(wd, "data", "intermediate", "hex", "center.csv") %>% 
  read.csv %>%
  dplyr::select(hex = GEOID, lon = X, lat = Y)

points <- rbind(urban_points, center)

for (i in 1:nrow(points)) {
  origins <- points[i,]
  dests <- points[i:nrow(points),]

  result <- osrmTable(loc = NULL, src = origins, dst = dests, exclude = NULL,
                      gepaf = FALSE, measure = "distance")

  new_row <- result$distance %>%
    as_tibble(rownames = "o") %>%
    gather(key = "d", value = "dist", -o)

  if (i == 1) {
    dist_half <- new_row
  }

  dist_half <- dist_half %>%
    rbind(new_row)

  # safety dump
  if (i %% 300 == 0) {
    write.csv(dist_half, file = file.path(wd, "data", "intermediate", "osrm", "dist_half.csv"))
  }
}

# reflect matrix
dist_flip <- dist_half %>%
  dplyr::select(o = d, d = o, dist)
dist_all <- dist_half %>%
  rbind(dist_flip)

write.csv(dist_all, file = file.path(wd, "data", "intermediate", "osrm", "dist_all.csv"))

dist_center <- dist_all %>%
  filter(d == 0) %>%
  dplyr::select(hex = o, dist)

write.csv(dist_center, file = file.path(wd, "data", "intermediate", "osrm", "dist_center.csv"))


