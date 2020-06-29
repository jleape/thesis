library(dplyr)
# library(reticulate)
# use_virtualenv("myenv")

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
otp_path <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV/otp-travel-time-matrix" 
jython_path <- "/Users/jonathanleape/jython2.7.0/bin/jython"

# graph
build_graph <- function(cache_path, base_path, build_path, memory){
  
  memory <- ifelse(missing(memory), 4, memory)
  
  jar_path <- file.path(base_path, "otp-1.4.0-shaded.jar") %>%
    paste0("'", ., "'")
  base_path <- base_path%>%
    paste0("'", ., "'")
  
  system(paste(
    paste0("java -Xmx", memory, "G -jar"),
    jar_path,
    "--cache", cache_path, 
    "--basePath", base_path,
    "--build", build_path))
}

years <- c(2014, 2015, 2016, 2017, 2018, "auto")
base_path <- otp_path
cache_path <- otp_path %>%
  paste0("'", ., "'")

for (yr in years){
  build_path <- file.path(wd, "data", "input", "gtfs", yr)
  build_graph(cache_path, base_path, build_path)
}

# tt matrix
query_ttm <- function(otp_path, ttm_args, memory, otp_version){
  
  memory <- ifelse(missing(memory), 10, memory)
  otp_version <- ifelse(missing(otp_version), "1.4.0", otp_version)
  otp_path_quote <- otp_path %>%
    paste0("'", ., "'")
  
  wd <- getwd()
  setwd(otp_path)
  
  system(paste(
    jython_path,
    paste0("-J-XX:-UseGCOverheadLimit -J-Xmx", memory, "G"), # set memory limit
    paste0("-Dpython.path=otp-", otp_version, "-shaded.jar"), #file.path(otp_path, "otp-1.4.0-shaded.jar"), # OTP version
    file.path(otp_path_quote, "python_script_args.py"),
    ttm_args))
  
  setwd(wd)
}

points <- file.path(wd, "data", "intermediate", "hex", "hex_points.csv") %>%
  paste0("'", ., "'")
time <- "07 30 00"
mode <- "WALK,BUS,RAIL"
maxTimeSec <- 7200
output_vars <- "walk_distance travel_time boardings"

ttm_args <- paste(
  # "--graph", graph,
  # "--router", router,
  "--points", points,
  # "--dests", dests,
  # "--origins", origins,
  # "--date", date,
  "--time", time,
  "--mode", mode,
  "--maxTimeSec", maxTimeSec,
  # "--output_file", output_file,
  "--output_vars", output_vars
)

years <- c("2014", "2015", "2016", "2017")
dates <- list("2014" = "2014 10 14",
              "2015" = "2015 10 15",
              "2016" = "2016 10 13",
              "2017" = "2017 10 19")

# for (yr in years){
#   output_path <- paste0("ttm_", yr, ".csv") %>%
#     file.path(wd, "data", "intermediate", "otp", .) %>%
#     paste0("'", ., "'")
#   ttm_args_2 <- paste(ttm_args, 
#                       "--graph", file.path(wd, "data", "input", "gtfs"),
#                       "--router", yr,
#                       "--date", dates[yr],
#                       "--output_file", output_path)
#   print(ttm_args)
#   query_ttm(otp_path, ttm_args_2)
# }

### COMBINAR TTMS
import_ttm <- function(yr, filepath) {
  filepath %>%
    read.csv() %>%
    mutate(ano = yr)
}

ttm_yrs <- 2014:2017
ttm_paths <- lapply(ttm_yrs, 
                    function(yr) file.path(wd, "data", "intermediate", "otp", paste0("ttm_", yr, ".csv")))

# ttm <- do.call(rbind, pmap(list(ttm_yrs, ttm_paths), import_ttm)) %>%
#   mutate(tt_bin = case_when(travel_time <= 1800 ~ "30",
#                             travel_time <= 3600 ~ "60",
#                             travel_time <= 5400 ~ "90",
#                             travel_time <= 7200 ~ "120",
#                             travel_time <= 9000 ~ "150",
#                             travel_time <= 10800 ~ "180",
#                             travel_time > 10800 ~ ">180")) %>%
#   saveRDS(file = file.path(wd, "data", "intermediate","otp", "ttm.Rds"))
# 
# write.csv(ttm, file = file.path(wd, "data", "intermediate","otp", "ttm.csv"))
  
### auto ttm ####
output_path <- file.path(wd, "data", "intermediate", "otp", "ttm_auto2.csv") %>%
  paste0("'", ., "'")
origins <- file.path(wd, "data", "intermediate", "hex", "cad_hex_pts.csv") %>%
  paste0("'", ., "'")
dests <- file.path(wd, "data", "intermediate", "hex", "cad_hex_pts.csv") %>%
  paste0("'", ., "'")
graph <- file.path(wd, "data", "input", "gtfs") %>%
  paste0("'", ., "'")

ttm_args <- paste(
  "--graph", graph,
  "--router", "auto",
  # "--points", points,
  "--dests", origins,
  "--origins", dests,
  "--date", "2017 10 19",
  "--time", "07 30 00",
  "--mode", "CAR",
  "--maxTimeSec", 7200,
  "--output_file", output_path,
  "--output_vars", "distance travel_time"
)

query_ttm(otp_path, ttm_args)

ttm <- file.path(wd, "data", "intermediate", "otp", "ttm_auto2.csv") %>% 
  read.csv(stringsAsFactors = F)


