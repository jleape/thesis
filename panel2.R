library(tidyverse)
library(lubridate)
library(stringi)
library(MatchIt)
library(zoo)
set.seed(123)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

months <- seq(ymd('2011-01-01'), ymd('2017-12-01'), by = '1 month') %>%
  tibble(month = .)

#### IMPORT TABLES ####
# add acc and dist
cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS() %>%
  rename(hex = ID)

acc <- file.path(wd, "data", "intermediate", "hex", "acc.Rds") %>%
  readRDS() %>%
  mutate(year = as.integer(ano)) %>%
  filter(tt == 90)

dist_center <- file.path(wd, "data", "intermediate", "osrm", "dist_center.csv") %>%
  read.csv(stringsAsFactors = F) %>%
  mutate(dist_center = dist / 1000) %>% 
  dplyr::select(hex, dist_center)

dist_all <- file.path(wd, "data", "intermediate", "osrm", "dist_all.csv") %>%
  read.csv(stringsAsFactors = F) %>% 
  mutate(dist = dist / 1000) %>% 
  dplyr::select(o, d, dist)

# # CADUNICO
# cadunico <- file.path(wd, "data", "intermediate", "cadunico_std.Rds") %>%
#   readRDS() %>%
#   ungroup() %>%
#   distinct(cpfstd, cadunico_ano, .keep_all = T) %>%
#   dplyr::select(cpfstd, cadunico_ano, idade, 
#                 cep, feminino,
#                 cor_negra, cor_branca, cor_outras,
#                 analfabeto, fundamental_completo, 
#                 medio_completo, superior_completo)
# 
# # fill missing data
# cadunico_2011 <- cadunico %>% 
#   filter(cadunico_ano == 2012) %>%
#   mutate(cadunico_ano = 2011,
#          idade = idade - 1)
# 
# cadunico_fill <- cadunico %>% 
#   rbind(cadunico_2011) %>%
#   complete(cpfstd, cadunico_ano) %>%
#   arrange(cpfstd, cadunico_ano) %>%
#   group_by(cpfstd) %>%
#   fill(cep:superior_completo, .direction = "down") %>%
#   fill(cep:superior_completo, .direction = "up") %>%
#   mutate(idade = na.approx(idade, maxgap = Inf, rule = 2)) %>% 
#   ungroup()
# 
# cadunico_hex <- cadunico_fill %>% 
#   left_join(., cep_hex, by = "cep") %>%
#   group_by(cpfstd) %>%
#   filter(!any(is.na(hex)) > 0) %>%
#   ungroup() %>%
#   drop_na() %>%
#   left_join(., dplyr::select(acc, origin, year, acc), by = c("hex" = "origin", "cadunico_ano" = "year")) %>%
#   left_join(., dist_center, by = "hex") %>%
#   mutate(acc = ifelse(cadunico_ano >= 2014, replace_na(acc, 0), acc),
#          dist_center = replace_na(dist_center, 80)) %>% 
#   distinct(cpfstd, cadunico_ano, .keep_all = T)
# 
# saveRDS(cadunico_hex, file.path(wd, "data", "intermediate", "cadunico_hex.Rds"))

cadunico_hex <- file.path(wd, "data", "intermediate", "cadunico_hex.Rds") %>% 
  readRDS()

# Empreendimentos and lotteries
project_lotteries <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>% 
  readRDS()
  
projects <- project_lotteries %>% 
  dplyr::select(cod_lv, units, hex, dist_center, acc2014, acc2015, acc2016, acc2017) %>% 
  distinct(cod_lv, .keep_all = TRUE)

lotteries <- file.path(wd, "data", "intermediate", "lotteries.Rds") %>% 
  readRDS()

#### Matching lottery winners with control group ####
participantes <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>%
  # edital01.2014 was invalidated and edital26.2015 appears to be non-random
  filter(!edital %in% c("edital01.2014", "edital26.2015"),
         # Remove 490 who beneficiaries by other means (other tier or non general lotteries)
         cod_lv %in% project_lotteries$cod_lv,
         territorialidade == 0,
         year(dt_ent) < 2018,
         year(dt_ent) > 2011)

# origin neighborhood indicators
hex_indicators <- file.path(wd, "data", "intermediate", "hex", "hex_indicators.Rds") %>% 
  readRDS()

# RAIS
empinc_part <- file.path(wd, "data", "intermediate", "rais", "participantes", "empinc.Rds") %>%
  readRDS()

empinc_yr <- empinc_part %>%
  mutate(yr = year(month)) %>%
  dplyr::select(-month) %>%
  group_by(cpfstd, yr) %>%
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup()

# determine outlier filters
# ben_old_inc <- participantes %>% 
#   filter(ben == 1,
#          cod_lv %in% project_lotteries$cod_lv) %>% 
#   mutate(pre_yr = year(dt_ent) - 1) %>% 
#   distinct(cpfstd, pre_yr) %>% 
#   inner_join(., empinc_yr, by = c("cpfstd", "pre_yr" = "yr"))  %>% 
#   filter(hrs_wk < 100)
# 
# ben_max_r_inc <- ceiling(max(ben_old_inc$r_inc))
# ben_max_hrs_wk <- ceiling(max(ben_old_inc$hrs_wk))

ben_max_r_inc <- 13
ben_max_hrs_wk <- 88

# # matching vars
winner_pool <- participantes %>%
  filter(sorteado == 1,
         ben_futuro == 0) %>%
  # select first lottery win
  arrange(desc(sorteado), dt_sor) %>%
  distinct(cpfstd, .keep_all = T)

loser_pool <- participantes %>%
  filter(sorteado == 0,
         ben_futuro == 0) %>%
         # !cpfstd %in% winner_pool$cpfstd) %>%
  distinct(cpfstd, edital, .keep_all = T)
# 
# match_pool <- winner_pool %>%
#   rbind(loser_pool) %>%
#   # get pre-lottery cadunico data
#   mutate(pre_yr = year(dt_ent) - 1) %>%
#   inner_join(., cadunico_hex, by = c("cpfstd", "pre_yr" = "cadunico_ano")) %>%
#   rename(old_hex = hex) %>%
#   # get pre-lottery hex indicators
#   inner_join(., dplyr::select(hex_indicators, hex, favela, murder_rate), by = c("old_hex" = "hex")) %>%
#   mutate(old_murder_rate = replace_na(murder_rate, 0)) %>%
#   # Note: the following code unbalances the treatment groups
#   # remove participants without hex of original address
#   inner_join(., dplyr::select(project_lotteries,
#                               edital, pot_hex = hex),
#              by = "edital") %>%
#   group_by(cpfstd, edital) %>%
#   filter(!any(old_hex == pot_hex)) %>%
#   ungroup() %>%
#   distinct(cpfstd, edital, .keep_all = TRUE) %>%
#   # add pre-lottery RAIS stats
#   # rais vars are volatile, especially income. better to average rais vars over year
#   # mutate(pre_month = ymd(paste0(year(dt_sor), "-", month(dt_sor), "-01")) %m-% months(1)) %>%
#   # left_join(empinc_yr, by = c("cpfstd", "pre_month" = "month")) %>%
#   left_join(empinc_yr, by = c("cpfstd", "pre_yr" = "yr")) %>%
#   mutate(emp = replace_na(emp, 0),
#          r_inc = replace_na(r_inc, 0),
#          inc = replace_na(inc, 0),
#          hrs_wk = replace_na(hrs_wk, 0)) %>%
#   dplyr::select(cpfstd, edital, inscrito, sorteado, ben, ben_futuro,
#            cod_lv, dt_ins, dt_sor, dt_ent,
#            old_emp = emp, old_r_inc = r_inc, old_hrs_wk = hrs_wk, old_inc = inc,
#            old_hex, favela, old_murder_rate,
#            old_dist_center = dist_center,
#            feminino, old_idade = idade,
#            cor_negra, cor_branca, cor_outras,
#            old_analfabeto = analfabeto, old_fundamental_completo = fundamental_completo,
#            old_medio_completo = medio_completo, old_superior_completo = superior_completo) %>%
#   filter(old_hrs_wk <= ben_max_hrs_wk,
#          old_r_inc <= ben_max_r_inc,
#          old_idade >= 17)
# 
# saveRDS(match_pool, file.path(wd, "data", "intermediate", "match_pool.Rds"))

# match_pool <- file.path(wd, "data", "intermediate", "match_pool.Rds") %>%
#   readRDS()

# # matchit
# edital_list <- match_pool %>%
#   distinct(edital) %>%
#   pull()
# 
# for (i in 1:length(edital_list)) {
# 
#   e <- edital_list[i]
#   print(e)
# 
#   match_pool_e <- match_pool %>%
#     filter(edital == e) %>%
#     data.frame()
# 
#   mod_match_e <- matchit(formula = sorteado ~ old_emp + old_r_inc + old_hrs_wk +
#                            feminino + old_idade +
#                            old_fundamental_completo +
#                            old_medio_completo + old_superior_completo,
#                        method = "nearest",
#                        # old_emp yearly average, so exact match not feasible.
#                        # exact = c("old_emp"),
#                        m.order="random",
#                        data = match_pool_e)
# 
#   matches_e <- match.data(mod_match_e) %>%
#     dplyr::select(cpfstd, edital, inscrito, sorteado, ben, ben_futuro,
#                   cod_lv, dt_ins, dt_sor, dt_ent,
#                   old_emp, old_r_inc, old_hrs_wk, old_inc,
#                   old_hex, favela, old_murder_rate, old_dist_center,
#                   feminino, old_idade, cor_negra, cor_branca, cor_outras,
#                   old_analfabeto, old_fundamental_completo,
#                   old_medio_completo, old_superior_completo) %>% 
    # mutate(old_education = factor((old_fundamental_completo + old_medio_completo + old_superior_completo),
    #                               labels = c("Primary", "Secondary", "Higher")
# 
#   if (i == 1) {
#     matches <- matches_e
#   } else {
#     matches <- matches %>%
#       rbind(matches_e)
#   }
# }
# 
# saveRDS(matches, file.path(wd, "data", "intermediate", "matches.Rds"))

matches <- file.path(wd, "data", "intermediate", "matches.Rds") %>%
  readRDS()

# potential location and neighborhood variables for all participants
pot_vars <- matches %>% 
  dplyr::select(cpfstd, edital, old_hex, old_dist_center, old_murder_rate) %>%
  inner_join(., dplyr::select(project_lotteries, 
                              edital, pot_hex = hex, pot_acc = acc2017), 
             by = "edital") %>% 
  inner_join(., dist_center, by = c("pot_hex" = "hex")) %>%
  rename(pot_dist_center = dist_center) %>%
  inner_join(., dist_all, by = c("old_hex" = "o", "pot_hex" = "d")) %>%
  left_join(., dplyr::select(acc[acc$year == 2017,], origin, old_acc = acc), by = c("old_hex" = "origin")) %>%
  mutate(old_acc = replace_na(old_acc, 0),
         pot_del_dist_center = pot_dist_center - old_dist_center,
         pot_del_acc = pot_acc - old_acc,
         pot_displacement = abs(dist)) %>%
  inner_join(dplyr::select(hex_indicators, hex, 
                           pot_murder_rate = murder_rate), 
             by = c("pot_hex" = "hex")) %>%
  mutate(pot_del_murder_rate = pot_murder_rate - old_murder_rate) %>%
  group_by(cpfstd, edital) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  ungroup() %>%
  dplyr::select(cpfstd, edital, 
                old_acc,
                pot_dist_center, pot_acc, 
                pot_murder_rate,  
                pot_del_dist_center, pot_del_acc, 
                pot_del_murder_rate, 
                pot_displacement)

# new location and neighborhood variables for beneficiaries
new_vars <- matches %>% 
  filter(ben == 1) %>%
  dplyr::select(cpfstd, edital, cod_lv, old_hex, old_dist_center, old_murder_rate) %>%
  inner_join(., projects, by = "cod_lv") %>% 
  rename(new_hex = hex, new_dist_center = dist_center, new_acc = acc2017) %>% 
  inner_join(., dist_all, by = c("old_hex" = "o", "new_hex" = "d")) %>%
  left_join(., dplyr::select(acc[acc$year == 2017,], origin, old_acc = acc), by = c("old_hex" = "origin")) %>%
  mutate(old_acc = replace_na(old_acc, 0)) %>%
  mutate(del_dist_center = new_dist_center - old_dist_center,
         del_acc = new_acc - old_acc,
         displacement = abs(dist)) %>%
  inner_join(dplyr::select(hex_indicators, hex, 
                           new_murder_rate = murder_rate), 
             by = c("new_hex" = "hex")) %>%
  mutate(del_murder_rate = new_murder_rate - old_murder_rate) %>%
  distinct(cpfstd, edital, .keep_all = TRUE) %>%
  dplyr::select(cpfstd, edital, 
                new_dist_center, new_acc, 
                new_murder_rate,  
                del_dist_center, del_acc, 
                del_murder_rate, 
                displacement)

# remove(cadunico, match_pool, loser_pool, mod_match_e,
#        participantes, dist_all)

# stack winners and losers
panel2 <- matches %>% 
  # expand panel by months
  merge(months, ., by = NULL) %>%
  # add treatment variables
  mutate(year = year(month),
         group = ifelse(ben == 1, "beneficiary", NA),
         group = ifelse(sorteado == 1 & ben == 0, "non-complier", group),
         group = ifelse(sorteado == 0, "loser", group),
         winner = ifelse(sorteado == 1 & month > dt_sor, 1, 0),
         can_move = ifelse(sorteado == 1 & month > dt_ent, 1, 0),
         beneficiary = ifelse(ben == 1 & month > dt_ent, 1, 0),
         months_since_lottery = interval(dt_sor, month) %/% months(1),
         months_since_avail = interval(dt_ent, month) %/% months(1),
         months_since_lottery = ifelse(months_since_lottery < 0, 0, months_since_lottery),
         months_since_avail = ifelse(months_since_avail < 0, 0, months_since_avail),
         # months_since_win = winner * months_since_lottery,
         # months_since_win_ben = ben * months_since_win, 
         months_since_can_move = can_move * months_since_avail,
         months_since_move = beneficiary * months_since_avail,
         # months_since_lottery_sq = months_since_lottery^2, 
         months_since_avail_sq = months_since_avail^2,
         # months_since_win_sq = months_since_win^2, 
         # months_since_win_cu = months_since_win^3, 
         # months_since_win_ben_sq = months_since_win_ben^2, 
         # months_since_win_ben_cu = months_since_win_ben^3, 
         months_since_can_move_sq = months_since_can_move^2,
         months_since_move_sq = months_since_move^2) %>% 
  # note that education vars are endogenous
  left_join(., dplyr::select(cadunico_hex, cpfstd, cadunico_ano, idade,
                              analfabeto, fundamental_completo, 
                             medio_completo, superior_completo),
             by = c("cpfstd", "year" = "cadunico_ano")) %>%
  mutate(idade2 = idade^2,
         education = ifelse(analfabeto == 1, 0, 1) + 
           fundamental_completo + medio_completo + superior_completo,
         race = ifelse(cor_branca == 1, "white", "brown"),
         race = ifelse(cor_negra == 1, "black", race),
         race = ifelse(cor_outras == 1, "other", race)) %>%
  # add PMCMV location vars
  left_join(., pot_vars, by = c("cpfstd", "edital")) %>%
  left_join(., new_vars, by = c("cpfstd", "edital")) %>%
  # actual vars
  mutate(dist_center = ifelse(beneficiary == 0, old_dist_center, new_dist_center), 
         acc = ifelse(beneficiary == 0, old_acc, new_acc), 
         murder_rate = ifelse(beneficiary == 0, old_murder_rate, new_murder_rate), 
         displacement = ifelse(beneficiary == 0, 0, displacement)) %>%
  # instrumental vars
  mutate(z_dist_center = ifelse(can_move == 0, old_dist_center, pot_dist_center), 
         z_acc = ifelse(can_move == 0, old_acc, pot_acc), 
         z_murder_rate = ifelse(can_move == 0, old_murder_rate, pot_murder_rate), 
         z_displacement = ifelse(can_move == 0, 0, pot_displacement)) %>%
  # join economic variables from RAIS
  left_join(., empinc_part, by = c("cpfstd", "month")) %>% 
  # fill with zeros for those not found in RAIS
  mutate(emp = replace_na(emp, 0),
         inc = replace_na(inc, 0),
         hrs_wk = replace_na(hrs_wk, 0),
         r_inc = replace_na(r_inc, 0)) %>% 
  filter(hrs_wk <= 100)

saveRDS(panel2, file.path(wd, "data", "output", "panels", "panel2.Rds"))

# normalize panel
norm_max <- c("old_murder_rate",
              "old_dist_center",
              "old_acc",
              "old_emp", 
              "old_inc", 
              "old_r_inc", 
              "old_hrs_wk",
              "old_idade",
              "old_education",
              "pot_murder_rate",
              "pot_dist_center", 
              "pot_acc", 
              "pot_del_murder_rate",
              "pot_del_dist_center", 
              "pot_del_acc",
              "pot_displacement", 
              "new_murder_rate",
              "new_dist_center", 
              "new_acc",
              "z_murder_rate",
              "z_dist_center", 
              "z_acc",
              "z_displacement",
              "murder_rate",
              "dist_center", 
              "acc",
              "displacement",
              "idade", 
              "idade2",
              "education")

norm_48 <- c("months_since_can_move", 
            "months_since_move")

norm_2304 <- c("months_since_can_move_sq", 
              "months_since_move_sq")

# min-max
range01 <- function(x){
  (x-min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

panel2s <-  panel2 %>%
  mutate_at(vars(one_of(norm_max)), range01) %>% 
  mutate_at(vars(one_of(norm_48)), function(x) { x / 48 }) %>% 
  mutate_at(vars(one_of(norm_2304)), function(x) { x / 2304 })
  # mutate_at(vars(one_of(norm_z)), scale)

saveRDS(panel2s, file.path(wd, "data", "output", "panels", "panel2s.Rds"))
