library(tidyverse)
library(lubridate)
library(broom)
library(zoo)

options(scipen=999) #no scientific notation
options(digits = 3)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

general <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>% 
  filter(territorialidade == 0,
         year(dt_ent) < 2018,
         year(dt_ent) > 2011)

cep_hex <- file.path(wd, "data", "intermediate", "hex", "cep_hex.Rds") %>%
  readRDS() %>%
  rename(hex = ID)

# origin neighborhood indicators
hex_indicators <- file.path(wd, "data", "intermediate", "hex", "hex_indicators.Rds") %>% 
  readRDS()

# project info
project_lotteries <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>% 
  readRDS()

# RAIS
empinc_yr <- file.path(wd, "data", "intermediate", "rais",
                         "participantes", "empinc.Rds") %>%
  readRDS() %>%
  filter(year(month) >= 2011, 
         year(month) <= 2017) %>% 
  mutate(yr = year(month)) %>% 
  dplyr::select(-month) %>%
  group_by(cpfstd, yr) %>% 
  summarise_all(list(mean))

# CADUNICO
# cadunico <- file.path(wd, "data", "intermediate", "cadunico_std.Rds") %>%
#   readRDS() %>%
#   ungroup() %>%
#   distinct(cpfstd, cadunico_ano, .keep_all = T) %>%
#   dplyr::select(cpfstd, cadunico_ano, idade, 
#                 cep, feminino,
#                 cor_negra, cor_branca, cor_outras,
#                 analfabeto, fundamental_completo, 
#                 medio_completo, superior_completo)

cadunico <- file.path(wd, "data", "intermediate", "cadunico_std.Rds") %>% 
  readRDS()

# fill missing data
# cadunico_2011 <- cadunico %>% 
#   filter(cadunico_ano == 2012) %>%
#   mutate(cadunico_ano = 2011,
#          idade = idade - 1)

# cadunico_fill <- cadunico %>% 
#   rbind(cadunico_2011) %>%
#   complete(cpfstd, cadunico_ano) %>%
#   arrange(cpfstd, cadunico_ano) %>%
#   group_by(cpfstd) %>%
#   fill(cep:superior_completo, .direction = "down") %>%
#   fill(cep:superior_completo, .direction = "up") %>%
#   arrange(cpfstd, cadunico_ano) %>% 
#   mutate(idade = na.approx(idade, maxgap = Inf, rule = 2)) %>% 
#   ungroup()

# saveRDS(cadunico_fill, file.path(wd, "data", "intermediate", "cadunico_fill.Rds"))

cadunico_fill <- file.path(wd, "data", "intermediate", "cadunico_fill.Rds") %>% 
  readRDS()

# cadunico_hex <- cadunico %>% 
#   rbind(cadunico_2011) %>%
#   left_join(., cep_hex, by = "cep") %>%
#   group_by(cpfstd) %>%
#   filter(!any(is.na(hex)) > 0) %>% 
#   ungroup() %>%
#   distinct(cpfstd, cadunico_ano, .keep_all = T)
# 
# saveRDS(cadunico_hex, file.path(wd, "data", "intermediate", "cadunico_hex.Rds"))

cadunico_hex <- file.path(wd, "data", "intermediate", "cadunico_hex.Rds") %>% 
  readRDS()

# join functions
add_cadunico_vars <- function(sample, cad_version) {
  sample %>% 
    mutate(pre_yr = year(dt_ent) - 1) %>%
    inner_join(., cad_version, by = c("cpfstd", "pre_yr" = "cadunico_ano"))
}

add_hex_indicators <- function(sample) {
  sample %>% 
    inner_join(hex_indicators, by = c("hex")) %>%
    rename(old_ids = ids,
           old_hex = hex)
}

add_rais_vars <- function(sample) {
  sample %>% 
    mutate(pre_yr = year(dt_ent) - 1) %>%
    left_join(empinc_yr, by = c("cpfstd", "pre_yr" = "yr")) %>% 
    mutate(emp = replace_na(emp, 0),
           r_inc = replace_na(r_inc, 0),
           hrs_wk = replace_na(hrs_wk, 0)) %>% 
    rename(old_emp = emp, old_r_inc = r_inc, old_hrs_wk = hrs_wk)
}

# Variables
rais_vars <- c("old_emp", "old_r_inc", "old_hrs_wk")
rais_names <- c("Employed", "Income (MW)", "Weekly Work Hours")

hex_vars <- c("favela", "old_ids")
hex_names <- c("Favela Resident", "Social Dev. Index")

cad_vars <- c("idade", "feminino", 
              "cor_negra", "cor_branca", "cor_outras", 
              "analfabeto", "fundamental_completo",
              "medio_completo", "superior_completo")
cad_names <- c("Age", "Female", 
               "Black", "White", "Other race", 
               "Illiterate", "Primary School", 
               "Secondary School", "Higher Education")

balance_test <- function(data_pool, pool_name) {
  
  results <- data_pool %>%
    group_by(edital, dt_sor) %>% 
    count(sorteado) %>% 
    ungroup() %>%
    spread(key = sorteado, value = n) %>% 
    mutate(pool = pool_name,
           F_stat = NA,
           p_val = NA,
           nd_emp = NA,
           nd_r_inc = NA,
           nd_hrs_wk = NA) %>% 
    arrange(dt_sor) %>% 
    dplyr::select(pool, edital, winners = "1", losers = "0", 
                  F_stat, p_val, nd_emp, nd_r_inc, nd_hrs_wk)
  
  for (i in seq(results$edital)) {

    pool_i <- data_pool %>% 
      filter(edital == results$edital[i])
    
    # F-test
    x <- lm(sorteado ~ old_emp + old_r_inc + old_hrs_wk, data = pool_i) %>% 
      summary()
    
    results$F_stat[i] <- x$fstatistic[1]
    results$p_val[i] <- pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3],lower.tail=FALSE)
    
    # Normalized Differences
    nd <- pool_i %>% 
      group_by(sorteado) %>% 
      dplyr::select(starts_with("old")) %>%
      summarise_all(list(~mean(.), ~var(.))) %>% 
      ungroup()
    
    results$nd_emp[i] <- (nd$old_emp_mean[2] - nd$old_emp_mean[1]) / sqrt((nd$old_emp_var[1] + nd$old_emp_var[2]) / 2)
    results$nd_r_inc[i] <- (nd$old_r_inc_mean[2] - nd$old_r_inc_mean[1]) / sqrt((nd$old_r_inc_var[1] + nd$old_r_inc_var[2]) / 2)
    results$nd_hrs_wk[i] <- (nd$old_hrs_wk_mean[2] - nd$old_hrs_wk_mean[1]) / sqrt((nd$old_hrs_wk_var[1] + nd$old_hrs_wk_var[2]) / 2)
  }
  
  results
}

# Pool tests
# just rais
t1_rais <- general %>%
  add_rais_vars()

balance <- balance_test(t1_rais, "1. All Participants")
remove(t1_rais)

# cadunico
t2_cad <-  general %>% 
  add_cadunico_vars(cadunico) %>% 
  add_rais_vars()

balance <- balance %>% 
  rbind(balance_test(t2_cad, "2. In CADUNICO"))
remove(t2_cad)

# cadunico_fill
t3_cad_fill <-  general %>% 
  add_cadunico_vars(cadunico_fill) %>% 
  add_rais_vars()

balance <- balance %>% 
  rbind(balance_test(t3_cad_fill, "3. In Imputed CADUNICO"))
remove(t3_cad_fill)

# cadunico_hex
t4_cad_hex <-  general %>% 
  add_cadunico_vars(cadunico_hex) %>% 
  add_hex_indicators() %>%
  add_rais_vars()

balance <- balance %>% 
  rbind(balance_test(t4_cad_hex, "4. With CEP in CADUNICO"))
remove(t4_cad_hex)

# future lottery winners removed
winner_pool <- general %>%
  filter(ben_futuro == 0, 
         sorteado == 1) %>%
  arrange(desc(sorteado), dt_sor) %>%
  distinct(cpfstd, .keep_all = T)

loser_pool <- general %>%
  filter(ben_futuro == 0, 
         sorteado == 0, 
         !cpfstd %in% winner_pool$cpfstd) %>%
  distinct(cpfstd, edital, .keep_all = T)

no_future_pool <- winner_pool %>% 
  rbind(loser_pool)

t5_no_future <-  no_future_pool %>% 
  add_cadunico_vars(cadunico_hex) %>% 
  add_hex_indicators() %>%
  add_rais_vars()

balance <- balance %>% 
  rbind(balance_test(t5_no_future, "5. No Lottery Contamination"))
remove(t5_no_future)

# participants with origin in cadunico
t6_w_origin <- general %>% 
  add_cadunico_vars(cadunico_hex) %>% 
  add_rais_vars() %>% 
  inner_join(., dplyr::select(project_lotteries,
                              edital, hex),
             by = "edital") %>%
  group_by(cpfstd, edital) %>%
  filter(!any(hex.x == hex.y)) %>%
  ungroup() %>%
  distinct(cpfstd, edital, .keep_all = TRUE)

balance <- balance %>% 
  rbind(balance_test(t6_w_origin, "6. With Origin CEP"))
remove(t6_w_origin)

# match pool
t7_match_pool <- file.path(wd, "data", "intermediate", "match_pool.Rds") %>% 
  readRDS()

balance <- balance %>% 
  rbind(balance_test(t7_match_pool, "7. Matching Pool"))
remove(t7_match_pool)

# matches in panel
t8_near_matches <- file.path(wd, "data", "intermediate", "matches.Rds") %>% 
  readRDS()

balance <- balance %>% 
  rbind(balance_test(t8_near_matches, "8. Nearest Matching"))
remove(t8_near_matches)

balance %>% 
  saveRDS(file = file.path(wd, "data", "output", "balance", "balance.Rds"))

balance <- file.path(wd, "data", "output", "balance", "balance.Rds") %>% 
  readRDS()

library(stringr)
balance <- file.path(wd, "data", "output", "balance", "balance.Rds") %>%
  readRDS() %>% 
  filter(pool %in% c("1. All Participants", "2. In CADUNICO", "6. With Origin CEP", 
                     "8. Nearest Matching")) %>%
  mutate(pool = str_replace(pool, "6.", "3."),
         pool = str_replace(pool, "8.", "4."))

# plot p-val
png(file.path(wd, "data", "output", "balance", "p_val.png"),
    width = 1200, height = 800)

ggplot(balance) +
  aes(x = pool, y = p_val) +
  geom_boxplot(fill = "#2171b5") +
  labs(x = "\nStage of Data Processing", 
       y = "P-value\n", 
       title = "Balance Test",
       subtitle = "All RAIS Variables") + 
  theme_minimal() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()

# plot F-stat
png(file.path(wd, "data", "output", "balance", "F_stat.png"),
    width = 1200, height = 800)

ggplot(balance) +
  aes(x = pool, y = F_stat) +
  geom_boxplot(fill = "#2171b5") +
  labs(x = "\nStage of Data Processing", 
       y = "F-statistic\n", 
       title = "Balance Test",
       subtitle = "All RAIS Variables") + 
  theme_minimal() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()

# plot normalized differences
# emp
png(file.path(wd, "data", "output", "balance", "nd_emp.png"),
    width = 1200, height = 800)

ggplot(balance) +
  aes(x = pool, y = nd_emp) +
  geom_boxplot(fill = "#2171b5") +
  labs(x = "\nStage of Data Processing", 
       y = "Normalized Differences\n", 
       title = "Balance Test",
       subtitle = "Employment") + 
  theme_minimal() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()

# r_inc
png(file.path(wd, "data", "output", "balance", "nd_r_inc.png"),
    width = 1200, height = 800)

ggplot(balance) +
  aes(x = pool, y = nd_r_inc) +
  geom_boxplot(fill = "#2171b5") +
  labs(x = "\nStage of Data Processing", 
       y = "Normalized Differences\n", 
       title = "Balance Test",
       subtitle = "Income (MW)") + 
  theme_minimal() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()

# hrs_wk
png(file.path(wd, "data", "output", "balance", "nd_hrs_wk.png"),
    width = 1200, height = 800)

ggplot(balance) +
  aes(x = pool, y = nd_hrs_wk) +
  geom_boxplot(fill = "#2171b5") +
  labs(x = "\nStage of Data Processing", 
       y = "Normalized Differences\n", 
       title = "Balance Test",
       subtitle = "Hours per Week") + 
  theme_minimal() + 
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()
