library(tidyverse)
library(lubridate)
library(kableExtra)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
table_path <- file.path(wd, "data", "output", "summary_stats")

panel <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS()

# location attributes by project
panel_proj_loc <- panel %>% 
  filter(month == ymd("2017-12-01"),
         beneficiary == 1) %>% 
  dplyr::select(cod_lv, 
                "Job Accessibility (% of jobs) in 2017" = acc_act, "Proximity to Downtown (km)" = dist_center_act,
                "Moving Distance (km)" = displacement_act, 
                "Social Dev. Index" = ids_act, "Murder Rate (per 100k)" = murder_rate_act) %>% 
  group_by(cod_lv) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup() %>%
  mutate_at(.vars = c(2:6), ~signif(., 3))

table_path <- file.path(wd, "data", "output", "summary_stats")

project_lotteries <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>%
  readRDS()

projects_loc <- file.path(wd, "data", "intermediate", "projects.Rds") %>%
  readRDS() %>% 
  inner_join(., project_lotteries, by = "cod_lv") %>%
  dplyr::select(cod_lv, Name = nome, Units = units.x) %>% 
  unique() %>% 
  inner_join(panel_proj_loc, by = "cod_lv") %>% 
  dplyr::select(-cod_lv) %>% 
  arrange(Name)

setwd(table_path)
kable(projects_loc, "html", align = c("l", "r", "r", "r", "r", "r", "r")) %>% 
  kable_styling("striped", full_width = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  save_kable("projects_loc.png")

# mean project attributes distortion
projects <- file.path(wd, "data", "intermediate", "projects.Rds") %>%
  readRDS() %>% 
  dplyr::select(cod_lv, nome)

projects_loc <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>%
  readRDS() %>% 
  inner_join(., projects, by = "cod_lv") %>% 
  inner_join(hex_indicators, by = "hex") %>% 
  rename(Name = nome, Units = units) %>% 
  write_csv("/Users/jonathanleape/Dropbox (MIT)/PMCMV/Thesis/graphics/project_lotteries.csv")

# beneficiary attributes by project
panel_proj_ppl <- panel %>% 
  filter(ben == 1) %>%
  filter(month == ymd("2017-12-01")) %>%
  # mutate(pre_yr = year(dt_ent) - 1) %>%
  # filter(year(month) == pre_yr) %>%
  mutate(cor_parda = ifelse(cor_branca + cor_negra + cor_outras == 0, 1, 0)) %>% 
  dplyr::select(cod_lv, 
                # "From Favela" = favela, "Female" = feminino, 
                "Age" = idade, 
                # "Race: Black" = cor_negra, "Race: Brown" = cor_parda, "Race: White" = cor_branca, #"Race: Other" = cor_outras, 
                "Illiterate" = analfabeto, "Primary School" = fundamental_completo, 
                "Secondary School" = medio_completo, "Higher Education" = superior_completo) %>% 
  group_by(cod_lv) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup() %>%
  # mutate_at(.vars = c(2:11), ~signif(., 3))
  mutate_at(.vars = c(2:6), ~signif(., 3))

projects_ppl <- file.path(wd, "data", "intermediate", "projects.Rds") %>%
  readRDS() %>% 
  inner_join(., project_lotteries, by = "cod_lv") %>%
  dplyr::select(cod_lv, Name = nome) %>% 
  unique() %>% 
  inner_join(panel_proj_ppl, by = "cod_lv") %>% 
  dplyr::select(-cod_lv) %>% 
  arrange(Name)

setwd(table_path)
kable(projects_ppl, "html", align = c("l", "r", "r", "r", "r", "r")) %>% #, "r", "r", "r", "r", "r")) %>% 
  kable_styling("striped", full_width = TRUE) %>% 
  column_spec(1, bold = TRUE) %>% 
  save_kable("projects_ppl_dec17.png")

# lotteries
panel_lott <- panel %>% 
  distinct(cpfstd, edital, .keep_all = TRUE) %>% 
  filter(group != "loser") %>% 
  # mutate(group = str_to_title(group)) %>% 
  count(group, edital) %>% 
  pivot_wider(names_from = group, values_from = n) %>% 
  rename(Beneficiaries = beneficiary, Deniers = denier)

panel_lott_neighb <- panel %>% 
  distinct(edital, .keep_all = TRUE) %>% 
  dplyr::select(edital, "Social Dev. Index" = new_ids, 
                "Murder Rate (per 100k)" = new_murder_rate)

lotteries <- file.path(wd, "data", "intermediate", "lotteries.Rds") %>%
  readRDS() %>% 
  mutate(lottery_num = substr(edital, 7, 8),
         lottery_yr = substr(edital, 10, 13),
         Lottery = paste("#", lottery_num, "of", lottery_yr)) %>% 
  arrange(lottery_yr, lottery_num) %>% 
  dplyr::select(edital, Lottery, "Lottery Date" = dt_sor, "Move-in Date" = dt_ent,
                "Dist. to Center\n(km)" = avg_dist_center, "Cumul. Opps\n(% of jobs in 2017)" = avg_acc2017) %>% 
  inner_join(panel_lott_neighb, by = "edital") %>% 
  inner_join(panel_lott, by = "edital") %>%
  dplyr::select(-edital) %>%
  mutate_at(.vars = c(4:9), ~signif(., 3)) %>% 
  mutate(Beneficiaries = replace_na(Beneficiaries, 0))

setwd(table_path)
kable(lotteries, "html", align = c("l", "c", "c", "c", "r", "r", "r", "r", "r")) %>% 
  kable_styling("striped", full_width = FALSE) %>% 
  column_spec(1, bold = TRUE, width = "10em") %>% 
  column_spec(c(4:5), width = "10em") %>% 
  save_kable("lotteries.png")
