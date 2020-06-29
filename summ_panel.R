library(tidyverse)
library(lubridate)
library(kableExtra)
# library(formattable)
# library(htmltools)
# library(webshot)
# options(pillar.sigfig=3) # tibble print

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
table_path <- file.path(wd, "data", "output", "summary_stats")

panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>%
  mutate(pre_yr = year(dt_ent) - 1,
         cor_parda = ifelse(cor_branca + cor_negra + cor_outras == 0, 1, 0))

# summary table function
summ_table <- function(panel, tag) {
  
  summ_mean <- panel %>%
    summarise_all(list(mean), na.rm = TRUE) %>% 
    ungroup() 
  
  summ_sd <- panel %>%
    summarise_all(list(sd), na.rm = TRUE) %>% 
    ungroup() 
  
  summ_min <- panel %>%
    summarise_all(list(min), na.rm = TRUE) %>% 
    ungroup()
  
  summ_max <- panel %>% 
    summarise_all(list(max), na.rm = TRUE) %>% 
    ungroup()
  
  summ <- rbind(summ_mean, summ_sd, summ_min, summ_max) %>%
    mutate_all(~signif(., 3))
  
  summ_t <- as_tibble(cbind(nms = names(summ), 
                            t(summ))) %>% 
    rename("Variable" = nms, "Mean" = V2, "Std. Deviation" = V3, 
           "Minimum" = V4, "Maximum" = V5)
  
  setwd(table_path)
  kable(summ_t, "html", align = c("l", "r", "r", "r", "r")) %>% 
    kable_styling("striped", full_width = FALSE) %>% 
    column_spec(1, bold = TRUE) %>% 
    save_kable(paste0("summ_", tag, ".png"))
}

# mean table function
mean_table <- function(panel, tag) {
  group_mean <- panel %>%
    group_by(`Treatment Group`) %>%
    summarise_all(list(mean), na.rm = TRUE) %>% 
    ungroup() %>% 
    dplyr::select(-`Treatment Group`)
  
  panel_mean <- panel %>%
    dplyr::select(-`Treatment Group`) %>% 
    summarise_all(list(mean), na.rm = TRUE) %>%
    ungroup()
  
  mean_tb <- rbind(panel_mean, group_mean) %>%
    mutate_all(~signif(., 3))
  
  mean_tb_t <- as_tibble(cbind(nms = names(mean_tb), 
                            t(mean_tb))) %>% 
    rename("Variable" = nms, "All Participants" = V2, 
           "Beneficiaries" = V3, "Non-compliers" = V4, "Losers" = V5)
  
  setwd(table_path)
  kable(mean_tb_t, "html", align = c("l", "r", "r", "r", "r")) %>% 
    kable_styling("striped", full_width = FALSE) %>% 
    column_spec(1, bold = TRUE) %>% 
    save_kable(paste0("means_", tag, ".png"))
}

# Means
# pre move-in
Pre_panel <- panel2 %>% 
  filter(year(month) == pre_yr,
         month(month) == 1) %>% 
  dplyr::select("Treatment Group" = group, 
                "Employed" = old_emp,
                "Weekly Work Hours" = old_hrs_wk, "Monthly Income (MW)" = old_r_inc,
                "Proximity to Downtown (km)" = old_dist_center,
                "Job Accessibility (% of jobs)" = old_acc,
                "Murder Rate (per 100k)" = old_murder_rate, 
                "From Favela" = favela, "Female" = feminino, "Age" = idade, 
                "Race: Black" = cor_negra, "Race: Brown" = cor_parda, 
                "Race: White" = cor_branca, "Race: Other" = cor_outras, 
                "Illiterate" = old_analfabeto, "Primary School" = old_fundamental_completo, 
                "Secondary School" = old_medio_completo, "Higher Education" = old_superior_completo)

mean_table(Pre_panel, "pre")

# Dec, 2017
Pos_panel <- panel2 %>% 
  filter(year(month) == 2017, 
         month(month) == 12) %>% 
  dplyr::select("Treatment Group" = group, 
                "Employed" = emp,
                "Weekly Work Hours" = hrs_wk, "Monthly Income (MW)" = r_inc,
                "Proximity to Downtown (km)" = dist_center,
                "Job Accessibility (% of jobs)" = acc,
                "Murder Rate (per 100k)" = murder_rate,
                "Moving Distance (km)" = displacement)

mean_table(Pos_panel, "post")

# Summaries
# pre move-in
Pre_panel <- panel2 %>% 
  filter(year(month) == pre_yr,
         month(month) == 1) %>% 
  dplyr::select("Weekly Work Hours" = old_hrs_wk, "Monthly Income (MW)" = old_r_inc,
                "Proximity to Downtown (km)" = old_dist_center,
                "Job Accessibility (% of jobs)" = old_acc,
                "Murder Rate (per 100k)" = old_murder_rate, 
                "Age" = idade)

summ_table(Pre_panel, "pre")

# Dec, 2017
Pos_panel <- panel2 %>% 
  filter(year(month) == 2017, 
         month(month) == 12) %>% 
  dplyr::select("Weekly Work Hours" = hrs_wk, "Monthly Income (MW)" = r_inc,
                "Proximity to Downtown (km)" = dist_center,
                "Job Accessibility (% of jobs)" = acc,
                "Murder Rate (per 100k)" = murder_rate,
                "Moving Distance (km)" = displacement)

summ_table(Pos_panel, "post")

empinc_part <- file.path(wd, "data", "intermediate", "rais", "participantes", "empinc.Rds") %>%
  readRDS()

empinc_yr <- empinc_part %>%
  mutate(yr = year(month)) %>%
  dplyr::select(-month) %>%
  group_by(cpfstd, yr) %>%
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup()

# # determine outlier filters
# loser_old_inc <- participantes %>% 
#   filter(sorteado == 0,
#          cod_lv %in% project_lotteries$cod_lv) %>% 
#   mutate(pre_yr = year(dt_ent) - 1) %>% 
#   distinct(cpfstd, pre_yr) %>% 
#   inner_join(., empinc_yr, by = c("cpfstd", "pre_yr" = "yr"))  %>% 
#   filter(hrs_wk < 100)
# 
# loser_max_r_inc <- max(loser_old_inc$r_inc)
# loser_max_hrs_wk <- max(loser_old_inc$hrs_wk)
# loser_perc_over2 <- (loser_old_inc %>% filter(r_inc > 2) %>% nrow()) / (loser_old_inc %>% nrow())
# 
# ben_old_inc <- participantes %>% 
#   filter(ben == 1,
#          cod_lv %in% project_lotteries$cod_lv) %>% 
#   mutate(pre_yr = year(dt_ent) - 1) %>% 
#   distinct(cpfstd, pre_yr) %>% 
#   inner_join(., empinc_yr, by = c("cpfstd", "pre_yr" = "yr"))  %>% 
#   filter(hrs_wk < 100)
# 
# ben_max_r_inc <- max(ben_old_inc$r_inc)
# ben_max_hrs_wk <- max(ben_old_inc$hrs_wk)
# ben_perc_over2 <- (ben_old_inc %>% filter(r_inc > 2) %>% nrow()) / (ben_old_inc %>% nrow())

# Formattable

# # https://www.littlemissdata.com/blog/prettytables
# summ17_pretty <- formattable(summ17_t, align = c("l", "r", "r", "r", "r", "r"))
# 
# export_formattable <- function(f, file, width = "100%", height = NULL, 
#                                background = "white", delay = 0.2) {
#   w <- as.htmlwidget(f, width = width, height = height)
#   path <- html_print(w, background = background, viewer = NULL)
#   url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
#   webshot(url,
#           file = file,
#           selector = ".formattable_widget",
#           delay = delay)
# }
# 
# fp <- file.path(wd, "data", "output", "summary_stats", "summ17_pretty.png")
# export_formattable(summ17_pretty, fp)

