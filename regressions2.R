library(stringi) # substring replacement
library(broom) # tidy summary
library(dplyr)
library(lubridate)
library(kableExtra)
options(scipen=999)

# Sys.sleep(7200)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
reg_path <- file.path(wd, "data", "output", "regressions", "17_yr_factor")

source(file.path(wd, "pmcmv", "reg_funs.R"))

num_yrs <- 4
panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS()%>% 
  arrange(desc(month)) %>% 
  distinct(cpfstd, edital, months_since_avail, .keep_all = TRUE) %>%
  filter(months_since_avail %% 12 == 0) %>%
  mutate(yrs_since_avail = months_since_avail / 12,
         yrs_since_move = months_since_move / 12,
         yrs_since_can_move = months_since_can_move / 12) %>%
  filter(yrs_since_avail <= num_yrs)

# endogenous
en_vars <- c("factor(yrs_since_move)")

# instrumental
in_vars <- c("factor(yrs_since_can_move)")

# exogenous
ex_dem <- c("favela", 
            "old_dist_center",
            "feminino", "idade", "idade2", 
            "cor_negra", "cor_branca", "cor_outras", 
            "old_analfabeto", "old_fundamental_completo", 
            "old_medio_completo", "old_superior_completo")

ex_dum <- c("factor(month)", "factor(edital)")

ex_vars1 <- c(ex_dem, ex_dum)

# models
m_tag <- "11t"

# summary variables
t <- c("factor(yrs_since_can_move)1",
       "factor(yrs_since_can_move)2",
       "factor(yrs_since_can_move)3",
       "factor(yrs_since_can_move)4",
       "factor(yrs_since_can_move)5",
       "factor(yrs_since_move)1",
       "factor(yrs_since_move)2",
       "factor(yrs_since_move)3",
       "factor(yrs_since_move)4",
       "factor(yrs_since_move)5",
       "months_since_can_move", 
       "months_since_can_move_sq", 
       "months_since_move", 
       "months_since_move_sq", 
       "dist_center", "murder_rate", "displacement",
       "old_r_inc", "old_emp", "old_hrs_wk",
       ex_dem)

pl <- c(`factor(yrs_since_move)1` = "Year 1",
        `factor(yrs_since_move)2` = "Year 2",
        `factor(yrs_since_move)3` = "Year 3",
        `factor(yrs_since_move)4` = "Year 4",
        `factor(yrs_since_move)5` = "Year 5",
        months_since_move = "Months Since Move",
        months_since_move_sq = "Sq. of Months",
        dist_center = "Proximity to Downtown (km)",
        acc = "Job Accessibility (% of jobs)",
        murder_rate = "Murder Rate (per 100k)",
        displacement = "Moving Distance (km)",
        old_dist_center = "Orig. Proximity to Downtown (km)",
        old_acc = "Orig. Job Accessibility (% of jobs)",
        old_murder_rate = "Orig. Murder Rate (per 100k)",
        favela = "Orig. Favela",
        old_emp = "Orig. Employment Status",
        old_r_inc = "Orig. Income (MW)",
        old_hrs_wk = "Orig. Weekly Hours",
        feminino = "Female",
        idade = "Age",
        idade2 = "Sq. of Age",
        cor_negra = "Race: Black",
        cor_branca = "Race: White",
        cor_outras = "Race: Other",
        old_analfabeto = "Orig. Illiterate",
        old_fundamental_completo = "Orig. Primary School",
        old_medio_completo = "Orig. Secondary School",
        old_superior_completo = "Orig. Higher Education")

dum_plot <- function(y, pretty_y, results) {
  
  bin_plot <- ggplot(results) +
    geom_point(aes(x = year, group = model, color = model, y = estimate), 
               size = 3,
               position=position_dodge(0.4)) +
    geom_errorbar(aes(x = year, group = model, color = model, ymin=conf.low, ymax=conf.high), 
                  size = 1,
                  width = .2,
                  position=position_dodge(0.4)) +
    scale_color_brewer(name = "Model",
                       palette="Dark2") +
    scale_x_continuous(breaks=c(1, 2, 3, 4)) +
    labs(x = "Years Since Move-In Date",
         y = paste("Change in", pretty_y)) + 
    theme_minimal() + 
    theme(text = element_text(size=10),
          axis.text.x = element_text(hjust=1),
          legend.position = "bottom")
  
  png_name <- paste0("ATE_dum.png")
  ggsave(filename = png_name, 
         plot = bin_plot,
         device = "png",
         path = file.path(reg_path, "plot", y),
         width = 15, #default 15
         height = 10, #default 10.74
         units = "cm")
}

### regressions ####
run_models <- function(panel, y, pretty_y) {
  
  ex_vars <- c(paste0("old_", y), ex_vars1)
  
  print(ex_vars)
  
  num_part <- panel %>% 
    distinct(cpfstd) %>%
    nrow()
  
  res_digits <- ifelse(y == "emp", 4, 2)
  
  fit_1s <- ols1s(panel, y, in_vars, ex_vars, robust = FALSE)
  tidy_t_1s <- tidy(fit_1s)
  tidy_ci_1s <- confint_tidy(fit_1s, conf.level = 0.90)
  tidy_1s <- cbind(tidy_t_1s, tidy_ci_1s) %>% 
    mutate(model = "ITT") %>%
    dplyr::select(model, term, estimate, conf.low, conf.high, p.value) %>%
    filter(term %in% t)
  
  fit_2s <- ols2s(panel, y, en_vars, in_vars, ex_vars, robust = FALSE)
  tidy_t_2s <- tidy(fit_2s)
  tidy_ci_2s <- confint_tidy(fit_2s, conf.level = 0.90)
  tidy_2s <- cbind(tidy_t_2s, tidy_ci_2s) %>% 
    mutate(model = "LATE") %>%
    dplyr::select(model, term, estimate, conf.low, conf.high, p.value) %>%
    filter(term %in% t)
  
  # combine tibbles
  results <- rbind(tidy_1s, tidy_2s) %>% 
    filter(grepl("factor", term)) %>% 
    separate(term, into = c("left", "right"), sep = "_move\\)", remove = TRUE) %>% 
    mutate(year = as.integer(right))
  
  # save results
  results %>%
    saveRDS(file.path(reg_path, "tidy", y, "ATE_dum.Rds"))
  
  # read results
  results <- file.path(reg_path, "tidy", y, "ATE_dum.Rds") %>%
    readRDS()
  
  results_wide <- tidy_1s %>%
    cbind(dplyr::select(tidy_2s,
                        term_2s = term,
                        estimate_2s = estimate,
                        conf.low_2s = conf.low, conf.high_2s = conf.high,
                        p.value_2s = p.value))
  
  # save results
  results_wide %>%
    saveRDS(file.path(reg_path, "tidy", y, "ATE_dum_wide.Rds"))
  
  # read results
  # results_wide <- file.path(reg_path, "tidy", y, "ATE_dum_wide.Rds") %>%
  #   readRDS()
  
  # style kable
  res_kable <- results_wide %>% 
    dplyr::select(term_2s, estimate, conf.low, conf.high, p.value, 
                  estimate_2s, conf.low_2s, conf.high_2s, p.value_2s) %>% 
    mutate(term_2s = pl[term_2s]) %>%
    mutate_at(c(2:4, 6:8), ~as.character(signif(., 3))) %>%
    kable("html", 
          align = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
          col.names = c("Term", "Est.", "C.I. low", "C.I. high", "p-value", "Est.", "C.I. low", "C.I. high", "p-value"),
          digits = 3) %>% 
    kable_styling("striped", "condensed", full_width = FALSE) %>% 
    add_header_above(c(" " = 1, "ITT" = 4, "LATE" = 4)) %>% 
    add_footnote(label = paste("Sample:", num_part, "participants, 2011 - 2017"))
  
  # export png
  setwd(file.path(reg_path, "png", y))
  save_kable(res_kable, "ATE_dum_wide.png")
  
  # export html
  setwd(file.path(reg_path, "html", y))
  save_kable(res_kable, "ATE_dum_wide.html")
  
  # export plot
  dum_plot(y, pretty_y, results)
}

run_models(panel2, "r_inc", "Monthy Income (MW)")
run_models(panel2, "emp", "Employment Status")
run_models(panel2, "hrs_wk", "Weekly Hours")
