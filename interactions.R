library(AER) # two-stage regression
library(estimatr) # two-stage regression with clustered standard errors
library(stringi) # substring replacement
library(broom) # tidy summary
library(dplyr)
library(readr) # csv export
library(lubridate)
library(kableExtra)
options(scipen=999)

# Sys.sleep(7200)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
reg_path <- file.path(wd, "data", "output", "regressions", "17_interaction")

source(file.path(wd, "pmcmv", "reg_funs.R"))

panel <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() 

# endogenous 
en_vars <- list(c("months_since_move", "months_since_move_sq"),
                c("beneficiary", "months_since_move", "months_since_move_sq", 
                  "dist_center", "murder_rate", "displacement",
                  "dist_center:beneficiary", "murder_rate:beneficiary", "displacement:beneficiary",
                  "favela:beneficiary", "old_dist_center:beneficiary", "old_murder_rate:beneficiary",
                  "feminino:beneficiary", "idade:beneficiary", "idade2:beneficiary", 
                  "cor_branca:beneficiary", "cor_negra:beneficiary", "cor_outras:beneficiary", 
                  "old_analfabeto:beneficiary", "old_fundamental_completo:beneficiary", 
                  "old_medio_completo:beneficiary", "old_superior_completo:beneficiary"),
                c("months_since_move", "months_since_move_sq", 
                  "dist_center", "murder_rate", "displacement",
                  "dist_center:months_since_move", "murder_rate:months_since_move", "displacement:months_since_move",
                  "favela:months_since_move", "old_dist_center:months_since_move", "old_murder_rate:months_since_move",
                  "feminino:months_since_move", "idade:months_since_move", "idade2:months_since_move", 
                  "cor_branca:months_since_move", "cor_negra:months_since_move", "cor_outras:months_since_move", 
                  "old_analfabeto:months_since_move", "old_fundamental_completo:months_since_move", 
                  "old_medio_completo:months_since_move", "old_superior_completo:months_since_move"))

# instrumental
in_vars <- list(c("months_since_can_move", "months_since_can_move_sq"),
                c("can_move", "months_since_can_move", "months_since_can_move_sq", 
                  "z_dist_center", "z_murder_rate", "z_displacement",
                  "z_dist_center:can_move", "z_murder_rate:can_move", "z_displacement:can_move",
                  "favela:can_move", "old_dist_center:can_move", "old_murder_rate:can_move",
                  "feminino:can_move", "idade:can_move", "idade2:can_move", 
                  "cor_branca:can_move", "cor_negra:can_move", "cor_outras:can_move", 
                  "old_analfabeto:can_move", "old_fundamental_completo:can_move", 
                  "old_medio_completo:can_move", "old_superior_completo:can_move"),
                c("months_since_can_move", "months_since_can_move_sq", 
                  "z_dist_center", "z_murder_rate", "z_displacement",
                  "z_dist_center:months_since_can_move", "z_murder_rate:months_since_can_move", "z_displacement:months_since_can_move",
                  "favela:months_since_can_move", "old_dist_center:months_since_can_move", "old_murder_rate:months_since_can_move",
                  "feminino:months_since_can_move", "idade:months_since_can_move", "idade2:months_since_can_move", 
                  "cor_branca:months_since_can_move", "cor_negra:months_since_can_move", "cor_outras:months_since_can_move", 
                  "old_analfabeto:months_since_can_move", "old_fundamental_completo:months_since_can_move", 
                  "old_medio_completo:months_since_can_move", "old_superior_completo:months_since_can_move"))

# exogenous
ex_dem <- c("favela", "old_dist_center", #"old_murder_rate",
            "feminino", "idade", "idade2", 
            "cor_negra", "cor_branca", "cor_outras", 
            "old_analfabeto", "old_fundamental_completo", 
            "old_medio_completo", "old_superior_completo")

ex_dum <- c("factor(month)", "factor(edital)")

ex_vars1 <- c(ex_dem, ex_dum)

# models
m_tags <- c("11t")

# summary variables
t_1s <- c(in_vars[[1]],
        ex_dem)

t_2s <- c(en_vars[[1]],
          ex_dem)

pl_2s <- c(beneficiary = "Beneficiary",
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
        old_superior_completo = "Orig. Higher Education",
        `beneficiary:dist_center` = "T * Proximity to Downtown (km)",
        `beneficiary:murder_rate` = "T * Murder Rate (per 100k)",
        `beneficiary:displacement` = "T * Moving Distance (km)",
        `beneficiary:old_dist_center` = "T * Orig. Proximity to Downtown (km)",
        `beneficiary:old_acc` = "T * Orig. Job Accessibility (% of jobs)",
        `beneficiary:displacement` = "T * Moving Distance (km)",
        `beneficiary:old_murder_rate` = "T * Orig. Murder Rate (per 100k)",
        `beneficiary:favela` = "T * Orig. Favela",
        `beneficiary:old_emp` = "T * Orig. Employment Status",
        `beneficiary:old_r_inc` = "T * Orig. Income (MW)",
        `beneficiary:old_hrs_wk` = "T * Orig. Weekly Hours",
        `beneficiary:feminino` = "T * Female",
        `beneficiary:idade` = "T * Age",
        `beneficiary:idade2` = "T * Sq. of Age",
        `beneficiary:cor_negra` = "T * Race: Black",
        `beneficiary:cor_branca` = "T * Race: White",
        `beneficiary:cor_outras` = "T * Race: Other",
        `beneficiary:old_analfabeto` = "T * Orig. Illiterate",
        `beneficiary:old_fundamental_completo` = "T * Orig. Primary School",
        `beneficiary:old_medio_completo` = "T * Orig. Secondary School",
        `beneficiary:old_superior_completo` = "T * Orig. Higher Education")
# 
# pl_1s <-  sapply(pl_2s, stri_replace_all_fixed, "months_since_move", "months_since_can_move", USE.NAMES = TRUE) %>%
#   sapply(., stri_replace_all_fixed, "z_dist_center", "dist_center", USE.NAMES = FALSE) %>%
#   sapply(., stri_replace_all_fixed, "z_murder_rate", "murder_rate", USE.NAMES = FALSE) %>%
#   sapply(., stri_replace_all_fixed, "z_displacement", "displacement", USE.NAMES = FALSE)

y <- "hrs_wk"
fit_type <- "ols"
i <- 1
### regressions ####
# run_models <- function(panel, y, fit_type) {
  
  ex_vars <- c(paste0("old_", y), ex_vars1)
  
  num_part <- panel %>% 
    distinct(cpfstd) %>%
    nrow()
  
  num_months <- panel %>% 
    distinct(month) %>%
    nrow()
  
  print(paste("Var:", y, "Fit:", fit_type))
  
  res_digits <- ifelse(y == "emp", 4, 2)
  
  for(i in seq_along(m_tags)){

    print(m_tags[i])

    if (fit_type == "ols") {
      
      fit_1s <- ols1s(panel, y, in_vars[[i]], ex_vars)
      tidy_t_1s <- tidy(fit_1s)
      tidy_ci_1s <- confint_tidy(fit_1s, conf.level = 0.90)
      tidy_1s <- cbind(tidy_t_1s, tidy_ci_1s)
      
      fit_2s <- ols2s(panel, y, en_vars[[i]], in_vars[[i]], ex_vars)
      tidy_t_2s <- tidy(fit_2s)
      tidy_ci_2s <- confint_tidy(fit_2s, conf.level = 0.90)
      tidy_2s <- cbind(tidy_t_2s, tidy_ci_2s)
    }
    
    if (fit_type == "log1s") {
      
      fit_1s <- log1s(panel, y, in_vars[[i]], ex_vars)
      tidy_t_1s <- tidy(fit_1s, exponentiate = TRUE)
      tidy_ci_1s <- confint_tidy(fit_1s, conf.level = 0.90)
      tidy_1s <- cbind(tidy_t_1s, tidy_ci_1s)
      
      fit_2s <- log2s(panel, y, en_vars[[i]], in_vars[[i]], ex_vars)
      tidy_t_2s <- tidy(fit_2s, exponentiate = TRUE)
      tidy_ci_2s <- confint_tidy(fit_2s, conf.level = 0.90)
      tidy_2s <- cbind(tidy_t_2s, tidy_ci_2s)
    }

    # combine tibbles
    results_1s <- tidy_1s %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      filter(!grepl("factor", term),
             !grepl("Intercep", term))

    write_csv(results_1s, file.path(reg_path, "csv", y, paste0(fit_type, "1s_", m_tags[i], ".csv")))
    
    results_2s <- tidy_2s %>% 
      dplyr::select(term_2s = term,
                    estimate_2s = estimate,
                    conf.low_2s = conf.low, conf.high_2s = conf.high,
                    p.value_2s = p.value) %>%
      filter(!grepl("factor", term_2s),
             !grepl("Intercep", term_2s))

    write_csv(results_2s, file.path(reg_path, "csv", y, paste0(fit_type, "2s_", m_tags[i], ".csv")))
    
    results <- cbind(results_1s, results_2s)

    # save results
    results %>%
      saveRDS(file.path(reg_path, "tidy", y, paste0(fit_type, "_", m_tags[i], ".Rds")))
    
    # read results
    # results <- file.path(reg_path, "tidy", y, paste0(fit_type, "_", m_tags[i], ".Rds")) %>% 
    #   readRDS()
    
    # style kable
    res_kable <- results %>% 
      dplyr::select(term_2s, estimate, conf.low, conf.high, p.value, 
                    estimate_2s, conf.low_2s, conf.high_2s, p.value_2s) %>% 
      mutate(term_2s = pl_2s[term_2s]) %>%
      mutate_at(c(2:4, 6:8), ~as.character(signif(., 3))) %>%
      kable("html", 
            align = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
            col.names = c("Term", "Est.", "C.I. low", "C.I. high", "p-value", "Est.", "C.I. low", "C.I. high", "p-value"),
            digits = 3) %>% 
      kable_styling("striped", "condensed", full_width = FALSE) %>% 
      add_header_above(c(" " = 1, "ITT" = 4, "LATE" = 4)) %>% 
      add_footnote(label = paste("Sample:", num_part, "participants over", 
                                 num_months, "months, starting in 2011"))
    
    # export png
    setwd(file.path(reg_path, "png", y))
    save_kable(res_kable, paste0(fit_type, "_", m_tags[i], ".png"))
    
    # export html
    setwd(file.path(reg_path, "html", y))
    save_kable(res_kable, paste0(fit_type, "_", m_tags[i], ".html"))
  }
# }

# run_models(panel2, "r_inc", "ols")
