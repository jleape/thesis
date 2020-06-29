library(AER) # two-stage regression
library(stringi) # substring replacement
library(broom) # tidy summary
library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
options(scipen=999)

# Sys.sleep(7200)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
reg_path <- file.path(wd, "data", "output", "regressions", "17_yr_factor")

panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>%
  mutate(feminino = factor(feminino, labels = c("Male", "Female")),
         favela = factor(favela, labels = c("Not From Favela", "From Favela")))

# exogenous
ex_dem <- c("favela", "old_dist_center",
             "feminino", "idade", 
             "race", "old_education")

ex_dum <- c("factor(month)")

ex_vars <- c(ex_dem, ex_dum)

# plot functions ####
poly2_plot <- function(m_name, m_tag, nbin, results, bin_ranges, ci = FALSE) {
  
  treatment <- function(x, i, est_col) {
    results[i, 1 + est_col] * x + results[nbin + i, 1 + est_col] * x^2
  }
  
  if(ci) m_tag <- paste0("ci_", m_tag)
  
  png(filename=paste0("het2s_poly2_", m_tag, ".png"))
  
  # plot ylim
  y_ub <- 0
  y_lb <- 0
  for (i in seq(nbin)) {
    x_vtx <- -(results[i, 4]) / (2 * results[nbin + i, 4])
    y_vtx <- - (results[i, 4])^2 / (4 * results[nbin + i, 4])
    vert_ub <- ifelse((x_vtx > 0 & x_vtx < 48), y_vtx, 0)
    end_ub <- results[i, 4] * 48 + results[nbin + i, 4] * 48^2
    
    x_vtx <- -(results[i, 3]) / (2 * results[nbin + i, 3])
    y_vtx <- - (results[i, 3])^2 / (4 * results[nbin + i, 3])
    vert_lb <- ifelse((x_vtx > 0 & x_vtx < 48), y_vtx, 0)
    end_lb <- results[i, 3] * 48 + results[nbin + i, 3] * 48^2
    
    y_ub <- max(y_ub, vert_ub, end_ub, 0)
    y_lb <- min(y_lb, vert_lb, end_lb, 0)
  }
  
  plot(0, 0, xlim = c(0, 48), ylim = c(y_lb, y_ub), type = "n",
       # main = paste(m_name, "as Mechanism Driving Income Impacts"),
       xlab = "Months Since Move-In Date",
       ylab = "Change in Monthly Income (MW)",
       cex.lab=1.5, cex.axis=1.5)
  
  cl <- hcl.colors(nbin + 1, palette = "Viridis")
  x <- c(0:48)
  
  for (i in seq(nbin)) {
    lines(x, treatment(x, i, 1),
          col = cl[i], type = 'l', lwd = 2)
    if(ci) {
      lines(x, treatment(x, i, 2),
            col = cl[i], type = 'l', lty = "dashed")
      lines(x, treatment(x, i, 3),
            col = cl[i], type = 'l', lty = "dashed")
    }
  }
  
  legend("bottomleft", inset = 0.02, 
         legend = paste("Bin:", bin_ranges),
         col = cl[1:nbin], lwd = 2, cex = 1.2) 
  
  dev.off()
}

dum_plot <- function(m_name, m_tag, nbin, results, bin_ranges, ci = FALSE) {
  
  if(ci) m_tag <- paste0("ci_", m_tag)
  
  results <- results %>% 
    filter(year <= 5)
  
  y_ub <- max(results$conf.high)
  y_lb <- min(results$conf.low)
  
  png(filename=paste0("het2s_dum_", m_tag, ".png"))
  
  plot(0, 0, xlim = c(0, 5), ylim = c(y_lb, y_ub), type = "n",
       # main = paste(m_name, "as Mechanism Driving Income Impacts"),
       xlab = "Years Since Move-In Date",
       ylab = "Change in Monthly Income (MW)")
  
  cl <- hcl.colors(nbin + 1, palette = "Viridis")

  for (i in seq(nbin)) {
    
    bin_res <- results %>% 
      filter(bin == bin_ranges[i])
    
    lines(bin_res$year, bin_res$estimate,
          col = cl[i], type = 'l', lwd = 2)
    if(ci) {
      lines(bin_res$year, bin_res$conf.low,
            col = cl[i], type = 'l', lty = "dashed")
      lines(bin_res$year, bin_res$conf.high,
            col = cl[i], type = 'l', lty = "dashed")
    }
  }
  
  legend("topleft", inset = 0.02, 
         legend = paste("Bin:", bin_ranges),
         col = cl[1:nbin], lwd = 2, cex = 0.8) 
  
  dev.off()
}

dum_plot2 <- function(y, m_name, m_tag, nbin, results, bin_ranges, ci = FALSE) {
  
  if(ci) m_tag <- paste0("ci_", m_tag)

  bin_plot <- ggplot(results) +
    geom_point(aes(x = year, group = bin_num, color = bin_num, y = estimate), 
               size = 3,
               position=position_dodge(0.4)) +
    geom_errorbar(aes(x = year, group = bin_num, color = bin_num, ymin=conf.low, ymax=conf.high), 
                  size = 1,
                  width = .2,
                  position=position_dodge(0.4)) +
    scale_color_brewer(name = m_name,
                       palette="Dark2") +
    scale_x_continuous(breaks=c(1, 2, 3)) +
    labs(x = "Years Since Move-In Date",
         y = "Change in Monthly Income (MW)") + 
    theme_minimal() + 
    theme(text = element_text(size=10),
          axis.text.x = element_text(hjust=1),
          legend.position = "bottom")
  
  png_name <- paste0("het2s_dum_", m_tag, ".png")
  ggsave(filename = png_name, 
         plot = bin_plot,
         device = "png",
         path = file.path(reg_path, "plot", y),
         width = 15, #default 15
         height = 10, #default 10.74
         units = "cm")
}

# loop regressions ####
run_het2s <- function(panel, y, en_vars, en_type, in_vars, ex_vars, 
                      m_var, m_tag, m_name, nbin, bin_type, lot_dum) {
  
  # remove bin variables from ex_vars
  ex_vars <- ex_vars[!(ex_vars %in% c(m_var))]
  
  # add pre-treatment control for y
  ex_vars <- c(paste0("old_", y), ex_vars)
  
  if(lot_dum) ex_vars <- c(ex_vars, "factor(edital)")
  
  m_var <- quo(!! sym(m_var))
  
  panel <- panel %>% 
    filter(!is.na(!!m_var))
  
  if(bin_type == "equal") {
    panel <- panel %>% 
      mutate(m_bin = cut_number(!!m_var, n = nbin, right = FALSE))
    print(panel$m_bin %>% summary())
  }
  if(bin_type == "interval") {
    panel <- panel %>% 
      mutate(m_bin = cut_interval(!!m_var, n = nbin, right = FALSE))
    print(panel$m_bin %>% summary())
  }
  if(bin_type == "pretty") {
    panel <- panel %>% 
      mutate(m_bin = cut(!! m_var, pretty(!! m_var, n = 4, min.n = 3), ordered_result = TRUE))
    print(panel$m_bin %>% summary())
  }
  if(bin_type == "factor") {
    panel <- panel %>% 
      mutate(m_bin = as.factor(!! m_var))
    nbin <- length(levels(panel$m_bin))
    print(panel$m_bin %>% summary())
  }
  if(!bin_type %in% c("equal", "interval", "pretty", "factor")) {
    cuts <- as.numeric(unlist(strsplit(bin_type,",")))
    print(cuts)
    panel <- panel %>% 
      mutate(m_bin = cut(!! m_var, breaks = cuts, right = FALSE))
    nbin <- length(levels(panel$m_bin))
    print(panel$m_bin %>% summary())
  }
  # ntile_bin = ntile(!! m_var, n = 3),
  # cut_bin = cut(!! m_var, breaks = 3, right = FALSE, ordered_result = TRUE),
  # cut_int_len_bin = cut_interval(!! m_var, n = 3, width = 25))
  
  panel <- panel %>%
    filter(!is.na(m_bin))
  
  num_part <- panel %>%
    distinct(cpfstd) %>%
    nrow()
  
  x1_inter <- sapply(in_vars, paste0, ":m_bin")
  x1 <- c(x1_inter, ex_vars, "m_bin")

  x2_inter <- sapply(en_vars, paste0, ":m_bin")
  x2 <- c(x2_inter, ex_vars, "m_bin")

  formula_het2s <- paste(y, "~",
                         paste(x2, collapse = " + "), "|",
                         paste(x1, collapse = " + "))

  print(formula_het2s)

  fit <- ivreg(formula_het2s, data = panel, na.action = na.exclude)
  tidy_t <- tidy(fit)
  tidy_ci <- confint_tidy(fit, conf.level = 0.90)
  tidy_fit <- cbind(tidy_t, tidy_ci)
  # remove(fit)
  
  bin_ranges <- panel$m_bin %>% 
    levels() 

  if(en_type == "dum") {
    
    results <- tidy_fit %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      filter(grepl("yrs_since", term)) %>% 
      separate(term, into = c("left", "right"), sep = "_move\\)", remove = TRUE) %>% 
      separate(right, into = c("year", "bin"), sep = "(:m_bin)", 
               remove = TRUE, convert = TRUE) %>%
      mutate(bin_num = factor(rep(1:nbin, each = num_yrs), labels = bin_ranges)) %>% 
      mutate(term = paste("Year", year, "- Bin", bin)) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value, year, bin, bin_num)
  }
  if(en_type == "poly2") {
    
    results <- tidy_fit %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      filter(grepl("months_since", term))
    
    en_names <- c("Months", "Sq. of Months")
    
    pretty_terms <- sapply(en_names, paste, "- Bin", bin_ranges) %>% 
      as.vector()
    results$term <- pretty_terms
  }
  
  # save results
  results %>%
    saveRDS(file.path(reg_path, "tidy", y, paste0("het2s_", en_type, "_", m_tag, ".Rds")))
  
  # results <- file.path(reg_path, "tidy", y, paste0("het2s_", en_type, "_", m_tag, ".Rds")) %>%
  #   readRDS()

  # pretty table
  res_kable <- results %>%
    dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
    mutate_at(c(2, 3, 4), ~as.character(signif(., 3))) %>%
    kable("html",
          align = c("l", "r", "r", "r", "r"),
          col.names = c("Term", "Est.", "C.I. low", "C.I. high", "p-value"),
          digits = 3) %>%
    kable_styling("striped", "condensed", full_width = FALSE) %>%
    add_footnote(label = paste("Sample:", num_part, "participants")) %>%
    add_footnote(label = paste("Bins:", paste(bin_ranges, collapse = " ")))

  # export png
  setwd(file.path(reg_path, "png", y))
  save_kable(res_kable, paste0("het2s_", en_type, "_", m_tag, ".png"))

  # export html
  setwd(file.path(reg_path, "html", y))
  save_kable(res_kable, paste0("het2s_", en_type, "_", m_tag, ".html"))
  
  # plot
  setwd(file.path(reg_path, "plot", y))
  if(en_type == "poly2") {
    poly2_plot(m_name, m_tag, nbin, results, bin_ranges, ci = FALSE) 
    poly2_plot(m_name, m_tag, nbin, results, bin_ranges, ci = TRUE) 
  }
  if(en_type == "dum") {
    # dum_plot2(m_name, m_tag, nbin, results, bin_ranges, ci = FALSE) 
    dum_plot2(y, m_name, m_tag, nbin, results, bin_ranges, ci = TRUE) 
  }
}

# scenarios ####

# dummies
num_yrs <- 3

panel3 <- panel2 %>% 
  arrange(desc(month)) %>% 
  distinct(cpfstd, edital, months_since_avail, .keep_all = TRUE) %>%
  filter(months_since_avail %% 12 == 0) %>%
  mutate(yrs_since_avail = months_since_avail / 12,
         yrs_since_move = months_since_move / 12,
         yrs_since_can_move = months_since_can_move / 12) %>%
  filter(yrs_since_avail <= num_yrs)

# endogenous
en_type <- "dum"
en_vars <- c("factor(yrs_since_move)")

# instrumental
in_vars <- c("factor(yrs_since_can_move)")

# moderators
m_names <- c("Moving Distance (km)", "Project Proximity to Downtown (km)",
             "Project Job Accessibility (% of jobs)", "Project Murder Rate (per 100k)",
             "Gender", "Orig. Age", "Race", "Orig. Education", "Orig. Favela",
             "Orig. Proximity to Downtown (km)", "Orig. Murder Rate (per 100k)", 
             "Orig. Income (MW)")

m_vars <- c("pot_displacement", "pot_dist_center", 
            "pot_acc", "pot_murder_rate", 
            "feminino", "old_idade", "race", "old_education", "favela", 
            "old_dist_center", "old_murder_rate", "old_r_inc")

m_tags <- c("dis", "ctr", 
            "acc", "mur", 
            "fem", "age", "rac", "edu", "fav",
            "octr", "omur", "oinc")

bin_types <- c("0,30,60,90", "25,50,75", #"25,40,55,70", 
               "0,20,40,60", "0,25,50",
               "factor", "18,25,40,60,100", "factor", "factor", "factor",
               "0,30,60,90", "0,25,50", "0,1,1.5,2,16")

nbins <- c(NA, NA,
           3, NA, 
           NA, NA, NA, NA, NA,
           NA, NA, NA)

lot_dum <- F

# debug
# panel <- panel3
# y <- "r_inc"
# m_var <- "pot_dist_center"
# m_tag <- "ctr"
# m_name <- "Prox. Downtown"
# nbin <- NA
# bin_type <- "20,45,70"
# ci <- TRUE
i <- 12

for(i in 1:12) {
  print(m_vars[i])
  run_het2s(panel3, "r_inc",
            en_vars, en_type,
            in_vars, ex_vars,
            m_vars[i], m_tags[i], m_names[i],
            nbins[i], bin_types[i], lot_dum)
}

# panel <- panel2
# y <- "r_inc"
# m_var <- "new_acc"
# m_tag <- "a"
# m_name <- "Job Accessiblity"
# nbin <- 3
# bin_type <- "equal"
# ci <- TRUE

# # 2-degree polynomial
# # endogenous
# en_type <- "poly2"
# en_vars <- c("months_since_move", "months_since_move_sq")
# 
# # instrumental
# in_vars <- c("months_since_can_move", "months_since_can_move_sq")
# 
# # mechanisms
# m_names <- c("Pot. Moving Distance", "Pot. Proximity to Downtown (km)",
#              "Pot. Job Accessibility", "Pot. Murder Rate",
#              "Gender", "Orig. Age", "Race", "Orig. Education", "Orig. Favela",
#              "Orig. Proximity to Downtown (km)", "Orig. Murder Rate (per 100k)", 
#              "Orig. Income (MW)")
# 
# m_vars <- c("pot_displacement", "pot_dist_center", 
#             "pot_acc", "pot_murder_rate", 
#             "feminino", "old_idade", "race", "old_education", "favela", 
#             "old_dist_center", "old_murder_rate", "old_r_inc")
# 
# m_tags <- c("dis", "ctr", 
#             "acc", "mur", 
#             "fem", "age", "rac", "edu", "fav",
#             "octr", "omur", "oinc")
# 
# bin_types <- c("0,30,60,90", "25,40,55,70", 
#                "equal", "equal",
#                "factor", "18,30,45,60,90", "factor", "factor", "factor",
#                "0,20,40,60,80", "equal", "0,1,2,4,16")
# 
# nbins <- c(NA, NA,
#            3, 3, 
#            NA, NA, NA, NA, NA,
#            NA, 3, NA)
# 
# lot_dum <- T
# 
# for(i in c(12:12)) {
#   
#   m_var <- m_vars[i]
#   m_name <- m_names[i]
#   bin_type <- bin_types[i]
#   nbin <- nbins[i]
#   m_tag <- m_tags[i]
#   
#   print(paste("Studying", m_name, "using var", m_var, "with", bin_type, "bins"))
#   
#   run_het2s(panel2, "r_inc",
#             en_vars, en_type,
#             in_vars, ex_vars,
#             m_var, m_tag, m_name,
#             nbin, bin_type, lot_dum)
# 
# }