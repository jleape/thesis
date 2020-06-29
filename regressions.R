library(stringi) # substring replacement
library(broom) # tidy summary
library(dplyr)
library(lubridate)
library(kableExtra)
options(scipen=999)

# Sys.sleep(7200)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
reg_path <- file.path(wd, "data", "output", "regressions", "17_robust")

source(file.path(wd, "pmcmv", "reg_funs.R"))

panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>% 
  arrange(desc(month)) %>% 
  distinct(cpfstd, edital, months_since_avail, .keep_all = TRUE) %>%
  filter(months_since_avail %% 12 == 0) %>%
  mutate(yrs_since_move = months_since_move / 12,
         yrs_since_can_move = months_since_can_move / 12)

# endogenous 
en_vars <- list(c("factor(yrs_since_move)"),
                c("months_since_move", "months_since_move_sq"))

# instrumental
in_vars <- list(c("factor(yrs_since_can_move)"),
                c("months_since_can_move", "months_since_can_move_sq"))

# exogenous
ex_dem <- c("favela", 
            "old_dist_center",
            "feminino", "idade", "idade2", 
            "cor_negra", "cor_branca", "cor_outras", 
            "old_analfabeto", "old_fundamental_completo", 
            "old_medio_completo", "old_superior_completo")

ex_dum <- c("factor(month)")

ex_vars1 <- c(ex_dem, ex_dum)

# models
m_tags11 <- c("11t_r") #, "11c", "11ci", "11d", "11cd", "11cdi", "11ids", "11m", "11all") 

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

# plot functions ####
poly2_plot <- function(fit_type, pretty_y, m_tag, results, ci = FALSE) {
  
  treatment <- function(x, est_col) {
    results[1, 1 + est_col] * x + results[2, 1 + est_col] * x^2
  }
  
  if(ci) m_tag <- paste0("ci_", m_tag)

  png(filename=paste0(fit_type, "_", m_tag, ".png"))
  
  # plot ylim
  x_vtx <- - (results[1, 4]) / (2 * results[2, 4])
  y_vtx <- - (results[1, 4])^2 / (4 * results[2, 4])
  vert_ub <- ifelse((x_vtx > 0 & x_vtx < 48), y_vtx, 0)
  end_ub <- results[1, 4] * 48 + results[2, 4] * 48^2
  
  x_vtx <- - (results[1, 3]) / (2 * results[2, 3])
  y_vtx <- - (results[1, 3])^2 / (4 * results[2, 3])
  vert_lb <- ifelse((x_vtx > 0 & x_vtx < 48), y_vtx, 0)
  end_lb <- results[1, 3] * 48 + results[2, 3] * 48^2
  
  y_ub <- max(vert_ub, end_ub, 0)
  y_lb <- min(vert_lb, end_lb, 0)
  
  plot(0, 0, xlim = c(0, 48), ylim = c(y_lb, y_ub), type = "n",
       main = paste("Treatment Effect on", pretty_y),
       xlab = "Months Since Move-In Date",
       ylab = paste("Change in", pretty_y))
  
  x <- c(0:48)

  lines(x, treatment(x, 1),
        col = "black", type = 'l', lwd = 2)
  if(ci) {
    lines(x, treatment(x, 2),
          col = "black", type = 'l', lty = "dashed")
    lines(x, treatment(x, 3),
          col = "black", type = 'l', lty = "dashed")
  }
  
  dev.off()
}

dum_plot <- function(fit_type, pretty_y, m_tag, results, ci = FALSE) {
  
  if(ci) m_tag <- paste0("ci_", m_tag)
  
  bin_plot <- ggplot(results) +
    geom_point(aes(x = year, group = model, color = model, y = estimate), 
               size = 3,
               position=position_dodge(0.4)) +
    geom_errorbar(aes(x = year, group = model, color = model, ymin=conf.low, ymax=conf.high), 
                  size = 1,
                  width = .2,
                  position=position_dodge(0.4)) +
    scale_color_brewer(name = m_name,
                       palette="Dark2") +
    scale_x_continuous(breaks=c(1, 2, 3, 4)) +
    labs(x = "Years Since Move-In Date",
         y = paste("Change in", pretty_y)) + 
    theme_minimal() + 
    theme(text = element_text(size=10),
          axis.text.x = element_text(hjust=1),
          legend.position = "bottom")
  
  png_name <- paste0("fit_type", "_dum_", m_tag, ".png")
  ggsave(filename = png_name, 
         plot = bin_plot,
         device = "png",
         path = file.path(reg_path, "plot", y),
         width = 15, #default 15
         height = 10, #default 10.74
         units = "cm")
}

### regressions ####
run_models <- function(panel, y, pretty_y, yr, fit_type) {
  
  ex_vars <- c(paste0("old_", y), ex_vars1)
  # if(fit_type %in% c("ols1s", "ols2s")) { 
  #   ex_vars <- c(ex_vars, "factor(edital)")
  # } else {
  #   ex_vars <- ex_vars[!(ex_vars %in% c("old_dist_center", "old_murder_rate", 
  #                                       "cor_negra", "cor_branca", "cor_outras"))]
  # }
  
  panel <- panel %>% 
    filter(year >= yr)
  
  num_part <- panel %>% 
    distinct(cpfstd) %>%
    nrow()
  
  num_months <- panel %>% 
    distinct(month) %>%
    nrow()
  
  print(paste("Var:", y, "Fit:", fit_type, "Since:", yr))
  
  res_digits <- ifelse(y == "emp", 4, 2)
  
  m_tags <- ifelse(yr == 2011, m_tags11, m_tags14)
  
  for(i in seq_along(m_tags)){

    print(m_tags[i])

    if (fit_type == "ols1s") {
      fit <- ols1s(panel, y, in_vars[[i]], ex_vars, robust = FALSE) %>%
        tidy(conf.int = TRUE, conf.level = 0.90)
    }
    if (fit_type == "log1s") {
      fit <- log1s(panel, y, in_vars[[i]], ex_vars) %>%
        tidy(conf.int = TRUE, conf.level = 0.90, exponentiate = TRUE)
    }
    if (fit_type == "ols2s") {
      fit <- ols2s(panel, y, en_vars[[i]], in_vars[[i]], ex_vars, robust = FALSE) %>%
        tidy(conf.int = TRUE, conf.level = 0.90)
    }
    if (fit_type == "log2s") {
      fit <- log2s(panel, y, en_vars[[i]], in_vars[[i]], ex_vars) %>%
        tidy(conf.int = TRUE, conf.level = 0.90, exponentiate = TRUE)
    }

    # combine tibbles
    results <- fit %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value) %>%
      filter(term %in% t)

    # save results
    results %>%
      saveRDS(file.path(reg_path, "tidy", y, paste0(fit_type, "_", m_tags[i], ".Rds")))
    
    # read results
    # results <- file.path(reg_path, "tidy", y, paste0(fit_type, "_", m_tags[i], ".Rds")) %>% 
    #   readRDS()
    
    # style kable
    res_kable <- results %>% 
      mutate(term = pl[term]) %>%
      mutate_at(c(2:4), ~as.character(signif(., 3))) %>%
      kable("html", 
            align = c("l", "r", "r", "r", "r"),
            col.names = c("Term", "Est.", "C.I. low", "C.I. high", "p-value"),
            digits = 3) %>% 
      kable_styling("striped", "condensed", full_width = FALSE) %>% 
      # add_header_above(c(" " = 1, "Unscaled" = 4, "Normalized" = 4)) %>% 
      add_footnote(label = paste("Sample:", num_part, "participants over", 
                                 num_months, "months, starting in", yr))
    
    # export png
    setwd(file.path(reg_path, "png", y))
    save_kable(res_kable, paste0(fit_type, "_", m_tags[i], ".png"))
    
    # export html
    setwd(file.path(reg_path, "html", y))
    save_kable(res_kable, paste0(fit_type, "_", m_tags[i], ".html"))
    
    # export plot
    # if(fit_type %in% c("ols1s", "ols2s")) {
    #   setwd(file.path(reg_path, "plot", y))
    #   poly2_plot(fit_type, pretty_y, m_tags[i], results, ci = FALSE)
    #   poly2_plot(fit_type, pretty_y, m_tags[i], results, ci = TRUE)
    # }
  }
}

run_models(panel2, "r_inc", "Monthy Income (MW)", 2011, "ols1s")
run_models(panel2, "r_inc", "Monthy Income (MW)", 2011, "ols2s")
# run_models(panel2, "emp", "Employment Status", 2011, "ols1s")
# run_models(panel2, "emp", "Employment Status", 2011, "ols2s")
# run_models(panel2, "hrs_wk", "Weekly Hours", 2011, "ols1s")
# run_models(panel2, "hrs_wk", "Weekly Hours", 2011, "ols2s")

# # ITT vs LATE plot
# y <- "emp"
# fit_type <- "ols"
# pretty_y <- "Employment"
# m_tag <- "11t"
# ci = TRUE
# results <- file.path(reg_path, "tidy", y, paste0(fit_type, "_", m_tag, ".Rds")) %>% 
#   readRDS()
# 
# setwd(file.path(reg_path, "plot", y))
# 
# treatment <- function(x, est_col) {
#   results[1, 1 + est_col] * x + results[2, 1 + est_col] * x^2
# }
# 
# if(ci) m_tag <- paste0("ci_", m_tag)
# 
# png(filename=paste0(fit_type, "_", m_tag, ".png"))
# 
# # plot ylim
# y_ub <- .03
# y_lb <- -0.01
# 
# plot(0, 0, xlim = c(0, 48), ylim = c(y_lb, y_ub), type = "n",
#      # main = paste("Treatment Effect on", pretty_y),
#      xlab = "Months Since Move-In Date",
#      ylab = paste("Change in", pretty_y),
#      cex.lab=1.5, cex.axis=1.5)
# 
# x <- c(0:48)
# 
# #ITT
# lines(x, treatment(x, 1),
#       col = "#1b9e77", type = 'l', lwd = 3)
# if(ci) {
#   lines(x, treatment(x, 2),
#         col = "#1b9e77", type = 'l', lwd = 2, lty = "dashed")
#   lines(x, treatment(x, 3),
#         col = "#1b9e77", type = 'l', lwd = 2, lty = "dashed")
# }
# 
# #LATE
# lines(x, treatment(x, 6),
#       col = "#7570b3", type = 'l', lwd = 3)
# if(ci) {
#   lines(x, treatment(x, 7),
#         col = "#7570b3", type = 'l', lwd = 2, lty = "dashed")
#   lines(x, treatment(x, 8),
#         col = "#7570b3", type = 'l', lwd = 2, lty = "dashed")
# }
# 
# legend("topleft", inset = 0.02, 
#        legend = c("ITT", "LATE"),
#        col = c("#1b9e77", "#7570b3"), lwd = 2, cex = 1.2) 
# 
# dev.off()


# Regression summary ####
# detach("package:dplyr", unload = TRUE)
# library(predictshine)
# predictshine(ols1s_inc)
