library(broom) # tidy summary
library(dplyr)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(DescTools)
options(scipen=999)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
reg_path <- file.path(wd, "data", "output", "regressions", "17_yr_factor")

scale_vars <- c("idade", "old_dist_center", "old_murder_rate", "old_r_inc", 
                "pot_dist_center", "pot_murder_rate", "pot_displacement")

panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>% 
  filter(sorteado == 1) %>% 
  distinct(cpfstd, edital, .keep_all = TRUE) %>% 
  dplyr::select(-starts_with("month")) %>% 
  mutate_at(vars(one_of(scale_vars)), scale)

num_ben <- panel2 %>% 
  filter(group == "beneficiary") %>% 
  nrow()

num_den <- panel2 %>% 
  filter(group == "non-complier") %>% 
  nrow()

# panel2s <- file.path(wd, "data", "output", "panels", "panel2s.Rds") %>% 
#   readRDS() %>% 
#   filter(sorteado == 1) %>% 
#   distinct(cpfstd, edital, .keep_all = TRUE) %>% 
#   dplyr::select(-starts_with("month"))

# covariates
ex_dem <- c("feminino", "idade",
            "cor_negra", "cor_branca", "cor_outras", 
            "old_dist_center", "old_murder_rate", "favela",
            "old_r_inc",
            "old_analfabeto", "old_fundamental_completo", 
            "old_medio_completo", "old_superior_completo")

pot_vars <- c("pot_dist_center", "pot_murder_rate", "pot_displacement")

t <- factor(c(pot_vars, ex_dem), levels = c(pot_vars, ex_dem))

pl <- c(feminino = "Female", 
        idade = "Age", 
        cor_negra = "Race: Black", 
        cor_branca = "Race: White", 
        cor_outras = "Race: Other", 
        old_dist_center = "Orig. Proximity to Downtown (km)", 
        old_acc = "Orig. Job Accessibility (% of jobs)", 
        old_murder_rate = "Orig. Murder Rate (per 100k)", 
        favela = "Orig. Favela", 
        old_emp = "Orig. Employment Status",
        old_r_inc = "Orig. Income (MW)",
        old_hrs_wk = "Orig. Weekly Hours",
        old_analfabeto = "Orig. Illiterate", 
        old_fundamental_completo = "Orig. Primary School", 
        old_medio_completo = "Orig. Secondary School", 
        old_superior_completo = "Orig. Higher Education",
        pot_dist_center = "Project Proximity to Downtown (km)", 
        pot_acc = "Project Job Accessibility (% of jobs)", 
        pot_murder_rate = "Project Murder Rate (per 100k)", 
        pot_displacement = "Project Moving Distance (km)")

# proximity to downtown
covars <- c(pot_vars, ex_dem)

fit <- glm(reformulate(covars, "ben"), 
           data = panel2, 
           family = binomial(link = 'logit'))

fit %>% 
  summary()

R_sq <- PseudoR2(fit, "all")

R_sq

tidy_fit <- fit %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.90)

# fit_s <- glm(reformulate(covars, "ben"), 
#              data = panel2s, 
#              family = binomial(link = 'logit')) %>% 
#   tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.90)

results <- tidy_fit %>% 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
  # cbind(dplyr::select(fit_s, estimate_s = estimate, 
  #                     conf.low_s = conf.low, conf.high_s = conf.high, 
  #                     p.value_s = p.value)) %>% 
  filter(term %in% t)

results %>%
  saveRDS(file.path(reg_path, "tidy", "ben", "log1s_ben.Rds"))

# plot
results <- results %>%
  mutate(term = factor(pl[term], levels = pl))

setwd(file.path(reg_path, "plot", "ben"))
png("log1s_ben.png",
    width = 1600, height = 800)

ggplot(results) +
  geom_errorbar(aes(x = term, ymin=conf.low, ymax=conf.high, 
                    color = ifelse(p.value < 0.05, "#2171b5", "light grey")), 
                size = 1.5,
                width = 0.2) +
  geom_point(aes(x = term, y = estimate, 
                 color = ifelse(p.value < 0.05, "#2171b5", "light grey")), 
             size = 6) +
  scale_color_identity() + 
  labs(x = "\nDecision Factors", 
       y = "Odds Ratio\n", 
       title = "Factors Affecting Decision to Move",
       subtitle = "Odds of Moving to PMCMV Unit for Lottery Winners") + 
  theme_minimal() + 
  theme(text = element_text(size=35),
        axis.text.x = element_text(angle=30, hjust=1))

dev.off()

# style kable
res_kable <- results %>% 
  mutate(term = pl[term]) %>%
  mutate_at(c(2:4), ~as.character(signif(., 3))) %>%
  kable("html", 
        align = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
        col.names = c("Term", "Est.", "C.I. low", "C.I. high", "p-value"),
        digits = 3) %>% 
  kable_styling("striped", "condensed", full_width = FALSE) %>% 
  # add_header_above(c(" " = 1, "Unscaled" = 4, "Normalized" = 4)) %>% 
  add_footnote(label = paste("Sample:", num_ben, "beneficiaries,", 
                             num_den, "non-compliers."))

# export png
setwd(file.path(reg_path, "png", "ben"))
save_kable(res_kable, "log1s_ben.png")

# export html
setwd(file.path(reg_path, "html", "ben"))
save_kable(res_kable, "log1s_ben.html")
  
# detach("package:dplyr", unload = TRUE)
# library(predictshine)
# predictshine(fit)

