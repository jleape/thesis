library(tidyverse)
library(lubridate)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

months <- seq(ymd('2011-01-01'), ymd('2017-12-01'), by = '1 month') %>%
  tibble(month = .)

empinc_part <- file.path(wd, "data", "intermediate", "rais", "participantes", "empinc.Rds") %>%
  readRDS()

# participant summary
monthly_summ_part <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>% 
  # edital01.2014 was invalidated and edital26.2015 does not appear to be random
  filter(!edital %in% c("edital01.2014", "edital26.2015"),
         territorialidade == 0,
         ben_futuro == 0,
         year(dt_ent) < 2018,
         year(dt_ent) > 2011) %>% 
  mutate(group = ifelse(ben == 1, "beneficiary", NA),
         group = ifelse(sorteado == 1 & ben == 0, "non-complier", group),
         group = ifelse(sorteado == 0, "loser", group)) %>% 
  arrange(desc(sorteado), dt_sor) %>%
  distinct(cpfstd, group) %>% 
  merge(months, ., by = NULL) %>%
  left_join(empinc_part, by = c("cpfstd", "month")) %>% 
  mutate(emp = replace_na(emp, 0),
         inc = replace_na(inc, 0),
         hrs_wk = replace_na(hrs_wk, 0),
         r_inc = replace_na(r_inc, 0)) %>% 
  filter(r_inc <= 4,
         hrs_wk <= 120) %>%
  dplyr::select(month, "Treatment Group" = group, 
                "Employment Rate" = emp, 
                "Monthly Income (BRL)" = inc, 
                "Weekly Work Hours" = hrs_wk,
                "Monthly Income (MW)" = r_inc) %>% 
  group_by(month, `Treatment Group`) %>%
  summarise_all(list(mean)) %>% 
  mutate(Sample = "all participants") %>% 
  ungroup()

remove(empinc_part)

# panel summary
monthly_summ_panel <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>%
  dplyr::select(month, "Treatment Group" = group, 
                "Employment Rate" = emp, "Monthly Income (BRL)" = inc, 
                "Weekly Work Hours" = hrs_wk, "Monthly Income (MW)" = r_inc) %>% 
  group_by(month, `Treatment Group`) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  ungroup() %>% 
  mutate(Sample = "panel")

# combined summary
monthly_summ_rais <- monthly_summ_panel %>% 
  rbind(monthly_summ_part)

saveRDS(monthly_summ_rais, file.path(wd, "data", "intermediate", "rais", "monthly_summ_rais.Rds"))

# monthly_summ_rais <- file.path(wd, "data", "intermediate", "rais", "monthly_summ_rais.Rds") %>% 
#   readRDS()

emp_monthly_summ <- ggplot(monthly_summ_rais) +
  aes(x = month, y = `Employment Rate`, colour = `Treatment Group`) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", 
       y = "Fraction Formally Employed", 
       title = "Employment Rate by Treatment Group", 
       color = "Treatment Group") +
  theme_minimal() +
  facet_wrap(vars(Sample)) + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title= element_blank(), # element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center")

ggsave(filename = "emp_monthly_summ.png", 
       plot = emp_monthly_summ,
       device = "png",
       path = file.path(wd, "data", "output", "summary_stats"),
       width = 16.5, #default 15
       height = 10, #default 10.74
       units = "cm")

inc_monthly_summ <- ggplot(monthly_summ_rais) +
  aes(x = month, y = `Monthly Income (BRL)`, colour = `Treatment Group`) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", 
       y = "Mean Monthly Income (BRL)", 
       title = "Income by Treatment Group", 
       color = "Treatment Group") +
  theme_minimal() +
  facet_wrap(vars(Sample)) + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title= element_blank(), # element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center")

ggsave(filename = "inc_monthly_summ.png", 
       plot = inc_monthly_summ,
       device = "png",
       path = file.path(wd, "data", "output", "summary_stats"),
       width = 16.5, #default 15
       height = 10, #default 10.74
       units = "cm")

hrs_wk_monthly_summ <- ggplot(monthly_summ_rais) +
  aes(x = month, y = `Weekly Work Hours`, colour = `Treatment Group`) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", 
       y = "Mean Weekly Work Hours", 
       title = "Weekly Work Hours by Treatment Group", 
       color = "Treatment Group") +
  theme_minimal() +
  facet_wrap(vars(Sample)) + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title= element_blank(), # element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center")

ggsave(filename = "hrs_wk_monthly_summ.png", 
       plot = hrs_wk_monthly_summ,
       device = "png",
       path = file.path(wd, "data", "output", "summary_stats"),
       width = 16.5, #default 15
       height = 10, #default 10.74
       units = "cm")

r_inc_monthly_summ <- ggplot(monthly_summ_rais) +
  aes(x = month, y = `Monthly Income (MW)`, colour = `Treatment Group`) +
  geom_line(size = 1L) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Month", 
       y = "Mean Monthly Income (MW)", 
       title = "Income by Treatment Group", 
       color = "Treatment Group") +
  theme_minimal() +
  facet_wrap(vars(Sample)) + 
  theme(legend.position="bottom",
        legend.box='horizontal',
        legend.title= element_blank(), # element_text(size=8),
        legend.text=element_text(size=10),
        legend.justification = "center")

ggsave(filename = "r_inc_monthly_summ.png", 
       plot = r_inc_monthly_summ,
       device = "png",
       path = file.path(wd, "data", "output", "summary_stats"),
       width = 16.5, #default 15
       height = 10, #default 10.74
       units = "cm")