library(dplyr)
library(lubridate)
set.seed(123)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

ben_geral <- file.path(wd, "data", "intermediate", "beneficiaries.Rds") %>%
  readRDS() %>%
  filter(territorialidade == 0) %>%
  dplyr::select(-territorialidade, acc2014, acc2015, acc2016, hex, ben)

# emp_central_old <- c("Taroni", "Destri", "Vidal")
# emp_central_new <- c("Guadalupe", "Colonia_Juliano_Moreira")
# emp_marginal_old <- c("Sevilha", "Zaragoza", "Cascais", "Estoril", "Evora")
# emp_marginal_new <- c("Porto_Fino", "Park_Onix")

ben_central_old <- ben_geral %>%
  # filter(cod_lv %in% emp_central_old) %>%
  filter(acc2017 > 15,
         year(dt_ent) < 2016) %>%
  sample_n(50) %>%
  mutate(rank = row_number(),
         location = "central",
         tenure = "long")

ben_central_new <- ben_geral %>%
  # filter(cod_lv %in% emp_central_new) %>%
  filter(acc2017 > 15,
         year(dt_ent) > 2016) %>%
  sample_n(50) %>%
  mutate(rank = row_number(),
         location = "central",
         tenure = "short")

ben_marginal_old <- ben_geral %>%
  # filter(cod_lv %in% emp_marginal_old) %>%
  filter(acc2017 < 15,
         year(dt_ent) < 2016) %>%
  sample_n(50) %>%
  mutate(rank = row_number(),
         location = "marginal",
         tenure = "long")

ben_marginal_new <- ben_geral %>%
  # filter(cod_lv %in% emp_marginal_new) %>%
  filter(acc2017 < 15,
         year(dt_ent) > 2016) %>%
  sample_n(50) %>%
  mutate(rank = row_number(),
         location = "marginal",
         tenure = "short")

sample <- rbind(ben_central_old, ben_central_new, ben_marginal_old, ben_marginal_new)

sample_cpf <- sample %>%
  dplyr::select(cpfstd) %>%
  unique()

write_csv(sample, file.path(wd, "Survey", "sample1.csv"))

sample1_serasa <- file.path(wd, "Survey", "sample1_serasa.csv") %>% 
  read.csv(header = TRUE, stringsAsFactors = FALSE) %>% 
  inner_join(., ben_geral, by = "cpfstd")
  

