library(tidyverse)
library(stringi)
library(zoo)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

validate_cpf <- function(cpfs, data){
  split <- str_split_fixed(cpfs, "", 11)
  split <- apply(split, 2, function(x) as.numeric(x))
  weights <- seq(10, 2)
  fd <- sweep(split[,1:9], MARGIN = 2, weights, `*`)
  sum <- rowSums(fd)
  remainder <- sum %% 11
  first_digit <- 11 - remainder
  first_digit <- ifelse(first_digit > 9, 0, first_digit)
  split_fd <- cbind(split, first_digit, deparse.level = 0)
  weights <- c(11, weights)
  sd <- sweep(split_fd[,c(1:9, 12)], MARGIN = 2, weights, `*`)
  sums <- rowSums(sd)
  remainders <- sums %% 11
  second_digit <- 11 - remainders
  second_digit <- ifelse(second_digit > 9, 0, second_digit)
  cod_validate <- cbind(first_digit, second_digit, deparse.level = 0)
  cod_disponivel <-  split[,10:11]
  valid_code <- ifelse(cod_validate == cod_disponivel, 1, 0)
  colnames(valid_code) <- c("first_digit_valid", "second_digit_valid")
  valid_code <- as_tibble(valid_code)
  return(bind_cols(data, valid_code))	
}

# lottery winners
# import_sorteados <- function(edital){
#   sorteados_edital <- file.path(wd, "data", "input", paste0(edital, ".xlsx")) %>%
#     read_excel() %>%
#     mutate(cpf = as.numeric(CPF),
#            cpfstd = sprintf("%011.0f", cpf)) %>%
#     filter(nchar(sorteados_novos$cpfstd) <= 11) %>%
#     validate_cpf(cpfs = sorteados_novos$cpfstd, data = .) %>%
#     filter(sorteados_novos$first_digit_valid == 0 | 
#              sorteados_novos$second_digit_valid == 0) %>% #o erro esta no PDF da secretaria, deixar assim mesmo
#     dplyr::select(cpfstd, id_edital, premio) %>% 
#     mutate(valor = 1) %>% 
#     arrange(cpfstd) %>% 
#     spread(key = "id_edital", value = "valor", fill = 0) %>% 
#     filter(!is.na(cpfstd), !duplicated(cpfstd)) %>%
#     mutate(vezes_sorteado = rowSums(sorteados[, c(3:3)], na.rm = T)) %>%
#     filter(vezes_sorteado > 0) %>%
#     mutate(sorteado = 1) %>%
#     mutate(primeiro_ins = edital)
#   
#   sorteados_edital
# }

# sorteados <- do.call(rbind, lapply(editais, import_sorteados))

### Import participants ####
inscritos <- file.path(wd, "data", "input", "loteria", "inscritos_wide.Rds") %>%
  readRDS() %>% 
  filter(!is.na(cpfstd)) %>%
  gather(colnames(.)[!(colnames(.) %in% c("cpfstd", 
                                          "vezes_aparece", 
                                          "primeiro_sorteio", 
                                          "ultimo_sorteio"))],
         key = "edital", 
         value = "inscrito") %>%
  filter(inscrito == 1)

### Import lottery winners ####
sorteados <- file.path(wd, "data", "input", "loteria", "sorteados_wide.Rds") %>%
  readRDS() %>%
  filter(!is.na(cpfstd)) %>%
  dplyr::select(-vezes_aparece, -primeiro_sorteio, -ultimo_sorteio) %>%
  gather(colnames(.)[!(colnames(.) %in% c("cpfstd"))],
         key = "edital", 
         value = "sorteado") %>%
  filter(sorteado == 1)
  
### Import beneficiaries ####
ben_emp <- file.path(wd, "data", "input", "empreendimentos", "ben_emp.xlsx") %>%
  read_excel()

project_lotteries <- file.path(wd, "data", "intermediate", "project_lotteries.Rds") %>% 
  readRDS()

beneficiarios <- file.path(wd, "data", "input", "loteria", "beneficiarios.Rds") %>%
  readRDS() %>%
  filter(!is.na(cpfstd)) %>%
  mutate(nome = stri_trim(nome),
         empreendimento = stri_trim(empreendimento)) %>%
  select(cpfstd, nome, empreendimento, ben_ano = ano) %>%
  arrange(-ben_ano) %>%
  distinct(cpfstd, .keep_all = TRUE)

beneficiaries <- beneficiarios %>% 
  inner_join(., ben_emp, by = c("empreendimento" = "ben_emp")) %>%
  inner_join(., sorteados, by = c("cpfstd")) %>%
  left_join(., project_lotteries, by = c("edital", "cod_lv")) %>% 
  arrange(desc(dt_sor)) %>%
  distinct(cpfstd, .keep_all = T) %>%
  dplyr::select(cpfstd, nome, cod_lv, edital, territorialidade, dt_ent, 
                hex, dist_center, acc2014, acc2015, acc2016, acc2017) %>%
  mutate(ben = 1,
         ben_sorteio = edital)

saveRDS(beneficiaries, file.path(wd, "data", "intermediate", "beneficiaries.Rds"))

beneficiaries <- file.path(wd, "data", "intermediate", "beneficiaries.Rds") %>% 
  readRDS()

# Lotteries table
lotteries <- file.path(wd, "data", "intermediate", "lotteries.Rds") %>% 
  readRDS()

### Join databases ####
participantes <- inscritos %>%
  left_join(., sorteados, by = c("cpfstd", "edital")) %>%
  mutate(sorteado = replace_na(sorteado, 0)) %>%
  left_join(., dplyr::select(beneficiaries, cpfstd, cod_lv, ben_sorteio, dt_ent, edital, ben), by = c("cpfstd", "edital")) %>%
  mutate(ben = replace_na(ben, 0)) %>%
  # fill ben_sorteio for CPF
  arrange(desc(ben)) %>% 
  group_by(cpfstd) %>% 
  na.locf(na.rm = F) %>% 
  ungroup() %>%
  # identify beneficiaries of future lotteries who will be removed from the panel
  mutate(ben_futuro = ifelse(cpfstd %in% beneficiaries$cpfstd &
                               ben == 0, 
                             1, 0)) %>%
  # discarded lottery
  filter(edital != "edital01.2014") %>%
  # use average move-in date for non-movers
  inner_join(., dplyr::select(lotteries, edital, dt_ins, dt_sor, dt_ent_mean = dt_ent, territorialidade), by = "edital") %>%
  mutate(dt_ent = as_date(ifelse(!is.na(dt_ent), dt_ent_mean, dt_ent))) %>% # update move-in date with date for specific project
  dplyr::select(-dt_ent_mean)

participantes <- participantes %>%
  validate_cpf(cpfs = participantes$cpfstd, data = .) %>% 
  filter(first_digit_valid == 1,
         second_digit_valid == 1) %>% 
  dplyr::select(-first_digit_valid, -second_digit_valid)

saveRDS(participantes, file = file.path(wd, "data", "intermediate", "participantes.Rds"))
