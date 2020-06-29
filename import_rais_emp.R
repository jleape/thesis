library(tidyverse)
library(broom)
library(lubridate)

options(scipen=999) #no scientific notation
options(digits = 3)


# rais_emp includes multiple records per cpf and month
# empinc combines them to give overall income, working hours and employment status by month

# PATHS
wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"
rais_path <- file.path(wd, "data", "input", "rais")

part_cpfs <- file.path(wd, "data", "intermediate", "participantes.Rds") %>%
  readRDS() %>%
  dplyr::select(cpfstd) %>%
  unique()

saveRDS(part_cpfs, file = file.path(wd, "data", "intermediate", "part_cpfs.Rds"))

# part_cpfs <- file.path(wd, "data", "intermediate", "part_cpfs.Rds") %>%
#   readRDS()

### Employment status and income from RAIS ####
import_rais_emp <- function(yr, rais_path, rais_cols) {
  print(yr) 
  # import raw RAIS
  filepath <- file.path(rais_path, paste0("RJ", yr, "ID.txt"))
  cols <- unlist(rais_cols[yr], use.names=FALSE)
  
  rais_emp_yr <- filepath %>%
    read.csv2(fileEncoding = "WINDOWS-1252", strip.white = TRUE, stringsAsFactors = F) %>% # , nrows = 1000)
    # standardize column names
    rename(emp_31dez = cols[1],
           mes_deslig = cols[2],
           nat_jur = cols[3],
           ind_cei = cols[4],
           tipo_estab = cols[5],
           dt_admissao = cols[6],
           rem_dez = cols[7],
           rem_dez_sm = cols[8],
           rem_med = cols[9],
           rem_med_sm = cols[10],
           temp_empr = cols[11],
           ult_rem = cols[12],
           sal_nom = cols[13],
           cpf =  cols[14],
           ident =  cols[15],
           cnpj =  cols[16],
           nome =  cols[17],
           ocup_2002 = cols[18],
           qt_dias_afast = cols[19],
           horas_contr =  cols[20]) %>%
    mutate(rais_yr = yr,
           cpfstd = sprintf("%011.0f", cpf),
           # rem_dez = as.numeric(gsub(",", ".", rem_dez)),
           # rem_dez_sm = as.numeric(gsub(",", ".", rem_dez_sm)),
           rem_med = as.numeric(gsub(",", ".", rem_med)),
           # rem_med_sm = as.numeric(gsub(",", ".", rem_med_sm)),
           # temp_empr = as.numeric(gsub(",", ".", temp_empr)),
           # ult_rem = as.numeric(gsub(",", ".", ult_rem)),
           dt_admissao = paste0("0", dt_admissao),
           ano_admissao = substr(dt_admissao, nchar(dt_admissao)-4+1, nchar(dt_admissao)),
           mes_admissao = substr(dt_admissao, nchar(dt_admissao)-6+1, nchar(dt_admissao)-4),
           dia_admissao = substr(dt_admissao, nchar(dt_admissao)-8+1, nchar(dt_admissao)-6),
           dt_start = ymd(paste(ano_admissao, mes_admissao, dia_admissao, sep = "-")),
           ano_deslig = ifelse(mes_deslig > 0, rais_yr, NA),
           dt_end = ymd(paste(ano_deslig, mes_deslig, "1", sep = "-"))) %>%
    dplyr::select(cpfstd,
                  rais_yr,
                  inc_avg = rem_med, # rem_dez, rem_dez_sm, rem_med_sm,
                  dt_start, 
                  dt_end,
                  # nat_jur, nome, ind_cei, temp_empr, ident,
                  # emp_31dez, tipo_estab, ult_rem, sal_nom, cnpj,
                  # job_id = ocup_2002,
                  # days_absent = qt_dias_afast, 
                  hrs_wk = horas_contr)
  
  saveRDS(rais_emp_yr, file = file.path(wd, "data", "intermediate", "rais", "all_cpfs",
                                        "rais_emp", paste0("rais_emp_", yr, ".Rds")))
  
  # employment status and income by month
  months <- seq(ymd(paste0(yr, '-01-01')), ymd(paste0(yr, '-12-01')), by='1 month') %>%
    tibble(month = .)
  
  empinc_yr <- rais_emp_yr %>%
    # expand to months
    merge(months, ., by = NULL) %>%
    # inc and emp by month
    mutate(emp = ifelse(month >= dt_start & (month < dt_end | is.na(dt_end)), 1, 0),
           inc = ifelse(emp == 1, inc_avg, 0),
           hrs_wk = ifelse(emp == 1, hrs_wk, 0)) %>%
    # aggregate by cpf
    group_by(cpfstd, month) %>%
    summarise(emp = ifelse(sum(emp, na.rm = T) > 0, 1, 0),
              inc = sum(inc, na.rm = T),
              hrs_wk = sum(hrs_wk, na.rm = T)) %>%
    ungroup() %>%
    dplyr::select(cpfstd, month, emp, inc, hrs_wk)
  
  saveRDS(empinc_yr, file = file.path(wd, "data", "intermediate", "rais", "all_cpfs", 
                                      "empinc", paste0("empinc_", yr, ".Rds")))
}

rais_cols <- list(
  "2007" = c("empem3112", "mesdeslig", "naturjur", "indceivinc", "tipoestbl", "dtadmissao", 
             "remdezr", "remdezembro", "remmedr", "remmedia", "tempempr", "ultrem", "salcontr", 
             "cpf", "identificad", "radiccnpj", "nome", "ocup2002", "qtdiasafas", "horascontr"),
  "2010" = c("EMP.EM.31.12", "MES.DESLIG", "NAT.JURIDICA", "IND.CEI.VINC", "TIPO.ESTBL", "DT.ADMISSAO", 
             "REM.DEZ..R..", "REM.DEZEMBRO", "REM.MED..R..", "REM.MEDIA", "TEMP.EMPR", "ULT.REM", "SAL.CONTR",
             "CPF", "IDENTIFICAD", "RADIC.CNPJ", "NOME", "OCUP.2002", "QT.DIAS.AFAS", "HORAS.CONTR"),
  "2011" = c("Vínculo.Ativo.31.12", "Mês.Desligamento", "Natureza.Jurídica", "Ind.CEI.Vinculado",
             "Tipo.Estab", "Data.Admissão.Declarada", 
             "Vl.Remun.Dezembro.Nom", "Vl.Remun.Dezembro..SM.", "Vl.Remun.Média.Nom", "Vl.Remun.Média..SM.",
             "Tempo.Emprego", "Vl.Última.Remuneração.Ano", "Vl.Salário.Contratual",
             "CPF", "CNPJ...CEI", "CNPJ.Raiz", "Nome.Trabalhador", "CBO.Ocupação.2002",
             "Qtd.Dias.Afastamento", "Qtd.Hora.Contr"))

rais_cols[c("2008", "2009")] <- rais_cols["2010"]
rais_cols[c("2012", "2013", "2014", "2015", "2016", "2017")] <- rais_cols["2011"]

rais_emp_yrs <- names(rais_cols)

# loop through RAIS years
# lapply(rais_emp_yrs, import_rais_emp,
#        rais_path = rais_path,
#        rais_cols = rais_cols)

#### combine years
# filter by participants
filter_part <- function(file, part_cpfs){
  print(file)
  file %>%
    readRDS() %>%
    inner_join(part_cpfs, by = "cpfstd")
}

# rais_emp
# rais_emp_files <- file.path(wd, "data", "intermediate", "rais", "all_cpfs", "rais_emp") %>%
#   list.files(full.names = T)
# 
# rais_emp <- do.call(rbind, lapply(rais_emp_files, 
#                                      filter_part,
#                                      part_cpfs = part_cpfs))
# 
# saveRDS(rais_emp, file = file.path(wd, "data", "intermediate", "rais", "participantes", 
#                                       "rais_emp.Rds"))

# empinc
min_wage <- file.path(wd, "data", "input", "rais", "sal_min.csv") %>% 
  read.csv(stringsAsFactors = F) %>% 
  mutate(dt = ymd(paste(year, month, "01", sep = "-"))) %>%
  dplyr::select(dt, MW)

empinc_files <- file.path(wd, "data", "intermediate", "rais", "all_cpfs", "empinc") %>%
  list.files(full.names = T)

empinc_part_n <- do.call(rbind, lapply(empinc_files, 
                                  filter_part,
                                  part_cpfs = part_cpfs)) 

empinc_part <- empinc_part_n %>% 
  inner_join(min_wage, by = c("month" = "dt")) %>% 
  mutate(r_inc = inc / MW) %>% 
  dplyr::select(-MW)

saveRDS(empinc_part, file = file.path(wd, "data", "intermediate", "rais", "participantes", 
                                   "empinc.Rds"))
