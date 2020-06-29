library(tidyverse)
library(broom)
library(purrr)

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

### IMPORT CADUNICO ####
import_cadunico <- function(yr, filepath) {
  
  print(yr)
  
  filepath %>%
    readRDS() %>%
    dplyr::select(cpfstd, co_familiar_fam, dt_cadastramento_memb, co_sexo_pessoa,
                  dt_nasc_pessoa, co_raca_cor_pessoa, co_sabe_ler_escrever_memb,
                  co_curso_freq_pessoa_memb, dt_cadastro_fam, dt_alteracao_fam,
                  vl_renda_media_fam, nu_cep_logradouro_fam, co_local_domic_fam,
                  co_especie_domic_fam, qt_comodos_domic_fam, co_material_piso_fam,
                  co_material_domic_fam, co_agua_canalizada_fam, co_abaste_agua_domic_fam,
                  co_banheiro_domic_fam, co_escoa_sanitario_domic_fam, 
                  co_destino_lixo_domic_fam, co_iluminacao_domic_fam, 
                  co_calcamento_domic_fam, qt_pessoas_domic_fam, qt_familias_domic_fam,
                  vl_desp_energia_fam, vl_desp_agua_esgoto_fam, vl_desp_gas_fam,
                  vl_desp_alimentacao_fam, vl_desp_transpor_fam, vl_desp_aluguel_fam,
                  marc_pbf) %>%
    mutate(cadunico_ano = yr,
           ano_cadastro = as.numeric(substr(dt_cadastramento_memb, 1, 4)),
           feminino = ifelse(co_sexo_pessoa == 1, 0, 1),
           idade = cadunico_ano - as.numeric(substr(dt_nasc_pessoa, 1, 4)),
           idade = ifelse(idade > 90, 90, idade),
           cor_branca = ifelse(as.numeric(co_raca_cor_pessoa) == 1, 1, 0),
           cor_negra = ifelse(as.numeric(co_raca_cor_pessoa) == 2, 1, 0),
           cor_parda = ifelse(as.numeric(co_raca_cor_pessoa) == 4, 1, 0),
           cor_outras = ifelse(as.numeric(co_raca_cor_pessoa) == 3 | as.numeric(co_raca_cor_pessoa) == 5, 1, 0),
           analfabeto = ifelse(as.numeric(co_sabe_ler_escrever_memb) == 1, 0, 1),
           fundamental_completo = ifelse(as.numeric(co_curso_freq_pessoa_memb) >= 5, 1, 0),
           medio_completo = ifelse(as.numeric(co_curso_freq_pessoa_memb) >= 8, 1, 0),
           superior_completo = ifelse(as.numeric(co_curso_freq_pessoa_memb) == 13, 1, 0),
           renda_familia = as.numeric(vl_renda_media_fam),
           cep = as.numeric(nu_cep_logradouro_fam),
           familia_urbana = ifelse(as.numeric(co_local_domic_fam) == 1, 1, 0),
           domicilio_part_perm = ifelse(as.numeric(co_especie_domic_fam) == 1, 1, 0),
           comodos = as.numeric(qt_comodos_domic_fam),
           piso_improvisado = ifelse(as.numeric(co_material_piso_fam) <= 3 | as.numeric(co_material_piso_fam) == 7, 1, 0),
           material_improvisado = ifelse(as.numeric(co_material_domic_fam) !=1 & 
                                           as.numeric(co_material_domic_fam) !=3, 1, 0),
           agua_encanada = ifelse(as.numeric(co_agua_canalizada_fam) == 1, 1, 0),
           abastecimento_imiprovisado = ifelse(as.numeric(co_abaste_agua_domic_fam) != 1, 1, 0),
           possui_banheiro = ifelse(as.numeric(co_banheiro_domic_fam) == 1, 1, 0),
           esgoto_improvisado = ifelse(as.numeric(co_escoa_sanitario_domic_fam) > 2, 1, 0),
           lixo_improvisado = ifelse(as.numeric(co_destino_lixo_domic_fam) >2 , 1, 0),
           iluminacao_improvisada = ifelse(as.numeric(co_iluminacao_domic_fam) > 2, 1, 0),
           calcamento_improvisado = ifelse(as.numeric(co_calcamento_domic_fam) >=2, 1, 0),
           pessoas_por_domic = as.numeric(qt_pessoas_domic_fam),
           familias_por_domic = as.numeric(qt_familias_domic_fam), 
           despesa_energia = as.numeric(vl_desp_energia_fam),
           despesa_agua_esgoto = as.numeric(vl_desp_agua_esgoto_fam),
           despesa_gas = as.numeric(vl_desp_gas_fam),
           despesa_alimentacao = as.numeric(vl_desp_alimentacao_fam),
           despesa_transporte = as.numeric(vl_desp_transpor_fam),
           despesa_aluguel = as.numeric(vl_desp_aluguel_fam),
           bolsa_familia = as.numeric(marc_pbf),
           contador = 1,
           aparece = ifelse(is.na(idade), 0, 1),
           cadastro_antes_2011 = ifelse(ano_cadastro <= 2011, 1, 0)) %>%
    group_by(co_familiar_fam) %>%
    mutate(pessoas_familia = sum(contador)) %>%
    ungroup() %>%
    dplyr::select(cpfstd, co_familiar_fam, ano_cadastro, feminino, idade, cor_branca, cor_negra, cor_parda,
                  cor_outras, analfabeto, fundamental_completo, medio_completo, superior_completo,
                  renda_familia, cep, familia_urbana, domicilio_part_perm, comodos, piso_improvisado,
                  material_improvisado, agua_encanada, abastecimento_imiprovisado, possui_banheiro,
                  esgoto_improvisado, lixo_improvisado, iluminacao_improvisada, calcamento_improvisado,
                  pessoas_por_domic, familias_por_domic, despesa_energia, despesa_agua_esgoto,
                  despesa_gas, despesa_alimentacao, despesa_transporte, despesa_aluguel, bolsa_familia,
                  cadunico_ano, contador, aparece, cadastro_antes_2011)
}

cadunico_yrs <- 2012:2018
cad_completo_dir <- file.path(wd, "data", "input", "cadunico", "completo")
cad_completo_paths <- lapply(cadunico_yrs, function(yr) file.path(cad_completo_dir, paste0("completo_", yr, ".Rds")))

cadunico_std <- do.call(rbind, pmap(list(cadunico_yrs, cad_completo_paths), import_cadunico)) %>%
  filter(!duplicated(cpfstd), !is.na(cpfstd))

saveRDS(cadunico_std, file = file.path(wd, "data", "intermediate","cadunico_std.Rds"))

### JOIN PARTICIPANTES X CADUNICO ####
participantes <- readRDS(file.path(wd, "data", "intermediate", "participantes.Rds"))
cadunico_std <- readRDS(file.path(wd, "data", "intermediate","cadunico_std.Rds"))

participantes_cadunico <- participantes %>%
  inner_join(cadunico_std, by = "cpfstd") %>% # 1.978.968 registros
  group_by(cpfstd) %>%
  mutate(analfabeto = round(mean(analfabeto, na.rm = TRUE)),
         feminino = round(mean(feminino, na.rm = TRUE)),
         fundamental_completo = round(mean(fundamental_completo, na.rm = TRUE)),
         medio_completo = round(mean(medio_completo, na.rm = TRUE)),
         superior_completo = round(mean(superior_completo, na.rm = TRUE)),
         cor_branca = round(mean(cor_branca, na.rm = TRUE)),
         cor_negra = round(mean(cor_negra, na.rm = TRUE)),
         cor_parda = round(mean(cor_parda, na.rm = TRUE)),
         cor_outras = round(mean(cor_outras, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(feminino, 
                 fundamental_completo, 
                 medio_completo,
                 superior_completo,
                 cor_branca,
                 cor_negra,
                 cor_parda,
                 cor_outras,
                 idade,
                 analfabeto), 
            ~ replace(., which(.=="NaN"), 0))

saveRDS(participantes_cadunico, file = file.path(wd, "data", "intermediate", "participantes_cadunico.Rds"))
#----------------------------------
