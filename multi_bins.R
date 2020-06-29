library(dplyr) 
library(tidyr)
library(broom)
library(ggplot2) 

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

panel2 <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>% 
  readRDS() %>% 
  filter(months_since_avail == 24) %>%
  mutate(pot_displacement = cut(pot_displacement, 
                                breaks = c(0,30,60,90), 
                                right = FALSE),
         pot_dist_center = cut(pot_dist_center, 
                               breaks = c(25,40,55,70), 
                               right = FALSE),
         pot_murder_rate = cut(pot_murder_rate, 
                               breaks = c(16,24,32,40), 
                               right = FALSE),
         old_emp = factor(round(2 * old_emp) / 2,
                          levels = c(0, 0.5, 1),
                          labels = c("Unemployed", "Partially Employed", "Employed")),
         old_r_inc = cut(old_r_inc, 
                         breaks = c(0,1,2,4,16),
                         right = FALSE),
         gender = factor(feminino,
                         levels = c(0, 1),
                         labels = c("Male", "Female")),
         favela = factor(favela,
                         levels = c(0, 1),
                         labels = c("Formal", "Favela")),
         age = cut(idade, 
                   breaks = c(18,30,45,60,90),
                   right = FALSE),
         old_edu2 = ifelse(old_education == 4, 2, 1),
         old_edu2 = ifelse(old_education == 0, 0, old_edu2),
         old_edu2 = factor(old_edu2,
                           levels = c(0:2),
                           labels = c("Illiterate", "Some School",
                                      "University")),
         old_dist_center = cut(old_dist_center, 
                               breaks = c(0,20,40,60,80),
                               right = FALSE)) %>%
  dplyr::select(sorteado, edital, emp, r_inc, 
                pot_displacement, pot_dist_center, pot_murder_rate,
                old_emp, old_r_inc, gender, age, old_edu2, 
                old_dist_center, favela) %>% 
  drop_na(0)
  
panel_bins <- panel2 %>%
  mutate(bin = group_indices(., edital, old_r_inc, gender, pot_displacement)) %>%
  add_count(bin, sorteado) %>% 
  filter(n > 30) %>% 
  add_count(bin, name = "m") %>% 
  filter(m > n)

bin_vals <- panel_bins %>% 
  distinct(bin, .keep_all = TRUE) %>% 
  dplyr::select(bin, edital, old_r_inc, gender, pot_displacement)

t_test_stack <- function(data1, var){
  
  bins <- data1 %>% 
    distinct(bin) %>% 
    pull()
  
  t_formula <- as.formula(paste0(var, " ~ sorteado"))
  print(t_formula)
  
    for(i in seq(bins)){
      
      bin_i <- bins[i]
      print(bin_i)
      
      data2 <- data1 %>% 
        filter(bin == bin_i)
    
      counts <- data2 %>%
        count(sorteado)
      
      new_row <- t.test(t_formula, data = data2) %>% 
        tidy() %>% 
        mutate(n_loser = counts$n[1],
               n_winner = counts$n[2],
               bin = bin_i) %>% 
        rename(difference = estimate, loser = estimate1, winner = estimate2) %>%
        dplyr::select(bin, difference, loser, winner, 
                      statistic, p.value, n_loser, n_winner)
      
      if(i == 1){
        t_tests <- new_row
      } else {
        t_tests <- rbind(t_tests, new_row)
      }
  }
  t_tests
}

emp_t <- t_test_stack(panel_bins, "emp") %>% 
  filter(p.value < 0.05) %>%
  arrange(difference) %>% 
  inner_join(bin_vals, by = "bin")

r_inc_t <- t_test_stack(panel_bins, "r_inc") %>% 
  filter(p.value < 0.05) %>%
  arrange(difference) %>% 
  inner_join(bin_vals, by = "bin")


