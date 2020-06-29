library(dplyr)
library(AER) # two-stage regression
library(estimatr) # robust SE
options(scipen=999)

### regression functions ####
# ordinary least squares
ols1s <- function(panel, y, in_vars, ex_vars, robust = FALSE) {
  
  x <- c(in_vars, ex_vars)
  
  if (robust) {
    lm_robust(reformulate(x, y), data = panel, 
              clusters = edital, alpha = 0.1, 
              return_vcov = FALSE)
  } else {
    lm(reformulate(x, y), data = panel)
  }
  
}

# logistic for binary y
log1s <- function(panel, y, in_vars, ex_vars) {
  
  x <- c(in_vars, ex_vars)
  glm(reformulate(x, y), data = panel, family = binomial(link = 'logit'))
}

# probit for binary y
prb1s <- function(panel, y, in_vars, ex_vars) {
  
  x <- c(in_vars, ex_vars)
  glm(reformulate(x, y), data = panel, family = binomial(link = 'probit'))
}

# two-stage least squares: first stage
stage1 <- function(panel, en_vars, in_vars, ex_vars) {
  
  x <- c(in_vars, ex_vars)
  fit_list <- vector("list", length(en_vars))
  
  for(i in seq_along(en_vars)){
    
    y <- en_vars[i]
    formula_1s <- paste(y, "~", paste(x, collapse = " + "))
    print(formula_1s)
    # use logit for binary y
    if (length(unique(panel[[y]])) == 2) { 
      fit_list[[i]] <- glm(formula_1s, data = panel, family = binomial(link = 'logit'))
    } else { 
      fit_list[[i]] <- lm(formula_1s, data = panel)
    }
    
  }
  fit_list
}

# two-stage least squares
ols2s <- function(panel, y, en_vars, in_vars, ex_vars, robust = FALSE) {
  
  x1 <- c(in_vars, ex_vars)
  x2 <- c(en_vars, ex_vars)
  
  formula_2s <- paste(y, "~", 
                      paste(x2, collapse = " + "), "|", 
                      paste(x1, collapse = " + "))
  
  if (robust) {
    iv_robust(formula_2s, data = panel, 
              clusters = edital, alpha = 0.1, 
              return_vcov = FALSE)
  } else {
    ivreg(formula_2s, data = panel)
  }
}

# two-stage logistic
log2s <- function(panel, y, en_vars, in_vars, ex_vars) {
  
  # stage 1
  x1 <- c(in_vars, ex_vars)
  en_vars_hat <- vector()
  for (en in en_vars) {
    en_hat <- paste0(en, "_hat")
    panel[[en_hat]] <- lm(reformulate(x1, en), data = panel) %>%
      predict()
    en_vars_hat <- c(en_vars_hat, en_hat)
  }
  
  # two-stage logistic
  x2 <- c(en_vars_hat, ex_vars)
  glm(reformulate(x2, y), data = panel, family = binomial(link = 'logit'))
  
}

# two-stage probit
prb2s <- function(panel, y, en_vars, in_vars, ex_vars) {
  
  # stage 1
  x1 <- c(in_vars, ex_vars)
  en_vars_hat <- vector()
  for (en in en_vars) {
    en_hat <- paste0(en, "_hat")
    panel[[en_hat]] <- lm(reformulate(x1, en), data = panel) %>%
      predict()
    en_vars_hat <- c(en_vars_hat, en_hat)
  }
  
  # two-stage logistic
  x2 <- c(en_vars_hat, ex_vars)
  glm(reformulate(x2, y), data = panel, family = binomial(link = 'probit'))
}
