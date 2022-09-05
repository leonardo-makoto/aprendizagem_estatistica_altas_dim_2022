library(tidyverse)
library(purrr)

rm(list=ls())
gc()
set.seed(448)
ma <- matrix(rnorm(100*50), ncol=50)

(mean(ma[1,]))
(var(ma[1,]))

df_ma <- data.frame(ma) %>% 
  mutate(media_estimada = rowMeans(select(., starts_with("X")), na.rm = TRUE)) %>% 
  relocate(media_estimada, .before = X1) %>% 
  mutate(across(( starts_with("X")), ~abs(.x - media_estimada), .names = "{col}_desvio") ) %>% 
  mutate(theta_chapeu = rowMeans(select(., ends_with("desvio")), na.rm = TRUE)) %>% 
  relocate(theta_chapeu, .before = X1) %>% 
  mutate(theta = sqrt(2/pi),
         vies_theta_chapeu = mean(theta_chapeu) - theta,
         var_theta_chapeu = var(theta_chapeu),
         eqm_theta_chapeu = var_theta_chapeu + var_theta_chapeu^2
         
         ) %>% 
  relocate(theta, vies_theta_chapeu, var_theta_chapeu, eqm_theta_chapeu)
  

theta <- sqrt(2/pi)
vies <- mean(df_ma$theta_chapeu) - theta
var_theta_chapeu <- var(df_ma$theta_chapeu)
eqm_theta_chapeu <- var_theta_chapeu + vies^2