
rm(list=ls())
gc()

library(tidyverse)
beta_zero <- 3.52
beta_um  <-  6.2
beta_dois <-  3.47

vetor_beta <- c(beta_zero, beta_um, beta_dois)

obs_teste <-  c(6.25,3.17,54.45)

(predicao_01 <- beta_zero + beta_um*obs_teste[1] + beta_dois*obs_teste[2])

(funcao_custo <- (obs_teste[3] - predicao_01 )^2)

beta_zero_linha <- -6.48
beta_um_linha <- 8.25
beta_dois_linha <- -4.56
beta_tres_linha <- 7.09
beta_quatro_linha <- 6.7
(obs_teste_linha <-  c(8.75,6.88,893.57))

(predicao_02 <- beta_zero_linha + beta_um_linha*obs_teste_linha[1] + beta_dois_linha*obs_teste_linha[2]+ beta_tres_linha*obs_teste_linha[1]^2 + beta_quatro_linha*obs_teste_linha[2]^2)


(funcao_custo_linha <- (obs_teste_linha[3] - predicao_02 )^2)

# formatC(numero, format = "f", digits = 3)

sao_paulo_properties <- read.csv("//wilbor/C/Lixo/leonardomakoto/doutorado_estatística/MAE59040501 - Aprendizagem Estatística em Altas Dimensões (2022)/aulas/aula2/sao-paulo-properties.csv")

db <- sao_paulo_properties %>% 
  filter(Negotiation.Type == "rent")

fit <- lm(Price ~ Condo + Size + Rooms + Toilets + Suites + Parking + Furnished, db)
sm <- summary(fit)

dados_predicao <- data.frame(t(c(Condo = 1000, Size = 55, Rooms = 2, Toilets = 1, Suites = 0, Parking = 1, Furnished = 0)))

(predicao <- predict(fit, dados_predicao))
formatC(1678.432, format = "f", digits = 3)
#1678.432 



db <- db %>% 
  mutate( predito =predict(fit, db))

(eqm_amostra <- sum((db$Price - db$predito)^2) / count(db))
(eqm_amostra_dois <- mean((db$observado - db$predito)^2))


obs_pred <- data.frame(observado = db$Price,
                       predito = db$predito)


# calulo do erro dentro da amostra - opcao 1
(eqm <- sum((obs_pred$observado - obs_pred$predito)^2) / count(db))

# calulo do erro dentro da amostar - opcao 2
(eqm <- mean((obs_pred$observado - obs_pred$predito)^2))

formatC((mean(sm$residuals^2)), format = "f", digits = 3)

fit_2 <- lm(Price ~ Condo + Size + Rooms + I(Rooms^2) + Toilets + Suites + I(Suites^2) + I(Suites^3) + Parking + Furnished, db)
sm_2 <- summary(fit_2)

formatC((mean(sm_2$residuals^2)), format = "f", digits = 3)

db <- db %>% 
  mutate( predito_2 =predict(fit_2, db))

(eqm_dois_amostra <- sum((db$Price - db$predito_2)^2) / count(db))

(eqm_dois_amostra_dois <- mean((db$observado - db$predito_2)^2))