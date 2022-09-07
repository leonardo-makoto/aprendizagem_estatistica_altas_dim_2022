

# tarefa 3

library(tidyverse) # pacote para manipulação das bases
library(here)
library(ggplot2)
options(scipen = 999) # forçando números em notação não científica

# sao_paulo_properties <- read.csv("//wilbor/C/Lixo/leonardomakoto/doutorado_estatística/MAE59040501 - Aprendizagem Estatística em Altas Dimensões (2022)/aulas/aula2/sao-paulo-properties.csv")
dados <- read.csv(here("aula2","sao-paulo-properties.csv"), fileEncoding = "UTF-8")
aluguel <- dados[dados$Negotiation.Type == "rent", ]
set.seed(1)


# modelos lineares
fit_1 <- lm(Price ~ Condo + Size + Rooms +Toilets + Suites + Parking + Furnished, aluguel)
fit_2 <- lm(Price ~ Condo + Size + Rooms + I(Rooms^2) + Toilets + Suites + I(Suites^2) + I(Suites^3) + Parking + Furnished, aluguel)
fit_3 <- lm(Price ~ Condo + Size + log(Size) + Rooms + Toilets + Suites + Parking + Furnished, aluguel)

# previsões

aluguel <- aluguel %>%
  mutate(fit_1_predito = predict(fit_1, aluguel),
         fit_2_predito = predict(fit_2, aluguel),
         fit_3_predito = predict(fit_3, aluguel))

(erro_1_dentro <- mean((predict(fit_1, aluguel) - aluguel$Price)^2))
(erro_2_dentro <- mean((predict(fit_2, aluguel) - aluguel$Price)^2))

(erro_3_dentro <- mean((predict(fit_3, aluguel) - aluguel$Price)^2))

# modelo linear - treino e validação

# conjunto de treinamento
ids_treino <- sample(1:nrow(aluguel),
                     size = 0.80 * nrow(aluguel),
                     replace = FALSE)

treino_tv <- aluguel[ids_treino, ] # dados de treino
teste_tv <- aluguel[-ids_treino, ] # dados de teste

fit_1_tv <- lm(Price ~ Condo + Size + Rooms +Toilets + Suites + Parking + Furnished, treino_tv)
fit_2_tv <- lm(Price ~ Condo + Size + Rooms + I(Rooms^2) + Toilets + Suites + I(Suites^2) + I(Suites^3) + Parking + Furnished, treino_tv)
fit_3_tv <- lm(Price ~ Condo + Size + log(Size) + Rooms + Toilets + Suites + Parking + Furnished, treino_tv)

# erro dentro da amostra
(erro_1_dentro_tv <- mean((predict(fit_1_tv, treino_tv) - treino_tv$Price)^2))
(erro_2_dentro_tv <- mean((predict(fit_2_tv, treino_tv) - treino_tv$Price)^2))
(erro_3_dentro_tv <- mean((predict(fit_3_tv, treino_tv) - treino_tv$Price)^2))

# erro fora da amostra
(erro_1_fora_tv <- mean((predict(fit_1_tv, teste_tv) - teste_tv$Price)^2))
(erro_2_fora_tv <- mean((predict(fit_2_tv, teste_tv) - teste_tv$Price)^2))
(erro_3_fora_tv <- mean((predict(fit_3_tv, teste_tv) - teste_tv$Price)^2))


# validação cruzada
lote <- sample(1:5, size = nrow(aluguel), replace = TRUE)

table(lote) # numero de observacoes de cada lote
k <- 5 # numero de lotes na valicadao cruzada
erro_tmp <-  numeric(k) #vetor que vai guardar estimativa dos erros de cada lote
erro_1_tmp <- erro_tmp
erro_2_tmp <- erro_tmp
erro_3_tmp <- erro_tmp



for(i in 1:k){
  treino_vc <- aluguel[lote != i,] # dados de treino
  teste_vc <- aluguel[lote == i,] # dados de teste

  fit_1_vc <- lm(Price ~ Condo + Size + Rooms +Toilets + Suites + Parking + Furnished, treino_vc)
  fit_2_vc <- lm(Price ~ Condo + Size + Rooms + I(Rooms^2) + Toilets + Suites + I(Suites^2) + I(Suites^3) + Parking + Furnished, treino_vc)
  fit_3_vc <- lm(Price ~ Condo + Size + log(Size) + Rooms + Toilets + Suites + Parking + Furnished, treino_vc)

  # erro fora da amostra para o lote i
  erro_1_tmp[i] <- mean((predict(fit_1_vc, teste_vc) - teste_vc$Price)^2)
  erro_2_tmp[i] <- mean((predict(fit_2_vc, teste_vc) - teste_vc$Price)^2)
  erro_3_tmp[i] <- mean((predict(fit_3_vc, teste_vc) - teste_vc$Price)^2)

  }


(erro_vc_1 <- mean(erro_1_tmp))
(erro_vc_2 <- mean(erro_2_tmp))
(erro_vc_3 <- mean(erro_3_tmp))




# com um novo conjunto de amostra

dados_2 <- read.csv(here("aula3","sao-paulo-properties-test.csv"), fileEncoding = "UTF-8")
aluguel_2 <- dados_2[dados_2$Negotiation.Type == "rent", ]


(erro_teste_2 <- mean((predict(fit_1, aluguel_2) - aluguel_2$Price))^2)
#
# # validação cruzada
#
# lote <- sample(1:5, size = nrow(aluguel_2), replace = TRUE)
# table(lote) # numero de observacoes de cada lote
# k <- 5 # numero de lotes na valicadao cruzada
# erro_tmp_2 <-  numeric(k) #vetor que vai guardar estimativa dos erros de cada lote
#
#
#
# for(i in 1:k){
#   treino_vc_2 <- aluguel[lote != i,] # dados de treino
#   teste_vc_2 <- aluguel[lote == i,] # dados de teste
#
#   fit_1_vc_2 <- lm(Price ~ Condo + Size + Rooms +Toilets + Suites + Parking + Furnished, treino_vc_2)
#
#   # erro fora da amostra para o lote i
#   erro_tmp_2[i] <- mean((predict(fit_1_vc_2, teste_vc_2) - teste_vc_2$Price)^2)
#
#
# }
#
# (erro_vc_2_2 <- mean(erro_tmp_2))
