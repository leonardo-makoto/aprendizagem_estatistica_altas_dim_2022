

# tarefa 3

library(tidyverse)

# sao_paulo_properties <- read.csv("//wilbor/C/Lixo/leonardomakoto/doutorado_estatística/MAE59040501 - Aprendizagem Estatística em Altas Dimensões (2022)/aulas/aula2/sao-paulo-properties.csv")
dados <- read.csv("//wilbor/C/Lixo/leonardomakoto/doutorado_estatística/MAE59040501 - Aprendizagem Estatística em Altas Dimensões (2022)/aulas/aula2/sao-paulo-properties.csv", fileEncoding = "UTF-8")
aluguel <- dados[dados$Negotiation.Type == "rent", ]
set.seed(1)
lote <- sample(1:5, size = nrow(aluguel), replace = TRUE)

fit_2 <- lm(Price ~ Condo + Size + Rooms + I(Rooms^2) + Toilets + Suites + I(Suites^2) + I(Suites^3) + Parking + Furnished, db)
