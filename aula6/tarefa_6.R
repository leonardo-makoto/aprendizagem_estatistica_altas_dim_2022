

library(here) # pacote para caminhos relativos
library(tree) # pacote para árvores de regressão
library(randomForest) # pacote para florestas aleatórias
here()
dados <- read.csv(here("aula2","sao-paulo-properties.csv"), fileEncoding = "UTF-8")
variaveis <- c("Price", "Condo","Size","Rooms","Toilets", "Suites","Parking",
               "Elevator","Furnished","Swimming.Pool","New",
               "Latitude","Longitude")

aluguel <- dados[dados$Negotiation.Type == "rent" & dados$Size <=200, variaveis]

set.seed(12345)

ids <- sample(nrow(aluguel), size = .75*nrow(aluguel), replace = FALSE)

# árvore
arvore <- tree(Price ~ . , data = aluguel[ids,])

arvore_pred_dentro <- predict(arvore, newdata = aluguel[ids,])


(arvore_eqm_treino <- mean((predict(arvore, newdata = aluguel[ids,]) - aluguel[ids,1])^2))
# 1673451


(arvore_pred_fora <-  mean((predict(arvore, newdata = aluguel[-ids,]) - aluguel[-ids,1])^2))
# 2114715


#random forest
# modelo floresta aleatória, considerando como 4 o número de variáveis aleatoriamente selecionadas como candidatas em cada split.
set.seed(1)
floresta <- randomForest(Price ~ ., data = aluguel[ids,], mtry = 4)

(floresta_eqm_treino <-  mean((predict(floresta, newdata = aluguel[ids,]) - aluguel[ids,1])^2))
# 221725.1
(floresta_eqm_fora <- mean((predict(floresta, newdata = aluguel[-ids,]) - aluguel[-ids,1])^2))
# 1207172


# bagging

set.seed(1)
bagging <- randomForest(Price ~ ., data = aluguel[ids,], mtry = ncol(aluguel)-1)


(bagging_eqm_treino <-  mean((predict(bagging, newdata = aluguel[ids,]) - aluguel[ids,1])^2))
# 167723.9
(bagging_eqm_fora <- mean((predict(bagging, newdata = aluguel[-ids,]) - aluguel[-ids,1])^2))
#  1157607

# estimando o melhor modelo em uma amostra nova
dados_2 <- read.csv(here("aula3","sao-paulo-properties-test.csv"), fileEncoding = "UTF-8")
variaveis <- c("Price", "Condo","Size","Rooms","Toilets", "Suites","Parking",
               "Elevator","Furnished","Swimming.Pool","New",
               "Latitude","Longitude")

aluguel_2 <- dados_2[dados_2$Negotiation.Type == "rent" & dados_2$Size <=200, variaveis]


bagging2 <- randomForest(Price ~ ., data = aluguel, mtry = ncol(aluguel)-1)
(bagging_eqm_dados_2<- mean((predict(bagging2, newdata = aluguel_2) - aluguel_2[,1])^2))
# 989786 usando só a amostra teste da primeira amostra
# 902644.8 usando a primeira amostra inteira
