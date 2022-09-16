# bibliotecas usadas na analise ----------------------------------------

library(leaps) # tem as funcoes de selecao de variaveis
library(glmnet) # tem as funcoes para lasso, ridge e elasticnet
library(ISLR) # tem o conjunto de dados Credit
library(ggplot2) # funcoes para criacao de graficos
library(plotmo) # funcoes para criar graficos com objetos do glmnet


# --------- Parte 1 -------------------------------------------------------
# estimamdo diferentes tipos de erro

# funcao que gera dados para simulacao ------------------------------------

f <- function(x){
  return (45 * tanh(x/1.9 - 7) + 60)
}

gera_salarios <- function(anos_estudo, media_erro = 0, desvio_erro = 4){
  return (f(anos_estudo) + rnorm(n = length(anos_estudo),
                                 mean = media_erro, sd = desvio_erro))
}

gera_salarios(1)
gera_salarios(1)
gera_salarios(5:10)

# -------------------------------------------------------------------------
# Modelo linear -----------------------------------------------------------
set.seed(1) # semente aleatoria

n_obs <- 100 # numero de observacoes que serao geradas
anos_estudo <- runif(n = n_obs,
                     min = 8, max = 18) # valores para anos de estudo
salario <- gera_salarios(anos_estudo) # valores para salario

# cria o dataframe com todos os dados
dados <- data.frame(x = anos_estudo, y = salario)

fit1 <- lm(y ~ x, dados) # ajuste do modelo

summary(fit1) # sumario das estimativas do modelo

# grafico com resultados
ggplot(data = dados, aes(x, y)) +
  geom_point() + # dados usados pelo modelo
  geom_abline(intercept = fit1$coefficients[1],
              slope = fit1$coefficients[2], color = "red")

# erro dentro da amostra
(erro_dentro <- mean((predict(fit1, dados) - dados$y)^2))

# -------------------------------------------------------------------------
# Modelo linear - treino e validação --------------------------------------
set.seed(123)

# seleciona aleatoriamente 80% dos dados para construir o
# conjunto de treinamento
ids_treino <- sample(1:nrow(dados),
                     size = 0.80 * nrow(dados),
                     replace = FALSE)

treino <- dados[ids_treino, ] # dados de treino
teste <- dados[-ids_treino, ] # dados de teste

fit2 <- lm(y ~ x, treino) # ajuste do modelo linear

summary(fit2) # sumario das estimativas do modelo

# erro dentro da amostra
(erro_dentro2 <- mean((predict(fit2, treino) - treino$y)^2))
# erro fora dentro da amostra
(erro_fora2 <- mean((predict(fit2, teste) - teste$y)^2))

# -------------------------------------------------------------------------
# Modelo linear - validacao cruzada ---------------------------------------
set.seed(1)

k <- 5 # numero de lotes na valicadao cruzada

# definicao dos lotes
lote <- sample(1:k, size = nrow(dados), replace = TRUE)

table(lote) # numero de observacoes em cada lote

erro_tmp <- numeric(k) # vetor que vai guardar estimativa dos
# erros em cada lote

# inicio da validacao cruzada
for(i in 1:k){
  treino <- dados[lote != i,] # dados de treino
  teste <- dados[lote == i,] # dados de teste

  fit_vc <- lm(y ~ x, treino) # estimacao do modelo

  # erro fora da amostra para o lote i
  erro_tmp[i] <- mean((predict(fit_vc, teste) - teste$y)^2)
}
# fim da validacao cruzada

# erro fora da amostra estimado pela validacao cruazada
(erro_vc <- mean(erro_tmp))


# Comparativo -------------------------------------------------------------
# comparativo do erro estimado em diferentes cenarios
data.frame(erro = c("Dentro da amostra",
                    "Separação treino e validação",
                    "Validação cruzada"),
           valor = c(erro_dentro, erro_fora2, erro_vc))


# --------- Parte 2 -------------------------------------------------------
# métodos de seleção de variáveis e regularização
# carregar e visualizar dados ------------------------------------------

data(Credit) # carrega os dados

?Credit

dim(Credit) # numero de linhas e colunas

View(Credit) # visualizar como planilha

# grafico de dispersao Income e Balance
ggplot(Credit, aes(x=Income, y=Balance))+
  geom_point()

# grafico de dispersao Income e Balance, colorindo pela coluna Married
ggplot(Credit, aes(x=Income, y=Balance, color=Married))+
  geom_point()

# TAREFA: completar a análise descritiva para compreender melhor os dados

# separacao dos dados em treino e teste -----------------------------------
set.seed(321)

ids <- sample(nrow(Credit),
              size = .75*nrow(Credit),
              replace = FALSE) # indice treinamento


# dataframe para armazenar resultados
resultados <- data.frame(modelo = c("best", "forward", "backward",
                                    "lasso", "ridge", "elastic-net"),
                         erro_dentro = NA,
                         erro_fora = NA)

resultados


# selecao de variaveis --------------------------------------------------

# ---- selecao progressiva (forward stepwise) -----

fw <- regsubsets(Balance ~ ., # forumula
                 data = Credit[ids,-1], # dados
                 nvmax = ncol(Credit)-2, # numero max de variaveis
                 method = "forward") # metodo

fw_sum <- summary(fw)

ggplot(data.frame(bic=fw_sum$bic, n=1:10), aes(n, bic)) +
  geom_point() +
  geom_vline(xintercept = which.min(fw_sum$bic), col="red") +
  scale_x_continuous(breaks=1:10) +
  labs(title = "BIC - forward", x = "Número de variáveis",
       y = "BIC")

num_coef <- which.min(fw_sum$bic) # numero de coeficientes escolhidos
fw_coef <- coef(fw, num_coef) # valores dos coeficientes estimados

# calculo do erro estimado dentro e fora da amostra
dados_matriz <- model.matrix(Balance ~ ., data=Credit[,-1])
fw_erro_dentro <- mean((dados_matriz[ids, names(fw_coef)] %*% fw_coef -
                          Credit$Balance[ids])^2)
fw_erro_fora <- mean((dados_matriz[-ids, names(fw_coef)] %*% fw_coef -
                        Credit$Balance[-ids])^2)

resultados[which(resultados$modelo=="forward"), 2:3] = c(fw_erro_dentro, fw_erro_fora)
resultados

# Tarefa: trocar o método na função regsubsets para fazer seleção
# de variáveis usando o melhor subconjunto e a seleção regressiva
# ?regsubsets


# regularizacao -----------------------------------------------------------

# preparacao de variaveis para os modelos usando glmnet

X <- model.matrix(Balance ~ .,
                  data = Credit[,-1])[,-1] # X deve ser uma matrix

class(X)

View(X)

y <- Credit$Balance


# LASSO -------------------------------------------------------------------

lasso <- glmnet(X[ids,], y[ids], alpha = 1, nlambda = 500)

plot_glmnet(lasso, lwd = 2, cex.lab = 1.3, xvar = "lambda")

# LASSO - validação cruzada
set.seed(1)
cv_lasso <- cv.glmnet(X[ids,], y[ids], alpha = 1)

plot(cv_lasso, cex.lab = 1.3)

y_lasso_dentro <- predict(cv_lasso, newx = X[ids,],
                          s = cv_lasso$lambda.1se) # valor predito dentro da amostra

y_lasso_fora <- predict(cv_lasso, newx = X[-ids,],
                        s = cv_lasso$lambda.1se) # valor predito fora da amostra

lasso_erro_dentro <- mean((y_lasso_dentro - y[ids])^2)

lasso_erro_fora <- mean((y_lasso_fora - y[-ids])^2)

resultados[which(resultados$modelo=="lasso"), 2:3] = c(lasso_erro_dentro, lasso_erro_fora)
resultados

# Tarefa: usar a função glmnet para ajustar o modelo ridge (alpha=0) e
# tambem elastic-net com lambda=0.5

# comparativo final -------------------------------------------------------

resultados[order(resultados$erro_fora),]

