
library(here) # pacote para caminhos relativos
library(leaps) # tem as funcoes de selecao de variaveis
library(glmnet) # tem as funcoes para lasso, ridge e elasticnet
library(ISLR) # tem o conjunto de dados Credit
library(ggplot2) # funcoes para criacao de graficos
library(plotmo) # funcoes para criar graficos com objetos do glmnet

here()
dados <- read.csv(here("aula2","sao-paulo-properties.csv"), fileEncoding = "UTF-8")

variaveis <- c("Price", "Condo","Size","Rooms","Toilets", "Suites","Parking",
               "Elevator","Furnished","Swimming.Pool","New",
               "Latitude","Longitude")

aluguel <- dados[dados$Negotiation.Type == "rent" & dados$Size <=200, variaveis]

set.seed(12345)
ids <- sample(nrow(aluguel), size = .75*nrow(aluguel), replace = FALSE)
resultados <- data.frame(modelo = c("best", "forward", "backward",
                                    "lasso", "ridge", "elastic-net"),
                         erro_dentro = NA,
                         erro_fora = NA)

# best subset (bs)
bs <-regsubsets(Price ~ ., # forumula
                data = aluguel[ids,], # dados
                nvmax = ncol(aluguel), # numero max de variaveis
)

bs_sum <- summary(bs)

ggplot(data.frame(bic=bs_sum$bic, n=1:12), aes(n, bic)) +
  geom_point() +
  geom_vline(xintercept = which.min(bs_sum$bic), col="red") +
  scale_x_continuous(breaks=1:12) +
  labs(title = "BIC - best subset", x = "Número de variáveis",
       y = "BIC")

num_coef <- which.min(bs_sum$bic) # numero de coeficientes escolhidos
bs_coef <- coef(bs, num_coef) # valores dos coeficientes estimados
# calculo do erro estimado dentro e fora da amostra
dados_matriz <- model.matrix(Price ~ ., data=aluguel)
bs_erro_dentro <- mean((dados_matriz[ids, names(bs_coef)] %*% bs_coef -
                          aluguel$Price[ids])^2)
bs_erro_fora <- mean((dados_matriz[-ids, names(bs_coef)] %*% bs_coef -
                        aluguel$Price[-ids])^2)

resultados[which(resultados$modelo=="best"), 2:3] = c(bs_erro_dentro, bs_erro_fora)
resultados


# forward (fw)
fw <- regsubsets(Price ~ ., # forumula
                 data = aluguel[ids,], # dados
                 nvmax = ncol(aluguel), # numero max de variaveis
                 method = "forward") # metodo

fw_sum <- summary(fw)

ggplot(data.frame(bic=fw_sum$bic, n=1:12), aes(n, bic)) +
  geom_point() +
  geom_vline(xintercept = which.min(fw_sum$bic), col="red") +
  scale_x_continuous(breaks=1:12) +
  labs(title = "BIC - forward", x = "Número de variáveis",
       y = "BIC")


num_coef <- which.min(fw_sum$bic) # numero de coeficientes escolhidos
fw_coef <- coef(fw, num_coef) # valores dos coeficientes estimados
# calculo do erro estimado dentro e fora da amostra
dados_matriz <- model.matrix(Price ~ ., data=aluguel)
fw_erro_dentro <- mean((dados_matriz[ids, names(fw_coef)] %*% fw_coef -
                          aluguel$Price[ids])^2)
fw_erro_fora <- mean((dados_matriz[-ids, names(fw_coef)] %*% fw_coef -
                        aluguel$Price[-ids])^2)

resultados[which(resultados$modelo=="forward"), 2:3] = c(fw_erro_dentro, fw_erro_fora)
resultados


# regularizacao -----------------------------------------------------------

# preparacao de variaveis para os modelos usando glmnet

X <- model.matrix(Price ~ .,
                  data = aluguel)[,-1] # X deve ser uma matrix

class(X)

View(X)

y <- aluguel$Price

# LASSO -------------------------------------------------------------------

lasso <- glmnet(X[ids,], y[ids], alpha = 1, nlambda = 500)

plot_glmnet(lasso, lwd = 2, cex.lab = 1.3, xvar = "lambda")

# LASSO - validação cruzada
# set.seed(1)
# cv_lasso <- cv.glmnet(X[ids,], y[ids], alpha = 1)
set.seed(12345)
cv_lasso <- cv.glmnet(X[ids,], aluguel$Price[ids], alpha = 1,
                      lambda = c(0.01, 0.1, 1, 2, 10, 100))
plot(cv_lasso)
cv_lasso$lambda.min
y_lasso_dentro <- predict(cv_lasso, newx = X[ids,],
                          s = cv_lasso$lambda.1se) # valor predito dentro da amostra

y_lasso_fora <- predict(cv_lasso, newx = X[-ids,],
                        s = cv_lasso$lambda.1se) # valor predito fora da amostra

lasso_erro_dentro <- mean((y_lasso_dentro - y[ids])^2)

lasso_erro_fora <- mean((y_lasso_fora - y[-ids])^2)

resultados[which(resultados$modelo=="lasso"), 2:3] = c(lasso_erro_dentro, lasso_erro_fora)
resultados



# comparativo final -------------------------------------------------------

resultados[order(resultados$erro_fora),]

