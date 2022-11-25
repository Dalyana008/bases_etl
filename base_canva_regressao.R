# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, rattle, tidyverse)

update.packages(oldPkgs = c("bookdown", "rmarkdown"))

install.packages("scales")
install.packages("rlang")

install.packages("caret")

install.packages("ggplot2")
install.packages("tiyverse")

install.packages("Rcpp")
# leitura da base de dados

install.packages("tidyverse", lib = "/usr/lib/R/library")

library(caret)

acidentes_Recife <- read.csv2 ('http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2caa8f41-ccd9-4ea5-906d-f66017d6e107/download/acidentes2021.csv', sep = ';', encoding = 'UTF-8',
                               stringsAsFactors = T)
#vizualizando a base de dados 

View(acidentes_Recife)


#dumies
acidentes_Recife_D <- acm.disjonctif(as.data.frame(acidentes_Recife$natureza_acidente))
names(acidentes_Recife_D) <- c ('Com vitima', 'sem vitima')

acidentes_Recife<- cbind(acidentes_Recife, acidentes_Recife_D)

# AED 
status(acidentes_Recife) # explorar a qualidade das variáveis
freq(acidentes_Recife) # explorar os fatores
plot_num(acidentes_Recife) # exploração das variáveis numéricas
profiling_num(acidentes_Recife) # estatísticas das variáveis numéricas


# Treino e Teste: Pre - processamento
particaoACIDENTES = createDataPartition(acidentes_Recife$velocidade_max_via, p=.7, list =F) # cria a particao 70-30

treinoACIDENTE = acidentes_Recife[particaoACIDENTES, ] # treino
testeACIDENTE = acidentes_Recife[-particaoACIDENTES, ] # - treino = teste

# Valida??o Cruzada: Pr?-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Treinamentos
# Maquina de Vetor de Suporte (SVM)
acidente_svm <- train(velociade_max_via ~ Com vítima + sem vítima + auto + caminhao + moto + condicao_via, data = treinoACIDENTE, method = "svmLinear", trControl = train.control)


# Regressão Linear
acidentes_Recife_lm <- train(velocidade_max_via ~ com vitima + sem vitima + Com vítima + sem vítima + auto + caminhao + moto , data = treinoACIDENTE, method = "lm", trControl = train.control)

summary(acidentes_Recife_lm) # sumário do modelo linear
plot(varImp(acidentes_Recife_lm))



## Arvore de Decisão
acidente_RPART <- train(velocidade_max_via ~ com vitima + sem vitima + auto  + caminhao + condicao_via, data = treinoACIDENTE, method = "rpart", trControl = train.control)

summary(acidente_RPART)
fancyRpartPlot(acidente_RPART$finalModel) # desenho da ?rvore
plot(varImp(acidente_RPART)) # import?ncia das vari?veis

# Bagging com Floresta Aleat?ria
acidente_RF <- train(velocidade_max_via ~ com vitima + sem vitima + auto + caminhao + condicao_via, data = treinoACIDENTE, method = "cforest", trControl = train.control)

plot(acidente_RF) # evolu??o do modelo
plot(varImp(acidente_RF)) # plot de import?ncia

# Boosting com Boosted Generalized Linear Model
acidente_ADA_CLASS <- train(velocidade_max_via ~ com vitima + sem vitima + auto + caminhao + condicao_via, data = treinoACIDENTE, method = "glmboost", trControl = train.control)

plot(acidente_ADA_CLASS) # evolu??o do modelo
print(acidente_ADA_CLASS) # modelo
summary(acidente_ADA_CLASS) # sum?rio

melhor_modelo <- resamples(list(SVM = acidente_svm, RPART = acidente_RPART, RF = acidente_RF, ADABOOST = acidente_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)

# relatório Rmarrkdown
title: "análise acidente recife"
output: html_document
date: "2022-11-25"
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# carregar as bibliotecas
pacman::p_load(cluster, ggplot2, plotly)

# pré-processamento
acidente_recife_cluster <- acidentes_Recife[ , -5]
str(acidente_recife_cluster)
# setar semente aleatória
set.seed(1)

# Agrupamento com kmeans
cls <- kmeans(x = acidente_recife_cluster, centers = 3) # aprendizagem ns
acidente_recife_cluster$cluster <- as.factor(cls$cluster) # passamos os clusters para a base original
head(acidente_recife_cluster)
```

# Agrupamento dos dados 'acidente recife'
## K-means

#Abaixo, você encontra o agrupamento da base de dados acidentes_Recife, usando a técnica de k-means.

```{r acidente_recife_cluster, echo=FALSE, warning=FALSE, message=FALSE}

grafico1 <- ggplot() 
+ geom_point(data = acidente_recife_cluster, mapping = aes(x = Com vitima, y = velocidade_max_via, colour = cluster)) + 
  geom_point(mapping = aes_string(x = cls$centers[ , "Com vitima"], y = cls$centers[ , "velocidade_max_via"]), color = "red", size = 4) +
  geom_text(mapping = aes_string(x = cls$centers[ , "Com vitima"], y = cls$centers[ , "velocidade_max_via"], label = 1:3), color = "white", size = 2) +
  theme_light()
ggplotly(grafico1)
```
