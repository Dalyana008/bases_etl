# projeto canvas dos dados
# acidente de transito em Recife 

# carrega as bibliotecas
pacman::p_load(ade4, arules, car, caret, corrplot, data.table, dplyr, e1071, forcats, funModeling, ggplot2, mlbench, mltools, randomForest, plotly, rattle, tidyverse)

acidentes_Recife <- read.csv2 ('https://web.archive.org/web/20220921185634/http://dados.recife.pe.gov.br/dataset/44087d2d-73b5-4ab3-9bd8-78da7436eed1/resource/2caa8f41-ccd9-4ea5-906d-f66017d6e107/download/acidentes2021.csv', sep = ';', encoding = 'UTF-8', stringsAsFactors = T)


#dumies
acidentes_Recife_D <- acm.disjonctif(as.data.frame(acidentes_Recife$natureza_acidente))
names(acidentes_Recife_D) <- c ('com_vitima', 'outros_vitima','sem_vitima', 'vitima_fatal')
acidentes_Recife<- cbind(acidentes_Recife, acidentes_Recife_D)

# Treino e Teste: Pre - processamento
particaoACIDENTES = createDataPartition(acidentes_Recife$moto, p=.7, list =F) # cria a particao 70-30

treinoACIDENTES = acidentes_Recife[particaoACIDENTES, ] # treino
testeACIDENTES = acidentes_Recife[-particaoACIDENTES, ] # - treino = teste

# Validaçãoo Cruzada: Pre-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Treinamentos
# Maquina de Vetor de Suporte (SVM)
acidentes_svm <- train(moto ~  com_vitima + sem_vitima + vitima_fatal + pedestre + viatura + ciclista + auto, data = treinoACIDENTES, method = "svmLinear", trControl = train.control)


# Regressão Linear
acidentes_Recife_lm <- train(moto ~ com_vitima + sem_vitima + vitima_fatal + pedestre + viatura + ciclista + auto, data = treinoACIDENTES, dados = treinoACIDENTES, method = "lm", trControl = train.control)

summary(acidentes_Recife_lm) # sumário do modelo linear
plot(varImp(acidentes_Recife_lm))



## Arvore de Decisão
acidentes_RPART <- train(moto ~  com_vitima + sem_vitima + vitima_fatal + pedestre + viatura + ciclista + auto, data = treinoACIDENTES, method = "rpart", trControl = train.control)

summary(acidentes_RPART)
fancyRpartPlot(acidentes_RPART$finalModel) # desenho da arvore
plot(varImp(acidentes_RPART)) # importa das variaveis

# Bagging com Floresta Aleatoria
acidentes_RF <- train(moto ~  com_vitima + sem_vitima + vitima_fatal + pedestre + viatura + ciclista + auto, data = treinoACIDENTES, method = "cforest", trControl = train.control)

plot(acidentes_RF) # evolucaoo do modelo
plot(varImp(acidentes_RF)) # plot de importancia

# Boosting com Boosted Generalized Linear Model
acidentes_ADA_CLASS <- train(moto ~  com_vitima + sem_vitima + outros + vitima_fatal + pedestre + ciclista + auto, data = treinoACIDENTES, method = "glmboost", trControl = train.control)

plot(acidentes_ADA_CLASS) # evolucao do modelo
print(acidentes_ADA_CLASS) # modelo
summary(acidentes_ADA_CLASS) # sumario

melhor_modelo <- resamples(list(SVM = acidentes_svm, RPART = acidentes_RPART, RF = acidentes_RF, ADABOOST = acidentes_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)

