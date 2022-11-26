
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
names(acidentes_Recife_D) <- c ('Com_vitima', 'sem_vitima')

acidentes_Recife<- cbind(acidentes_Recife, acidentes_Recife_D)


acidentes_Recife$velocidade_max_via <- discretize(acidentes_Recife$velocidade_max_via, method = "frequency", breaks = 2, labels = c("baixa", "alta"))

# Treino e Teste: Pre - processamento
particaoACIDENTES = createDataPartition(acidentes_Recife$velocidade_max_via, p=.7, list =F) # cria a particao 70-30

treinoACIDENTE = acidentes_Recife[particaoACIDENTES, ] # treino
testeACIDENTE = acidentes_Recife[-particaoACIDENTES, ] # - treino = teste

# Valida??o Cruzada: Pr?-processamento
# Controle de treinamento
train.control <- trainControl(method = "cv", number = 10, verboseIter = T) # controle de treino

# Treinamentos
# Maquina de Vetor de Suporte (SVM)

acidente_svm <- train(Com_vitima ~ velocidade_max_via+ acidente_verificado + tempo_clima  + sinalizacao + condicao_via + ponto_controle + situacao_placa + auto + caminhao + moto, data = treinoACIDENTE, method = "svmLinear", trControl = train.control)

# sumário da máquina no vetor de suporte 

acidente_svm
plot(varImp(acidente_svm))

# criar a maquina de vetor de suporte
svmAcidenteCLASS = svm(Com_vitima ~ velocidade_max_via + acidente_verificado + tempo_clima  + sinalizacao + condicao_via + ponto_controle + situacao_placa + auto + caminhao + moto, data = treinoACIDENTE, cost = 10, scale = F)

svmAcidente
plot(svmAcidenteCLASS, treinoACIDENTE, auto ~ moto)

# Arvore de Decisao

acidente_RPART <- train(Com_vitima ~ velocidade_max_via + acidente_verificado + tempo_clima  + sinalizacao + condicao_via + ponto_controle + situacao_placa + auto + caminhao + moto, data = treinoACIDENTE, method = "rpart", trControl = train.control)

summary(acidente_RPART)


fancyRpartPlot(acidente_RPART$finalModel) # desenho da arvore
plot(varImp(acidente_RPART)) # importancia das variaveis

# Bagging com Floresta Aleatoria
acidente_RF <- train(Com_vitima ~ velocidade_max_via + acidente_verificado + tempo_clima  + sinalizacao + condicao_via + ponto_controle + situacao_placa + auto + caminhao + moto, data = treinoACIDENTE, method = "cforest", trControl = train.control)

plot(acidente_RF) # evolucao do modelo
plot(varImp(acidente_RF)) # plot de importancia

install.packages("glmboost")
# Boosting com Boosted Generalized Linear Model
acidente_ADA_CLASS <- train(Com_vitima ~ velocidade_max_via  + auto + caminhao + moto, data = treinoACIDENTE, method = "glmboost", trControl = train.control)

plot(acidente_ADA_CLASS) # evolucao do modelo
print(acidente_ADA_CLASS) # modelo
summary(acidente_ADA_CLASS) # sum?rio

melhor_modelo <- resamples(list(SVM = acidente_svm, RPART = acidente_RPART, RF = acidente_RF, ADABOOST = acidente_ADA_CLASS))
melhor_modelo

summary(melhor_modelo)