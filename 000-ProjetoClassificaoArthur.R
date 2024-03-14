# Projeto de Classificação - Arthur
# 

# ---------------------------- PREPARAÇÃO DOS DADOS ------------------------------------------
setwd("D:/00-Formacao_DSA_Cientista_De_Dados/02-BigData_Analyticis_R_AML/Cap15/Projeto")
getwd()

# Carregando o dataset
df <- read.csv("credito.csv")
View(df)

# renomeando colunas de acordo com o dicionário do dataset
colnames(df) <- c('CheckingAcctStat', 'Duration', 'CreditHistory', 'Purpose', 'CreditAmount', 'SavingsBonds', 'Employment', 'InstallmentRatePecnt', 'SexAndStatus', 'OtherDetorsGuarantors', 'PresentResidenceTime', 'Property', 'Age', 'OtherInstallments', 'Housing', 'ExistingCreditsAtBank', 'Job', 'NumberDependents', 'Telephone', 'ForeignWorker', 'CreditStatus')

# percebe-se que colunas categoricas não estão como fatores, então vamos fazer a conversão
str(df) 

# vetor com colunas que queremos que sejam categorias (fatores)
tofactor <- c('CheckingAcctStat', 'CreditHistory', 'Purpose', 'SavingsBonds', 'Employment', 'SexAndStatus', 'OtherDetorsGuarantors', 'Property', 'OtherInstallments', 'Housing', 'Job', 'Telephone', 'ForeignWorker', 'CreditStatus')
df[, tofactor] <- lapply(df[,tofactor], factor) # função para converter em fatores
str(df) # agora sim estão como fatores

# ---------------------------- FEATURE ENGINEERING ------------------------------------------

# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}else{
  source("src/ClassTools.R")
  Credit <- read.csv("credito.csv", header = F, stringsAsFactors = F )
  metaFrame <- data.frame(colNames, isOrdered, I(factOrder))
  Credit <- fact.set(Credit, metaFrame)
  
  # Balancear o número de casos positivos e negativos
  Credit <- equ.Frame(Credit, 2)
}

# Transformando variáveis numéricas em variáveis categóricas
toFactors <- c("Duration", "CreditAmount", "Age")
maxVals <- c(100, 1000000, 100)
facNames <- unlist(lapply(toFactors, function(x) paste(x, "_f", sep = "")))
Credit[, facNames] <- Map(function(x, y) quantize.num(Credit[, x], maxval = y), toFactors, maxVals)

# str(Credit)

# Output 
if(Azure) maml.mapOutputPort('Credit')

# ---------------------------- ANALISE EXPLORATORIA  ------------------------------------------

Azure <- FALSE

if(Azure){
  source('src/ClassTools.R')
  Credit <- maml.mapInputPort(1)
}

# Plots usando ggplot2
library(ggplot2)
# lapply aplica uma função nos componentes de um vetor/lista/df
# essas funções abaixo aplicam a plotagem de gráficos para as colunas que são fatores e estão no vetor colNames2 do utilitário ClassTools
lapply(colNames2, function(x){
  if(is.factor(Credit[,x])){
    ggplot(Credit, aes_string(x)) + 
      geom_bar() + 
      facet_grid(. ~ CreditStatus) + 
      ggtitle(paste("Total de Credito Bom/Ruim por", x))}})

lapply(colNames2, function(x){
  if(is.factor(Credit[, x]) & x != "CheckingAcctStat"){
    ggplot(Credit, aes(CheckingAcctStat)) + 
      geom_bar() + 
      facet_grid(paste(x, "~ CreditStatus")) + 
      ggtitle(paste("Total de Credito Bom/Ruim CheckingAcctStat e", x))}})

# ---------------------------- FEATURE SELECTION  ------------------------------------------

Azure <- FALSE

if(Azure){
  
  source("src/ClassTools.R")
  Credit <- maml.mapImputPort(1)
}

# Modelo randomForest para criar um plot de importancia das variaveis
# essas variaveis que foram excluidas do modelo se deu pela analise exploratoria dos gráficos anteriores
library(randomForest)
modelo <- randomForest(CreditStatus ~ .
                       - Duration
                       - Age
                       - CreditAmount
                       - ForeignWorker
                       - NumberDependents
                       - Telephone
                       - ExistingCreditsAtBank
                       - PresentResidenceTime
                       - Job
                       - Housing
                       - SexAndStatus
                       - InstallmentRatePecnt
                       - OtherDetorsGuarantors
                       - Age_f
                       - OtherInstalments,
                       data = Credit,
                       ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)


# ---------------------------- CRIANDO O MODELO  ------------------------------------------

# Criar um modelo de classificação baseado em randomForest
library(randomForest)

# Cross tabulation
table(Credit$CreditStatus)

# Criando uma função para balancear as classes e gerar dados de treino e teste
splitData <- function(dataframe, seed = NULL){
  if(!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Gerando dados de treino e de teste
splits <- splitData(Credit, seed = 808)

# Separando os dados
dados_treino <- splits$trainset
dados_teste <- splits$testset

# verificando o balanceamento
nrow(dados_teste)
nrow(dados_treino)

# Construindo o modelo
modelo <- randomForest(CreditStatus ~ CheckingAcctStat
                       + Duration_f
                       + Purpose
                       + CreditHistory
                       + SavingsBonds
                       + Employment
                       + CreditAmount_f,
                       data = dados_treino,
                       ntree = 100,
                       nodesize = 10)

print(modelo)

# Comparando as previsões do modelo com os dados observados
require(randomForest)

# gerando previsões nos dados de teste
previsoes <- data.frame(observado = dados_teste$CreditStatus,
                        previsto = predict(modelo, newdata = dados_teste))

View(previsoes)

# ---------------------------- GERANDO UM CURVA ROC ----------------------------------------

install.packages('ROCR')
library('ROCR')

# gerando as classes de dados
class1 <- predict(modelo, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$CreditStatus

# gerando a curva ROC
pred <- prediction(class1[,2], class2)
perf <- performance(pred, 'tpr', 'fpr')

plot(perf, col = rainbow(10))

# gerando a Confusion Matrix com o Caret
library(caret)
confusionMatrix(previsoes$observado, previsoes$previsto)
                
