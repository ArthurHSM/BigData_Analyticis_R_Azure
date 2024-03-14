# ----------------------- CARGA E TRANSFORMAÇÃO DE DADOS ----------------------------------------------------------------------------

setwd("D:/00-Formacao_DSA_Cientista_De_Dados/02-BigData_Analyticis_R_AML/Cap14/Projeto")
getwd()

# Variavel que controla a execução do script
Azure <- FALSE

# Execução de acordo com o valor da variavel Azure
# Esse scrip "Tools" fornce funções uteis para tratamento dos dados, que pode ser utilizado em vários projetos. 
if(Azure){
  source("src/Tools.R") # execução do script no Azure quando a var Azure é verdadeira
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
} else {
  source("src/Tools.R") # essa condição possui o mesmo nome da acima somente pq o nome da pasta no PC é igual a do Azure ML
  bikes <- read.csv("bikes.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE) 
  
  # selecionar as varias que serão utilizadas (papel do DS definir isso)
  cols <- c("dteday", "mnth", "hr", "holiday", "workingday", "weathersit", "temp", "hum", "windspeed", "cnt")
  
  # criando um subet dos dados 
  bikes <- bikes[ , cols]
  
  # transformar o objeto de data (função util do scrip Tools.R)
  bikes$dteday <- char.toPOSIXct(bikes)
  # esta linha acima gera dois valores NA
  # esta linha abaixo os corrige
  bikes <- na.omit(bikes)
  
  # normalizar as variaveis preditoras numericas
  cols <- c("temp", "hum", "windspeed")
  bikes[ , cols] <- scale(bikes[, cols])
  
}

# Criar uma nova variavel para indicar dia da semana (já existe uma, mas esse processo garante que esteja correto)
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)

# Adicionar uma coluna com a quantidade de meses, o que vai ajudar na criação do modelo
bikes <- month.count(bikes)

# Criar um fator ordenado para o dia de semana, começando por segunda feira
bikes$dayWeek <- as.factor(weekdays(bikes$dteday))

str(bikes$dayWeek) # fatores estão em texto (português)

# Depara dos dias de semana de texto para numeros (baseado na linguagem do seu OS)
bikes$dayWeek <- as.numeric(ordered(bikes$dayWeek, 
                                    levels = c("segunda-feira",
                                               "terça-feira",
                                               "quarta-feira",
                                               "quinta-feira",
                                               "sexta-feira",
                                               "sábado",
                                               "domingo")))
str(bikes$dayWeek) # fatores estão em números

# Adiciona uma variavel com valores unicos para o horario do dia em dias da semana e dias de fim de semana
# Com isso diferenciamos as horas dos dias de semana, das horas em dias de fim de semana
# Ou seja, quando é fim de semana, o valor do horario é maior que 24
bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24)

# Transforma os valores de hora na madrugada, quando a demanda por bicicletas é praticamente nula
bikes$xformHr <- ifelse(bikes$hr > 4, bikes$hr - 5, bikes$hr + 19)

# Adiciona uma variavel com valores unicos para o horario do dia em dias da semana e dias de fim de semana
# considerando horas da madrugada
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24)
View(bikes)

# Resumindo, esses ajustes serviram para padrozinar as horas dos dias, seja em finais de semana e/ou madrugada,
# para uma escala unica de 0 a 47, onde 0 são 5hrs da manha de um dia de trabalho, e 47 são 4hrs da manha de um 
# dia de não trabalho. 

# dataset final
View(bikes)

# Gera saída no Azure ML
if(Azure) maml.mapOutputPort('bikes')

# ----------------------- CORRELAÇÃO DOS DADOS ----------------------------------------------------------------------------

# definindo vetor com colunas desejadas para fazer a correlação
cols <- c("mnth", "hr", "holiday", "workingday",
          "weathersit", "temp", "hum", "windspeed",
          "isWorking", "monthCount", "dayWeek", 
          "workTime", "xformHr", "cnt")
# metodos principais de correlação: Pearson, Spearman, Kendall

# vetor com nome dos metodos que utilizaremos
metodos <- c('pearson', 'spearman')
cors <- lapply(metodos, function(method)(cor(bikes[,cols], method = method)))

# plot
library(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0
  plot( levelplot(x, 
                  main = paste("Plot de correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)))
}

Map(plot.cors, cors, metodos)

# ----------------------- ANALISE DE SERIE TEMPORAL ----------------------------------------------------------------------------

# Construindo um time series plot para certos horarios em dias uteis e fins de semana
# Vetor com alguns horarios do dia
times <- c(7,9,12,15,18,20,22)

# Time Series plot
tms.plot <- function(times){
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) + 
    geom_line() + 
    ylab('Numero de Bikes') +
    labs(title = paste("Demanda de Bikes as ", as.character(times), ':00', sep = '')) + 
    theme(text = element_text(size = 20))
}

library(ggplot2)
lapply(times, tms.plot)

# ----------------------- ANALISE EXPLORATORIA DOS DADOS ----------------------------------------------------------------------------

# Convertendo a variavel dayweek para fator ordenado e plotando em ordem de tempo
bikes$dayWeek <- fact.conv(bikes$dayWeek)

# Criando uma lista com os titulos dos graficos que vamos criar
labels <- list("Boxplots - Demanda de Bikes por Hora",
               "Boxplots - Demanda de Bikes por Estação",
               "Boxplots - Demanda de Bikes por Dia Útil",
               "Boxplots - Demanda de Bikes por Dia da Semana")

xAxis <- list("hr", "weathersit", "isWorking", "dayWeek")

# Função para criar os plots
plot.boxes  <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt", group = X)) + 
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

Map(plot.boxes, xAxis, labels)

# Visualizando o relacionamento entre as variáveis preditoras e demanda por bike
labels <- c("Demanda de Bikes vs Temperatura",
            "Demanda de Bikes vs Humidade",
            "Demanda de Bikes vs Velocidade do Vento",
            "Demanda de Bikes vs Hora")

xAxis <- c("temp", "hum", "windspeed", "hr")

# Função para os Density Plots
plot.scatter <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt")) + 
    geom_point(aes_string(colour = "cnt"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, xAxis, labels)


# Explorando a interação entre tempo e dia, em dias da semana e fins de semana
labels <- list("Box plots - Demanda por Bikes as 09:00 para \n dias da semana e fins de semana",
               "Box plots - Demanda por Bikes as 18:00  para \n dias da semana e fins de semana")

Times <- list(9, 18)

plot.box2 <- function(time, label){ 
  ggplot(bikes[bikes$hr == time, ], aes(x = isWorking, y = cnt, group = isWorking)) + 
    geom_boxplot( ) + ggtitle(label) +
    theme(text = element_text(size = 18)) }

Map(plot.box2, Times, labels)

# Gera saída no Azure ML
if(Azure) maml.mapOutputPort('bikes')

# ----------------------- FEATURA SELECTION ----------------------------------------------------------------------------

library(randomForest)
modelo <- randomForest(cnt ~ .,
                       data = bikes,
                       ntree = 100,
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo)

# apos visualizar as mais importantes variaveis, podemos remover as demais e recriar o modelo

modelo <- randomForest(cnt ~ . -mnth
                       -hr
                       -workingday
                       -isWorking
                       -dayWeek
                       -xformHr
                       -workTime
                       -holiday
                       -windspeed
                       -monthCount
                       -weathersit,
                       data = bikes,
                       ntree = 100,
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo)


# ----------------------- CRIANDO MODELO NO R PARA BLOCO NO AZURE ML ----------------------------------------------------------------------------

# criando funções do Tools.R pois o modulo "Create R Model" não possui porta de entrada. 
# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  dataset$dteday <- set.asPOSIXct(dataset)
}else{
  bikes <- bikes
}

require(randomForest)
model <- randomForest(cnt ~ xformWorkHr + dteday + temp + hum, 
                      data = bikes, # altere o nome do objeto data para "dataset" de estiver trabalhando no Azure ML
                      ntree = 40, 
                      nodesize = 5)
print(model)

# ----------------------- PREVISÕES MODELO NO R PARA BLOCO NO AZURE ML ----------------------------------------------------------------------------

# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Variável que controla a execução do script
Azure <- FALSE


if(Azure){
  bikes <- dataset
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  bikes <- bikes
}

require(randomForest)
scores <- data.frame(actual = bikes$cnt,
                     prediction = predict(model, newdata = bikes))

