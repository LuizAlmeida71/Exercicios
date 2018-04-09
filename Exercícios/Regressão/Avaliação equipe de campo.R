
# Chegando diretório
getwd()
install.packages("readr")
install.packages("plyr")
install.packages("gmodels")
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(gmodels)


# 1. importando dataset
protocolo <- read_xlsx("C:/Curso Formação Cientista de Dados/Curso 1 - Big Data Analytics com R e Microsoft Azure Machine Learning/Exercícios/Regressão/Protocolos de TOI 20_03_18.xlsx", sheet = "tb_R")

# 2. Verificando dataset

# 2.1. analisando tipo de dados
glimpse(protocolo)
any(is.na(protocolo)) # verificar se tem linhas nulas
class(protocolo$situacaoUc)

# 2.2 retirar linhas NA
protoNNA <- na.omit(protocolo)
head(protoNNA)
any(is.na(protoNNA))
count(protoNNA)
protoNNA

# 2.3 retirar coluna "situacaoUc"com datas de algumas colunas
# coluna com muitos valores diferentes do designado.
protNNa <- protoNNA[,-c(2)]
View(protNNa)
count(protNNa)
glimpse(protNNa)

# 3 análise dos dados

# 3.1 Qual equipe mais produz?
# 3.1.1. histograma
ggplot(protNNa, aes(x=equipe))+
  geom_histogram(fill="dark blue", bins = 10, 
                 alpha = 0.5, stat = "count")+
  coord_flip()

# gráfico apresentou mais de uma moda e valores bem diverso. Uma possibilidade é equipe operan-
# do em horários bem diversos e ou dias bem diferentes.

# 3.1.2. quadro
contEquipe <-protNNa%>%group_by(equipe)%>%count()
contEquipe <- arrange(contEquipe, desc(n))
contEquipe

# 3.1.3 estatísticas
summary(contEquipe)

# 3.1.4 soma
sum(contEquipe$n)
#  número de linhas 34902

# 3.1.5 tabela de proporção
tabelaEquipe <- table(protNNa$equipe)
propTable <- round((prop.table(tabelaEquipe))* 100,2)
tbProp <- tbl_df(propTable)%>%rename(equipe=Var1)
tbProp
propTable <- tbProp%>%arrange(desc(n))
propTable

# 3.1.6 80% maiores
contagem<-select(propTable,n)
prinp<- slice(contagem,1:8)
sum(prinp)
# as principais equipes são:  DCCF09 - 12.5, DCCF04 - 12.0, DCCF11-10.5,DCCF07 - 10.5   
# DCCF03 - 10.4,DCCF05 - 9.25, DCCF10 - 7.29, DCCF01 - 7.22

# 3.2 Qual serviço mais demandado

# 3.2.1. histograma
ggplot(protNNa, aes(x=descServ))+
  geom_histogram(bins=15, fill = "green", alpha = 0.5,stat = "count")+
  coord_flip()

# 3.2.2. tabela
tabelaServico <- table(protNNa$descServ)
tabelaServico <- tbl_df(tabelaServico)
tabelaServico <- arrange(tabelaServico,desc(n))
tabelaServico

# 3.2.3. serviço
tabPropServ <- table(protNNa$descServ)
tabPropServ <- prop.table(tabPropServ)
tabPropServ <- tbl_df(tabPropServ)
tabPropServ <- arrange(tabPropServ,desc(n))
colPropServ <- select(tabPropServ, n)
paretoServ <- slice(colPropServ, 1:6)
somaPareto <- sum(paretoServ)
somaPareto
# Os seguintes serviços a seguir são 81,5% do total
#1 INSPEÇÃO NA MEDIÇÃO EM BT              0.524 
#2 INSPEÇÃO POR DENÚNCIA                  0.0936
#3 FICHA DE INSPEÇÃO                      0.0637
#4 INSPEÇÃO PROJETO RECADASTRAMENTO       0.0585
#5 SOLIC. INSPEÇÃO EM UNIDADE CONSUMIDORA 0.0497
#6 INSPECAO - UC COM CONSUMO ZERO         0.0254

# 3.3 Relacionamento entre variáveis categóricas
protNNa2 <- protNNa 
protNNa2$seleqp <- protNNa2$equipe%in%c("DCCF09", "DCCF04", "DCCF11", "DCCF07", "DCCF03", "DCCF05")
protNNa2$selesv <- protNNa2$descServ%in%c("INSPEÇÃO NA MEDIÇÃO EM BT", "INSPEÇÃO POR DENÚNCIA",
                                          "FICHA DE INSPEÇÃO", "INSPEÇÃO PROJETO RECADASTRAMENTO",
                                          "SOLIC. INSPEÇÃO EM UNIDADE CONSUMIDORA",
                                          "INSPECAO - UC COM CONSUMO ZERO")
protNNa2$

protNNa2 <- filter(protNNa2, seleqp==TRUE)
protNNa2 <- filter(protNNa2, selesv==TRUE)
glimpse(protNNa2)
count(protNNa2, equipe)
count(protNNa2, descServ)

CrossTable(x=protNNa2$equipe,y=protNNa2$codServ, chisq = TRUE)
chisq.test(x=recorte$codServ,y=recorte$equipe)












