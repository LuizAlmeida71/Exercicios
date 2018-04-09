## Análise da Equipe de Campo
## Fonte: Protocolos de TOI 20_03_18

# Chegando diretório
getwd()
install.packages("readxl")
install.packages("plyr")
install.packages("gmodels")
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(gmodels)

# 1. importando dataset
protocolo <- read_xlsx("D:/2015/Projeto Comercial/Equipe de campo/Análise protocolo de TOI/Protocolos de TOI 20_03_18.xlsx", sheet = "baseR")

# 2. Verificando dataset

# 2.1. analisando tipo de dados
glimpse(protocolo)
any(is.na(protocolo)) # verificar se tem linhas nulas
class(protocolo$situacaoUc)

# 2.2 retirar linhas NA
protoNNA <- na.omit(protocolo)
glimpse(protoNNA)
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
  geom_histogram(fill="dark blue", 
                 alpha = 0.5, stat = "count")

# gráfico apresentou mais de uma moda e valores bem diversos.Provável que a medida não
# seja homogênea. Por exemplo, equipes com horários ou dias com comportamento
# bem diferentes. Para comparar rendimento é necessário reclassificar as equipes. 

# 3.1.2. quadro
contEquipe <-protNNa%>%group_by(equipe)%>%count()
contEquipe <- arrange(contEquipe, desc(n))
contEquipe

# 3.1.3 estatísticas
summary(contEquipe)

# 3.1.4 soma
sum(contEquipe$n)
#  número de linhas 40334

# 3.1.5 tabela de proporção
tabelaEquipe <- table(protNNa$equipe)
tabelaEquipe
propTable <- round((prop.table(tabelaEquipe))* 100,2)
tbProp <- tbl_df(propTable)%>%rename(equipe=Var1)
tbProp
propTable <- tbProp%>%arrange(desc(n))
propTable

# 3.1.6 80% maiores
contagem<-select(propTable,n)
prinp<- slice(contagem,1:8)
sum(prinp)
# as principais equipes responsáveis por 85.88%:
#1  DCCF09 10.9 
#2  DCCF04 10.5 
#3  DCCF07  9.26
#4  DCCF11  9.24
#5  DCCF03  9.01
#6  DCCF05  8.06
#7  DCCF10  6.44
#8  DCCF01  6.38
#9  DCCF08  5.53
#10 DCCF02  5.31

# 3.2 Qual serviço mais demandado

# 3.2.1. histograma
ggplot(protNNa, aes(x=descServ))+
  geom_histogram(fill = "green", alpha = 0.5,stat = "count")

# 3.2.2. tabela com contagem dos serviços
tabelaServico <- table(protNNa$descServ)
tabelaServico <- tbl_df(tabelaServico)
tabelaServico <- arrange(tabelaServico,desc(n))
tabelaServico

# 3.2.3. tabela com a proporção dos serviços
tabPropServ <- table(protNNa$descServ)
tabPropServ <- prop.table(tabPropServ)
tabPropServ <- tbl_df(tabPropServ)
tabPropServ <- arrange(tabPropServ,desc(n))
colPropServ <- select(tabPropServ, n)
paretoServ <- slice(colPropServ, 1:6)
somaPareto <- sum(paretoServ)
somaPareto
tabPropServ

# Os seguintes serviços a seguir são 81,5% do total
#1 INSPEÇÃO NA MEDIÇÃO EM BT                0.482 
#2 INSPEÇÃO POR DENÚNCIA                    0.0831
#3 INSTALAÇÃO DE MEDIDOR                    0.0560
#4 FICHA DE INSPEÇÃO                        0.0551
#5 INSPEÇÃO PROJETO RECADASTRAMENTO         0.0522
#6 SOLIC. INSPEÇÃO EM UNIDADE CONSUMIDORA   0.0450
#7 INSPEÇÃO E IMPLANTAÇÃO DO PROJ 1 E+ (GB) 0.0446

# 3.3 Relacionamento entre variáveis categóricas
protNNa2 <- protNNa 
protNNa2$seleqp <- protNNa2$equipe%in%c("DCCF09", "DCCF04", "DCCF11", "DCCF07", "DCCF03", "DCCF05",
                                        "DCCF10","DCCF01","DCCF08","DCCF02")
protNNa2$selesv <- protNNa2$descServ%in%c("INSPEÇÃO NA MEDIÇÃO EM BT", "INSPEÇÃO POR DENÚNCIA",
                                          "FICHA DE INSPEÇÃO", "INSPEÇÃO PROJETO RECADASTRAMENTO",
                                          "SOLIC. INSPEÇÃO EM UNIDADE CONSUMIDORA",
                                          "INSTALAÇÃO DE MEDIDOR",
                                          "INSPEÇÃO E IMPLANTAÇÃO DO PROJ 1 E+ (GB)")

protNNa2 <- filter(protNNa2, seleqp==TRUE)
protNNa2 <- filter(protNNa2, selesv==TRUE)
protNNa2
count(protNNa2, equipe)
count(protNNa2, descServ)

CrossTable(x=protNNa2$equipe,y=protNNa2$codServ, chisq = TRUE)
chisq.test(x=protNNa2$equipe,y=protNNa2$codServ)

# Analisando

# 4.Tempo do Serviço

#4.1. Criar coluna com diferença valores positivos

soDatas <- select(protNNa,dataAbertura, dataConclusao)
difData <- soDatas$dataConclusao-soDatas$dataAbertura
protNNa3 <- protNNa
protNNa3$difData <- difData
protTime <- filter(protNNa3, difData>=0)
count(protNNa3)- count(protTime)
protTime$difData

# 2974 tem data de abertura, posterior a de encerramento

#4.2. Verificar estatísticas do tempo
#4.2.1 função de transformação de segundos em dias
diffHoras <- function(x){                       
  return ((x/86400))                        
}

#4.2.2 transformação da coluna diferença de horas  
protTime$difData <- sapply(protTime$difData, diffHoras)
summary(protTime$difData)

#4.2.3 histograma do desempenho em tempo
ggplot(protTime, aes(x=difData))+
  geom_histogram(breaks=seq(0,150,by=2),
                 col = "red",
                 fill = "blue",
                 alpha = 0.5)+
  geom_density(col=10)+
  labs(title = "Histograma Duração do Serviço")+
  labs(x="Tempos(em dias)", y="Contagem")+
  xlim(c(0,100))
  

qplot(protTime$difData, geom = "histogram",
      binwidith = 0.5,
      main = "Histograma Duração do Serviço",
      fill = I("blue"),
      col=I("red"),
      alpha= I(.2),
      xlim = c(0,100))

#4.2.4 Verificar presença de outliers

ggplot(protTime, aes(x= equipe,y=difData))+
  geom_boxplot(fill="yellow",
               alpha = 0.5,
               outlier.color = "blue",
               outlier.alpha = 0.8)+
  labs(title = "Boxplot Tempo por Equipe")+
  labs(x="Equipes", y="Tempos(em dias)")+
  ylim(c(0,100))+
  coord_flip()

# Há uma incrível presença de outliers analisando o boxplot.
# Pelo sumário estatístico o valor do 3º quartil é 30,5 dias, mas a média se apresenta
# na casa de 672,2 dias. É preciso entender o que está acontecendo.

#4.2.5 qual o tempo médio por equipe
protTime
diasXEquipe <- protTime%>%group_by(equipe)%>%mean(difData)%>%arrange(desc(n))
diasXEquipe




