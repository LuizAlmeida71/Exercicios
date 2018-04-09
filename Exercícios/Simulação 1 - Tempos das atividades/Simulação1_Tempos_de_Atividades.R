# Simulação 1 - Tempo de execução de atividades

# Passo 0 - Direcionar para o diretório de trabalho
setwd("C:/Curso Formação Cientista de Dados/Curso 1 - Big Data Analytics com R e Microsoft Azure Machine Learning/Exercícios/Simulação 1 - Tempos das atividades")
getwd()

# Passo 1 - Importar dataset
install.packages("readxl")
library(readxl)
?read_excel
dsTempos <- read_excel("Controle de desempenho.xlsx", sheet = "Filtro 29.01.18") 
dsTempos

# Passo 2 - Transformar em TBL
install.packages("dplyr")
library(dplyr)
tbTempos <- tbl_df(dsTempos)
tbTempos2 <- na.omit(tbTempos) #ou
tbTempos3 <- tbTempos[complete.cases(tbTempos),]
glimpse(tbTempos2)
glimpse(tbTempos3)
                
# Passo 3 - Aplicar glimpse
glimpse(tbTempos)

                # me interessam somente os campos Operador, Atividade e Tempo Total

# Passo 4 - Renomear Campos

tbRename <- rename(tbTempos, Tempo_Total = "Tempo Total")   
glimpse(tbRename)

# Passo 5 - Selecionar os campos de interesse
tbFocoTempo <- select(tbRename, Operador, Atividade, Tempo_Total)
glimpse(tbFocoTempo)

# Passo 6 - Fazer um View
View(tbFocoTempo)

# Passo 7 - Vericiar campos nulos
? select
tbFocoTempo <- na.omit(tbFocoTempo)

                # ou
tbFocoTempo <- tbFocoTempo[complete.cases(tbFocoTempo),]
glimpse(tbFocoTempo)

# Passo 8 - Verificar estatísticas
install.packages(c("tibble", "tidyr","purrr", "dplyr"))
install.packages("knitr")
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)
library(knitr)

tdEstat <- tbFocoTempo %>%
  group_by(Tempo_Total)%>% 
  do(enframe(summary(.$Tempo_Total)))%>%
  spread(name, value) %>%
  kable()

  
tdEstat

tbMedia <- summarise(tbFocoTempo, Media = mean(Tempo_Total))
tbMediana <- summarise(tbFocoTempo, Mediana = median(Tempo_Total))
tbModa <- summarise(tbFocoTempo, Modo = mode(Tempo_Total))
tbVariancia <- summarise(tbFocoTempo, Variancia = var(Tempo_Total))
tbDesvPad <- summarise(tbFocoTempo, sd(Tempo_Total))


dfEstat = data.frame(Media = tbMedia, Mediana = tbMediana, Moda = tbModa,Variância = tbVariancia, Desvio = tbDesvPad)
dfEstat

tbEstat = tbl_df(dfEstat)
glimpse(tbEstat)
View(tbEstat)

summary(tbFocoTempo$Tempo_Total)

glimpse(tbMedia)
glimpse(tbMediana)

# Passo 9 Gráfico de barra para contagem de categorias
library(ggplot2)


fatorAtividades <- factor(subAtividades$Atividade)
contAtividades <- count(tbFocoTempo,Atividade) %>% rename(Contagem = n) %>% select(Contagem)
vetAtividades <- contAtividades
vetAtividades
df = data.frame(Atividades=fatorAtividades,Contagem = contAtividades)
df

subAtividades <- select(tbFocoTempo, Atividade) %>%
  count(Atividade)

subAtividades

grafAtivi <- ggplot(df,aes(Atividades,y=Contagem, fill=Atividades))+
  geom_bar(colour = "black", stat="identity")+
  guides(fill = FALSE) +
  geom_label(label=rownames(df), color="white", size=4)


grafAtivi



# Passo 10 Tabela com tempos médios por categoria

# Passo 11 Gráfico de Pizza com contagem de contribuição das operadoras

