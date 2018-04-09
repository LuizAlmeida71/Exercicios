# Treinamento ggplot2

# Redirecionando o diretório de trabalho
setwd("C:/Curso Formação Cientista de Dados/Curso 1 - Big Data Analytics com R e Microsoft Azure Machine Learning/Exercícios/Simulação 2 - ggplot2")
getwd()

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

#0 - Importando dataset
df <- read_excel("Controle de desempenho.xlsx", sheet = "Filtro 29.01.18")
df

#1 - Trasformando em tbl
tbTempos <- tbl_df(df)
View(tbTempos)

#2 - gráfico de barras
# Nº de atividades
?count
tbContagem = group_by(tbTempos, Atividade) %>% count() 
tbContagem


barTempo <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n), alpha = factor(Atividade),
               fill=factor(Atividade)), stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) 

barTempo


barTempo2 <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n), alpha = factor(Atividade),
               fill=factor(Atividade)), stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) +
  coord_flip()

barTempo2


barTempo3 <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n), alpha = factor(Atividade),
               fill=factor(Atividade)), stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) +
  facet_wrap(~Atividade, nrow = 3)

barTempo3

barTempo4 <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n), alpha = factor(Atividade),
               fill=factor(Atividade)), stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) +
  facet_grid(Atividade ~ n)

barTempo4

barTempo5 <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n), alpha = factor(Atividade),
               fill=factor(Atividade)), stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) +
  labs(title = "Contagem dos levantamentos", x = "Atividades", y = "Nº Avaliações",
       color = factor("Atividade"))

barTempo5

barTempo5 <- ggplot(tbContagem) + 
  geom_bar(aes(x = factor(Atividade), y = factor(n),color = "blue"), 
               stat = "identity", position = "identity") +
  guides(fill = FALSE, alpha = FALSE) +
  labs(title = "Contagem dos levantamentos", x = "Atividades", y = "Nº Avaliações")

barTempo5
