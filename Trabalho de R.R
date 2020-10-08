#Importando as bibliotecas que serão usadas 
library(ggpubr)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyquant)

#Todos os dados deste trabalho foram coletado do site "brasil.io"

#Importando os dados de Covid-19 em Niterói
covid_nit <- read.csv('Downloads/niteroicovid19.csv')

#Explorando as colunas, cabeçalho, estrutura e resumo
names(covid_nit)
head(covid_nit)
str(covid_nit)
summary(covid_nit)

#Transformando em variável "data" e colocando em ordem cronológica
covid_nit$date <- as.Date(covid_nit$date)
covid_nit <- map_df(covid_nit, rev)

#Realizamos as mesmas etapas para os dados do Rio...
covid_riocity <- read.csv('Downloads/riocitycovid19.csv')
names(covid_riocity)
head(covid_riocity)
str(covid_riocity)
summary(covid_riocity)
covid_riocity$date <- as.Date(covid_riocity$date)
covid_riocity <- map_df(covid_riocity, rev)

#...e para os dados de São Gonçalo
sgcovid19 <- read.csv('Downloads/sgcovid19.csv')
names(sgcovid19)
head(sgcovid19)
str(sgcovid19)
summary(sgcovid19)
sgcovid19$date <- as.Date(sgcovid19$date)
sgcovid19 <- map_df(sgcovid19, rev)

#Plotando a evolução de óbitos em São Gonçalo
rjnitsg <- ggplot(sgcovid19,aes(x=date,y=new_deaths,linetype='-')) +
  #Usando a média móvel de 7 dias para suavizar as curvas
  geom_ma(size=1,n=7,aes(color="São Gonçalo")) +
  #Repetindo os passos para Nitreói e para o Rio
  geom_ma(size=1,data=covid_nit,n=7,aes(color='Niterói')) +
  geom_ma(size=1,data=covid_riocity,n=7,aes(color='Rio de Janeiro')) +
  #Adicionando título, removendo nome dos eixos e renomeando legenda
  ggtitle('Óbitos de Coronavirus (média móvel de 7 dias)') +
  labs(x = '', y = '') +
  #Definindo a palheta de cores manualmente
  scale_color_manual(values=c("São Gonçalo"="blue",
    "Niterói"="#D10046",'Rio de Janeiro'='#E1AF00')) + 
  #Removendo legenda da espessura da linha
  scale_linetype(guide = "none") +
  #Alterando tamanho do título, eixos e legenda,
  #além de apagar título da legenda e mudar a posição da mesma
  theme(plot.title = element_text(size=20), 
        axis.text = element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.position=c(.1,.8))

#Plotando somente para Niterói e São Gonçalo
nitsg <- ggplot(sgcovid19,aes(x=date,y=new_deaths,linetype='-')) +
  geom_ma(size=1,n=7,color='blue',show.legend=F) +
  geom_ma(size=1,data=covid_nit,n=7,color='#D10046',show.legend=F) +
  ggtitle('Óbitos de Coronavirus (média móvel de 7 dias) - Zoom') +
  labs(x = '', y = '') +
  theme(plot.title = element_text(size=20), 
        axis.text = element_text(size=12)) +
  #Criando anotações no gráfico (ao invés de legenda)
  annotate("text", x=c(as.Date('2020-05-24'),as.Date('2020-06-24')), 
           y=c(11,0.4),label=c("São Gonçalo","Niterói"),
           colour=c("blue","#D10046"), size=6)

#Plotando vários os 2 gráficos na mesma figura
ggarrange(rjnitsg, nitsg, ncol=1, nrow=2)

#Importando dados de Covid-19 de todos os estados
covid_estados <- read.csv('Downloads/covid19estados.csv')

#Explorando as colunas, cabeçalho, estrutura e resumo
names(covid_estados)
head(covid_estados)
str(covid_estados)
summary(covid_estados)

#Transformando em variável "data" e filtrando pelas últimas duas semanas
covid_estados$date <- as.Date(covid_estados$date)
covid_estados <- filter(covid_estados,date>'2020-06-16')

#Agrupando a última semana por estado e tirando a média de casos novos
covid_estados_1 <- covid_estados %>% filter(date>'2020-06-23') %>% 
  group_by(state) %>% summarise(avg=mean(new_confirmed))

#Agrupando a penúltima semana por estado e tirando a média de casos novos
covid_estados_2 <- covid_estados %>% filter(date<'2020-06-24') %>% 
  group_by(state) %>% summarise(avg=mean(new_confirmed))

#Calculando, entre as duas semanas, a variação média percentual de novos casos
covid_estados_1$difnewcases <- covid_estados_1$avg - covid_estados_2$avg
covid_estados_1$difnewcases <- covid_estados_1$difnewcases/covid_estados_2$avg

#Fazendo uma cópia da coluna de estados
#adiante, a coluna original será sobrescita por com as regiões
covid_estados_1$state_1 <- covid_estados_1$state
covid_estados_2$state_1 <- covid_estados_2$state

#Criando vetores de cada região
N <- c('N','AM','RR','AP','PA','TO','RO','AC')
NE <- c('NE','MA','BA','AL','SE','PB','PE','RN','CE','PI')
CO <- c('CO','MT','MS','GO','DF')
SE <- c('SE','SP','RJ','ES','MG')
S <- c('S','PR','RS','SC')
regioes <- list(N,NE,S,SE,CO)

#Substituindo os dados originais de estado pelos das regiões
for (k in regioes){
for (i in covid_estados_1$state){if(i %in% k){
  covid_estados_1$state <- gsub(i,k[1],covid_estados_1$state)}}}

#Renomeando as colunas para facilitar a identificação
covid_estados_1 <- covid_estados_1 %>% rename(estados=state_1,regioes=state)

#Agrupando as linhas por região (para que, na hora de plotar,
#as colunas de estados de mesma região fiquem agrupadas)
covid_estados_1$estados <- factor(covid_estados_1$estados,levels=
                                c(N[-1],NE[-1],CO[-1],SE[-1],S[-1]))

#Plotando a variação percentual de óbitos para cada estado
#diferentemente do outro gráfico, plotamos barras ao invés de linhas
ggplot(covid_estados_1,aes(x=estados,y=difnewcases)) + 
  geom_bar(stat="identity",aes(fill=regioes)) +labs(y = '') + 
  scale_fill_manual(values=c("#38170B","#BF1B0B","#EEAD2D","#66ADE5","#003366")) +
  ggtitle('Variação Percentual de Novos Casos (média móvel de 7 dias)') +
  #Definindo a faixa de valores do eixo y
  scale_y_continuous(breaks = seq(-0.75,1.5,0.25)) +
  theme(plot.title = element_text(size=20), 
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        legend.title = element_text(size=14,face="bold"),
        legend.text = element_text(size=13))

#Referências:
#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
#https://informationisbeautiful.net/visualizations/covid-19-coronavirus-infographic-datapack/
#http://www.uesc.br/editora/livrosdigitais_20140513/r_cientistas.pdf
#https://brasil.io/dataset/covid19/caso_full/
#https://rstudio.com/resources/cheatsheets/
#https://www.rdocumentation.org/
#https://www.r-bloggers.com/
#http://r-graph-gallery.com/
#https://stackoverflow.com/

