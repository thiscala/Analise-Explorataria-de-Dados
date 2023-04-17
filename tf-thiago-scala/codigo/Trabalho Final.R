library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

planilha_geral <- read_excel("./dados/covid_19_bauru_casos_geral.xlsx")
planilha_mortes <- read_excel("./dados/covid_19_bauru_mortes.xlsx",col_types = c("date", "text", "numeric", "date", "text", "text", "date", "numeric"))

idades <- planilha_mortes$'idade'

png(filename = "./graficos/distribuicao-idades.png", width = 800, height = 600)
hist(idades, main = "Distribuicao de acordo com as idades dos pacientes.", xlab = "Idade", ylab= "Frequencia", col=rgb(0.5,0.4,0.7,0.1))
grid(nx=NA, ny=NULL)
hist(idades, main = "Distribuicao de acordo com as idades dos pacientes.", xlab = "Idade", ylab= "Frequencia", col=rgb(0.5,0.4,0.7,0.1),add = TRUE)
dev.off()

comorbidades <- c(planilha_mortes$comorbidade[complete.cases(planilha_mortes$comorbidade)])
#View(comorbidades)
#separados por "e"
separados <- strsplit(comorbidades," e ")
#View(separados)
#transformando lista em vetor 
vetor_com <- unlist(separados)
#vetor_com
#frequencia das comorbidades
tabela_com <- as.data.frame(table(vetor_com))
tabela_com <- tabela_com[with(tabela_com,order(-Freq)),]
tabela_com <- tabela_com[1:10,]
View(tabela_com)

par(mgp=c(1,1,0))
png(filename = "./graficos/comorbidades.png", width = 1400, height = 600)
barplot(tabela_com$Freq, main ="Comorbidades dos obitos", xlab = "Comorbidades", ylab = "Frequencia", names.arg = tabela_com$vetor_com, ylim=c(0, 400) ,cex.names = 0.8, xaxs = "i", col = rainbow(30))
grid(nx=NA, ny=NULL)
barplot(tabela_com$Freq, main ="Comorbidades dos obitos", xlab = "Comorbidades", ylab = "Frequencia", names.arg = tabela_com$vetor_com, ylim=c(0, 400) ,cex.names = 0.8, xaxs = "i", col = rainbow(30),add = TRUE)
dev.off()

data_morte <- subset(planilha_mortes, data_obito >= "2020-07-09")
data_morte <- table(data_morte$data_obito)
png(filename = "./graficos/Obitos_variacao.png", width = 1400, height = 800)
plot(data_morte, type="b",las = 2, col="red", ylab = "Obitos", main = "Obitos por covid em Bauru")
dev.off()
View(data_morte)

hosp_priv <- planilha_mortes %>% filter(tipo_hosp=="privado")
hosp_publi <- planilha_mortes %>% filter(tipo_hosp=="público")
#hospital particular
inicio_priv <- hosp_priv$'inicio_sintoma'
fim_priv <- hosp_priv$'data_obito'
dias_priv <- difftime(as.Date(fim_priv), as.Date(inicio_priv), units = "days")
#View(dias_priv)
dias_priv <- na.omit(dias_priv)
#precisa ignorar os negativos
dias_priv[dias_priv < 0] = 0
media_priv <- mean(dias_priv)
View(media_priv)
#hospital publico
inicio_publi <- hosp_publi$'inicio_sintoma'
fim_publi <- hosp_publi$'data_obito'
dias_publi <- difftime(as.Date(fim_publi), as.Date(inicio_publi), units = "days")
#View(dias_publi)
dias_publi <- na.omit(dias_publi)
#precisa ignorar os negativos
dias_publi[dias_publi < 0] = 0
media_publi <- mean(dias_publi)
View(media_publi)
hospitais <- planilha_mortes$'tipo_hosp'
tabela_hosp <- table(hospitais)
tabela_hosp
png(filename = "./graficos/tipo_Permanencia_hospitalar.png", width = 1400, height = 600)
pie(tabela_hosp, main = "Tipos de permanencia hospitalar",col = c("#963939", "#403d3d"))
dev.off()
medias_hosp <- c(media_priv, media_publi)
png(filename = "./graficos/tempo_Permanencia_hospitalar.png", width = 800, height = 600)
barplot(as.vector(medias_hosp), main="Tempo medio de permanencia hospitalar", xlab="Tipos de permanencia", ylab="Media de dias da permanencia", names.arg = c ("Privado", "Publico"),ylim=c(0, 30), cex.names = 0.8, xaxs = "i", col=rainbow(25))
dev.off()

doses_tomadas <- planilha_mortes$'doses_vacina'
tabela_vacinas <- as.data.frame(table(doses_tomadas))
View(tabela_vacinas)
png(filename = "./graficos/doses_vacina.png", width = 800, height = 600)
barplot(tabela_vacinas$Freq, main ="Relacao entre doses tomadas da vacina e obitos", xlab = "Doses tomadas", ylab = "Obitos", names.arg = tabela_vacinas$doses_tomadas, ylim=c(0, 70) ,cex.names = 0.8, xaxs = "i", col = rainbow(10))
dev.off()

