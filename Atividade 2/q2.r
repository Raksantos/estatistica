# lendo a base de dados
install.packages("data.table")
library(data.table)
full_data <- as.data.frame(read.csv(file='BaseCovidAlagoas.csv', header = TRUE, sep=","))

# vector com a coluna "Idade" transformada para inteiros
por_idade = c()
for(i in full_data["Idade"]) { por_idade <- i }
por_idade <- as.numeric(gsub("([0-9]+).*$", "\\1", por_idade))

# tabela de obitos por mes
por_mes <- table(full_data$Mes)

#tabela de obitos por sexo
full_data$Sexo[full_data$Sexo == "Feminino "] <- "Feminino"

full_data$Sexo[full_data$Sexo == "Masculino "] <- "Masculino"

por_sexo <- table(full_data$Sexo); por_sexo

#1.1 Grafico de obitos por municipio (boxplot)
boxplot(por_idade, data = data, col = c("red"))

#1.2 Grafico de obitos por municipio (barra)
barplot(table(por_idade), main="Obitos por municipio", xlab="Idade", ylab="Obitos", col=rainbow(100))

#2 Grafico de Obitos por mes (Barras)
barplot(por_mes, main="Obitos por mes", xlab="Mes(Número)", ylab="Obitos", col=rainbow(length(por_mes)))

# 3 Graficos de Obitos por sexo (Pizza)
lab <- round(100*por_sexo/sum(por_sexo), 1)
pie(por_sexo,labels = lab, main="Obitos por sexo", col = rainbow(length(por_sexo)), radius = 0.75, cex=.6)
legend("topright", names(por_sexo), cex = 0.6, fill = rainbow(length(por_sexo)))

# 4 Graficos Obitos por idade (histograma)
hist(por_idade, col=rainbow(100), xlab="Idade", ylab="Obitos", main="Obitos por idade", breaks=25, xlim=c(1,100))

#5 Gráfico de óbitos por dia e média móvel simples de 7 em 7 dias

full_data["Dia"] <- as.Date(sprintf("%d-%d-%d",full_data$Ano, full_data$Mes, full_data$Data));
full_data["Semana"] <- week(sprintf("%d-%d-%d",full_data$Ano, full_data$Mes, full_data$Data));
head(full_data)

obitosSemana <- as.data.frame(cbind(full_data$Idade, full_data$Semana)); obitosSemana

barplot(table(full_data$Semana), main = "Media movel de obitos", col = "blue",
        ylim = c(0, 200), xlim = c(0, 55))
freq = table(cut(full_data$Semana, breaks = full_data$Seq, right=FALSE));
freq

medias = c();
tendencia = c();

for(i in 3:54){
   medias <- c(medias, mean(c(freq[i - 2], freq[i - 1])));
   if(mean(c(freq[i - 2], freq[i - 1])) * 1.15 > freq[i]){
     tendencia <- c(tendencia, "Queda")
   }
   
   if(mean(c(freq[i - 2], freq[i - 1])) * 1.15 < freq[i]){
     tendencia <- c(tendencia, "Cresceu")
   }
   
   else{
     tendencia <- c(tendencia, "Estável")
   }
   
}; medias; tendencia
