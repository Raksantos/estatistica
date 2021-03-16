# lendo a base de dados
install.packages("readxl")
library(readxl)
full_data <- read_excel("Base Covid Alagoas.xlsx")

# vector com a coluna "Idade" transformada para inteiros
por_idade = c()
for(i in full_data["Idade"]) { por_idade <- i }
por_idade <- as.numeric(gsub("([0-9]+).*$", "\\1", por_idade))

# tabela de obitos por mes
por_mes <- table(full_data$Mes)

#tabela de obitos por sexo
por_sexo <- table(full_data$Sexo)

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

#5 Essa questao eh doidera