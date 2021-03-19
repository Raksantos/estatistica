#Q3
dados <- as.data.frame(read.csv(file='BaseCovidAlagoas.csv', header = TRUE, sep=","))
dadosMaceio <- dados[dados$Municipio == " Maceio (AL)",];
head(dadosMaceio)

por_idade = c()
for(i in dadosMaceio["Idade"]) { por_idade <- i }
por_idade <- as.numeric(gsub("([0-9]+).*$", "\\1", por_idade))

# tabela de obitos por mes
por_mes <- table(dadosMaceio$Mes)

#tabela de obitos por sexo
dadosMaceio$Sexo[dadosMaceio$Sexo == "Feminino "] <- "Feminino"

por_sexo <- table(dadosMaceio$Sexo);


#1.1 Grafico de obitos (boxplot)
boxplot(por_idade, data = data, col = c("red"))
#É notável a presença de outliers, ou seja, muitos números fora do padrão
#entre os jovens da faixa entre (1 - 25 anos). Além disso, é possível ver
#uma distribuição assimétrica 
#


#1.2 Grafico de obitos (barra)
barplot(table(por_idade), main="Obitos pela idade - Maceio", xlab="Idade", ylab="Obitos", col=rainbow(100))
#É possível ver uma certa assimetria a esquerda no grafico, alem de um alto
#indice de mortes no intervalo de 55 a 89 anos

#2 Grafico de Obitos por mes - Maceio
barplot(por_mes, main="Obitos por mes - Maceio", xlab="Mes", ylab="Obitos", col=rainbow(length(por_mes)))
#Possível notar um grande número de mortes entre os meses de Maio - Junho e
#Janeiro - Feveiro, provavelmente causados pelas festividades de inicio do ano

# 3 Graficos de Obitos por sexo (Pizza)
lab <- round(100*por_sexo/sum(por_sexo), 1)
pie(por_sexo,labels = lab, main="Obitos por sexo - Maceio", col = rainbow(length(por_sexo)), radius = 0.75, cex=.6)
legend("topright", names(por_sexo), cex = 0.6, fill = rainbow(length(por_sexo)))
#O gráfico mostra que os homens apresentam quase que 10% de mais óbitos, em relação
#as mulheres na cidade de Maceio

# 4 Graficos Obitos por idade (histograma)
hist(por_idade, col=rainbow(100), xlab="Idade", ylab="Obitos", main="Obitos por idade - Maceio", breaks=25, xlim=c(1,100))
#É possível ver uma certa assimetria a esquerda no histograma, alem de um alto
#indice de mortes dos idosos de 50 a 90 anos, como também foi apresentado no
#gráfico de barra, mas no histogramas temos esses dados mais agrupados
