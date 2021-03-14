dados <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
             55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53,
             59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40);
dados = sort(dados);

#Letra A

Amplitude <- max (dados) - min(dados); Amplitude
NK <-  round( 1 + 3.222 * log10(length(dados))) ; NK #Numero de classes
AmpClasse <- Amplitude / NK ; AmpClasse <- 5.85; AmpClasse
limiteClass <- c(38.00, 43.85, 49.70, 55.55, 61.40, 67.25, 73.00)
classes<-c("38.00-43.85","43.85-49.70","49.70-55.55",
           "55.55-61.40","61.40-67.25", "67.25-73.00");
Freq = table(cut(dados, breaks = limiteClass, right=FALSE, labels=classes))
Freq;
h = hist(dados, breaks = limiteClass, ylab = "Frequ�ncias Absolutas",
         xlab = "Range",
         freq = TRUE,
         labels = classes, main = "Histograma",
         xlim = c(38,73),
         ylim = c(0, 20),
         col="green")

#Letra B

#Media
Media = mean(dados); Media;

#Desvio Padr�o
sd(dados); dados;

#Moda de Czuber

#Mediana
Mediana = median(dados); Mediana;

#Terceiro quartil
Q3 = quantile(dados, 0.75); Q3;

#P8, P50 e P80
Percentis = quantile(dados, c(.08, .50, .80)); Percentis

#AP2 - Coeficiente de assimetria de Pearson

mean(dados);
median(dados);
sd(dados);

AP2 = (mean(dados) - median(dados))/sd(dados); AP2

#K - Coeficiente para o grau de curtose
numerador = (quantile(dados, 0.75) - quantile(dados, 0.25));
denominador = 2 * (quantile(dados, 0.90) - quantile(dados, 0.10));

K = numerador/denominador; K;


#Letra C

boxplot(dados, col = c("red", "yellow", "orange"),
        main = "Boxplot da metragem di�ria da constru��o")
