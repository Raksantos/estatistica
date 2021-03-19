dados <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41,
             55, 49, 61, 49, 49, 52, 55, 60, 52, 54, 57, 47, 66, 60, 53,
             59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40);
dados = sort(dados);

#Letra A

Amplitude <- max (dados) - min(dados); Amplitude
NK <-  round( 1 + 3.222 * log10(length(dados))) ; NK #Numero de classes
AmpClasse <- Amplitude / NK ; AmpClasse <- 5.85; AmpClasse
limiteClass <- c(38.00, 43.85, 49.70, 55.55, 61.40, 67.25, 73.01)
classes<-c("38.00-43.85","43.85-49.70","49.70-55.55",
           "55.55-61.40","61.40-67.25", "67.25-73.01");

Freq = table(cut(dados, breaks = limiteClass, right=FALSE, labels=classes));
FreqAc <- cumsum(Freq); FreqRel <- prop.table(Freq); FreqRelAc <- cumsum(FreqRel);
TabResul = cbind(Freq,FreqAc, FreqRel = round(FreqRel*100,digits = 2),
                 FreqRelAc= round(FreqRelAc*100,digits = 2));
TabResul;

h = hist(dados, breaks = limiteClass, ylab = "Frequencias Absolutas",
         xlab = "Range",
         freq = TRUE,
         labels = classes, main = "Histograma",
         xlim = c(38,73),
         ylim = c(0, 20),
         col="green")

#Letra B

#Media
Media = mean(dados); Media;

#Desvio Padrão
sd(dados);

#Moda de Czuber

TabResul;
limiteClassModa = 55.55; freqModa = 18; freqAnt = 11; freqPost = 4;
numerador = freqModa - freqAnt; numerador;
denominador = (2 * freqModa - (freqAnt + freqPost)); denominador
fracao = numerador/denominador; fracao
Mo = limiteClassModa + fracao * AmpClasse; Mo
  

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
        main = "Boxplot da metragem diária da construão")

#Comentar o boxplot
#Vemos que a distribuição assimetrica dos dados eh negativa, ou seja, temos uma
#concentracao de dados maior na parte esquerda que da direita e o coeficiente de
#assimetria de person eh negativo. Alem disso, percebemos a presença de um outlier
#, onde o valor é 73 dos dados na tabela
