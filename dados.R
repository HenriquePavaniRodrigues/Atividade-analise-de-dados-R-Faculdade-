nivel_agua <- read.csv("C:/Users/henri/Desktop/Att_ciencia_dados/NIVEL de AGUA - DADOS3287_2022.csv", sep = ";", header = T)


head(nivel_agua)
str(nivel_agua)
View(nivel_agua)

#vetor com primera coluno do arquivo(todas linhas)

#1. Gráfico de DISPERSÃO 
cPedrido <- nivel_agua[,1]
cData <- nivel_agua[,2]
CAltura <- nivel_agua[,3]
CAltura

plot(cData, CAltura)

#2. HISTOGRAMA
#Histograma com c2 com classes de 1 a 20 com modificação na cor do gráfico
Lim_Sup_Area <- c(1:20)
Graf1 <- hist(CAltura, breaks = Lim_Sup_Area, plot = FALSE)
plot(Graf1, border = "dark blue", col = "light blue", main = "Histragrama Água", xlab = "Altura", labels= TRUE)
plot(Graf1, labels = TRUE)

#3. BOXPLOT
#Vertical
boxplot (CAltura, xlab = "Altura da água", ylab="Periodo", border = "dark blue", col = "light blue")
#Horizontal
boxplot (CAltura, ylab = "Altura da água", xlab="Periodo", horizontal=TRUE, border = "dark blue", col = "light blue")

boxplot(CAltura, CAltura, names=c("Altura 1", "Altura 2"))

#4. ESTATÍSTICAS

#4.1 Mínimo / Máximo / Amplitude

range(CAltura)
summary(CAltura)

#4.2 QUARTIS
q1 <- quantile(CAltura,0.25, na.rm=T)
q2 <- quantile(CAltura,0.5, na.rm=T)
q3 <- quantile(CAltura,0.75, na.rm=T)

Md <- q2

q1
q2
q3
Md

#4.3 MEDIA, DESVIO PADRAO e COEFICIENTE de VARIACAO
media <- mean(CAltura, an.rm=T)
desvio <- sd(CAltura, na.rm=T)
CV <- desvio/media

media
desvio
CV

#4.4 MODA
Freq <- table(cut(CAltura, breaks=c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0), right = F))
#listando todas as classes
names(Freq)

#identifica a maior frenquencia, igual a frequencia da moda
Freq_MODA <- max(Freq)
Freq_MODA

#identifica a classe da moda
names(Freq) [Freq == max(Freq)]



#4.5 Sumario Estatístico com sete Estatísticas Fundamentais:
#Mínimo / q1 / Mediana=q2 / Media / q3 / Máximo / Valores Ausentes (NA's)
summary(CAltura)


#5. DISTRIBUIÇÃO DE FREQUÊNCIAS
#Deve ser montada a Distribuição de Frequências dos dados

n2 <-length(CAltura)
n2

#numero de classes
No_Classes <- 1 + 3.22*log10(n2)
No_Classes

#Amplitude de cada classe
Min <- min(CAltura, na.rm=T)
Max <- max(CAltura, na.rm=T)
AAA <- Max - Min

Ampl_classe <- AAA/No_Classes
Ampl_classe

#NÃO incluindo o Lim Sup do intervalo
Frequencia <- table(cut(CAltura, breaks=c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0), right = F))
Frequencia

#incluindo o Lim Sup do intervalo
FrequenciaB <- table(cut(CAltura, breaks=c(0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0), right = T))
FrequenciaB



