set.seed(42)
#item 1
n <- 500
u1 <- runif(n)
u2 <- runif(n)

z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)

z <- c(z1, z2)

temperatura_cpu <- 62 + 3.5 * z

head(temperatura_cpu)

#item2
u1 <- runif(n)
u2 <- runif(n)

z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)

temp_manual <- 62 + 3.5 * c(z1, z2)
par(mfrow = c(1, 2))
hist(temp_manual, breaks = 30, probability = TRUE, 
     col = rgb(0.2, 0.4, 0.6, 0.5), main = "Box-Muller Manual",
     xlab = "Temperatura (°C)", ylab = "Densidade")
curve(dnorm(x, mean = 62, sd = 3.5), add = TRUE, col = "darkblue", lwd = 2)

temp_nativa <- rnorm(n = 1000, mean = 62, sd = 3.5)
hist(temp_nativa, breaks = 30, probability = TRUE, 
     col = rgb(0.2, 0.6, 0.2, 0.5), main = "rnorm() Nativo",
     xlab = "Temperatura (°C)", ylab = "Densidade")
curve(dnorm(x, mean = 62, sd = 3.5), add = TRUE, col = "darkgreen", lwd = 2)

par(mfrow = c(1, 1))

#item3
#(a)MEDIA-AMOSTRAL
media_manual <- mean(temp_manual)
media_nativa <- mean(temp_nativa)

cat("Media amostral manual: ", media_manual, "\n")
cat("Media amostral nativa: ", media_nativa, "\n")

#(b)DESVIO-PADRAO-AMOSTRAL
dp_manual <- sd(temp_manual)
dp_nativa <- sd(temp_nativa)

cat("D. padrão amostral manual: ", dp_manual, "\n")
cat("D. padrão amostral nativa: ", dp_nativa, "\n")

#(c)TEMP-MAX-MIN

max_manual <- max(temp_manual)
max_nativa <- max(temp_nativa)

cat("Max amostral manual: ", max_manual, "\n")
cat("Max amostral nativa: ", max_nativa, "\n")

min_manual <- min(temp_manual)
min_nativa <- min(temp_nativa)

cat("Min amostral manual: ", min_manual, "\n")
cat("Min amostral nativa: ", min_nativa, "\n")

#(d)P(T>68)
#EMPIRICA
cont_manual <- 0
cont_nativa <- 0 

for(i in 1:1000){
  if(temp_manual[i] > 68){
    cont_manual <- cont_manual + 1
  }
  if (temp_nativa[i] > 68){
    cont_nativa <- cont_nativa + 1
  }
}

cat("P(T>68) manual empirica é: ", cont_manual/1000, "\n")
cat("P(T>68) nativa empirica é: ", cont_nativa/1000, "\n")

#TEORICA
prob_teo_68 <- 1 - pnorm(68, mean = 62, sd = 3.5)
cat("P(T>68) teorica é: ", prob_teo_68, "\n")

#(e)P(60 <T <65)
#EMPIRICA
cont_manual <- 0
cont_nativa <- 0 

for(i in 1:1000){
  if(temp_manual[i] > 60 & temp_manual[i] < 65){
    cont_manual <- cont_manual + 1
  }
  if (temp_nativa[i] > 60 & temp_nativa[i] < 65){
    cont_nativa <- cont_nativa + 1
  }
}

cat("P(60 < T < 65) manual empirica é: ", cont_manual/1000, "\n")
cat("P(60 < T < 65) nativa empirica é: ", cont_nativa/1000, "\n")

#TEORICA
prob_teo_6065 <- pnorm(65, mean = 62, sd = 3.5) - pnorm(60, mean = 62, sd = 3.5)
cat("P(60 < T < 65) teorica é: ", prob_teo_6065, "\n")
#item 4
#(a)


hist(temp_manual, freq = FALSE, main = "Temperatura manual e nativa sobrepostas", xlab = "Valor", ylab = "Densidade", col = "lightblue", border = "white")
hist(temp_nativa, freq = FALSE, add = TRUE, col = rgb(1, 0, 0, 0.5), border = "white") # rgb(R, G, B, alpha)

curve(dnorm(x, mean = 62, sd = 3.5),
      add = TRUE,
      col = "blue",
      lwd = 2,
      lty = 2) 

#(f)P(T>75)
#TEORICO
prob_teo_75 <- 1 - pnorm(75, mean = 62, sd = 3.5)
cat("P(T>75) teorica é: ", prob_teo_75, "\n")

