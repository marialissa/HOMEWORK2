#item 2
range <- 0:50
p <- 0.7
n <- 50

pmf <- dbinom(range, size = n, prob = p)
cdf <- pbinom(range, size = n, prob = p)

plot(range, pmf, type = "h", 
     main = "PMF de X",
     xlab = "Número de Clientes (k) que pedem Sobremesa",
     ylab = "P(X = k)",
     col = "darkred",
     lwd = 2)
points(range, pmf, pch = 16, col = "darkred") # Adiciona pontos para clareza

plot(range, cdf, type = "s", # 's' para gráfico de escada (função passo)
     main = "CDF de X",
     xlab = "Número de Clientes (k) que pedem Sobremesa",
     ylab = "P(X <= k)",
     col = "darkred",
     lwd = 2)# Adiciona pontos no final de cada passo


#item 3
media <- n * p
cat("média:", media, "\n")
variancia <- n * p * (1 - p)
cat("variância:", variancia, "\n")
desvio_padrao <- sqrt(variancia)
cat("desvio padrão:", desvio_padrao, "\n")


#item 4
pa <- 1 - pbinom(19, size = n, prob = p) 
cat("P(X >= 20) =", pa, "\n")
pb <- pbinom(42, size = n, prob = p) - pbinom(30, size = n, prob = p)
cat("P(30 < X < 43) =", pb, "\n")
pc <- dbinom(31, size = n, prob = p) 
cat("P(X = 31) =", pc, "\n")


#item 6
plot(0:50, dbinom(0:50, size = 50, prob = 0.8), type = "h", 
     col = "blue", lwd = 2,
     main = "PMF com p = 0.8 e n = 50",
     xlab = "Número de Clientes (X)", ylab = "P(X = k)")
points(0:50, dbinom(0:50, size = 50, prob = 0.8), pch = 16, col = "blue")

plot(0:100, dbinom(0:100, size = 100, prob = 0.7), type = "h", 
     col = "red", lwd = 2,
     main = "PMF com n = 100 e p = 0.7",
     xlab = "Número de Clientes (X)", ylab = "P(X = k)")
points(0:100, dbinom(0:100, size = 100, prob = 0.7), pch = 16, col = "red")
