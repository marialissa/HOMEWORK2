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

