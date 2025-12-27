  #item 2
        #BINOMIAL
        n <- 10000000
        p <- 0.0000001
        exp <- n*p
        var <- n*p*(1-p)
        cat("Valor esperado: ", exp, "\n")
        cat("Variancia: ", exp)
        #O valor esperado e variancia da distr. Poisson = 1 

        
    #item 3
        k <- 0:40
        p <- sum(dpois(k, lambda = 1)*(1/(k+1)))
        cat("A probabilidade de ganhar o prêmio é: ", p)
        
    #item 4
        #SIMULACAO BINOMIAL
        win <- rbinom(n = 10000000, size = 10000000, prob = 0.0000001)
        hist(win)
    
        #SIMULACAO POISSON (Aprox.)
        win_aprox <- rpois(n = 10000000, lambda = 1)
        hist(win_aprox)
