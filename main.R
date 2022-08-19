#bibliotecas ------
library(MPV)
library(leaps)
library(MASS)
library(esquisse)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(olsrr)
library(car)
library(tidyverse)
#funcoes -----
  mse <- function(sm){
    mean(sm$residuals^2)}
#analise exploratorio ------
  db <- table.b1 #Banco de dados
  medidas_df <- do.call(cbind, lapply(db, summary)) %>% round(digits = 1)
  grid.table(medidas_df) #tabela de medidas de centralidade

  db_boxplot <-  db %>% 
    pivot_longer(c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9'),
                 names_to = 'variaveis', values_to = 'valores') #bd para boxplot
 ggplot(db_boxplot) + #boxplot
    aes(x = variaveis, y = valores, colour = variaveis) +
    geom_boxplot(fill = "#112446") +
    geom_jitter() +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    theme_bw() +
    theme(legend.position = "none")
  attach(db)
  corrplot::corrplot(cor(db),type = 'upper', method = 'number') #grafico de corr
#teste inicial de modelo --------------
  modeloT <- lm(y~.,data = db)
  summary(modeloT)#somente uma variavel significativa
  #selecao de variaveis--------
    
    modelo1 <- lm(y~1,data =  db)
    models <- regsubsets(y~.,data = db,nvmax = 9)
    summary(models)
    res.sum <- summary(models)
    data.frame(
      Adj.R2 = which.max(res.sum$adjr2),
      CP = which.min(res.sum$cp),
      BIC = which.min(res.sum$bic)
    )
    #backward elimination
      step(modeloT,direction ='backward')
    #forward selection
      step(modelo1,direction ='forward',scope = formula(modeloT))
#novo modelo depois de selecionar as variáveis ------------
    db[1:27,]
    modelo2 <- lm(y ~ x2 + x7 + x8 + x9,db)
    modelo3 <- lm(y ~ x2 + x7 + x8 + x9,db)
    modelo4 <- lm(y ~ x2 + x7 + x8,db)
    summary(modelo2)
    summary(modelo4)
    par(mfrow = c(2,2))
    plot(modelo2)
    crPlots(modelo3)
    shapiro.test(modelo2$residuals)
    #vizualizar modelo sem possíveis outliers
    X <- cbind(1,x2,x7,x8,x9)
    p <- 5
    D <- NA
    for(i in 1:nrow(X)){
      db_i <- slice(db,-i)
      modelo_aux <- lm(y ~ x2 + x7 + x8 + x9,db_i)
      b <- modelo2$coefficients %>% as.matrix()
      b_i <- modelo_aux$coefficients %>% as.matrix()
      aux <- t(b_i - b)%*%t(X)%*%(X)%*%(b_i - b)
      D[i] <- aux/(p*mse(modelo2))
    }
    # Não foi detectado pontos de alavancagem por cook's distance
    dfbetas <- NA
    C <- solve(t(X)%*%(X))
    for(i in 1:nrow(X)){
      for(j in 1:ncol(X)){
        db_i <- slice(db,-i)
        modelo_aux <- lm(y ~ x2 + x7 + x8 + x9,db_i)
        b_i  - modelo_aux$coefficients %>% as.matrix()
        dfbetas[i,j] <- (b[j] -b_i[j])/sqrt(* C[j,j])
      }
    }
  