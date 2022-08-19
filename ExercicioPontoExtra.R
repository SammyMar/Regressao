library(tidyverse)
df <- softdrink
summary(df)
attach(df)
df_tidy <- df %>% 
  pivot_longer(c('y','x1','x2'),
               names_to = 'variaveis', values_to = 'valores') 
#1 ---- 
  X <- cbind(1,x1,x2)
  bets <- solve(t(X)%*%X)%*%t(X)%*%y
#2 ----
  yhat <- X%*%bets
#3 ----
  e <- y - yhat
#4 ----
  aux <- 0
  for (i in 2:nrow(df)) {
    aux <- aux + e[i]*e[i-1] 
  }
  p <- aux/sum(e[-1]^2)
#5 ----
  #pela tabela do teste durbin-watson para alpha = 0.05
  Du =  1.55 ; Dl = 1.21
  while(1){
    #5.1
      x1_ <- x1[1]*sqrt(1-(p^2))
      x2_ <- x2[1]*sqrt(1-p^2)
      y_ <- y[1]*sqrt(1-p^2)
      for(i in 2:nrow(df)){
        x1_[i] <- x1[i] - p*x1[i-1]
        x2_[i] <- x2[i] - p*x2[i-1]
        y_[i] <- y[i] - p*y[i-1] 
      }
      X_ <- cbind(1,x1_,x2_)
    #5.2
      bets_ <- solve(t(X_)%*%X_)%*%t(X_)%*%y_
    #5.3
      yhat_ <- X_%*%bets_
    #5.4
      a <- y_ - yhat_
    #5.5
      aux <- 0
      for (i in 2:nrow(df)) {
        aux <- aux + (a[i]*a[i-1])^2
      }
      d <- aux/sum(a^2)
      inconclusivo = F
      if(d > Du){
          break
      }else if(d < Dl){
        aux <- 0
        for(i in 2:nrow(df)) aux <- aux + (a[i] * a[i-1])
        p = aux/sum(a[-1]^2)
      }else{
          inconclusivo = T
          break
      }
}
bets_
    