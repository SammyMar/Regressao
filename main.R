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
  # db %>% #ficou mt grande para colocar no artigo mas e um grafico bom
  #   ggpairs()
  db_boxplot <-  db %>% 
    pivot_longer(c('y','x1','x2','x3','x4','x5','x6','x7','x8','x9'),
                 names_to = 'variaveis', values_to = 'valores') #bd para boxplot
  # ggplot(db_boxplot) +
  #   aes(x = valores, y = variaveis, colour = variaveis) +
  #   geom_boxplot(fill = "#112446") +
  #   scale_color_viridis_d(option = "magma", 
  #                         direction = 1) + coord_flip() +
  #   labs(x = " ", y = "Variáveis") +
  #   theme_bw() +
  #   theme(legend.position = "none", 
  #         axis.title.y = element_text(size = 15L, 
  #                     face = "bold"), axis.title.x = element_text(size = 20L))
   ggplot(db_boxplot) + #boxplot
    aes(x = variaveis, y = valores, colour = variaveis) +
    geom_boxplot(fill = "#112446") +
    geom_jitter() +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    theme_bw() +
    theme(legend.position = "none")
  attach(db)
  corrplot::corrplot(cor(db),type = 'upper', method = 'number') #grafico de corr
#selecao de variaveis--------
    modeloT <- lm(y~.,data = db)
    modelo1 <- lm(y~1,data =  db)
    #all posible regression
      APR<- ols_step_all_possible(modeloT)
      plot(APR)
      APR_DF <- as.data.frame(APR)
      #selecionando melhor modelo pelo APR
        attach(APR_DF)
        cri1 <- APR_DF[order(adjr),"mindex"] %>% 
          head(10)
        cri2 <- APR_DF[order(cp),"mindex"] %>% 
          head(10)
        cri3 <- APR_DF[order(rsquare),"mindex"] %>% 
          head(10)
        freq <- c(cri1,cri2,cri3) 
        freq <- freq[duplicated(freq == T)] %>% 
          unique()%>% sort()
        APR_DF[mindex %in% freq,]
    #backward elimination
      step(modeloT,direction ='backward')
    #forward selection
      step(modelo1,direction ='forward',scope = formula(modeloT))
#novo modelo depois de selecionar as variáveis ------------
    modelo2 <- lm(y~x2 + x7 + x8 + x9,db)
    db[,c(1,3,8,9,10)] %>% ggpairs()
#model adequacy--------------
    #GLOBAL F TESTE
      summary(modelo2)
      qf(0.99,4,23) # rejeita a hipotese nula
     
    #r2 ajustado de 0.7666
    #r2 multiplo de 0.8012
    #p valor proximo de 0
      mse(modelo2)
#model assumptions ------------
      crPlots(modelo2)
      qqPlot(modelo2)
      plot(modelo2)    

      #teste de normalidade ---------
      shapiro.test(modelo2$residuals)
      lmtest::bptest(modelo2)
      durbinWatsonTest(modelo2)
#outlier nos residuos
      summary(rstandard(modelo2))
#Multicolineariedade
      regclass::VIF(modelo2)

    
    
    
    
    