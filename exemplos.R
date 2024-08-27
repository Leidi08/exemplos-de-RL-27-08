#Tv: investimento e propaganda na TV 
#Radio : investimento em propaganda na rádio 
#Newpaper: invetimento em propaganda em jornal impresso 
#Sales: milhares de unidade vendidas de um produto

# quero usar um modelo de regressao para vendas Sales como y variavel d

###############
#pacotes 
library(corrplot)
library(ggplot2)
##############

# 1) matriz de correlações
M <- cor(advertising)
corrplot(M,method = 'number')

# 2) analise individual
ggplot( data = advertising, aes (x = TV, y = Sales))+
  geom_point(size=4)+
  xlab( "investimento em propagnda n aTV")+
  ylab("vendas")+
  theme_classic()
  
# bonus : grafico de perfis
plot(advertising)

# 3) analise de regressao
# 1° usando todas as variaveis
modelo_1 <- lm(formula = Sales~., data = advertising)
summary(modelo_1)
#2° tirando as variaveis nao sigificativas 
modelo_2 <- lm(formula = Sales ~ TV + Radio, data = advertising)
summary(modelo_2)

#diagnostico dos residuos 

#1° verificação da media (testar se media e 0)
t.test(modelo_2$residuals)
#valor-p = 1 . Como é maior uqe 0,05, nao ha evidencias para rejeitar a hipotese de media 0.

#2° verificação de normalidade
qqnorm (modelo_2$residuals)
qqline (modelo_2$residuals)

shapiro.test(modelo_2$residuals) # valor-p 0,05 (ha elemntos para rejeitar a hipotese de normalidade)

hist(modelo_2$residuals)

#por conta de dois outliers, o teste de shpiro rejeita normalidade

#3° - homocedastidade
plot(modelo_2$fitted.values, modelo_2$residuals)
abline(0,0)
#verifica-se indicios de homocedastidade 

















