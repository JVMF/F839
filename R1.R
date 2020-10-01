## ANÁLISE DE DADOS DO EXPERIMENTO 1 DE F839-A (2S 2020)
## Autor: João Vitor Martins Fernandez
## Objetivo: dado um vetor de entrada de dados, o script cria um 
## dataframe e utiliza a função lm para fazer a regressão linear.
## As funções são para o plot dos gráficos
## Rodar as linhas 104 e 105 para os resultados da lente divergente
## Rodar as linhas 114 e 115 para os resultados da lente convergente

## ----- ENTRADA DE DADOS

# DADOS LENTE DIVERGENTE [m]
Halo <- c(0.091, 0.104, 0.118, 0.131, 0.143, 0.156, 0.168, 0.184, 0.197, 0.21)          # Halo eixo vertical
Distancia0 <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8)                       # Considerando espessura do celular (1.5 cm = 0.015 m)
Distancia2 <- c(0.9850, 1.185, 1.385, 1.585, 1.785, 1.985, 2.185, 2.385, 2.585, 2.785)  # Descontando a espessura do celular

# Distância flash-lente --> d0 = 1 m 
# Largura verdical da lente --> a = (0.032 +- 0.04) m ~ armação grossa

# DADOS LENTE CONVERGENTE [m]
Sombra <- c(0.059, 0.060, 0.062, 0.064, 0.066, 0.068, 0.071, 0.075 )
DistanciaZ <- c(0.000, 0.020, 0.035, 0.040, 0.060, 0.085, 0.100, 0.135 )
DistanciaZ1 <- c(0.000, 0.020, 0.035, 0.040, 0.060, 0.085, 0.100, 0.135 )

# Largura verdical da lente, no centro  --> a = (MEDIR +- 0.04) m ~ armação grossa

# CONVERSÃO EM DATAFRAME PARA A ANÁLISE
Dados.Conv <- data.frame(DistanciaZ,Sombra)
Dados.Divg <- data.frame(Distancia2,Halo)

## ----- FUNCOES PARA LINEARIZACAO

# LENTE DIVERGENTE
Divg.Lin <- function(dados){
  
  divg <- lm(Halo ~ I(Distancia2), 
             data = dados, 
             weights = (dados$row_std))  # Erro experimental
  
  red.pred <- predict(divg)
  
  return(list(fit = divg, red.pred = red.pred))
}

# LENTE CONVERGENTE
Conv.Lin <- function(dados){
  
  conv <- lm(Sombra ~ I(DistanciaZ), 
             data = dados,
             weights = (dados$row_std))  # Erro experimental
  
  red.pred <- predict(conv)
  
  return(list(fit = conv, red.pred = red.pred))
}

## ----- AJUSTE

# CHAMADA DA FUNÇÃO
Divg.fit <- Divg.Lin(Dados.Divg)  
Conv.fit <- Conv.Lin(Dados.Conv)

## ----- FUNCOES PARA PLOTAR OS GRAFICOS
library(ggplot2)

# LENTE DIVERGENTE
plot.Divg <- function(dados,ajuste,
                      rotuloX = "Distancia [m]",
                      rotuloY = "Halo [m]"){
  
  dados$ajuste <- ajuste$red.pred
  
  gr <- ggplot(data = dados, aes(x = Distancia2, y = Halo))
  gr <- gr + geom_point(aes(x = Distancia2, y = Halo), size = 2, shape = 1)
  gr <- gr + geom_line(aes(x = Distancia2, y = ajuste), colour = "red")
  gr <- gr + labs(x = rotuloX, y = rotuloY)
  gr <- gr + theme_gray(base_size = 18)
  gr <- gr + geom_errorbar(aes(ymin = Halo - 0.0005, ymax = Halo + 0.0005), width=.1)
  
  return (gr)
}

# Lente CONVERGENTE
plot.Conv <- function(dados,ajuste,
                      rotuloX = "Distancia [m]",
                      rotuloY = "Sombra [m]"){
  
  dados$ajuste <- ajuste$red.pred
  
  gr <- ggplot(data = dados, aes(x = DistanciaZ, y = Sombra))
  gr <- gr + geom_point(aes(x = DistanciaZ, y = Sombra), size = 2, shape = 1)
  gr <- gr + geom_line(aes(x = DistanciaZ, y = ajuste), colour = "orange")
  gr <- gr + labs(x = rotuloX, y = rotuloY)
  gr <- gr + theme_gray(base_size = 18)
  gr <- gr + geom_errorbar(aes(ymin = Sombra - 0.0005, ymax = Sombra + 0.0005), width = 0.01)
  
  return (gr)
}

## ----- PLOT DOS GRÁFICOS E RESULTADOS
Grafico.Lente.Divg <- plot.Divg(Dados.Divg, Divg.fit)
Grafico.Lente.Conv <- plot.Conv(Dados.Conv, Conv.fit)

# LENTE DIVERGENTE  ---> D = (1/d0 - (I(Distancia2))/a)
Grafico.Lente.Divg
summary(Divg.fit$fit) 

# d0 = 1 m
# a = (0.032 +- 0.004) m
# coef. angular: I(Distancia2) = (0.0659 +- 0.0005)
# Propagação de erro: Err.Divg = sqrt((1/0.032)^2 * (0.0005) + (0.0659 * log(0.032, base = exp(1))) * (0.004))
# RESULTADO: D = - 1.060606 --> D = - 1.1 +- 0.7 ~ (D = - 1.00)

# LENTE CONVERGENTE  ---> D = (I(DistanciaZ))/a
Grafico.Lente.Conv
summary(Conv.fit$fit) 

# a = (0.035 +- 0.004) m
# coef. angular: I(DistanciaZ) = (0.122 +- 0.006)
# Propagação de erro: Err.Conv = sqrt((1/0.035)^2 * (0.0005) + (0.122 * log(0.035, base = exp(1))) * (0.004))
# RESULTADO: D = 3.49862 --> D = -3.5 +- 0.6 ~ (D = 3.50)
