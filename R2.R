## ANÁLISE DE DADOS DO EXPERIMENTO 2 DE F839-A (2S 2020)
## Autor: João Vitor Martins Fernandez
## Objetivo: dado um vetor de entrada de dados, o script cria um 
## dataframe e utiliza a função lm para fazer a regressão linear.
## As funções são para o plot dos gráficos
## Dados apêndice 1: Rodar linhas 14 até 25;
## Dados corpo do relatório (lente traseira): Rodar linhas 35 até 41 
## Dados corpo do relatório (lente frontal): Rodar linhas 46 até 52 
## Gráfico: Rodar linha 104, Estatísticas: Rodar linha 105

## ----- ENTRADA DE DADOS

### DADOS APÊNDICE 1 <- Apenas lente traseira

# Distância entre o objeto e a lente
Do <- c(0.16, 0.18, 0.24, 0.305, 0.33, 0.40, 0.47, 0.515, 0.62, 0.655)
Do.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)

# Tamanho do objeto na imagem
C <- c(0.036, 0.034, 0.025, 0.020, 0.015, 0.014, 0.011, 0.010, 0.009, 0.008)
C.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)

A = 0.1            # Altura do objeto = 10 cm
alpha <- 17.6      # Fator de conversão horizontal 
M <- C/(A*alpha)   # Magnificação
IM <- (1/M)        # Inverso da Magnificação
IM.erro <- sqrt((0.0005/0.1)^2 + (mean(C)*0.0005/(0.1)^2)^2)

### DADOS CORPO DO RELATÓRIO <- Ambas as lentes

## LENTE TRASEIRA

# Distância entre o objeto e a lente
Do <- c(0.155, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60)
Do.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)

# Tamanho do objeto na imagem
C <- c(0.085, 0.065, 0.053, 0.043, 0.037, 0.032, 0.028, 0.025, 0.023, 0.021)
C.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)
alpha <- 19.1

## LENTE FRONTAL

# Distância entre o objeto e a lente
Do <- c(0.132, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60)
Do.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)

# Tamanho do objeto na imagem
C <- c(0.065, 0.054, 0.043, 0.031, 0.030, 0.027, 0.024, 0.021, 0.019, 0.018)
C.erro <- c(0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005,0.0005)
alpha <- 25

# Cálculo da Magnificação

A = 0.176          # Altura do objeto fotografado = 17.6 cm
M <- C/(A*alpha)   # Magnificação
IM <- (1/M)        # Inverso da Magnificação

# CONVERSÃO EM DATAFRAME PARA A ANÁLISE
Dados <- data.frame(Do,IM,IM.erro)

## ----- FUNCOES PARA LINEARIZACAO

# LENTE DIVERGENTE
Ajuste.Lin <- function(dados){
  
  LIN <- lm(Do ~ I(IM), 
            data = dados, 
            weights = 1/(IM.erro)^2)  # Incerteza
  
  red.pred <- predict(LIN)
  
  return(list(fit = LIN, red.pred = red.pred))
}

## ----- AJUSTE

# CHAMADA DA FUNÇÃO
Smartphone.fit <- Ajuste.Lin(Dados)  

## ----- FUNCOES PARA PLOTAR OS GRAFICOS
library(ggplot2)

plot.Smartphone <- function(dados,ajuste,
                      rotuloX = "1/M",
                      rotuloY = "Do [m]"){
  
  dados$ajuste <- ajuste$red.pred
  
  gr <- ggplot(data = dados, aes(x = IM, y = Do))
  gr <- gr + geom_point(aes(x = IM, y = Do), size = 2, shape = 1)
  gr <- gr + geom_line(aes(x = IM, y = ajuste), colour = "blue")
  gr <- gr + labs(x = rotuloX, y = rotuloY)
  gr <- gr + theme_gray(base_size = 18)
  gr <- gr + geom_errorbar(aes(xmin = IM - 0.005, xmax = IM + 0.005), width = .1, colour = "red")

  return (gr)
}

## ----- PLOT DOS GRÁFICOS E RESULTADOS
Grafico.Smartphone <- plot.Smartphone(Dados,Smartphone.fit)

# Resultados
Grafico.Smartphone
summary(Smartphone.fit$fit) 

# RESULTADOS CORPO DO RELATÓRIO
# TRASEIRA: f = (3.66 +- 0.03) mm <==> fabricante = 3.6 mm
# FRONTAL: f = (2.51 +- 0.09) mm <===> fabricante = 2.53 mm

# RESULTADOS APÊNDICE 1
# TRASEIRA: f = (2.9 +- 0.1) mm <==> fabricante = 2.8 mm
