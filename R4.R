## ANÁLISE DE DADOS DO EXPERIMENTO 3 DE F839-A (2S 2020)
## Autor: João Vitor Martins Fernandez
## Objetivo: dado um vetor de entrada de dados, o script cria um dataframe e utiliza a função lm para fazer a regressão linear.
## As funções são para o plot dos gráficos
## Dados apêndice 2: Rodar linhas 14 a 19 para água e 24 a 29 para o oleo 

## Gráfico: Rodar linha 76, Estatísticas: Rodar linha 77;

## ----- ENTRADA DE DADOS

## CÍRCULO ÁGUA

# Altura [mm]
A <- c(15, 17, 20, 23, 25, 30)
A.erro <- c(0.5,0.5,0.5,0.5,0.5,0.5)

# Diametro [mm]
D <- c(58.416, 73.125, 87.228, 88.304, 96.542, 107.101)
D.erro <- c(0.001,0.001,0.001,0.001,0.001,0.001)

## CÍRCULO ÓLEO

# Altura [mm]
A <- c(3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
A.erro <- c(0.5,0.5,0.5,0.5,0.5,0.5)

# Diametro [mm]
D <- c(7.901, 8.418, 12.643, 14.509, 16.664, 22.118)
D.erro <- c(0.001,0.001,0.001,0.001,0.001,0.001)

# CONVERSÃO EM DATAFRAME PARA A ANÁLISE
Dados <- data.frame(A,D,D.erro)

## ----- FUNCOES PARA LINEARIZACAO

# ALTURA x DIÂMETRO
Ajuste.Lin <- function(dados){
  
  LIN <- lm(A ~ I(D), 
            data = dados, 
            weights = 1/(D.erro)^2)  # Incerteza
  
  red.pred <- predict(LIN)
  
  return(list(fit = LIN, red.pred = red.pred))
}

## ----- AJUSTE

# CHAMADA DA FUNÇÃO
graf.fit <- Ajuste.Lin(Dados)  

## ----- FUNCOES PARA PLOTAR OS GRAFICOS
library(ggplot2)

plot.graf <- function(dados,ajuste,
                      rotuloX = "Diâmetro do círculo escuro [mm]",
                      rotuloY = "Profundidade [mm]"){
  
  dados$ajuste <- ajuste$red.pred
  
  gr <- ggplot(data = dados, aes(x = D, y = A))
  gr <- gr + geom_point(aes(x = D, y = A), size = 2, shape = 1)
  gr <- gr + geom_line(aes(x = D, y = ajuste), colour = "green")
  gr <- gr + labs(x = rotuloX, y = rotuloY)
  gr <- gr + theme_gray(base_size = 18)
  gr <- gr + geom_errorbar(aes(xmin = D - 0.001, xmax = D + 0.001), width = .9, colour = "red")
  
  return (gr)
}

## ----- PLOT DOS GRÁFICOS E RESULTADOS
Grafico.graf <- plot.graf(Dados,graf.fit)

# Resultados
Grafico.graf
summary(graf.fit$fit) 
