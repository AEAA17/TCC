# TCC
Temos dois códigos aqui no primeiro a instalação e utilização do biblioshiy, no segundo uma modelagem e otimização de uma reação.

# Código 1:

install.packages("bibliometrix")

library(bibliometrix)

biblioshiny()

# Pelo comando biblioshiny() é aberta uma página no navegado onde é possível carregar os artigos e selecionar as análises desejadas.

# Código 2:
# 1.Instalação e Carregamento dos Pacotes:
install.packages("rsm")
install.packages("lmtest")
library(rsm)
library(lmtest)
# Os pacotes rsm e lmtest são utilizados para análise de resposta superficial e testes de diagnóstico de modelos, respectivamente.

# 2.Criação e Análise do DataFrame planej:
Ordem <- c(1, 2, 3, 4, 5, 6, 7)
T <- c(-1, -1, 1, 1, 0, 0, 0)
C <- c(-1, 1, -1, 1, 0, 0, 0)
y <- c(13419, 18950, 16381, 17675, 16422, 16673, 16852)
planej <- data.frame(Ordem, T, C, y)
print(planej)
# Os vetores Ordem, T (Temperatura), C (Catalisador) e y (resposta) são combinados em um data frame chamado planej

modelo1 <- lm(y ~ T * C, data = planej) 
summary(modelo1) 
# Ajusta um modelo linear para analisar a interação entre T e C e seu impacto na resposta y e  Fornece um resumo estatístico do modelo ajustado

anov <- aov(modelo1) # Realizam uma análise de variância para verificar a significância dos efeitos
summary(anov)
# Realizam uma análise de variância para verificar a significância dos efeitos

shapiro.test(modelo1$residuals)
 # Testa a normalidade dos resíduos do modelo.
bptest(modelo1)
# Realiza o teste de Breusch-Pagan para verificar a presença de heterocedasticidade.

par(mfrow = c(1, 1))
plot(modelo1) 
# Gera gráficos de diagnóstico para avaliar a adequação do modelo.

# 3.Criação de um Modelo não codificado:
T1 <- c(200, 200, 250, 250, 225, 225, 225)
C1 <- c(0.1, 0.3, 0.1, 0.3, 0.2, 0.2, 0.2)
y <- c(13419, 18950, 16381, 17675, 16422, 16673, 16852)
modelo2 <- lm(y ~ T1 * C1)
# Novo modelo sem os fatores codificados para uma melhor visualização na superficie de resposta

persp(modelo2, C1 ~ T1, zlab = "MW", col = rainbow(50), contours = "colors")
# Cria um gráfico de superfície 3D para visualizar a resposta y em função de T1 e C1

legend("bottomright", legend = c("C1−Catalisador", "T1−Temperatura"), cex = 0.75, pt.cex = 0.75)
#  Adiciona uma legenda ao gráfico para identificar as variáveis.
