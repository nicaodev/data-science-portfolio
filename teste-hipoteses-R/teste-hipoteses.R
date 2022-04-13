# Teste de Hipóteses

setwd("D:/TFS/data-science-portfolio/teste-hipoteses-R")
getwd()

# O Teste de Hipóteses é uma ferramenta estatística útil que pode ser usada para tirar uma conclusão 
# sobre a população a partir de uma amostra. 

# Pacotes
install.packages("readxl")
library(readxl)
library(e1071)

# Carregando o Dataset
dados <- read_excel("dados/bola_futebol.xlsx")
View(dados)
dim(dados)
str(dados)

# Análise exploratoria

# Verificando se há valores missing (null,N/A)
colSums(is.na(dados))

# Calculando as estatísticas
summary(dados)

# Vetor com o nome das estatísticas
nomes_stats <- c("Média", "Desvio Padrão", "Variância", "Tipo de Bola")

# Calculando as estatísticas para a Bola com Revestimento Atual
dados_stats_atual <- c(round(mean(dados$Atual), digits = 2), 
                       round(sd(dados$Atual), digits = 2), 
                       round(var(dados$Atual), digits = 2),
                       "Bola com Revestimento Atual")

# Calculando as estatísticas para a Bola com Revestimento Novo
dados_stats_novo <- c(round(mean(dados$Novo), digits = 2), 
                      round(sd(dados$Novo), digits = 2), 
                      round(var(dados$Novo), digits = 2),
                      "Bola com Revestimento Novo")

# Combina os resultados para comparação
dados_stats_combined <- rbind(nomes_stats, dados_stats_atual, dados_stats_novo)
View(dados_stats_combined)

# Análise Univariada

# Range
range_atual <- max(dados$Atual) - min(dados$Atual)
range_atual

range_novo <- max(dados$Novo) - min(dados$Novo)
range_novo
# PROVAVELMENTE indica que a bola com novo revestimento tem uma maior variabilidade em termos de performance.

# Intervalo Interquartil
summary(dados)

IQR_atual <- IQR(dados$Atual)
IQR_atual

IQR_novo <- IQR(dados$Novo)
IQR_novo



# Ajusta a área de plotagem
par(mfrow = c(2,2))

# Histograma
hist(dados$Atual, 
     main = "Distância - Bola Com Revestimento Atual", 
     xlab = "Distância (metros)", 
     ylab = "Número de Bolas", 
     col = "Blue")

hist(dados$Novo, 
     main = "Distância - Bola Com Revestimento Novo", 
     xlab = "Distância (metros)", 
     ylab = "Número de Bolas", 
     col = "Green")



# Boxplot
boxplot(dados$Atual, 
        main = "Distância - Bola Com Revestimento Atual", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Blue", 
        horizontal = TRUE)

boxplot(dados$Novo, 
        main = "Distância - Bola Com Revestimento Novo", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Green", 
        horizontal = TRUE)


# A variável revestimento novo também é inclinada (assimétrica) à direita, pois a maioria dos dados 
# está localizada à direita do gráfico. A média é maior que a mediana.

summary(dados$Atual)
summary(dados$Novo)

# Assimetria
 

# Verificando assimetria
# Se a assimetria é menor que -1 ou maior que 1, a distribuição é altamente distorcida.
# Se a assimetria é entre -1 e -0,5 ou entre 0,5 e 1, a distribuição é enviesada (assimétrica) moderadamente.
# Se a assimetria é entre -0,5 e 0,5, a distribuição é aproximadamente simétrica.
skewness(dados$Atual)
summary(dados$Atual)

skewness(dados$Novo)
summary(dados$Novo)

# Curtose

# A curtose informa a altura e a nitidez do pico central, em relação a uma curva de sino padrão.
# A distribuição normal tem kurtosis igual a zero.

# Uma curtose negativa significa que sua distribuição é mais plana que uma curva normal com a 
# mesma média e desvio padrão. O raciocínio inverso é o mesmo.
kurtosis(dados$Atual)
kurtosis(dados$Novo)

# Teste de Normalidade - Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Hipótese Alternativa (H1): Os dados não são normalmente distribuídos. 

# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir a normalidade dos dados.

shapiro.test(dados$Atual)
shapiro.test(dados$Novo)

# Conclusão da Análise Univariada

# A partir do boxplot e demais análises, pode-se dizer que não há discrepâncias. 
# A média do modelo de revestimentpo atual é 270,3 e a média do novo modelo é 267,5 e o desvio padrão 
# do atual é 8,7529 e o desvio padrão do novo é 9,8969. Não PARECE haver diferença significativa.
# Mas o que vai dizer de fato é o teste de hipoteses.

# Análise Bivariada

# Ajusta a área de plotagem
par(mfrow = c(1,1))

# Scatter Plot
plot(dados$Atual, dados$Novo)

# Correlação
cor(dados$Atual, dados$Novo)

# O valor é negativo, mas proximo de zero. Praticamente nao há correlacao entre as variaveis. Que serve para o teste de hipoteses
# que tem outra suposição que é a total independencia das variaveis.


# Teste de Hipóteses

# O Teste de Hipóteses é uma forma de inferência estatística que usa dados de uma amostra para tirar 
# conclusões sobre um parâmetro populacional ou uma distribuição de probabilidade populacional. 

# Primeiro, é feita uma suposição provisória sobre o parâmetro ou distribuição. Essa suposição é chamada 
# de hipótese nula e é indicada por H0. Uma hipótese alternativa (denotada H1), é o oposto do que é 
# declarado na hipótese nula. O procedimento de teste de hipóteses envolve o uso de dados 
# de amostra para determinar se H0 pode ou não ser rejeitada. 

# Se H0 for rejeitada, a conclusão estatística é que a hipótese alternativa H1 é verdadeira.

# Formulação da Hipótese

# Hipótese Nula (H0) – Não há diferença significativa entre a distância percorrida das bolas de futebol 
# com revestimento atual e novo.
# H0: muAtual - muNovo igual a 0 (zero)

# Hipótese Alternativa (H1) – Há diferença significativa entre a distância percorridad as bolas de futebol 
# com revestimento atual e novo.
# H1: muAtual - muNovo diferente de 0 (zero)

# A condição preliminar para se aplicar o teste t é a existência de distribuição normal dos dados 
# em ambos grupos de dados. 

# Todavia, existe um impasse! Há três tipos de test t: 
# teste t de uma amostra
# teste t de amostras independentes 
# teste t de amostras relacionadas (pareadas)

# Usamos o teste t de uma amostra para verificar os valores da variável em relação a média conhecida 
# de uma população.

# Para realizarmos os testes de igualdade de variâncias e os testes de médias, precisamos que as 
# duas populações sejam independentes. Esse é um teste de amostras independentes. Por isso paired = F. 

?t.test
teste_hipo <- t.test(dados$Atual, dados$Novo, paired = F, conf.level = 0.95, alternative = "t") 
teste_hipo



# Diferença das médias
delta_mean <- mean(dados$Atual) - mean(dados$Novo)
delta_mean

# Desvio padrão da diferença entre os dados
delta_desvio <- sd(dados$Atual - dados$Novo)
delta_desvio

# Size Effect
size_effect = delta_mean/delta_desvio
size_effect

# Power Test - Força do Teste ( Teste do TESTE. Se estamos c condições para confiar no teste T)
library(pwr)
?pwr.t.test
dim(dados)
power_teste <- pwr.t.test(n = 40, d = size_effect, sig.level = 0.05, alternative = "t")
power_teste

# Tamanho ideal da amostra
tamanho_amostra <- pwr.t.test(power = .95, d = 0.5, type = "t", alternative = "t", sig.level = .05)
tamanho_amostra

# Conclusao

#Resultado do Teste de Hipóteses
#1. O valor-p é 0,188 e, portanto, falhamos em rejeitar a hipótese nula. Logo, podemos
#dizer que não há diferença significativa entre a distância percorrida pela bola de
#futebol com revestimento atual e novo.
#2. Deve haver algum fator diferente do revestimento que afeta a distância percorrida
#pela bola de futebol.
#3. A empresa não deveria introduzir o novo modelo no mercado.
#Intervalos de confiança
#1. Intervalo de confiança de 95% para o modelo atual [273.0743, 267.4757]
#2. Intervalo de confiança de 95% para o novo modelo [270.6652, 264.3348]
#Assim, a respectiva média populacional deve estar dentro dessa faixa para obter
#resultados consistentes.
#Força do Teste e Tamanho da Amostra
#1. A força do teste é 0,144, o que é baixo.
#2. Tamanho da amostra: O teste deve ser realizado com um tamanho de amostra
#maior, pelo menos 105 registros.
#3. É sempre melhor ter um tamanho de amostra maior para obter melhores
#resultados, mas também deve-se levar em consideração outros fatores como custo,
#praticabilidade e tempo.

#Recomendações para o cliente X
#1. Estatisticamente, não há diferença significativa entre a distância percorrida entre as
#bolas de futebol com revestimento atual e novo.
#2. A empresa deve continuar seu estudo e considerar outros fatores que afetam a
#distância percorrida, como o tamanho e o peso da bola, seu material de fabricação -
#        enchimento, tecnologia e número de gomos.
#3. A empresa também deve considerar a tecnologia predominante no setor e os
#produtos lançados por seus concorrentes.
#4. A partir de agora, para aumentar sua participação no mercado, a empresa deve se
#concentrar em outras áreas como marketing, estratégia e vendas e seguir
#trabalhando em pesquisa e desenvolvimento.


