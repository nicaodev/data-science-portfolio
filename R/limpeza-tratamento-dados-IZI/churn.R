setwd("D:/TFS/data-science-portfolio/R/limpeza-tratamento-dados-IZI")


#Importa dados

dados = read.csv("dados/Churn.csv", sep = ";", na.strings ="", stringsAsFactors = T)

View(dados)
head(dados)

summary(dados)

# Definindo os nomes das colunas

colnames(dados) = c("Id", "Score", "Estado", "Genero", "Idade", "Patrimonio", "Saldo", "Produtos", "TemCartCredito", "Ativo", "Salario", "Saiu")
head(dados)

## Exploração de dados

counts= table(dados$Estado)

barplot(counts, main="Estados", xlab="Estados")

counts = table(dados$Genero)
barplot(counts, main="Gêneros", xlab="Gêneros")

# Explorando score

summary(dados$Score)
boxplot(dados$Score)
hist(dados$Score)

summary(dados$Idade)
boxplot(dados$Idade)
hist(dados$Idade)

summary(dados$Saldo)
boxplot(dados$Saldo) + options(scipen = 999)
hist(dados$Saldo)

summary(dados$Salario)
boxplot(dados$Salario)
boxplot(dados$Salario,outline = F)

# Explorando valores faltantes
dados[!complete.cases(dados),]

# Tratando dados salariais faltantes N/A com mediana.


summary(dados$Salario)
median(dados$Salario,na.rm = T)
dados[is.na(dados$Salario),]$Salario = median(dados$Salario,na.rm = T)
dados[!complete.cases(dados$Salario),]

# Tratando Generos com moda ( o Valor que mais aparece irá para os N/As)


unique(dados$Genero)
summary(dados$Genero)
dados[is.na(dados$Genero) | dados$Genero == "M" ,]$Genero = "Masculino" 
dados[dados$Genero == "F" | dados$Genero == "Fem", ]$Genero = "Feminino" 
summary(dados$Genero)
dados$Genero =   factor(dados$Genero) # Remove generos n utilizados.
summary(dados$Genero)

# Tratando Idade's fora do dominio.

summary(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 110 ,]$Idade
dados[is.na(dados$Idade),]
median(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 110 ,]$Idade = median(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 110 ,]
summary(dados$Idade)

# Tratando dados duplicados

x =  dados[duplicated(dados$Id),]
x

dados = dados[-c(82),] # Id 81 duplicado, excluindo pelo indice 82.


#buscamos a linha que estava duplicada
dados[dados$Id == x$Id ,]
#verificamos novamente duplicados
x =  dados[duplicated(dados$Id),]
x


# Trtando Estado que estão fora do dominio categorico (PR, SC, RS)


unique(dados$Estado)
summary(dados$Estado)
dados[!dados$Estado %in% c("RS","SC","PR"),]$Estado = "RS"
summary(dados$Estado)
dados$Estado =   factor(dados$Estado)
summary(dados$Estado)


# Tratando Outliers com desvio-padrao.

desv = sd(dados$Salario, na.rm = T)
desv
dados[dados$Salario >= 2 *desv  , ]$Salario
boxplot(dados$Salario)
boxplot(dados$Salario, outline = F)
x = boxplot(dados$Salario)$out
x
median(dados$Salario)
dados[dados$Salario >= 2 *desv  , ]$Salario = median(dados$Salario)
dados[dados$Salario >= 2 *desv  , ]$Salario

x = boxplot(dados$Salario)
x