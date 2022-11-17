# Definindo diretório de trabalho
setwd("D:/R-4.2.1/DSA/Cap17")

# Carregando e visualizando os dados
dados <- read.csv("dataset.csv")
View(dados)

# Organização dos dados

# Quantas linhas tem casos completos?
complete_cases <- sum(complete.cases(dados))
# Quantas linhas tem casos incompletos?
not_complete_cases <- sum(!complete.cases(dados))

# Qualo percentual de dados incompletos?
percentual <- not_complete_cases / nrow(dados) * 100
percentual

# Removendo os objetos anteriores para liberar memória RAM
rm(complete_cases)
rm(not_complete_cases)

# Renomeando as colunas
nomes_colunas <- names(dados)
nomes_colunas

nomes_colunas[1] <- "NomePais"
nomes_colunas[2] <- "Ano"
nomes_colunas[3] <- "IndicadorNivelVida"
nomes_colunas[4] <- "PIB_Per_Capita"
nomes_colunas[5] <- "SuporteSocial"
nomes_colunas[6] <- "ExpectativaDeVida"
nomes_colunas[7] <- "IndicadorDeLiberdade"
nomes_colunas[8] <- "IndicadorGenerosidade"
nomes_colunas[9] <- "IndicadorCorrupcao"
nomes_colunas[10] <- "IndicadorEmocoesPositivas"
nomes_colunas[11] <- "IndicadorEmocoesNegativas"

names(dados) <- nomes_colunas

View(dados)

# Verificando quantos países foram incluídos na coleta de dados
length(unique(dados$NomePais))

# Lista os países únicos e grava o resultado (antes de remover registros com valores NA)
list_countries_with_na <- unique(dados$NomePais)
list_countries_with_na

# Vamos eliminar linhas com valores NA
dados <- na.omit(dados)

# Dimensões
dim(dados)

# Lista de países após remover valores NA
list_of_countries_without_na <- unique(dados$NomePais)
list_of_countries_without_na

# Verificando se perdemos países ao remover valores NA
length(list_countries_with_na)
length(list_of_countries_without_na)

# Verificando a diferença antes e depois de remover valores NA
setdiff(list_countries_with_na, list_of_countries_without_na)

# Remove os objetos
rm(list_countries_with_na)
rm(list_of_countries_without_na)

# Pergunta 1 - O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
#              Qual a correlação entre essas duas variáveis?

# Removendo dados missing, já que há poucos comparado ao volume de dados
sum(is.na(dados$PIB_Per_Capita))
sum(is.na(dados$ExpectativaDeVida))

# Scatter Plot
plot(dados$PIB_Per_Capita, dados$ExpectativaDeVida, main = "Expectativa de vida ao nascer x Log PIB", xlab = "Log do PIB", ylab = "Expectativa de vida ao nascer") 
# Aparente correlação positiva

# Correlação
cor(dados$PIB_Per_Capita, dados$ExpectativaDeVida) # 0.86

# Pergunta 2 - Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção nos negócios e no governo? 
#              Qual a correlação entre essas duas variáveis?

# Scatter Plot
plot(dados$IndicadorNivelVida, dados$IndicadorCorrupcao, main = "Percepção da Corrupção x Escala de Vida", xlab = "Escala de Vida", ylab = "Percepção da Corrupção") 
# Aparente fraca correlação negativa

# Correlação
cor(dados$IndicadorNivelVida, dados$IndicadorCorrupcao) # -0.448

# Pergunta 3 - O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
#              Qual a correlação entre essas duas variáveis?

# Scatter Plot
plot(dados$IndicadorNivelVida, dados$IndicadorEmocoesPositivas, main = "Felicidade x Escala de Vida", xlab = "Escala de Vida", ylab = "Felicidade")
# Aparente correlação positiva

cor(dados$IndicadorNivelVida, dados$IndicadorEmocoesPositivas) # 0.533

# Pergunta 4 - O país com o menor índice de suporte social tem maior percepção de corrupção em relação às empresas e ao governo no país?

# Checando qual o país com o menor índice de suporte social
library(dplyr)
temp <- dados %>% group_by(NomePais) %>% summarize(mediaSuporteSocial = mean(SuporteSocial), mediaIndicadorCorrupcao = mean(IndicadorCorrupcao))
temp[temp$mediaSuporteSocial == min(temp$mediaSuporteSocial), ] # Menor Suporte Social 
temp[temp$mediaIndicadorCorrupcao == max(temp$mediaIndicadorCorrupcao), ] #Maior percepção da corrupção
# São diferentes

# Pergunta 5 - Pessoas generosas são mais felizes?

# Scatter Plot
plot(dados$IndicadorGenerosidade, dados$IndicadorEmocoesPositivas, main = "Felicidade x Generosidade", xlab = "Generosidade", ylab = "Felicidade")

# Coeficiente de correlação
cor(dados$IndicadorGenerosidade, dados$IndicadorEmocoesPositivas)

# Teste de correlação
cor.test(dados$IndicadorGenerosidade, dados$IndicadorEmocoesPositivas, alternative = "greater", method = "pearson") # p-valor < 2.2e-16 => Correlação positiva

# Concluímos que sim, pessoas generosas são mais felizes

dados %>% group_by(NomePais) %>% summarise(across(everything(), mean))


