#===============================================================================
### EP 02 - B
### Alunos
### Paulo Henrique de Almeida Soares Pimenta,NUSP:7599770
### Fernanda Miwa Yamasaka,NUSP:11923084
### Rebeca Benevides Pinto,NUSP:15305032
#===============================================================================

#===============================================================================
### Carregando bibliotecas necessárias
if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)
pacman::p_load(dplyr, readxl)
#===============================================================================

#===============================================================================
### [1]   Leitura do banco.
dados <- as.data.frame(readxl::read_excel("Banco aulas 2026.xlsx", sheet = 1))
#===============================================================================

#===============================================================================
#===============================================================================
### Analise dos dados antes da tabela ter os dados de hipertensao e diabetes serem criados
#===============================================================================
dim(dados)
dplyr::glimpse(dados)
summary(dados)
### Ajuste em algumas variaveis que serão usadas em exercícios futuros
#unique(dados$ESCOLARIDADE)
dados$ESCOLARIDADE <- factor(dados$ESCOLARIDADE, levels = c("Sem escola formal", "Primeiro Grau", "Segundo Grau",
                                                            "Graduado"), ordered = TRUE)
dados$SEXO <- factor(dados$SEXO)
#===============================================================================
#===============================================================================
# [2] Criando as variáveis diabetes e hipertensão usando ifelse 
# | significa "ou".
# Se algum criterio for verdadeiro, classificar como "Sim".
# Se todos forem falsos, classificar como "Não".
# Se nenhum for verdadeiro e houver criterio desconhecido, manter NA.
dados$HIPERTENSAO <- ifelse(dados$HASTRAT == "Sim" | dados$PAS >= 140 | dados$PAD >= 90, "Sim", "Não")
dados$DIABETES <- ifelse(dados$DIABTRAT == "Sim" | dados$GLIC >= 126 | dados$HBGLIC >= 6.5 | dados$TTOG >= 200, "Sim", "Não")
#===============================================================================
### Analise dos dados após a tabela ter os dados de diabetes e hipertensão criados
#===============================================================================
dim(dados)
dplyr::glimpse(dados)
summary(dados)
#===============================================================================
### Verificando as tabelas de dados
#===============================================================================
table(dados$HIPERTENSAO, useNA = "ifany")
table(dados$DIABETES, useNA = "ifany")
#===============================================================================
# [3] Definir as metas de pressão arterial.
#===============================================================================
# Inicialmente, deixar as metas desconhecidas para todos.
dados$META_PAS <- NA
dados$META_PAD <- NA
#===============================================================================
# [3.1] Metas para pessoas com diabetes.
dados$META_PAS[which(dados$DIABETES == "Sim")] <- 130
dados$META_PAD[which(dados$DIABETES == "Sim")] <- 80
#===============================================================================
# [3.2] Metas para pessoas sem diabetes e com menos de 60 anos.
dados$META_PAS[which(dados$DIABETES == "Não" & dados$IDADE < 60)] <- 140
dados$META_PAD[which(dados$DIABETES == "Não" & dados$IDADE < 60)] <- 90
#===============================================================================
# [3.3] Metas para pessoas sem diabetes e com 60 anos ou mais.
# Conforme esclarecimento do professor, incluir idade igual a 60.
dados$META_PAS[which(dados$DIABETES == "Não" & dados$IDADE >= 60)] <- 150
dados$META_PAD[which(dados$DIABETES == "Não" & dados$IDADE >= 60)] <- 90
#===============================================================================
# [4] Verificação das metas
dados$PA_NA_META <- (dados$PAS < dados$META_PAS & dados$PAD < dados$META_PAD)
#head(dados[, c("IDADE", "DIABETES", "PAS", "PAD", "META_PAS", "META_PAD", "PA_NA_META")], n=50)
#===============================================================================
# [5] Definir a conduta para hipertensão.
#===============================================================================
## Inicialmente, deixar a conduta desconhecida para todos.
dados$CONDUTA_HAS <- NA
## Estabelecer as atribuições referentes as condutas
## Dentro da meta: manter a conduta atual.
dados$CONDUTA_HAS[which(dados$PA_NA_META == TRUE)] <- "manter conduta"
## Fora da meta e sem tratamento: introduzir medicamento.
dados$CONDUTA_HAS[which(dados$PA_NA_META == FALSE & dados$HASTRAT == "Não")] <- "introduzir medicamento"
## Fora da meta e com tratamento: aumentar a dose.
dados$CONDUTA_HAS[which(dados$PA_NA_META == FALSE & dados$HASTRAT == "Sim")] <- "aumentar dose"
# head(dados[, c("IDADE", "DIABETES", "PAS", "PAD", "HASTRAT", "META_PAS", "META_PAD", "PA_NA_META", "CONDUTA_HAS")], n=50)
#===============================================================================
# [6] Verificar a meta para diabetes.
#===============================================================================
## As duas condições precisam ser verdadeiras.
## Não é necessário conhecer o diagnóstico para verificar a meta, nem conhecer o 
## ...tratamento para atribuir "manter conduta" quando ambos os exames estão abaixo dos limites.
dados$DM_NA_META <- (dados$GLIC < 130 & dados$HBGLIC < 7)
#head(dados[, c("DIABETES", "GLIC", "HBGLIC", "DIABTRAT", "DM_NA_META")],n = 15)
#===============================================================================
# [7] Definir conduta para diabetes
#===============================================================================
## Inicialmente, deixar a conduta desconhecida para todos.
dados$CONDUTA_DM <- NA
## Estabelecer as atribuições referentes as condutas
## Dentro da meta: manter a conduta atual.
dados$CONDUTA_DM[which(dados$DM_NA_META == TRUE)] <- "manter conduta"
## Fora da meta e sem tratamento: introduzir medicamento.
dados$CONDUTA_DM[which(dados$DM_NA_META == FALSE & dados$DIABTRAT == "Não")] <- "introduzir medicamento"
## Fora da meta e com tratamento: aumentar a dose.
dados$CONDUTA_DM[which(dados$DM_NA_META == FALSE & dados$DIABTRAT == "Sim")] <- "aumentar dose"
head(dados[, c("IDADE", "DIABETES", "GLIC", "HBGLIC", "DIABTRAT", "DM_NA_META", "CONDUTA_DM")], n=50)
## Contar as condutas para diabetes, incluindo os dados ausentes.
table(dados$CONDUTA_DM, useNA = "ifany")
#===============================================================================
# [8] Definir as metas de HDL e LDL
#===============================================================================
### Inicialmente, deixar os limites desconhecidos.
dados$META_LDL <- NA
dados$META_HDL <- NA

### [8.1] Limite do LDL.
### Diabetes OU história de DAC positiva.
dados$META_LDL[which(dados$DIABETES == "Sim" | dados$HPDAC == "Sim")] <- 100
### Sem diabetes E sem história de DAC.
dados$META_LDL[which(dados$DIABETES == "Não" & dados$HPDAC == "Não")] <- 130
### [8.2] Limite do HDL.
dados$META_HDL[which(dados$SEXO == "M")] <- 40
dados$META_HDL[which(dados$SEXO == "F")] <- 50
#===============================================================================
# [9] Verificar se o participante está dentro da meta de colesterol.
#===============================================================================
dados$DLP_NA_META <- (dados$LDL < dados$META_LDL & dados$HDL > dados$META_HDL)
#head(dados[, c("IDADE", "SEXO", "DIABETES", "HPDAC", "LDL", "HDL", "META_LDL", "META_HDL", "DLP_NA_META")], n = 15)
#===============================================================================
# [10] Definir a conduta para dislipidemia.
#===============================================================================
# Inicialmente, deixar a conduta de dislipidemia desconhecida.
dados$CONDUTA_DLP <- NA
#===============================================================================
# [10.1] Dentro das duas metas: manter conduta.
#===============================================================================
dados$CONDUTA_DLP[which(dados$DLP_NA_META == TRUE)] <- "manter conduta"
#===============================================================================
# [10.2] LDL mais de 30 acima OU HDL mais de 10 abaixo do limite:
# introduzir medicamento.
#===============================================================================