#===============================================================================
### EP 02 - A
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
##==============================================================================
### Analise dos dados antes da tabela ter os dados de hipertensao e diabetes serem criados
##==============================================================================
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
# Se algum criterio for verdadeiro, recebe "Sim".
# Se todos forem falsos, recebe "Não".
# Se nenhum for verdadeiro e houver algum criterio desconhecido, recebe NA.
dados$HIPERTENSAO <- ifelse(dados$HASTRAT == "Sim" | dados$PAS >= 140 | dados$PAD >= 90, "Sim", "Não")
dados$DIABETES <- ifelse(dados$DIABTRAT == "Sim" | dados$GLIC >= 126 | dados$HBGLIC >= 6.5 | dados$TTOG >= 200, "Sim", "Não")
##==============================================================================
### Analise dos dados depois da tabela ter os dados de hipertensao e diabetes 
### criados
##==============================================================================
dim(dados)
dplyr::glimpse(dados)
summary(dados)
#===============================================================================
# [3] Identificar quem pode ter o risco calculado.

### Inicialmente, considerar todos elegíveis.
dados$ELEGIVEL <- TRUE

### Excluir idades fora do intervalo de 30 a 74 anos.
dados$ELEGIVEL[which(dados$IDADE < 30 | dados$IDADE > 74)] <- FALSE

### Excluir história de DAC positiva ou desconhecida.
dados$ELEGIVEL[which(dados$HPDAC == "Sim")] <- FALSE
dados$ELEGIVEL[is.na(dados$HPDAC)] <- FALSE

### Excluir quem tem algum dado necessário ausente.
dados$ELEGIVEL[is.na(dados$SEXO)] <- FALSE
dados$ELEGIVEL[is.na(dados$IDADE)] <- FALSE
dados$ELEGIVEL[is.na(dados$DIABETES)] <- FALSE
dados$ELEGIVEL[is.na(dados$TABAGISMO)] <- FALSE
dados$ELEGIVEL[is.na(dados$PAS)] <- FALSE
dados$ELEGIVEL[is.na(dados$PAD)] <- FALSE
dados$ELEGIVEL[is.na(dados$LDL)] <- FALSE
dados$ELEGIVEL[is.na(dados$HDL)] <- FALSE

### Guardar os números das linhas dos participantes elegíveis.
elegiveis <- which(dados$ELEGIVEL)

### Mostrar as primeiras linhas selecionadas.
head(elegiveis)
#===============================================================================
# [4] Criar a variável de risco com NA para todos.
dados$RISCO_DAC_10_ANOS <- NA
### Criar a coluna de pontos da idade, inicialmente ausente para todos.
dados$PTS_IDADE <- NA
dados$PTS_LDL <- NA
dados$PTS_HDL <- NA
dados$PTS_PA <- NA
dados$PTS_DIABETES <- NA
dados$PTS_FUMO <- NA
dados$PTS_TOTAL <- NA
#===============================================================================
# [5] Calcular os pontos do escore.
#===============================================================================
# Por enquanto, estudar apenas a primeira pessoa elegivel.
# Repetir os cálculos para cada participante elegível.
for (i in elegiveis) {
    ##==============================================================================
    ### [5.1] Pontos da idade para homens.
    ##==============================================================================
    if (dados$SEXO[i] == "M") {
      
      if (dados$IDADE[i] < 35) {
        dados$PTS_IDADE[i] <- -1
        
      } else if (dados$IDADE[i] < 40) {
        dados$PTS_IDADE[i] <- 0
        
      } else if (dados$IDADE[i] < 45) {
        dados$PTS_IDADE[i] <- 1
        
      } else if (dados$IDADE[i] < 50) {
        dados$PTS_IDADE[i] <- 2
        
      } else if (dados$IDADE[i] < 55) {
        dados$PTS_IDADE[i] <- 3
        
      } else if (dados$IDADE[i] < 60) {
        dados$PTS_IDADE[i] <- 4
        
      } else if (dados$IDADE[i] < 65) {
        dados$PTS_IDADE[i] <- 5
        
      } else if (dados$IDADE[i] < 70) {
        dados$PTS_IDADE[i] <- 6
        
      } else {
        dados$PTS_IDADE[i] <- 7
      }
    }
    ##==============================================================================
    ### [5.2] Pontos do LDL para homens.
    ##==============================================================================
    if (dados$SEXO[i] == "M") {
      
      if (dados$LDL[i] < 100) {
        dados$PTS_LDL[i] <- -3
        
      } else if (dados$LDL[i] < 160) {
        dados$PTS_LDL[i] <- 0
        
      } else if (dados$LDL[i] < 190) {
        dados$PTS_LDL[i] <- 1
        
      } else {
        dados$PTS_LDL[i] <- 2
      }
    }
    ##==============================================================================
    # [5.3] Pontos do HDL para homens.
    ##==============================================================================
    if (dados$SEXO[i] == "M") {
      
      if (dados$HDL[i] < 35) {
        dados$PTS_HDL[i] <- 2
        
      } else if (dados$HDL[i] < 45) {
        dados$PTS_HDL[i] <- 1
        
      } else if (dados$HDL[i] < 60) {
        dados$PTS_HDL[i] <- 0
        
      } else {
        dados$PTS_HDL[i] <- -1
      }
    }
    ##==============================================================================
    # [5.4] Pontos da pressão arterial para homens.
    ##==============================================================================
    # Começar pelas faixas mais altas.
    if (dados$SEXO[i] == "M") {
      
      if (dados$PAS[i] >= 160 | dados$PAD[i] >= 100) {
        dados$PTS_PA[i] <- 3
        
      } else if (dados$PAS[i] >= 140 | dados$PAD[i] >= 90) {
        dados$PTS_PA[i] <- 2
        
      } else if (dados$PAS[i] >= 130 | dados$PAD[i] >= 85) {
        dados$PTS_PA[i] <- 1
        
      } else {
        dados$PTS_PA[i] <- 0
      }
    }
    ##==============================================================================
    # [5.5] Pontos do diabetes para homens.
    ##==============================================================================
    if (dados$SEXO[i] == "M") {
      
      if (dados$DIABETES[i] == "Sim") {
        dados$PTS_DIABETES[i] <- 2
        
      } else {
        dados$PTS_DIABETES[i] <- 0
      }
    }
    ##==============================================================================
    ### [5.1] Pontos da idade para mulheres.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$IDADE[i] < 35) {
        dados$PTS_IDADE[i] <- -9
        
      } else if (dados$IDADE[i] < 40) {
        dados$PTS_IDADE[i] <- -4
        
      } else if (dados$IDADE[i] < 45) {
        dados$PTS_IDADE[i] <- 0
        
      } else if (dados$IDADE[i] < 50) {
        dados$PTS_IDADE[i] <- 3
        
      } else if (dados$IDADE[i] < 55) {
        dados$PTS_IDADE[i] <- 6
        
      } else if (dados$IDADE[i] < 60) {
        dados$PTS_IDADE[i] <- 7
        
      } else {
        dados$PTS_IDADE[i] <- 8
      }
    }
    ##==============================================================================
    # [5.2] Pontos do LDL para mulheres.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$LDL[i] < 100) {
        dados$PTS_LDL[i] <- -2
        
      } else if (dados$LDL[i] < 160) {
        dados$PTS_LDL[i] <- 0
        
      } else {
        dados$PTS_LDL[i] <- 2
      }
    }
    ##==============================================================================
    # [5.3] Pontos do HDL para mulheres.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$HDL[i] < 35) {
        dados$PTS_HDL[i] <- 5
        
      } else if (dados$HDL[i] < 45) {
        dados$PTS_HDL[i] <- 2
        
      } else if (dados$HDL[i] < 50) {
        dados$PTS_HDL[i] <- 1
        
      } else if (dados$HDL[i] < 60) {
        dados$PTS_HDL[i] <- 0
        
      } else {
        dados$PTS_HDL[i] <- -2
      }
    }
    ##==============================================================================
    # [5.4] Pontos da pressão arterial para mulheres.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$PAS[i] >= 160 | dados$PAD[i] >= 100) {
        dados$PTS_PA[i] <- 3
        
      } else if (dados$PAS[i] >= 140 | dados$PAD[i] >= 90) {
        dados$PTS_PA[i] <- 2
        
      } else if (dados$PAS[i] >= 120 | dados$PAD[i] >= 80) {
        dados$PTS_PA[i] <- 0
        
      } else {
        dados$PTS_PA[i] <- -3
      }
    }
    ##==============================================================================
    # [5.5] Pontos do diabetes para mulheres.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$DIABETES[i] == "Sim") {
        dados$PTS_DIABETES[i] <- 4
        
      } else {
        dados$PTS_DIABETES[i] <- 0
      }
    }
    ##==============================================================================
    # [5.6] Pontos do tabagismo para homens e mulheres.
    ##==============================================================================
    if (dados$TABAGISMO[i] == "Atual") {
      dados$PTS_FUMO[i] <- 2
    } else {
      dados$PTS_FUMO[i] <- 0
    }
    ##==============================================================================
    # [5.7] Somar os seis componentes do escore.
    ##==============================================================================
    # Somar os pontos da pessoa selecionada.
    dados$PTS_TOTAL[i] <- dados$PTS_IDADE[i] +
      dados$PTS_LDL[i] +
      dados$PTS_HDL[i] +
      dados$PTS_PA[i] +
      dados$PTS_DIABETES[i] +
      dados$PTS_FUMO[i]
    ##==============================================================================
    # [5.8] Converter os pontos em risco de DAC em 10 anos - Homens: tabela LDL Pts.
    ##==============================================================================
    if (dados$SEXO[i] == "M") {
      
      if (dados$PTS_TOTAL[i] <= -3) {
        dados$RISCO_DAC_10_ANOS[i] <- "1%"
        
      } else if (dados$PTS_TOTAL[i] <= -1) {
        dados$RISCO_DAC_10_ANOS[i] <- "2%"
        
      } else if (dados$PTS_TOTAL[i] == 0) {
        dados$RISCO_DAC_10_ANOS[i] <- "3%"
        
      } else if (dados$PTS_TOTAL[i] <= 2) {
        dados$RISCO_DAC_10_ANOS[i] <- "4%"
        
      } else if (dados$PTS_TOTAL[i] == 3) {
        dados$RISCO_DAC_10_ANOS[i] <- "6%"
        
      } else if (dados$PTS_TOTAL[i] == 4) {
        dados$RISCO_DAC_10_ANOS[i] <- "7%"
        
      } else if (dados$PTS_TOTAL[i] == 5) {
        dados$RISCO_DAC_10_ANOS[i] <- "9%"
        
      } else if (dados$PTS_TOTAL[i] == 6) {
        dados$RISCO_DAC_10_ANOS[i] <- "11%"
        
      } else if (dados$PTS_TOTAL[i] == 7) {
        dados$RISCO_DAC_10_ANOS[i] <- "14%"
        
      } else if (dados$PTS_TOTAL[i] == 8) {
        dados$RISCO_DAC_10_ANOS[i] <- "18%"
        
      } else if (dados$PTS_TOTAL[i] == 9) {
        dados$RISCO_DAC_10_ANOS[i] <- "22%"
        
      } else if (dados$PTS_TOTAL[i] == 10) {
        dados$RISCO_DAC_10_ANOS[i] <- "27%"
        
      } else if (dados$PTS_TOTAL[i] == 11) {
        dados$RISCO_DAC_10_ANOS[i] <- "33%"
        
      } else if (dados$PTS_TOTAL[i] == 12) {
        dados$RISCO_DAC_10_ANOS[i] <- "40%"
        
      } else if (dados$PTS_TOTAL[i] == 13) {
        dados$RISCO_DAC_10_ANOS[i] <- "47%"
        
      } else {
        dados$RISCO_DAC_10_ANOS[i] <- ">=56%"
      }
    }
    ##==============================================================================
    # [5.8] Converter os pontos em risco de DAC em 10 anos - Mulheres: tabela LDL Pts.
    ##==============================================================================
    if (dados$SEXO[i] == "F") {
      
      if (dados$PTS_TOTAL[i] <= -2) {
        dados$RISCO_DAC_10_ANOS[i] <- "1%"
        
      } else if (dados$PTS_TOTAL[i] <= 1) {
        dados$RISCO_DAC_10_ANOS[i] <- "2%"
        
      } else if (dados$PTS_TOTAL[i] <= 3) {
        dados$RISCO_DAC_10_ANOS[i] <- "3%"
        
      } else if (dados$PTS_TOTAL[i] == 4) {
        dados$RISCO_DAC_10_ANOS[i] <- "4%"
        
      } else if (dados$PTS_TOTAL[i] == 5) {
        dados$RISCO_DAC_10_ANOS[i] <- "5%"
        
      } else if (dados$PTS_TOTAL[i] == 6) {
        dados$RISCO_DAC_10_ANOS[i] <- "6%"
        
      } else if (dados$PTS_TOTAL[i] == 7) {
        dados$RISCO_DAC_10_ANOS[i] <- "7%"
        
      } else if (dados$PTS_TOTAL[i] == 8) {
        dados$RISCO_DAC_10_ANOS[i] <- "8%"
        
      } else if (dados$PTS_TOTAL[i] == 9) {
        dados$RISCO_DAC_10_ANOS[i] <- "9%"
        
      } else if (dados$PTS_TOTAL[i] == 10) {
        dados$RISCO_DAC_10_ANOS[i] <- "11%"
        
      } else if (dados$PTS_TOTAL[i] == 11) {
        dados$RISCO_DAC_10_ANOS[i] <- "13%"
        
      } else if (dados$PTS_TOTAL[i] == 12) {
        dados$RISCO_DAC_10_ANOS[i] <- "15%"
        
      } else if (dados$PTS_TOTAL[i] == 13) {
        dados$RISCO_DAC_10_ANOS[i] <- "17%"
        
      } else if (dados$PTS_TOTAL[i] == 14) {
        dados$RISCO_DAC_10_ANOS[i] <- "20%"
        
      } else if (dados$PTS_TOTAL[i] == 15) {
        dados$RISCO_DAC_10_ANOS[i] <- "24%"
        
      } else if (dados$PTS_TOTAL[i] == 16) {
        dados$RISCO_DAC_10_ANOS[i] <- "27%"
        
      } else {
        dados$RISCO_DAC_10_ANOS[i] <- ">=32%"
      }
    }
} 
##==============================================================================
### Conferir as variaveis e os pontos da pessoa selecionada.
##==============================================================================
#dados$IDADE[i]
#dados$LDL[i]
#dados$HDL[i]
#dados$PAS[i]
#dados$PAD[i]
#dados$DIABETES[i]
#dados$TABAGISMO[i]
### Conferir os pontos das variáveis
#dados$PTS_IDADE[i]
#dados$PTS_LDL[i]
#dados$PTS_HDL[i]
#dados$PTS_PA[i]
#dados$PTS_DIABETES[i]
#dados$PTS_FUMO[i]
#dados$PTS_TOTAL[i]
### Conferir risco
#dados$RISCO_DAC_10_ANOS[i]
##==============================================================================
# Contar elegíveis e não elegíveis.
table(dados$ELEGIVEL)
# Contar quantos riscos estão preenchidos ou ausentes.
table(is.na(dados$RISCO_DAC_10_ANOS))
##==============================================================================
# [6] Conferir se o risco foi preenchido somente para os elegíveis.
##==============================================================================
table(Elegivel = dados$ELEGIVEL, Risco_ausente = is.na(dados$RISCO_DAC_10_ANOS))
### A leitura é:
### - Não elegíveis: nenhum risco preenchido e 596 ausentes.
### - Elegíveis: 2.404 riscos preenchidos e nenhum ausente.
##==============================================================================
# [7] Exportar o banco completo.
##==============================================================================
write.csv(
  dados,
  file = "banco_EP2A.csv",
  row.names = FALSE,
  na = "NA",
  fileEncoding = "UTF-8"
)
# Mostrar a pasta em que o arquivo foi salvo.
file.exists("banco_EP2A.csv")
getwd()
