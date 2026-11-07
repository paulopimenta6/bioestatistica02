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
pacman::p_load(dplyr, readxl, officer, flextable)
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

#===============================================================================
#===============================================================================
# [8] Criar o relatorio Word com as tabelas e os resultados do exercicio 2 A.
# Execute apos os passos anteriores. O objeto dados ja deve estar calculado.
#===============================================================================

# Funcao: recebe o banco calculado e o nome do Word a salvar.
# Apenas resume e apresenta os resultados; nao modifica o banco nem o escore.
gerar_word_ep2a <- function(dados, arquivo = "EP02A_relatorio.docx") {
  
  # 8.1 Resumo da amostra. sum() conta as condicoes verdadeiras.
  resumo <- data.frame(
    Situacao = c("Total de participantes", "Elegíveis", "Não elegíveis",
                 "Risco calculado", "Risco ausente"),
    N = c(nrow(dados), sum(dados$ELEGIVEL, na.rm = TRUE),
          sum(!dados$ELEGIVEL, na.rm = TRUE),
          sum(!is.na(dados$RISCO_DAC_10_ANOS)),
          sum(is.na(dados$RISCO_DAC_10_ANOS)))
  )
  resumo$Percentual <- sprintf("%.1f%%", 100 * resumo$N / nrow(dados))
  
  # 8.2 Contar cada criterio de exclusao separadamente.
  # Uma pessoa pode aparecer em mais de uma linha; estas contagens nao se somam.
  faltantes <- is.na(dados$SEXO) | is.na(dados$IDADE) |
    is.na(dados$DIABETES) | is.na(dados$TABAGISMO) |
    is.na(dados$PAS) | is.na(dados$PAD) | is.na(dados$LDL) | is.na(dados$HDL)
  exclusoes <- data.frame(
    Criterio = c("Idade menor que 30 anos", "Idade maior que 74 anos",
                 "História de DAC positiva ou desconhecida",
                 "Ausência de pelo menos um preditor necessário"),
    N = c(sum(dados$IDADE < 30, na.rm = TRUE),
          sum(dados$IDADE > 74, na.rm = TRUE),
          sum(dados$HPDAC == "Sim" | is.na(dados$HPDAC), na.rm = TRUE),
          sum(faltantes))
  )
  # Detalhar quais preditores estao ausentes.
  variaveis <- c("SEXO", "IDADE", "DIABETES", "TABAGISMO", "PAS", "PAD", "LDL", "HDL")
  ausencias <- data.frame(Variavel = variaveis, Ausentes = 0)
  for (j in seq_along(variaveis)) {
    ausencias$Ausentes[j] <- sum(is.na(dados[[variaveis[j]]]))
  }
  
  # 8.3 Distribuicao dos riscos em ordem numerica, preservando os limites >=.
  categorias <- c("1%", "2%", "3%", "4%", "5%", "6%", "7%", "8%", "9%",
                  "11%", "13%", "14%", "15%", "17%", "18%", "20%", "22%",
                  "24%", "27%", "33%", "40%", "47%", ">=32%", ">=56%")
  riscos <- data.frame(Risco = categorias, Homens = 0, Mulheres = 0, Total = 0)
  for (j in seq_along(categorias)) {
    riscos$Homens[j] <- sum(dados$RISCO_DAC_10_ANOS == categorias[j] &
                              dados$SEXO == "M", na.rm = TRUE)
    riscos$Mulheres[j] <- sum(dados$RISCO_DAC_10_ANOS == categorias[j] &
                                dados$SEXO == "F", na.rm = TRUE)
    riscos$Total[j] <- sum(dados$RISCO_DAC_10_ANOS == categorias[j], na.rm = TRUE)
  }
  n_calculados <- sum(!is.na(dados$RISCO_DAC_10_ANOS))
  riscos$Percentual <- NA_character_
  if (n_calculados > 0) {
    riscos$Percentual <- sprintf("%.1f%%", 100 * riscos$Total / n_calculados)
  }
  
  # 8.4 Conferencia cruzada: cada linha corresponde a uma situacao.
  conferencia <- data.frame(
    Elegibilidade = c("Não elegível", "Elegível"),
    Risco_preenchido = c(
      sum(!dados$ELEGIVEL & !is.na(dados$RISCO_DAC_10_ANOS), na.rm = TRUE),
      sum(dados$ELEGIVEL & !is.na(dados$RISCO_DAC_10_ANOS), na.rm = TRUE)),
    Risco_ausente = c(
      sum(!dados$ELEGIVEL & is.na(dados$RISCO_DAC_10_ANOS), na.rm = TRUE),
      sum(dados$ELEGIVEL & is.na(dados$RISCO_DAC_10_ANOS), na.rm = TRUE))
  )
  
  # Funcao auxiliar: recebe uma tabela e devolve sua versao formatada.
  # Usa fonte Arial, cabecalho destacado e largura ajustada a pagina.
  formatar <- function(tabela) {
    ft <- flextable::flextable(tabela)
    ft <- flextable::theme_booktabs(ft)
    ft <- flextable::font(ft, fontname = "Arial", part = "all")
    ft <- flextable::fontsize(ft, size = 10, part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::bg(ft, bg = "#E8EEF3", part = "header")
    ft <- flextable::padding(ft, padding = 4, part = "all")
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1,
                                          opts_word = list(split = FALSE))
    ft <- flextable::paginate(ft, init = FALSE, hdr_ftr = TRUE)
    return(ft)
  }
  
  # 8.5 Criar documento A4, margens de 2 cm (medidas em polegadas).
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 21 / 2.54, height = 29.7 / 2.54),
    page_margins = officer::page_mar(top = 2 / 2.54, bottom = 2 / 2.54,
                                     left = 2 / 2.54, right = 2 / 2.54)))
  doc <- officer::body_add_par(doc, "Risco de doença arterial coronariana em dez anos", style = "heading 1")
  doc <- officer::body_add_par(doc, "Exercício programa 2 versão A", style = "heading 1")
  doc <- officer::body_add_par(doc, "Paulo Henrique de Almeida Soares Pimenta — NUSP 7599770")
  doc <- officer::body_add_par(doc, "Fernanda Miwa Yamasaka — NUSP 11923084")
  doc <- officer::body_add_par(doc, "Rebeca Benevides Pinto — NUSP 15305032")
  
  # Adicionar uma linha em branco.
  doc <- officer::body_add_par(doc, "")
  
  doc <- officer::body_add_par(doc, paste(
    "Calculamos o risco de DAC em dez anos pela tabela LDL Pts de Wilson et al. (1998).",
    "Dos", nrow(dados), "participantes,", n_calculados,
    "tiveram risco calculado e", sum(is.na(dados$RISCO_DAC_10_ANOS)),
    "permaneceram com risco ausente. Todos os participantes foram mantidos no banco."))
  doc <- officer::body_add_par(doc, "Método e critérios", style = "heading 1")
  doc <- officer::body_add_par(doc, paste(
    "Usamos os pontos LDL Pts, fora dos colchetes, conforme sexo, idade, LDL, HDL,",
    "pressão arterial, diabetes e tabagismo atual. Para a pressão, prevalece a maior",
    "pontuação indicada por PAS ou PAD. A soma foi convertida em risco pela tabela de cada sexo.",
    "Diabetes foi definido pelos critérios do exercício. Não imputamos dados ausentes."))
  doc <- officer::body_add_par(doc, paste(
    "O risco não foi calculado para idade inferior a 30 ou superior a 74 anos, história",
    "de DAC positiva ou desconhecida ou ausência de qualquer preditor necessário.",
    "Os limites de idade 30 e 74 foram incluídos."))
  doc <- officer::body_add_par(doc, "Tabela 1 Composição da amostra", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(resumo))
  doc <- officer::body_add_par(doc, "Percentuais calculados sobre todos os participantes.")
  doc <- officer::body_add_par(doc, "Tabela 2 Critérios de exclusão", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(exclusoes))
  doc <- officer::body_add_par(doc, "As contagens se sobrepõem: uma pessoa pode atender a mais de um critério. Não somar as linhas.")
  doc <- officer::body_add_par(doc, "Tabela 3 Ausências nos preditores", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(ausencias))
  
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Tabela 4 Distribuição do risco em dez anos", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, formatar(riscos))
  doc <- officer::body_add_par(doc, paste(
    "Homens, Mulheres e Total são contagens. Percentual usa somente participantes com risco calculado.",
    "As categorias >=32% (mulheres) e >=56% (homens) representam limites inferiores, não riscos exatos.",
    "Por esse motivo, não calculamos uma média tratando esses limites como valores exatos."))
  doc <- officer::body_add_par(doc, "Tabela 5 Elegibilidade e preenchimento do risco", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(conferencia))
  doc <- officer::body_add_par(doc, paste(
    "A conferência deve apresentar zero risco preenchido entre não elegíveis e zero risco ausente",
    "entre elegíveis. O banco individual completo está no arquivo banco_EP2A.csv."))
  doc <- officer::body_add_par(doc, "Convenções e referências", style = "heading 1")
  doc <- officer::body_add_par(doc, paste(
    "O código inclui o total masculino -3 na categoria de risco de 1%, adotando <=-3 para fechar",
    "a lacuna da tabela, que imprime <-3 seguido de -2. Para LDL, 190 pertence à categoria >=190,",
    "coerentemente com a Tabela 7 do artigo. Essas convenções foram preservadas no relatório."))
  doc <- officer::body_add_par(doc, "Wilson PWF et al. Prediction of coronary heart disease using risk factor categories. Circulation. 1998;97:1837–1847. DOI: 10.1161/01.CIR.97.18.1837.")
  doc <- officer::body_add_par(doc, "Framingham Heart Study. Coronary Heart Disease (10-year risk), LDL Pts. https://www.framinghamheartstudy.org/fhs-for-researchers/fhs-risk-functions/coronary-heart-disease-10-year-risk/")
  doc <- officer::body_add_par(doc, "Fonte dos dados e das regras de exclusão: banco das aulas e enunciado do exercício-programa 2 A.")
  
  # print() grava o documento. Feche o Word antes de gerar novamente.
  print(doc, target = arquivo)
  return(invisible(arquivo))
}

# Chamar a funcao depois de calcular e exportar o banco.
gerar_word_ep2a(dados)
file.exists("EP02A_relatorio.docx")
getwd()