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
pacman::p_load(dplyr, readxl, officer, flextable)
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
#head(dados[, c("IDADE", "SEXO", "DIABETES", "HPDAC", "LDL", "HDL", "META_LDL", "META_HDL", "DLP_NA_META")], n = 50)
#===============================================================================
# [10] Definir a conduta para dislipidemia.
#===============================================================================
# Inicialmente, deixar a conduta de dislipidemia desconhecida.
dados$CONDUTA_DLP <- NA
#===============================================================================
# [10.1] Dentro das duas metas: "manter conduta"
#===============================================================================
dados$CONDUTA_DLP[which(dados$DLP_NA_META == TRUE)] <- "manter conduta"
#===============================================================================
# [10.2] Excesso de LDL > 30 OU déficit de HDL > 10: "introduzir medicamento"
#===============================================================================
dados$CONDUTA_DLP[which(
  dados$DLP_NA_META == FALSE &
  ((dados$LDL - dados$META_LDL > 30) |
    (dados$META_HDL - dados$HDL > 10))
)] <- "introduzir medicamento"
#===============================================================================
# [10.3] Fora da meta, com excesso de LDL <= 30 e déficit de HDL <= 10. "orientar dieta e atividade física"
#===============================================================================
dados$CONDUTA_DLP[which(
  dados$DLP_NA_META == FALSE &
    (dados$LDL - dados$META_LDL <= 30) &
    (dados$META_HDL - dados$HDL <= 10)
)] <- "orientar dieta e atividade física"
#===============================================================================
# Conferência dos resultados das orientações
#===============================================================================
head(dados[, c("LDL", "HDL", "META_LDL", "META_HDL", "DLP_NA_META", "CONDUTA_DLP")], n = 50)
table(dados$CONDUTA_DLP, useNA = "ifany")
#===============================================================================
# [11] Conferir o banco e as três condutas.
#===============================================================================
# Confirmar que todos os participantes continuam no banco.
nrow(dados)
# Conferir cada conduta, incluindo os valores ausentes.
table(dados$CONDUTA_HAS, useNA = "ifany")
table(dados$CONDUTA_DM, useNA = "ifany")
table(dados$CONDUTA_DLP, useNA = "ifany")
#===============================================================================
# [12] Exportar o banco original com as novas variáveis.
#===============================================================================
write.csv(
  dados,
  file = "banco_EP2B.csv",
  row.names = FALSE,
  na = "NA",
  fileEncoding = "UTF-8"
)
# Confirmar que o arquivo foi criado.
file.exists("banco_EP2B.csv")
# Mostrar a pasta onde ele foi salvo.
getwd()

#===============================================================================
#===============================================================================
# [13] Gerar relatorio Word com as metas e as condutas do exercicio 2 B.
# Execute depois dos passos anteriores, usando o objeto dados ja calculado.
#===============================================================================

# Funcao: recebe o banco calculado e o nome do arquivo Word.
# Apenas apresenta os resultados; nao altera dados, metas ou condutas.
gerar_word_ep2b <- function(dados, arquivo = "EP02B_relatorio.docx") {
  
  # 13.1 Contar os diagnosticos. Ausentes entram como uma categoria separada.
  diagnosticos <- data.frame(
    Diagnostico = c("Hipertensão", "Diabetes"),
    Sim = c(sum(dados$HIPERTENSAO == "Sim", na.rm = TRUE),
            sum(dados$DIABETES == "Sim", na.rm = TRUE)),
    Nao = c(sum(dados$HIPERTENSAO == "Não", na.rm = TRUE),
            sum(dados$DIABETES == "Não", na.rm = TRUE)),
    Ausentes = c(sum(is.na(dados$HIPERTENSAO)), sum(is.na(dados$DIABETES)))
  )
  names(diagnosticos) <- c("Diagnóstico", "Sim", "Não", "Ausentes")
  
  # 13.2 Tabelas explicativas das metas usadas no codigo.
  metas_pa <- data.frame(
    Grupo = c("Com diabetes, em qualquer idade", "Sem diabetes, idade < 60 anos",
              "Sem diabetes, idade >= 60 anos"),
    PAS = c("< 130", "< 140", "< 150"),
    PAD = c("< 80", "< 90", "< 90")
  )
  names(metas_pa) <- c("Grupo", "PAS (mmHg)", "PAD (mmHg)")
  metas_lipidios <- data.frame(
    Sexo = c("Homens", "Mulheres", "Homens", "Mulheres"),
    Condicao = c("Diabetes ou história de DAC", "Diabetes ou história de DAC",
                 "Sem diabetes e sem história de DAC", "Sem diabetes e sem história de DAC"),
    LDL = c("< 100", "< 100", "< 130", "< 130"),
    HDL = c("> 40", "> 50", "> 40", "> 50")
  )
  names(metas_lipidios) <- c("Sexo", "Condição", "LDL (mg/dL)", "HDL (mg/dL)")
  
  # 13.3 Contar TRUE, FALSE e NA nas verificacoes das metas.
  colunas_meta <- c("PA_NA_META", "DM_NA_META", "DLP_NA_META")
  cumprimento <- data.frame(
    Avaliacao = c("Pressão arterial", "Diabetes", "Dislipidemia"),
    Dentro = 0, Fora = 0, Indeterminado = 0
  )
  for (j in seq_along(colunas_meta)) {
    x <- dados[[colunas_meta[j]]]
    cumprimento$Dentro[j] <- sum(x == TRUE, na.rm = TRUE)
    cumprimento$Fora[j] <- sum(x == FALSE, na.rm = TRUE)
    cumprimento$Indeterminado[j] <- sum(is.na(x))
  }
  names(cumprimento) <- c("Avaliação", "Dentro da meta", "Fora da meta", "Indeterminado")
  
  # 13.4 Resumir as condutas em n (%), sobre todos os participantes.
  # sprintf() formata a contagem e o percentual com uma casa decimal.
  categorias <- c("manter conduta", "introduzir medicamento", "aumentar dose",
                  "orientar dieta e atividade física")
  condutas <- data.frame(
    Conduta = c("Manter conduta", "Introduzir medicamento", "Aumentar dose",
                "Orientar dieta e atividade física", "Ausente (NA)", "Total"),
    HAS = "", DM = "", DLP = ""
  )
  colunas_conduta <- c("CONDUTA_HAS", "CONDUTA_DM", "CONDUTA_DLP")
  n_total <- nrow(dados)
  for (j in seq_along(colunas_conduta)) {
    x <- dados[[colunas_conduta[j]]]
    for (k in seq_along(categorias)) {
      n <- sum(x == categorias[k], na.rm = TRUE)
      condutas[k, j + 1] <- sprintf("%d (%.1f%%)", n, 100 * n / n_total)
    }
    n_ausentes <- sum(is.na(x))
    condutas[5, j + 1] <- sprintf("%d (%.1f%%)", n_ausentes, 100 * n_ausentes / n_total)
    condutas[6, j + 1] <- sprintf("%d (100.0%%)", n_total)
  }
  # Estas acoes nao fazem parte das regras das respectivas condutas.
  condutas[4, 2:3] <- "Não se aplica"
  condutas[3, 4] <- "Não se aplica"
  names(condutas) <- c("Conduta", "Hipertensão n (%)", "Diabetes n (%)", "Dislipidemia n (%)")
  
  # 13.5 Conferir a correspondencia entre metas e condutas por contagens.
  # Zero e o resultado esperado em cada uma das duas colunas de divergencia.
  verificacao <- data.frame(
    Avaliacao = c("Hipertensão", "Diabetes", "Dislipidemia"),
    Dentro_sem_manter = 0, Manter_sem_meta_confirmada = 0
  )
  for (j in seq_along(colunas_meta)) {
    meta <- dados[[colunas_meta[j]]]
    acao <- dados[[colunas_conduta[j]]]
    verificacao$Dentro_sem_manter[j] <- sum(
      meta == TRUE & (is.na(acao) | acao != "manter conduta"), na.rm = TRUE)
    verificacao$Manter_sem_meta_confirmada[j] <- sum(
      acao == "manter conduta" & (is.na(meta) | meta == FALSE), na.rm = TRUE)
  }
  names(verificacao) <- c("Avaliação", "Dentro da meta sem manter conduta",
                          "Manter conduta sem meta confirmada")
  
  # Funcao auxiliar: recebe uma tabela e devolve uma tabela Word formatada.
  # Nao realiza calculos: apenas define fonte, cabecalho e largura.
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
  
  # 13.6 Criar documento A4, margens de 2 cm. Medidas convertidas em polegadas.
  doc <- officer::read_docx()
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 21 / 2.54, height = 29.7 / 2.54),
    page_margins = officer::page_mar(top = 2 / 2.54, bottom = 2 / 2.54,
                                     left = 2 / 2.54, right = 2 / 2.54)))
  # heading 1 existe no modelo utilizado; evita depender do estilo Title.
  doc <- officer::body_add_par(doc, "Condutas para hipertensão diabetes e dislipidemia", style = "heading 1")
  doc <- officer::body_add_par(doc, "Exercício programa 2 versão B", style = "heading 2")
  doc <- officer::body_add_par(doc, "Paulo Henrique de Almeida Soares Pimenta — NUSP 7599770")
  doc <- officer::body_add_par(doc, "Fernanda Miwa Yamasaka — NUSP 11923084")
  doc <- officer::body_add_par(doc, "Rebeca Benevides Pinto — NUSP 15305032")
  
  # Adicionar uma linha em branco.
  doc <- officer::body_add_par(doc, "")
  
  doc <- officer::body_add_par(doc, paste(
    "Classificamos as condutas dos", n_total, "participantes segundo as regras da versão B.",
    "Foram criadas CONDUTA_HAS, CONDUTA_DM e CONDUTA_DLP, preservando todos os registros.",
    "As frequências e os percentuais abaixo são calculados diretamente do banco ao gerar o relatório."))
  doc <- officer::body_add_par(doc, "Tabela 1 Diagnósticos derivados", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(diagnosticos))
  doc <- officer::body_add_par(doc, paste(
    "Hipertensão: tratamento, PAS >= 140 ou PAD >= 90. Diabetes: tratamento, GLIC >= 126,",
    "HBGLIC >= 6,5 ou TTOG >= 200. Um critério positivo conhecido basta para Sim.",
    "A classificação como Não exige todos os critérios negativos conhecidos; demais situações permanecem NA."))
  
  doc <- officer::body_add_par(doc, "Metas e regras de classificação", style = "heading 1")
  doc <- officer::body_add_par(doc, "Tabela 2 Metas de pressão arterial", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(metas_pa))
  doc <- officer::body_add_par(doc, paste(
    "Conforme esclarecimento do professor, a última faixa inclui exatamente 60 anos (IDADE >= 60).",
    "Ambas as pressões devem ser menores que os limites. Dentro da meta: manter conduta.",
    "Fora da meta: introduzir medicamento se HASTRAT = Não; aumentar dose se HASTRAT = Sim."))
  doc <- officer::body_add_par(doc, "Meta para diabetes", style = "heading 2")
  doc <- officer::body_add_par(doc, paste(
    "Exigimos simultaneamente GLIC < 130 mg/dL e HBGLIC < 7% para estar dentro da meta.",
    "A regra é aplicada a todos os participantes, independentemente do diagnóstico.",
    "Dentro da meta: manter conduta. Fora: introduzir medicamento se DIABTRAT = Não",
    "ou aumentar dose se DIABTRAT = Sim."))
  doc <- officer::body_add_par(doc, "Tabela 3 Metas de LDL e HDL", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(metas_lipidios))
  doc <- officer::body_add_par(doc, paste(
    "As duas metas devem ser atendidas para manter conduta. Fora da meta, introduzir medicamento",
    "se LDL exceder seu limite em mais de 30 mg/dL ou HDL estiver mais de 10 mg/dL abaixo do limite.",
    "Caso ambos os desvios sejam de no máximo 30 e 10, respectivamente, orientar dieta e atividade física.",
    "Os desvios preservam a direção: LDL - META_LDL e META_HDL - HDL; não se usa valor absoluto."))
  
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Resultados das classificações", style = "heading 1")
  doc <- officer::body_add_par(doc, "Tabela 4 Cumprimento das metas", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(cumprimento))
  doc <- officer::body_add_par(doc, "Indeterminado corresponde a NA na verificação da meta. Cada linha totaliza o número de participantes.")
  doc <- officer::body_add_par(doc, "Tabela 5 Distribuição das condutas", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(condutas))
  doc <- officer::body_add_par(doc, paste(
    "Os percentuais usam todos os", n_total, "participantes, incluindo os ausentes.",
    "As somas percentuais podem variar ligeiramente de 100% por arredondamento.",
    "Não se aplica identifica ações não previstas para aquela condição."))
  doc <- officer::body_add_par(doc, "Tabela 6 Conferência das metas e da manutenção da conduta", style = "heading 2")
  doc <- flextable::body_add_flextable(doc, formatar(verificacao))
  doc <- officer::body_add_par(doc, "Espera-se zero nas duas colunas de divergência. Esta conferência avalia a correspondência entre estar na meta e manter conduta.")
  
  doc <- officer::body_add_par(doc, "Tratamento das informações ausentes", style = "heading 1")
  doc <- officer::body_add_par(doc, paste(
    "Ausência de informação não foi convertida em resposta negativa. Para hipertensão e diabetes,",
    "estar dentro da meta permite manter conduta mesmo sem conhecer o tratamento atual.",
    "Fora da meta, tratamento desconhecido impede escolher entre introduzir medicamento e aumentar dose."))
  doc <- officer::body_add_par(doc, paste(
    "Para dislipidemia, um desvio grave conhecido permite introduzir medicamento mesmo se o outro",
    "componente estiver desconhecido. A orientação de dieta exige estar fora da meta e conhecer",
    "que ambos os desvios não ultrapassam os limites. As metas são determinadas antes das comparações;",
    "quando não podem ser definidas, permanecem NA. O código não avalia todas as metas alternativas possíveis."))
  doc <- officer::body_add_par(doc, "Fonte e arquivo individual", style = "heading 2")
  doc <- officer::body_add_par(doc, paste(
    "Fonte: Banco aulas 2026.xlsx e enunciado do exercício-programa 2, versão B, com a correção",
    "da faixa etária confirmada pelo professor. As regras são as do exercício.",
    "O arquivo banco_EP2B.csv contém o banco completo com os resultados individuais, metas e condutas."))
  
  # Gravar o Word na pasta de trabalho. Feche-o antes de gerar novamente.
  print(doc, target = arquivo)
  return(invisible(arquivo))
}

# Executar a funcao depois de calcular e exportar o banco.
gerar_word_ep2b(dados)
file.exists("EP02B_relatorio.docx")
getwd()