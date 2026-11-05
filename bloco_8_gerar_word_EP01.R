#===============================================================================
# [8] GERAR O RELATORIO WORD DO EXERCICIO 1
# Cole este bloco no FINAL do script, depois do item [7].
# Execute os itens anteriores primeiro: este bloco usa os resultados calculados.
# Instale estes pacotes uma unica vez, se necessario:
# install.packages(c("officer", "flextable"))
if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)
pacman::p_load(flextable, officer)
#===============================================================================
# 8.1 Preparar copias das tabelas para apresentacao.
# As tabelas e os calculos originais nao sao alterados.
tabela_word <- tabela
names(tabela_word) <- c("Variável / medida", "Homens", "Mulheres", "p-valor")
tabela_word[[4]][is.na(tabela_word[[4]])] <- "—"

suplementar_word <- suplementar
names(suplementar_word) <- c("Variável", "Resumo principal", "Teste", "Justificativa")
#===============================================================================
# Intervalos: os percentuais sao multiplicados por 100.
# Os limites sao retirados dos objetos calculados anteriormente.
intervalos_word <- data.frame(
  Resultado = c("Média da PAD (mmHg)", "Diabetes entre classificáveis (%)",
                "Mulheres com ao menos primeiro grau (%)",
                "Média da idade de quem nunca fumou (anos)"),
  IC = c("95%", "95%", "99%", "99%"),
  Estimativa = c(media_pad, 100 * soma_casos_diabetesm / soma_total_diabetesm,
                100 * soma_primeiro_grau / soma_total_muleheres, media_idade),
  Inferior = c(ic_pad[1], 100 * ic_diabetesm[1],
               100 * ic_primeiro_grau_mulheres[1], ic_idade[1]),
  Superior = c(ic_pad[2], 100 * ic_diabetesm[2],
               100 * ic_primeiro_grau_mulheres[2], ic_idade[2])
)
# formatC() apresenta duas casas decimais, com virgula decimal.
for (j in 3:5) {
  intervalos_word[[j]] <- formatC(intervalos_word[[j]], format = "f",
                                 digits = 2, decimal.mark = ",")
}

# Os nomes das linhas de item7 viram uma coluna para aparecerem no Word.
item7_word <- data.frame(Medida = rownames(item7), item7, row.names = NULL)
for (j in 2:4) {
  item7_word[[j]] <- formatC(item7_word[[j]], format = "f",
                            digits = 2, decimal.mark = ",")
}
names(item7_word) <- c("Medida", "HbA1c (%)", "HDL — mulheres (mg/dL)",
                       "TTOG — hipertensos (mg/dL)")
#===============================================================================
# 8.2 Funcao de formatacao: recebe uma tabela de dados e devolve
# uma tabela pronta para inserir no Word. Nao realiza testes estatisticos.
# Cabecalho azul, fonte Arial, bordas discretas e largura ajustada a pagina.
formatar_tabela <- function(x) {
  ft <- flextable(x)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- bg(ft, bg = "#18354A", part = "header")
  ft <- color(ft, color = "white", part = "header")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "all")
  ft <- padding(ft, padding = 4, part = "all")
  ft <- set_table_properties(ft, layout = "autofit", width = 1,
                             opts_word = list(split = FALSE))
  # Permitir tabelas longas, mantendo o cabecalho junto as primeiras linhas.
  ft <- paginate(ft, init = FALSE, hdr_ftr = TRUE)
  return(ft)
}
#===============================================================================
# 8.3 Criar o documento e definir pagina A4 com margens de 2 cm.
# As medidas de pagina e margens sao informadas em polegadas (cm / 2,54).
doc <- read_docx()
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(width = 21 / 2.54, height = 29.7 / 2.54),
  page_margins = page_mar(top = 2 / 2.54, bottom = 2 / 2.54,
                          left = 2 / 2.54, right = 2 / 2.54)
))
# body_add_par() adiciona um paragrafo; style escolhe o estilo do Word.
doc <- body_add_par(doc, "Exercício-programa 1", style = "heading 1")
doc <- body_add_par(doc, "Análise descritiva e inferência estatística", style = "heading 2")
doc <- body_add_par(doc, "Paulo Henrique de Almeida Soares Pimenta — NUSP: 7599770")
doc <- body_add_par(doc, "Fernanda Miwa Yamasaka — NUSP: 11923084")
doc <- body_add_par(doc, paste("Participantes:", nrow(dados),
                               "| Homens:", nrow(homens), "| Mulheres:", nrow(mulheres)))
#===============================================================================
# 8.4 Tabela principal.
doc <- body_add_par(doc, "1. Características da amostra", style = "heading 1")
doc <- body_add_par(doc, paste(
  "As variáveis numéricas são apresentadas como média (desvio padrão) e",
  "mediana [P25; P75]. As categóricas são apresentadas como n (%),",
  "usando os valores conhecidos de cada variável em cada sexo.",
  "Os dados ausentes são indicados separadamente."
))
doc <- body_add_flextable(doc, value = formatar_tabela(tabela_word))
doc <- body_add_par(doc, paste(
  "Nota: testes bilaterais, com nível de significância de 5%.",
  "Welch compara médias; Mann–Whitney compara postos e não deve ser",
  "interpretado automaticamente como comparação de medianas.",
  "Há um p-valor por variável. Sexo define os grupos e não recebe teste.",
  "DP: desvio padrão. P25 e P75: percentis 25 e 75."
))
#===============================================================================
# 8.5 Tabela suplementar, em nova pagina.
doc <- body_add_break(doc)
doc <- body_add_par(doc, "2. Medidas e métodos estatísticos", style = "heading 1")
doc <- body_add_par(doc, paste(
  "A tabela identifica o resumo principal; a tabela descritiva também",
  "apresenta medidas complementares. A avaliação da distribuição combina",
  "Shapiro–Wilk e inspeção gráfica. Um teste de normalidade isolado",
  "não determina a escolha do teste de comparação."
))
doc <- body_add_flextable(doc, value = formatar_tabela(suplementar_word))
#===============================================================================
# 8.6 Intervalos de confianca, usando os valores calculados nos itens 5 e 6.
doc <- body_add_break(doc)
doc <- body_add_par(doc, "3. Intervalos de confiança", style = "heading 1")
doc <- body_add_flextable(doc, value = formatar_tabela(intervalos_word))
doc <- body_add_par(doc, paste(
  "Os intervalos das médias utilizam a distribuição t.",
  "Os intervalos das proporções são de Wilson, sem correção de continuidade.",
  "Para diabetes, o denominador inclui somente participantes classificáveis;",
  "para escolaridade, mulheres com informação conhecida.",
  "As categorias Primeiro Grau, Segundo Grau e Graduado foram interpretadas",
  "como ao menos primeiro grau completo."
))
#===============================================================================
# 8.7 Estatisticas solicitadas no item 7.
doc <- body_add_par(doc, "4. Estatísticas descritivas adicionais", style = "heading 1")
doc <- body_add_flextable(doc, value = formatar_tabela(item7_word))
doc <- body_add_par(doc, paste(
  "N representa observações válidas. Média, quantis e dispersão excluem",
  "valores ausentes. Mediana e P50 são apresentados conforme o enunciado.",
  "DP e variância são amostrais; os quantis usam o padrão type = 7 do R.",
  "A variância é expressa no quadrado da unidade da variável."
))
doc <- body_add_par(doc, "Fonte e observações", style = "heading 2")
doc <- body_add_par(doc, paste(
  "Fonte: banco de dados fornecido para as aulas de 2026 e enunciado do",
  "exercício-programa 1. Hipertensão e diabetes foram classificados pelos",
  "critérios do exercício, preservando valores desconhecidos.",
  "Resultados não significativos não demonstram equivalência entre grupos.",
  "A generalização depende do desenho amostral e do mecanismo de ausência dos dados."
))
#===============================================================================
# 8.8 Salvar. print() grava o objeto doc como arquivo .docx.
# Feche o arquivo no Word antes de executar novamente esta linha.
print(doc, target = "EP01_relatorio.docx")
file.exists("EP01_relatorio.docx")
getwd()
# Revise a paginacao no Word antes de imprimir ou entregar.
