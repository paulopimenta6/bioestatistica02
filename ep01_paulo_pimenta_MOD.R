# Paulo Pimenta
# Bioestatística II — EP1 — 2026
#===============================================================================
### Leitura do banco no diretório do repositório
# Execute este script com o arquivo "Banco aulas 2026.xlsx" na mesma pasta.
#===============================================================================
### Criando diretórios para salvar os resultados
#===============================================================================
### Criando diretórios para armazenar os resultados obtidos
dir.create("resultados", showWarnings = FALSE)
dir.create("resultados/graficos", showWarnings = FALSE)
dir.create("resultados/tabelas", showWarnings = FALSE)
#===============================================================================

#===============================================================================
### Carregando bibliotecas necessárias
library(readxl)
#===============================================================================

#===============================================================================
# [1] Leitura do banco
dados <- as.data.frame(
  readxl::read_excel("Banco aulas 2026.xlsx", sheet = 1)
)

##==============================================================================
### Analise dos dados antes da tabela ter os dados de hipertensao e diabetes 
### criados
##==============================================================================
dim(dados)
summary(dados)
### Ajuste em algumas variaveis que serão usadas em exercícios futuros
#unique(dados$ESCOLARIDADE)
dados$ESCOLARIDADE <- factor(dados$ESCOLARIDADE, levels = c("Sem escola formal", "Primeiro Grau", "Segundo Grau",
                                                            "Graduado"), ordered = TRUE)
dados$SEXO <- factor(dados$SEXO)
#===============================================================================

#===============================================================================
# [2] Criando as variáveis diabetes e hipertensão usando ifelse 
### Quando uma variável atender o requisito com | "ou", logo será sim, caso 
### contrário sera não. Em casos em que ocorra NA e nenhum criterio seja aceito
### sera NA, pois o "Não" usa o condicional $ "e".
dados$HIPERTENSAO <- ifelse(
  dados$HASTRAT == "Sim" | dados$PAS >= 140 | dados$PAD >= 90,
  "Sim", "Não"
)
dados$DIABETES <- ifelse(
  dados$DIABTRAT == "Sim" | dados$GLIC >= 126 |
    dados$HBGLIC >= 6.5 | dados$TTOG >= 200,
  "Sim", "Não"
)

##==============================================================================
### Analise dos dados depois da tabela ter os dados de hipertensao e diabetes 
### criados
##==============================================================================
dim(dados)
summary(dados)
#===============================================================================

#===============================================================================
# [3] Criando tabela descritiva
homens <- subset(dados, SEXO == "M")
mulheres <- subset(dados, SEXO == "F")
var_numec <- c("IDADE", "PAS", "PAD", "GLIC", "HBGLIC", "TTOG",
               "LDL", "HDL", "PCR")

#===============================================================================
### Para variáveis numéricas é preciso fazer as verificações de normalidade e 
### como os dados são abaixo de 5000 usarei, inicialmente o tste de Shapiro-Wilk
#===============================================================================

normalidade <- data.frame()
for (v in var_numec) {
  p_homens <- shapiro.test(na.omit(homens[[v]]))$p.value
  p_mulheres <- shapiro.test(na.omit(mulheres[[v]]))$p.value
  normalidade <- rbind(normalidade,
                       data.frame(Variavel = v, p.value.Homens = p_homens, p.value.Mulheres = p_mulheres))
}
#===============================================================================
### p-valor menor que 0,05: rejeitamos H0, indicando evidência contra a normalidade.
### p-valor maior ou igual a 0,05: não rejeitamos H0, mas isso não comprova que a distribuição é normal.
#===============================================================================
# Exibindo a normalidade
print(normalidade)

#===============================================================================
### Variavel p.value.Homens p.value.Mulheres
### 1    IDADE   5.862784e-22     6.862515e-22
### 2      PAS   1.309503e-02     2.622557e-08
### 3      PAD   9.355229e-02     2.845908e-04
### 4     GLIC   1.406795e-14     2.067260e-17
### 5   HBGLIC   2.612906e-05     1.808162e-05
### 6     TTOG   1.120463e-08     4.490145e-10
### 7      LDL   2.012727e-01     1.428855e-01
### 8      HDL   1.377695e-02     3.129716e-03
### 9      PCR   8.182283e-44     8.146625e-44
#===============================================================================

#===============================================================================
### Histogramas e Q-Q plot
### as classes usam por padrão o método de Sturges)
#===============================================================================
for (v in var_numec) {
  
  # Dados sem valores ausentes
  xh <- na.omit(homens[[v]])
  xm <- na.omit(mulheres[[v]])
  
  # Verificar se existem dados suficientes
  if (length(xh) < 2 || length(xm) < 2 ||
      sd(xh) == 0 || sd(xm) == 0) {
    next
  }
  
  # Limites comuns para homens e mulheres
  limites <- range(c(xh, xm))
  folga <- 0.08 * diff(limites)
  limite_x <- limites + c(-folga, folga)
  
  # Classes comuns dos histogramas
  #classes <- pretty(limites, n = 12)
  classes <- pretty(limites,
                    n = nclass.Sturges(c(xh, xm))) #Usarei Sturges
  
  # Densidades estimadas
  dh <- density(xh)
  dm <- density(xm)
  
  # Altura máxima para os histogramas
  limite_y <- 1.15 * max(
    hist(xh, breaks = classes, plot = FALSE)$density,
    hist(xm, breaks = classes, plot = FALSE)$density,
    dh$y, dm$y,
    dnorm(mean(xh), mean(xh), sd(xh)),
    dnorm(mean(xm), mean(xm), sd(xm))
  )


  # Organização: 4 gráficos + 1 legenda
  layout(matrix(c(1, 2, 3, 4, 5, 5),
                nrow = 3, byrow = TRUE),
         heights = c(1, 1, 0.25))
  
  par(mar = c(4, 4, 2.5, 1),
      mgp = c(2.2, 0.7, 0),
      cex.axis = 0.85,
      cex.lab = 0.9,
      cex.main = 0.9)
  
  ### HISTOGRAMA - HOMENS
  hist(xh,
       breaks = classes,
       probability = TRUE,
       xlim = limite_x,
       ylim = c(0, limite_y),
       main = paste(v, "- Homens"),
       xlab = v,
       ylab = "Densidade",
       col = "lightgray",
       border = "white")
  
  lines(dh, col = "red", lwd = 2)
  
  curve(dnorm(x, mean = mean(xh), sd = sd(xh)),
        add = TRUE, col = "blue", lwd = 2)
  
  ### Q-Q PLOT - HOMENS
  qqnorm(xh,
         main = paste("Q-Q", v, "- Homens"),
         pch = 1,
         cex = 0.6)
  
  qqline(xh, col = "red", lwd = 2)
  
  ### HISTOGRAMA - MULHERES
  hist(xm,
       breaks = classes,
       probability = TRUE,
       xlim = limite_x,
       ylim = c(0, limite_y),
       main = paste(v, "- Mulheres"),
       xlab = v,
       ylab = "Densidade",
       col = "lightgray",
       border = "white")
  
  lines(dm, col = "red", lwd = 2)
  
  curve(dnorm(x, mean = mean(xm), sd = sd(xm)),
        add = TRUE, col = "blue", lwd = 2)
  
  ### Q-Q PLOT - MULHERES
  qqnorm(xm,
         main = paste("Q-Q", v, "- Mulheres"),
         pch = 1,
         cex = 0.6)
  
  qqline(xm, col = "red", lwd = 2)
  
  ### LEGENDA COMPARTILHADA
  par(mar = c(0, 0, 0, 0))
  
  plot.new()
  
  legend("center",
         legend = c("Densidade estimada",
                    "Normal teórica ajustada"),
         col = c("red", "blue"),
         lwd = 2,
         horiz = TRUE,
         bty = "n",
         cex = 0.85)

  }

# Restaurar layout padrão
layout(1)
par(mfrow = c(1, 1))

#===============================================================================
### Somente LDL tem p >= 0,05 nos dois sexos: 0,20127 e 0,14289.
### Nas demais variaveis, pelo menos um grupo tem p < 0,05.
### Shapiro e sensivel com n grande. Não rejeitar nao prova normalidade.
### Histogramas e Q-Q complementam Shapiro. Nenhum extremo sera excluido.
### Histogramas e Q-Q plot apresentaram características de quasi-normalidade em alguns grupos 
### Vou considerar PAS e PAD também como sendo normais devido as formas dos gráficos
### ...e dos gráficos Q-Q plot
#===============================================================================

#===============================================================================
### Teste parametrico
#===============================================================================
# Testes bilaterais; alfa = 0,05; participantes independentes.
# As escolhas abaixo sao especificas deste banco e dos diagnosticos acima.
# Teste t de Welch (nao exige variancias iguais).
### LDL
# p-value = 0,4942. Nao foi detectada diferenca entre as medias de LDL.
# Aceita-se H0
teste_ldl <- t.test(homens$LDL, mulheres$LDL, paired = FALSE)
print(teste_ldl)
### PAS
# p-value < 2.2e-16. Foi detecada diferença entre as medias de PAS.
# Rejeita-se H0
teste_pas <- t.test(homens$PAS, mulheres$PAS, paired = FALSE)
print(teste_pas)
### PAD
# p-value < 2.2e-16. Foi detecada diferença entre as medias de PAD.
# Rejeita-se H0
teste_pad <- t.test(homens$PAD, mulheres$PAD, paired = FALSE)
print(teste_pad)
#===============================================================================
### Teste não parametrico
#===============================================================================
### Demais numericas: Wilcox-Mann-Whitney, chamado wilcox.test() no R.
### exact = FALSE usa aproximacao com ajuste para empates.
### correct = TRUE aplica correcao de continuidade.
#===============================================================================
### Data frame com os p-values dos metodos não paramtricos
#===============================================================================
p_values_numericas <- data.frame(
  LDL = teste_ldl$p.value, 
  PAS = teste_pas$p.value, 
  PAD = teste_pad$p.value,
  IDADE = wilcox.test(IDADE ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value,
  GLIC = wilcox.test(GLIC ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value,
  HBGLIC = wilcox.test(HBGLIC ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value,
  TTOG = wilcox.test(TTOG ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value,
  HDL = wilcox.test(HDL ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value,
  PCR = wilcox.test(PCR ~ SEXO, dados, exact = FALSE, correct = TRUE)$p.value
)
print(p_values_numericas)
#===============================================================================
###   LDL         PAS          PAD     IDADE     GLIC    HBGLIC      TTOG           HDL       PCR
### 0.4942263 9.45904e-99 6.853623e-95 0.9984182 0.236411 0.2464579 0.2440649 5.586307e-276 0.8386926
#===============================================================================
### Nos métodos não-paramétricos:
### HDL: p < 0,001, ou seja, há diferença entre as distribuições de HDL entre homens e mulheres. 
### ...Demais numericas: p >= 0,05 não apresentando diferença.
### Mann-Whitney compara distribuicoes/postos/ranks e so pode ser interpretado 
###... como comparacao de medianas sob condicoes adicionais de formas semelhantes.
### Testes removem casos sem a variavel analisada ou sem SEXO, nao a pessoa
### de todas as outras analises. Nao usamos na.rm dentro de t.test/wilcox.test.
#===============================================================================

#===============================================================================
### Uma funcao pequena evita repetir a formatacao da mesma tabela nove vezes.
### Esta funcao apenas resume os dados: nao escolhe nem executa testes.
#===============================================================================
# Calcula os dois resumos para uma variável numérica
resumir <- function(x) {
  
  media <- mean(x, na.rm = TRUE)
  desvio <- sd(x, na.rm = TRUE)
  
  quartis <- quantile(
    x,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE,
    names = FALSE
  )
  
  c(
    media_dp = sprintf("%.2f (%.2f)", media, desvio),
    mediana_quartis = sprintf(
      "%.2f [%.2f; %.2f]",
      quartis[2], quartis[1], quartis[3]
    )
  )
}

### 3a Monta a tabela com quatro colunas
tabela <- data.frame()

for (v in var_numec) {
  
  resumo_homens <- resumir(homens[[v]])
  resumo_mulheres <- resumir(mulheres[[v]])
  
  p <- format.pval(
    p_values_numericas[[v]],
    digits = 4,
    eps = 0.001
  )
  
  tabela <- rbind(
    tabela,
    data.frame(
      Variavel = c(
        paste(v, "— média (DP)"),
        paste(v, "— mediana [P25; P75]"),
        paste(v, "— válidos / ausentes")
      ),
      
      Homens = c(
        unname(resumo_homens),
        paste(
          sum(!is.na(homens[[v]])),
          sum(is.na(homens[[v]])),
          sep = " / "
        )
      ),
      
      Mulheres = c(
        unname(resumo_mulheres),
        paste(
          sum(!is.na(mulheres[[v]])),
          sum(is.na(mulheres[[v]])),
          sep = " / "
        )
      ),
      
      p_valor = c(p, "", "")
    )
  )
}

# Mostra a tabela completa
print(tabela, row.names = FALSE)

# Opcional: abre a tabela em uma aba no RStudio
# View(tabela)

#===============================================================================
### Variaveis categóricas
### Será usado o teste chi-quadrado, pois é usado para estudar associação/independência 
### entre duas variáveis categóricas
### Categoricas: n e % dentro de cada sexo, considerando apenas valores validos. 
#===============================================================================
var_categ <- c("SEXO", "TABAGISMO", "ESCOLARIDADE", "HASTRAT",
                 "DIABTRAT", "HPDAC", "HIPERTENSAO", "DIABETES")
p_values_categoricas <- c()
testes_categoricas <- c()

for (v in var_categ) {
  contagens <- table(dados[[v]], factor(dados$SEXO, levels = c("M", "F")))
  porcentagens <- 100 * prop.table(contagens, margin = 2)
  p <- NA_real_
  nome_teste <- "Nao se aplica"
  if (v != "SEXO") {
    teste <- suppressWarnings(chisq.test(contagens, correct = FALSE))
    nome_teste <- "Qui-quadrado"
    if (any(teste$expected < 5)) {
      teste <- fisher.test(contagens)
      nome_teste <- "Fisher"
    }
    p <- teste$p.value
    p_values_categoricas[v] <- p
  }
  testes_categoricas[v] <- nome_teste
  for (j in 1:nrow(contagens)) {
    tabela <- rbind(tabela, data.frame(
      Variavel = paste(v, rownames(contagens)[j], "n (%)"),
      Homens = sprintf("%d (%.1f%%)", contagens[j, 1], porcentagens[j, 1]),
      Mulheres = sprintf("%d (%.1f%%)", contagens[j, 2], porcentagens[j, 2]),
      p_valor = if (j == 1) format.pval(p, digits = 4, eps = 0.001) else ""))
  }
  tabela <- rbind(tabela, data.frame(
    Variavel = paste(v, "ausentes, n"),
    Homens = as.character(sum(is.na(homens[[v]]))),
    Mulheres = as.character(sum(is.na(mulheres[[v]]))), p_valor = ""))
}


print(tabela, row.names = FALSE)


#===============================================================================
# [4] TABELA SUPLEMENTAR 
#===============================================================================
#===============================================================================
### Tabela suplementar medidas resumo e testes estatísticos foram escolhidos em 
### cada caso, e justifique brevemente.
#===============================================================================
# Todas as categoricas testadas tiveram contagens esperadas >= 5: Pearson.
# SEXO define os grupos e nao e testado contra si mesmo (p = NA).
# TABAGISMO, HPDAC e HIPERTENSAO: p < 0,001; demais categoricas: p > 0,05.
# Homens: PAS mediana 127, PAD 89, HDL 40. Mulheres: 116, 78 e 51.
# Hipertensao entre classificaveis: homens 55,3%; mulheres 32,2%.
# Associacao nao demonstra causalidade; p > 0,05 nao demonstra equivalencia.
#===============================================================================
suplementar <- data.frame(
  Variavel = c(var_numec, var_categ),
  Resumo = c(ifelse(var_numec == "LDL", "Media (DP)",
                    "Mediana [P25; P75]"), rep("n (% dos validos)", 8)),
  Teste = c(ifelse(var_numec == "LDL", "t de Welch", "Mann-Whitney"),
            testes_categoricas),
  Justificativa = c(
    ifelse(var_numec == "LDL", "Shapiro >= 0,05 nos dois grupos",
           "Shapiro < 0,05 em pelo menos um grupo"),
    "SEXO define os grupos; nao recebe teste",
    rep("Categorias; todas as contagens esperadas >= 5", 7)))
print(suplementar, row.names = FALSE)

#===============================================================================
# [5a] IC95% da média de PAD
#===============================================================================
media_pad <- mean(dados$PAD, na.rm = TRUE)
ic_pad <- t.test(dados$PAD, conf.level = 0.95)$conf.int
print(media_pad)
print(ic_pad)
# n = 2950; media = 83,66589 mmHg; IC95% = [83,12563; 84,20614].
# IC da media usa t com n-1 graus de liberdade. A amostra grande sustenta
# a aproximacao da distribuicao da media.

#===============================================================================
# [5b] IC95% da frequência de diabetes 
#===============================================================================
### Casos totais de exames, ou seja, "Sim" e "Não"
diabetesm <- dados$DIABETES
### Somente os casos com diabetes, ou seja, o "Sim"
casos_diabetesm <- subset(diabetesm, diabetesm == "Sim") 
### Número de casos e tamanho da amostra
soma_casos_diabetesm <- length(casos_diabetesm)
soma_total_diabetesm <- sum(!is.na(diabetesm))

### Calculando Intervalo de Confiança com 95%
ic_diabetesm <- prop.test(
  x = soma_casos_diabetesm,
  n = soma_total_diabetesm,
  conf.level = 0.95,
  correct = FALSE
)$conf.int

print(100 * c(Proporcao = soma_casos_diabetesm / soma_total_diabetesm,
              Inferior = ic_diabetesm[1], Superior = ic_diabetesm[2]))
# Proporção calculada entre os participantes com classificação conhecida.
# Os 297 NA nao entram no denominador. Inferencia refere-se aos classificaveis.
# Generalizar para toda a população-alvo depende da amostragem
# e do mecanismo que levou à ausência de classificação.
# 1224 / 2703 = 45,28302%; IC95% de Wilson = [43,41451%; 47,16492%].
# prop.test tambem calcula um teste de proporcao; aqui usamos SOMENTE o IC.
# correct = FALSE desativa a correção de continuidade.
# As contagens são discretas: podem ser 1223, 1224 ou 1225 pessoas, mas não 1224,5 pessoas. 
# O procedimento aproximado usado por prop.test() se apoia em uma distribuição contínua. 
# A correção de continuidade ajusta essa aproximação para levar em conta os saltos das contagens.

#===============================================================================
# [6a] IC99% das mulheres com no mínimo o primeiro grau
#===============================================================================
### Escolaridade das mulheres
escolaridade_mulheres <- mulheres$ESCOLARIDADE
### Mulheres com pelo menos o primeiro grau
### subset já remove os casos com NA
primeiro_grau <- subset(escolaridade_mulheres, escolaridade_mulheres != "Sem escola formal") 
### Número de casos e tamanho da amostra
soma_primeiro_grau <- length(primeiro_grau)
soma_total_muleheres <- sum(!is.na(escolaridade_mulheres))
### Calculando Intervalo de Confiança com 99%
ic_primeiro_grau_mulheres <- prop.test(
  x = soma_primeiro_grau,
  n = soma_total_muleheres,
  conf.level = 0.99,
  correct = FALSE
)$conf.int

print(100 * c(Proporcao = soma_primeiro_grau / soma_total_muleheres,
              Inferior = ic_primeiro_grau_mulheres[1], Superior = ic_primeiro_grau_mulheres[2]))
# 1102 / 1493 = 73,81112%; IC99% = [70,77943%; 76,63211%].
# Primeiro Grau, Segundo Grau e Graduado contam como ao menos primeiro grau.
# Interpretamos essas categorias como etapas completas, conforme a pergunta.

#===============================================================================
# [6b] IC99% da idade que nunca fumou
#===============================================================================
### subset exclui os valores NAs
nunca_fumaram <- subset(dados, TABAGISMO == "Nunca")
media_idade <- mean(nunca_fumaram$IDADE)
ic_idade <- t.test(nunca_fumaram$IDADE, conf.level = 0.99)$conf.int
print(media_idade)
print(ic_idade)
print(c(Media = media_idade,
              Inferior = ic_idade[1], Superior = ic_idade[2]))
# n = 2056; media = 55,29134 anos; IC99% = [54,79818; 55,78451].

#===============================================================================
# [7] Estatísticas Descritivas
#===============================================================================
### Uma funcao curta aplica exatamente as mesmas contas as tres amostras.
estatisticas <- function(x) {
  c(N = sum(!is.na(x)), Ausentes = sum(is.na(x)),
    Media = mean(x, na.rm = TRUE), Mediana = median(x, na.rm = TRUE),
    quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE, type = 7),
    Maximo = max(x, na.rm = TRUE), Minimo = min(x, na.rm = TRUE),
    DP = sd(x, na.rm = TRUE), Variancia = var(x, na.rm = TRUE))
}
hipertensos <- subset(dados, HIPERTENSAO == "Sim")
item7 <- data.frame(
  HBGLIC = estatisticas(dados$HBGLIC),
  HDL_mulheres = estatisticas(mulheres$HDL),
  TTOG_hipertensos = estatisticas(hipertensos$TTOG))
print(round(item7, 4))
# DP e variancia amostrais usam n-1. Quantis usam type = 7, padrao do R.