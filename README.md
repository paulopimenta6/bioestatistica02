# Exercícios de Bioestatística em R

Análises desenvolvidas para praticar estatística descritiva, inferência estatística e criação de variáveis em R, com código comentado e construção passo a passo.

O projeto reúne o **Exercício-programa 1**, com comparação de características entre homens e mulheres, e o **Exercício-programa 2**, nas versões **A** (risco de doença arterial coronariana em 10 anos pelo sistema de pontos de Framingham baseado em LDL) e **B** (classificação de condutas para hipertensão, diabetes e dislipidemia segundo o enunciado).

As três análises foram concluídas. Os resultados apresentados abaixo foram conferidos nas execuções dos scripts, e os arquivos de saída foram gerados.

## Objetivos de aprendizagem

- Importar e explorar dados de uma planilha.
- Criar variáveis com condições lógicas e tratar valores ausentes.
- Produzir tabelas descritivas e gráficos de distribuição.
- Aplicar e interpretar testes estatísticos e intervalos de confiança.
- Usar estruturas `if`, `else if` e `for` para implementar regras.
- Exportar resultados em CSV e gerar um relatório Word pelo próprio R.

## Exercício-programa 1

O script contempla:

1. Importação e exploração do banco.
2. Criação das variáveis `HIPERTENSAO` e `DIABETES` segundo o enunciado.
3. Descrição das variáveis por sexo, incluindo contagens de dados ausentes.
4. Avaliação das distribuições por Shapiro–Wilk, histogramas e gráficos Q–Q.
5. Comparações com teste t de Welch, Mann–Whitney e qui-quadrado, com previsão de Fisher para contagens esperadas pequenas.
6. Tabela suplementar com medidas, testes e justificativas.
7. Intervalos de confiança de 95% e 99% e estatísticas descritivas adicionais.
8. Geração de um documento Word com tabelas e notas explicativas.

Os p-valores são interpretados em conjunto com os resumos e as características das distribuições. Não rejeitar uma hipótese não demonstra igualdade entre grupos, e Mann–Whitney não é interpretado automaticamente como teste de medianas.

## Exercício-programa 2 — versão A

O cálculo utiliza exclusivamente a versão **LDL Pts** de Wilson et al. (1998), com pontuações específicas por sexo para idade, LDL, HDL, pressão arterial e diabetes, além da pontuação de tabagismo.

O risco permanece como `NA` quando há:

- Idade menor que 30 ou maior que 74 anos.
- História de doença arterial coronariana positiva ou desconhecida.
- Ausência de algum dado necessário: sexo, idade, diabetes, tabagismo, PAS, PAD, LDL ou HDL.

Todos os participantes são mantidos no banco. As pontuações de cada componente e sua soma permitem acompanhar o cálculo.

### Resultado da execução no banco utilizado

| Situação | Participantes |
|---|---:|
| Total | 3.000 |
| Elegíveis, com risco calculado | 2.404 |
| Não elegíveis, com risco ausente | 596 |

### Convenções documentadas

- Apenas tabagismo atual acrescenta pontos; tabagismo prévio e nunca ter fumado recebem zero.
- Para pressão arterial, prevalece a maior pontuação indicada por PAS ou PAD.
- LDL igual a 190 mg/dL pertence à categoria `>=190`, seguindo a separação das categorias na Tabela 7 do artigo.
- A tabela masculina imprime `<-3` seguido de `-2`, deixando uma lacuna em `-3`. O código adota `<=-3` para a primeira categoria, como interpretação explicitamente documentada.
- Os resultados `>=56%` para homens e `>=32%` para mulheres preservam os limites superiores da tabela de pontos. Representam limites inferiores de risco, não percentuais exatos.

A variável final de risco é textual para preservar valores como `"7%"` e `">=56%"`. Os valores ausentes são `NA`, e não a palavra `"NA"` usada como categoria durante os cálculos.

## Exercício-programa 2 — versão B

A versão B cria três variáveis de conduta para os 3.000 participantes: `CONDUTA_HAS`, `CONDUTA_DM` e `CONDUTA_DLP`. As regras são as fornecidas pelo exercício; o artigo de Wilson e a página de Framingham fundamentam a versão A.

### Hipertensão — `CONDUTA_HAS`

| Situação | Meta de PAS (mmHg) | Meta de PAD (mmHg) |
|---|---|---|
| Com diabetes, independentemente da idade | < 130 | < 80 |
| Sem diabetes, idade < 60 anos | < 140 | < 90 |
| Sem diabetes, idade >= 60 anos | < 150 | < 90 |

**Esclarecimento do professor:** a última faixa inclui exatamente 60 anos. Embora o enunciado original indique “maior que 60”, foi confirmado o uso de `IDADE >= 60`.

As duas pressões devem estar abaixo dos limites. Dentro da meta, a classificação é “manter conduta”. Fora dela, `HASTRAT` determina “introduzir medicamento” para quem não está em tratamento ou “aumentar dose” para quem já está.

### Diabetes — `CONDUTA_DM`

Estar dentro da meta exige simultaneamente `GLIC < 130` mg/dL e `HBGLIC < 7`%. Dentro da meta, a classificação é “manter conduta”. Fora dela, `DIABTRAT` define “introduzir medicamento” ou “aumentar dose”.

A avaliação é feita para todos os participantes. Diagnóstico de diabetes e cumprimento da meta são informações distintas: uma pessoa com diabetes pode apresentar os dois exames dentro dos limites.

### Dislipidemia — `CONDUTA_DLP`

| Sexo | Diabetes ou história de DAC | Meta de LDL (mg/dL) | Meta de HDL (mg/dL) |
|---|---|---|---|
| Masculino | Pelo menos um positivo | < 100 | > 40 |
| Feminino | Pelo menos um positivo | < 100 | > 50 |
| Masculino | Ambos negativos | < 130 | > 40 |
| Feminino | Ambos negativos | < 130 | > 50 |

A conduta combina os dois exames:

- **Manter conduta:** LDL e HDL atendem simultaneamente às metas.
- **Introduzir medicamento:** LDL mais de 30 mg/dL acima do limite ou HDL mais de 10 mg/dL abaixo do limite.
- **Orientar dieta e atividade física:** fora da meta, com excesso de LDL de no máximo 30 e déficit de HDL de no máximo 10 mg/dL.

Os desvios são calculados com direção: `LDL - META_LDL` e `META_HDL - HDL`. Não se utiliza valor absoluto, pois um HDL acima do limite não representa déficit. Exatamente 30 acima no LDL ou 10 abaixo no HDL ainda pertence à orientação de dieta e atividade física, desde que o outro desvio também não ultrapasse seu limite.

### Resultados conferidos

| Conduta | Hipertensão | Diabetes | Dislipidemia |
|---|---:|---:|---:|
| Manter conduta | 1.425 | 2.109 | 561 |
| Introduzir medicamento | 1.015 | 605 | 528 |
| Aumentar dose | 179 | 78 | Não se aplica |
| Orientar dieta e atividade física | Não se aplica | Não se aplica | 1.535 |
| Ausente (`NA`) | 381 | 208 | 376 |
| **Total** | **3.000** | **3.000** | **3.000** |

As contagens de hipertensão incorporam a correção para idade `>= 60`. O CSV mantém o banco original e acrescenta diagnósticos, limites de metas, verificações e condutas.

## Arquivos e organização

Os nomes abaixo correspondem aos usados durante o desenvolvimento. Se os scripts forem renomeados, ajuste as chamadas correspondentes.

| Arquivo | Finalidade |
|---|---|
| `ep01.R` | Script principal do exercício 1 |
| `bloco_8_gerar_word_EP01.R` | Formatação e geração do relatório Word |
| `exercicio_2A.r` | Script do exercício 2, versão A |
| `exercicio_2B.r` | Script do exercício 2, versão B |
| `Banco aulas 2026.xlsx` | Arquivo de entrada esperado pelos scripts; necessário para reproduzir a análise |
| `EP01_relatorio.docx` | Relatório gerado pelo exercício 1 |
| `banco_EP2A.csv` | Banco exportado com as novas variáveis do exercício 2 A |
| `banco_EP2B.csv` | Banco exportado com metas e condutas do exercício 2 B |

A presença do banco no repositório não é pressuposta: a execução depende de uma cópia local do arquivo de entrada, com as colunas e categorias esperadas. As contagens acima descrevem o banco utilizado neste estudo.

## Como executar

### 1. Preparar o ambiente

Use R e, opcionalmente, RStudio. Instale os pacotes uma única vez:

```r
install.packages(c("readxl", "dplyr", "officer", "flextable", "pacman"))
```

O script da versão B utiliza também `pacman::p_load()` para carregar os pacotes.

### 2. Conferir a pasta de trabalho

```r
getwd()
file.exists("Banco aulas 2026.xlsx")
```

O segundo comando deve retornar `TRUE`. Caso o arquivo tenha outro nome ou esteja em outra pasta, ajuste o caminho no comando de importação do script.

### 3. Executar o exercício 1

Execute o script na ordem, da importação aos resultados. Ao final, a chamada abaixo executa o bloco de geração do Word:

```r
source("bloco_8_gerar_word_EP01.R", encoding = "UTF-8")
```

Esse bloco depende dos objetos criados pelo script principal, como `tabela`, `suplementar`, `item7` e os intervalos de confiança. Não deve ser executado isoladamente em uma sessão vazia.

O arquivo `EP01_relatorio.docx` é gravado na pasta de trabalho. Feche-o no Word antes de gerar novamente e confira a paginação antes de imprimir.

### 4. Executar o exercício 2 A

Execute o script completo, da criação das variáveis à exportação. As colunas são inicializadas com `NA` antes do laço, e os cálculos são executados somente para as linhas elegíveis.

As conferências e a exportação ficam **fora do `for`**, depois de seu encerramento:

```r
table(dados$ELEGIVEL)

table(
  Elegivel = dados$ELEGIVEL,
  Risco_ausente = is.na(dados$RISCO_DAC_10_ANOS)
)
```

Resultado esperado no banco utilizado:

```text
        Risco_ausente
Elegivel FALSE TRUE
   FALSE     0  596
   TRUE   2404    0
```

O CSV é exportado em UTF-8, com vírgulas como separadores, ponto decimal e `NA` para valores ausentes. Uma nova exportação substitui o arquivo de mesmo nome na pasta de trabalho.

### 5. Executar o exercício 2 B

Execute o script completo na ordem, começando pela importação da planilha original. Não aplique as exclusões de elegibilidade da versão A à versão B.

As etapas criam os diagnósticos, definem as metas, verificam seu cumprimento e atribuem as três condutas. Ao final, confira:

```r
nrow(dados)
table(dados$CONDUTA_HAS, useNA = "ifany")
table(dados$CONDUTA_DM, useNA = "ifany")
table(dados$CONDUTA_DLP, useNA = "ifany")
```

A exportação salva o banco completo:

```r
write.csv(
  dados,
  file = "banco_EP2B.csv",
  row.names = FALSE,
  na = "NA",
  fileEncoding = "UTF-8"
)

file.exists("banco_EP2B.csv")
getwd()
```

A confirmação esperada é `TRUE`. O arquivo é salvo na pasta de trabalho e substituído quando a exportação é executada novamente.

## Tratamento dos dados ausentes

Para os diagnósticos derivados, um critério positivo conhecido permite classificar o participante como `"Sim"`, mesmo que outro critério esteja ausente. A classificação `"Não"` exige que todos os critérios necessários sejam conhecidos e negativos.

Na análise descritiva, os denominadores das porcentagens consideram os valores conhecidos de cada variável. Os intervalos de proporções usam os participantes classificáveis. Portanto, a generalização depende do desenho amostral e do mecanismo de ausência dos dados.

Na versão B, valores ausentes não são convertidos em respostas negativas. Quando um critério conhecido basta para definir a ação, a classificação é possível mesmo com outra informação ausente. Por exemplo, um déficit de HDL superior a 10 mg/dL permite classificar “introduzir medicamento” mesmo sem uma meta de LDL conhecida.

Se a pressão está fora da meta, mas `HASTRAT` é desconhecido, `CONDUTA_HAS` permanece `NA`: falta informação para escolher entre introduzir medicamento e aumentar a dose. Dentro da meta, essa informação não é necessária para “manter conduta”.

A implementação determina as metas antes de compará-las aos exames. Quando uma meta necessária não pode ser definida, ela permanece `NA`; o código não tenta deduzir a conduta comparando todas as metas possíveis. Por isso, as contagens de ausentes correspondem a esse procedimento documentado.

## Entregáveis

- **Exercício 1:** script R e relatório Word com tabelas e resultados.
- **Exercício 2 A:** script R e CSV com o banco e as novas variáveis.
- **Exercício 2 B:** script R e CSV com o banco e as novas variáveis.

O enunciado permite escolher uma das versões do exercício 2 para entrega. Ambas foram desenvolvidas aqui para praticar a sintaxe e a implementação das regras.

## Referências

- Enunciado dos exercícios-programa de avaliação de 2026 e banco de dados fornecido para as aulas.
- Wilson PWF, D’Agostino RB, Levy D, Belanger AM, Silbershatz H, Kannel WB. **Prediction of coronary heart disease using risk factor categories.** Circulation. 1998;97:1837–1847. [DOI: 10.1161/01.CIR.97.18.1837](https://doi.org/10.1161/01.CIR.97.18.1837).
- Framingham Heart Study. [Coronary Heart Disease — 10-year risk](https://www.framinghamheartstudy.org/fhs-for-researchers/fhs-risk-functions/coronary-heart-disease-10-year-risk/).

## Autoria

**Exercício 1 e exercício 2 A:**

- Paulo Henrique de Almeida Soares Pimenta
- Fernanda Miwa Yamasaka
- Rebeca Benevides Pinto

**Exercício 2 B:**

- Paulo Henrique de Almeida Soares Pimenta
- Fernanda Miwa Yamasaka
- Rebeca Benevides Pinto

Projeto de finalidade educacional, desenvolvido para praticar programação e interpretação estatística conforme os exercícios propostos.
