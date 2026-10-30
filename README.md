# Bioestatística II — EP1

Repositório do Exercício-Programa 1 da disciplina de Bioestatística II (mestrado, 2026).

## Arquivos

- `Banco aulas 2026.xlsx` — banco usado nas aulas (3.000 indivíduos, 16 variáveis).
- `ep01_paulo_pimenta_MOD.R` — script com a análise do EP1.
- `Exercícios programa de avaliação 2026.pdf` — enunciado do EP1.
- `README.md` — este arquivo.

## Como executar

1. Abra o script no RStudio.
2. Instale o pacote `readxl`, se necessário:

   ```r
   install.packages("readxl")
   ```

3. No RStudio, defina o diretório de trabalho para a pasta do script:
   `Session > Set Working Directory > To Source File Location`.
4. Mantenha o arquivo `Banco aulas 2026.xlsx` na mesma pasta do script.
5. Execute o script.

O script cria as pastas `resultados/`, `resultados/graficos/` e `resultados/tabelas/` e imprime os resultados no console.

## Conteúdo do EP1

O script:

- lê o banco;
- cria as variáveis `HIPERTENSAO` e `DIABETES`;
- faz a análise descritiva por sexo;
- verifica normalidade e aplica testes de hipótese;
- calcula os intervalos de confiança pedidos;
- calcula as estatísticas descritivas do item 7.

## Observação

O PDF do enunciado também está incluído neste repositório.

## Autor

Paulo Pimenta — 2026.
