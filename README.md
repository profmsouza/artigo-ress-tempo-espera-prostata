# Tempo para início do tratamento oncológico de neoplasia de próstata no Sistema Único de Saúde (Minas Gerais, 2014-2019)

[![DOI](https://zenodo.org/badge/1173723046.svg)](https://doi.org/10.5281/zenodo.18890407)
[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Submetido_RESS-brightgreen.svg)]()

Repositório contendo os dados anonimizados e o código-fonte computacional para a reprodução integral das análises estatísticas do manuscrito submetido à **Epidemiologia e Serviços de Saúde: revista do SUS (RESS)**.

## 👥 Autores
Keila Cristina Machado Quintão Vila Real, Leonardo Oliveira Leão e Silva, Márcio Luís Moreira de Souza, Bernardo Gomes Barbosa Nogueira, Suely Maria Rodrigues.

## 📌 Resumo do Projeto
Este estudo epidemiológico observacional, transversal e analítico avaliou o cumprimento da Lei dos 60 dias (Lei nº 12.732/2012) para o início do tratamento de pacientes com câncer de próstata no estado de Minas Gerais. O estudo utilizou dados secundários de 24.685 casos provenientes dos Registros Hospitalares de Câncer (RHC) e empregou modelos de Regressão Logística Múltipla para identificar os fatores associados ao atraso terapêutico.

## 📂 Estrutura do Repositório
Para garantir a transparência metodológica e a reprodutibilidade (Ciência Aberta), este repositório contém apenas os arquivos essenciais para a análise:

* `dados_saude_do_homem.csv`: Banco de dados secundário, de domínio público, extraído do Sistema Informatizado de Apoio aos RHC (SisRHC) e estritamente anonimizado.
* `script-artigo1.R`: Script comentado em linguagem R contendo todas as etapas de limpeza de dados (*feature engineering*), estatística descritiva (Tabelas) e modelagem inferencial (Regressão Logística e construção dos *Forest Plots*).

## ⚙️ Reprodutibilidade
As análises foram integralmente realizadas no ambiente R. Para reproduzir os resultados:
1. Clone ou baixe este repositório.
2. Abra o arquivo `script-artigo1.R` no seu RStudio.
3. O script utiliza o gerenciador `pacman` para instalar e carregar automaticamente todas as dependências necessárias (como `tidyverse`, `tableone`, `broom`, `ggplot2`, entre outros). Basta executar o código linha a linha ou compilar todo o documento.

## 📜 Licença e Disponibilidade
Os dados brutos e não anonimizados são de custódia da Coordenação de Vigilância do Câncer da Secretaria de Estado de Saúde de Minas Gerais (SES-MG). O recorte anonimizado e os códigos aqui presentes estão disponíveis abertamente para uso acadêmico e validação por pares.
