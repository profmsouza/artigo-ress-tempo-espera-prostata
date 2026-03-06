# ==============================================================================
# PROJETO: Tempo para Início do Tratamento Oncológico (Neoplasia de Próstata)
# OBJETIVO: Avaliar o cumprimento da Lei dos 60 dias (Lei nº 12.732/2012)
# AUTORES: Keila Cristina Machado Quintão Vila Real, Leonardo Oliveira Leão e Silva, 
#          Márcio Luís Moreira de Souza¹, Bernardo Gomes Barbosa Nogueira, Suely Maria Rodrigues²
# ==============================================================================
# ¹ Bioestatístico responsável pelas análises e o presente script
# ² Coordenadora da pesquisa

# 1. PACOTES E AMBIENTE --------------------------------------------------------
# A função p_load do pacote 'pacman' verifica se os pacotes estão instalados. 
# Se não estiverem, ele instala e já carrega (require) na memória.
if(!require("pacman")) install.packages("pacman", dep=T)

pacman::p_load(
  tidyverse,  # Manipulação de dados (dplyr, tidyr, ggplot2)
  survival,   # Análise de sobrevivência (se aplicável futuramente)
  survminer,  # Gráficos de sobrevivência
  tableone,   # Criação de tabelas demográficas (Tabela 1)
  gtsummary,  # Tabelas de resumo estatístico elegantes
  broom,      # Transformar saídas de modelos em dataframes (tidy)
  readxl,     # Leitura de arquivos Excel (.xls / .xlsx)
  MASS,       # Funções estatísticas adicionais
  car,        # Testes de regressão aplicados
  epitools,   # Ferramentas para epidemiologia (Odds Ratio, etc.)
  lubridate,  # Manipulação simplificada de datas
  stringr     # Manipulação de textos/strings
)

# 2. CARREGAMENTO DE DADOS E DICIONÁRIOS ---------------------------------------
# Carrega a base de dados principal anonimizada
dados <- read.csv2("dados_saude_do_homem.csv")

# Criação de um dicionário (data.frame) para padronizar os estadiamentos do tumor.
# Isso facilita a transformação de diversas categorias em grupos binários úteis.
tabela_estadiamento <- data.frame(
  estadiam = c("0", "1", "1C", "2", "2A", "2B", "2C", 
               "3", "3A", "3B", "3C", "4", "4A", "4B", "88", "99"),
  rotulo = c("Estádio 0", "Estádio I", "Estádio IC", 
             "Estádio II", "Estádio IIA", "Estádio IIB", "Estádio IIC", 
             "Estádio III", "Estádio IIIA", "Estádio IIIB", "Estádio IIIC", 
             "Estádio IV", "Estádio IVA", "Estádio IVB", "Não se aplica", "Ignorado"),
  estadio_geral = c("0", "I", "I", "II", "II", "II", "II", 
                    "III", "III", "III", "III", "IV", "IV", "IV", "N/A", "Ignorado"),
  estadiam_bin = c("Inicial", "Inicial", "Inicial", "Inicial", "Inicial", 
                   "Inicial", "Inicial", "Avançado", "Avançado", "Avançado", 
                   "Avançado", "Avançado", "Avançado", "Avançado", NA, NA),
  stringsAsFactors = FALSE
)

# 3. LIMPEZA E ENGENHARIA DE RECURSOS (FEATURE ENGINEERING) --------------------
# O pipe (%>%) repassa o resultado de uma linha para a próxima operação
dados_clean <- dados %>%
  
  # Cruza a base principal com o dicionário de estadiamento
  left_join(tabela_estadiamento[,c("estadiam", "rotulo", "estadio_geral", "estadiam_bin")], 
            by = "estadiam") %>%
  
  # Remove registros inválidos ou corrompidos na variável de tratamento
  filter(tratamentoantesdodiag != "tratamentoantesdodiag") %>%
  
  mutate(
    # --- TRATAMENTO DE DATAS ---
    dt_diagnostico  = dmy(dtdiagno),
    dt_inicio_trat  = dmy(datainitrt),
    dt_primeira_con = dmy(datapricon),
    
    # --- CÁLCULO DE TEMPOS (EM DIAS) ---
    # Tempo 1: Da primeira consulta até o diagnóstico (Tabela 1)
    tempo_diag_dias = as.numeric(dt_diagnostico - dt_primeira_con),
    
    # Tempo 2: Do diagnóstico até o tratamento (Tabela 2)
    tempo_dias = as.numeric(dt_inicio_trat - dt_diagnostico),
    
    # --- CRIAÇÃO DAS CATEGORIAS (TABELAS 1 E 2) ---
    
    # Categorias da Tabela 1
    cat_tempo_diag = case_when(
      tempo_diag_dias < 0 ~ "Diagnóstico antes da primeira consulta",
      tempo_diag_dias >= 0 & tempo_diag_dias < 30 ~ "Menos de 30 dias",
      tempo_diag_dias >= 30 & tempo_diag_dias <= 60 ~ "Entre 30 a 60 dias",
      tempo_diag_dias > 60 ~ "Mais de 60 dias",
      TRUE ~ NA_character_
    ),
    # Transformando em fator para garantir a ordem correta na tabela
    cat_tempo_diag = factor(cat_tempo_diag, levels = c(
      "Diagnóstico antes da primeira consulta", "Menos de 30 dias", 
      "Entre 30 a 60 dias", "Mais de 60 dias"
    )),
    
    # Categorias da Tabela 2
    cat_tempo_trat = case_when(
      tempo_dias < 0 ~ "Tratamento iniciado antes",
      tempo_dias >= 0 & tempo_dias < 30 ~ "Menos de 30 dias",
      tempo_dias >= 30 & tempo_dias <= 60 ~ "Entre 30 a 60 dias",
      tempo_dias > 60 ~ "Mais de 60 dias",
      TRUE ~ NA_character_
    ),
    # Transformando em fator para garantir a ordem correta na tabela
    cat_tempo_trat = factor(cat_tempo_trat, levels = c(
      "Tratamento iniciado antes", "Menos de 30 dias", 
      "Entre 30 a 60 dias", "Mais de 60 dias"
    )),
    
    # Variável binária original da Lei dos 60 dias para a Regressão Logística
    prazoleitratamento_calc = case_when(
      tempo_dias > 60 ~ "mais de 60 dias",
      tempo_dias >= 0 & tempo_dias <= 60 ~ "ate 60 dias",
      TRUE ~ NA_character_
    ),    
    # --- AGRUPAMENTO DE VARIÁVEIS CATEGÓRICAS ---
    
    # Agrupa a idade em faixas etárias de 10 em 10 anos
    faixa_etaria_prim_cons = cut(
      as.numeric(idade1consulta),
      breaks = c(seq(0, 80, by = 10), Inf), 
      labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
      right = FALSE
    ),
    
    # Simplifica a variável de origem (Se veio do SUS = Sim, caso contrário = Não)
    origemenSUS = ifelse(origemencaminhamento == "SUS", "Sim", "Não"), 
    
    # Cria variável binária para cor de pele branca
    corbranca = ifelse(racacor2 == "branca", "Sim", "Não"),
    
    # Agrupa o método diagnóstico em duas categorias principais
    basediag_agrupada = case_when(
      basediag %in% c("histologia tum prim", "histologia metastase", "citologia") ~ "Confirmacao Microscopica",
      basediag %in% c("clinica", "exame por imagem", "marcadores tumorais", "pesquisa clinica") ~ "Diagnostico Clinico/Imagem",
      basediag == "s/info" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Simplifica o estado civil para focar no suporte social
    status_conjugal_bin = case_when(
      estadocivil %in% c("casado", "uniao consensual") ~ "Com Companheiro",
      estadocivil %in% c("solteiro", "separacao judicial", "viuvo") ~ "Sem Companheiro",
      TRUE ~ NA_character_
    ),
    
    # Reduz a escolaridade para 3 níveis principais
    escolaridade_agrupada = case_when(
      escolaridade %in% c("nenhuma", "fund. incompleto") ~ "Baixa Escolaridade",
      escolaridade %in% c("fund. completo", "nivel medio") ~ "Media Escolaridade",
      escolaridade %in% c("superior incompleto", "superior completo") ~ "Alta Escolaridade",
      escolaridade == "s/info" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Classifica a ocupação usando detecção de padrões de texto (RegEx)
    ocupacao_temp = iconv(tolower(descricao), to="ASCII//TRANSLIT"), # Remove acentos e joga pra minúsculo
    classe_trabalho = case_when(
      str_detect(ocupacao_temp, "agro|rural|lavrador") ~ "Rural/Agro",
      str_detect(ocupacao_temp, "engenheir|medic|advogad|professor") ~ "Superior",
      str_detect(ocupacao_temp, "pedreiro|servente|motorista") ~ "Manual",
      str_detect(ocupacao_temp, "tecnico|auxiliar|recepcionista") ~ "Tecnico/Admin",
      str_detect(ocupacao_temp, "vendedor|comerci|domestica") ~ "Servicos",
      TRUE ~ "Outros"
    )
  ) %>%
  
  # --- REMOÇÃO DE COLUNAS DESNECESSÁRIAS ---
  # Descarta as colunas antigas que já foram transformadas ou que não serão usadas
  dplyr::select(-tratamentoantesdodiag, -tpcaso, -datainitrt, -datapricon,
                -trancursodiagtrat, -trancurso1consultdiag, -diagnosticoantesdaconsulta,
                -dtdiagno, -ocupacao, -codigo, -faixaconsultadiag, -estadiam,
                -basediag, -estadocivil, -estadofinal, -examesdiagn, -escolaridade,
                -primeirotrathosp, -ocupacao_temp, -descricao, -faixa_idade,
                -origemencaminhamento, -racacor2, -rzntr2, -alcoolismo, -tabagismo, 
                -idade1consulta) %>%
  
  # Converte todas as variáveis de texto (character) para fator (categorical)
  mutate_if(is.character, as.factor)

# --- DEFINIÇÃO DE CATEGORIAS DE REFERÊNCIA (BASELINE) ---
# Passo crucial para a Regressão Logística: definir o que é o "normal/padrão" (Odds Ratio = 1)
# 1. Evento de interesse é o ATRASO. Logo, a base é iniciar o tratamento em "ate 60 dias"
dados_clean$prazoleitratamento_calc <- relevel(dados_clean$prazoleitratamento_calc, ref = "ate 60 dias")
# 2. Queremos ver o risco do SUS. Logo, a base (referência) é o atendimento privado ("Não")
dados_clean$origemenSUS <- relevel(dados_clean$origemenSUS, ref = "Não")
# 3. Queremos ver a proteção da clínica. Logo, a base é a confirmação microscópica
dados_clean$basediag_agrupada <- relevel(dados_clean$basediag_agrupada, ref = "Confirmacao Microscopica")


# ==============================================================================
# 4. FUNÇÕES DE FORMATAÇÃO E ESTILO (PADRÃO REVISTA RESS)
# ==============================================================================

# Formata Odds Ratio (OR) e IC: 2 casas decimais e vírgula
formata_ress_or <- function(x) {
  format(round(x, 2), nsmall = 2, decimal.mark = ",", trim = TRUE)
}

# Formata Porcentagem (%): 1 casa decimal e vírgula (exigência da RESS)
formata_ress_pct <- function(x) {
  format(round(x, 1), nsmall = 1, decimal.mark = ",", trim = TRUE)
}

# Define um tema limpo, padronizado e unificado para os gráficos
tema_padrao <- theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.y = element_text(face = "bold", size = 12, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank() # Remove grades fracas ao fundo
  )


# ==============================================================================
# 5. GERAÇÃO DAS TABELAS DESCRITIVAS (Tabela 1 e Tabela 2)
# ==============================================================================

# Função auxiliar didática para extrair a frequência (N) e a Porcentagem (%)
gerar_tabela_frequencia <- function(dados, variavel, nome_coluna) {
  dados %>%
    filter(!is.na({{variavel}})) %>% # Remove eventuais dados faltantes do cálculo
    count({{variavel}}) %>%
    mutate(
      `%` = formata_ress_pct((n / sum(n)) * 100) # Calcula a porcentagem e aplica a formatação RESS
    ) %>%
    rename(!!nome_coluna := {{variavel}}, N = n)
}

# ------------------------------------------------------------------------------
# Exportando a Tabela 1
# ------------------------------------------------------------------------------
tabela_1 <- gerar_tabela_frequencia(dados_clean, cat_tempo_diag, 
                                    "Tempo da primeira consulta ao diagnóstico")

cat("\n--- TABELA 1: Tempo de espera da primeira consulta até o diagnóstico ---\n")
print(as.data.frame(tabela_1), row.names = FALSE)


# ------------------------------------------------------------------------------
# Exportando a Tabela 2
# ------------------------------------------------------------------------------
tabela_2 <- gerar_tabela_frequencia(dados_clean, cat_tempo_trat, 
                                    "Tempo para início do tratamento a contar da data do diagnóstico")

cat("\n--- TABELA 2: Tempo para início do tratamento a contar do diagnóstico ---\n")
print(as.data.frame(tabela_2), row.names = FALSE)


# ==============================================================================
# 6. ANÁLISE UNIVARIADA: TESTE EXATO DE FISHER
# ==============================================================================
# Avalia a associação de variáveis individuais com o atraso de forma isolada.

# 6.1 Efeito do Tipo de Diagnóstico
t_diag_raw <- table(dados_clean$basediag_agrupada, dados_clean$prazoleitratamento_calc)

# Forçamos a ordem das Linhas (Exposição) e das Colunas (Desfecho na Coluna 1)
t_diag_ajustada <- t_diag_raw[c("Diagnostico Clinico/Imagem", "Confirmacao Microscopica"), 
                              c("mais de 60 dias", "ate 60 dias")]
teste_diag <- fisher.test(t_diag_ajustada)

# 6.2 Efeito da Origem do Encaminhamento (SUS)
t_sus_raw <- table(dados_clean$origemenSUS, dados_clean$prazoleitratamento_calc)

# Forçamos a ordem das Linhas e Colunas
t_sus_ajustada <- t_sus_raw[c("Sim", "Não"), 
                            c("mais de 60 dias", "ate 60 dias")]
teste_sus <- fisher.test(t_sus_ajustada)

# 6.3 Consolidação dos Resultados Univariados
# Cria um dataframe para armazenar os Odds Ratios brutos e desenhar o gráfico
df_uni <- data.frame(
  Fator = c("Diagnóstico Clínico\n(vs Micro)", "Origem SUS\n(vs Privado)"),
  OR = c(teste_diag$estimate, teste_sus$estimate),
  Lower = c(teste_diag$conf.int[1], teste_sus$conf.int[1]),
  Upper = c(teste_diag$conf.int[2], teste_sus$conf.int[2])
)

# Define cor: OR abaixo de 1 é proteção (Azul); OR acima de 1 é risco (Vermelho)
df_uni$Tipo <- ifelse(df_uni$OR < 1, "Fator de Proteção", "Fator de Risco")

# Cria a etiqueta que aparecerá no gráfico, já padronizada para as regras da RESS
df_uni$label <- paste0("OR ", formata_ress_or(df_uni$OR), 
                       " (IC95% ", formata_ress_or(df_uni$Lower), ";", formata_ress_or(df_uni$Upper), ")")

# 6.4 Forest Plot Univariado (Figura 1)
ggplot(df_uni, aes(y = Fator, x = OR, color = Tipo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") + # Linha nula
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.1, size = 1) + # Margem de Erro
  geom_point(size = 4) + # Ponto da Estimativa (OR)
  geom_text(aes(label = label), vjust = -1.5, fontface = "bold", show.legend = FALSE) + # Texto
  scale_color_manual(values = c("Fator de Proteção" = "blue", "Fator de Risco" = "red")) +
  tema_padrao +
  labs(
    title = "Fatores Associados ao Atraso no Tratamento (>60 dias)",
    subtitle = "Análise Univariada (Não Ajustada)",
    x = "Odds Ratio (Razão de Chances)",
    y = ""
  ) +
  coord_cartesian(xlim = c(0, 3.0)) # Fixa o eixo X para facilitar comparação entre gráficos


# ==============================================================================
# 7. ANÁLISE MULTIVARIADA: REGRESSÃO LOGÍSTICA (GLM)
# ==============================================================================
# Avalia o impacto conjunto das variáveis, controlando os fatores de confusão.

# 7.1 Construção do Modelo
# Variável dependente (~): Atraso. Variáveis independentes (+): SUS, Diagnóstico
modelo_multi <- glm(prazoleitratamento_calc ~ origemenSUS + basediag_agrupada, 
                    family = binomial(link = "logit"), 
                    data = dados_clean)

# O summary bruto exibe log-odds; usaremos o pacote broom para converter em Odds Ratio
# 7.2 Extração e Limpeza dos Resultados (Tidy Data)
tabela_modelo <- tidy(modelo_multi, exponentiate = TRUE, conf.int = TRUE)

tabela_modelo <- tabela_modelo %>%
  filter(term != "(Intercept)") %>% # O intercepto não é relevante para o Forest Plot
  mutate(
    # Renomeia os termos do R para nomes legíveis e padronizados
    Fator = case_when(
      term == "origemenSUSSim" ~ "Origem SUS\n(vs Privado)",
      term == "basediag_agrupadaDiagnostico Clinico/Imagem" ~ "Diagnóstico Clínico\n(vs Micro)",
      TRUE ~ term
    ),
    Tipo = ifelse(estimate > 1, "Fator de Risco", "Fator de Proteção"),
    # Cria etiqueta formatada para RESS usando a função correta
    label = paste0("OR Ajustado ", formata_ress_or(estimate), 
                   " (IC95% ", formata_ress_or(conf.low), ";", formata_ress_or(conf.high), ")")
  )

# Visualiza tabela ajustada no console
print(tabela_modelo[, c("Fator", "estimate", "p.value", "conf.low", "conf.high")])

# 7.3 Forest Plot Multivariado (Figura 2)
ggplot(tabela_modelo, aes(y = Fator, x = estimate, color = Tipo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1, size = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = label), vjust = -1.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Fator de Proteção" = "blue", "Fator de Risco" = "red")) +
  tema_padrao +
  labs(
    title = "Fatores Associados ao Atraso no Tratamento (>60 dias)",
    subtitle = "Análise Multivariada (Ajustada)",
    x = "Odds Ratio Ajustado",
    y = ""
  ) +
  coord_cartesian(xlim = c(0, 3.0))

# ================================ FIM DO SCRIPT ================================
