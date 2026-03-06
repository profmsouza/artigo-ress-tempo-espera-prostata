if(!require("pacman")) install.packages("pacman", dep=T)
pacman::p_load(tidyverse, survival, survminer, tableone, ggplot2, gtsummary, broom, readxl, broom, MASS, car, gtsummary)

dados <- read_xls("base_final_final_com_dicionário_das_variáveis.xls")
names(dados)[29] = "descricao"

# Criando o data.frame com a tabela de estadiamento
tabela_estadiamento <- data.frame(
  estadiam = c("0", "1", "1C", "2", "2A", "2B", "2C", 
             "3", "3A", "3B", "3C", "4", "4A", "4B", 
             "88", "99"),
  
  rotulo = c("Estádio 0", "Estádio I", "Estádio IC", 
             "Estádio II", "Estádio IIA", "Estádio IIB", "Estádio IIC", 
             "Estádio III", "Estádio IIIA", "Estádio IIIB", "Estádio IIIC", 
             "Estádio IV", "Estádio IVA", "Estádio IVB", 
             "Não se aplica", "Ignorado"),
  
  # --- NOVA COLUNA 1: Agrupamento Clássico (Romano) ---
  estadio_geral = c("0", "I", "I", 
                    "II", "II", "II", "II", 
                    "III", "III", "III", "III", 
                    "IV", "IV", "IV", 
                    "N/A", "Ignorado"),
  
  # --- NOVA COLUNA 2: Agrupamento Epidemiológico (Binário) ---
  # Geralmente: 0, I e II são "Precoce/Inicial", III e IV são "Avançado"
  estadiam_bin = c("Inicial", "Inicial", "Inicial", 
                            "Inicial", "Inicial", "Inicial", "Inicial", 
                            "Avançado", "Avançado", "Avançado", "Avançado", 
                            "Avançado", "Avançado", "Avançado", 
                            NA, NA), # 88 e 99 viram NA para não sujar a análise
  
  descricao = c(
    "Carcinoma in situ (não invasivo)",
    "Tumor inicial, pequeno e localizado",
    "Subdivisão do Estádio I",
    "Tumor localmente avançado",
    "Subdivisão inicial do Estádio II",
    "Subdivisão intermediária do Estádio II",
    "Subdivisão avançada do Estádio II",
    "Tumor avançado regionalmente",
    "Subdivisão inicial do Estádio III",
    "Subdivisão intermediária do Estádio III",
    "Subdivisão avançada do Estádio III",
    "Metástase à distância",
    "Metástase ou avanço local agressivo",
    "Metástase à distância avançada",
    "Tumores sem estadiamento TNM",
    "Informação desconhecida ou não registrada"
  ),
  
  stringsAsFactors = FALSE
)

# Verificar estrutura dos dados
glimpse(dados)

# Converter variáveis de data
dados_clean <- dados %>%
  left_join(tabela_estadiamento[,c(1,4)], "estadiam") %>% 
  filter(tratamentoantesdodiag != "tratamentoantesdodiag") %>%
  dplyr::select(-tratamentoantesdodiag,-tpcaso,-datainitrt,-datapricon,
         -trancursodiagtrat,-trancurso1consultdiag,-diagnosticoantesdaconsulta,
         -dtdiagno,-ocupacao,-codigo,-faixaconsultadiag,-estadiam) %>% 
  mutate(
    faixa_etaria_prim_cons = cut(
      idade1consulta,
      breaks = c(seq(0, 80, by = 10), Inf), # Define as quebras de 5 em 5
      labels = c(
        "0-9", "10-19", "20-29", "30-39",
        "40-49", "50-59", "60-69", "70-79",
        "80+"
      ),
      right = FALSE # Importante: Faz o intervalo ser [0, 5) -> inclui 0, exclui 5
    ),
    origemenSUS = ifelse(origemencaminhamento == "SUS", "Sim", "Não"),
    corbranca = ifelse(racacor2 == "branca", "Sim", "Não"),
    basediag_agrupada = case_when(
      # Grupo Microscópico (Certeza)
      basediag %in% c("histologia tum prim", 
                      "histologia metastase", 
                      "citologia") ~ "Confirmacao Microscopica",
      
      # Grupo Não Microscópico (Evidência Clínica/Imagem)
      basediag %in% c("clinica", 
                      "exame por imagem", 
                      "marcadores tumorais", 
                      "pesquisa clinica") ~ "Diagnostico Clinico/Imagem",
      
      # O que fazer com s/info? (Sugestão: tratar como NA ou Outros)
      basediag == "s/info" ~ NA_character_, # Ou "Ignorado"
      
      TRUE ~ NA_character_ # Segurança para casos não mapeados
    ),
    status_conjugal_bin = case_when(
      estadocivil %in% c("casado", "uniao consensual") ~ "Com Companheiro",
      estadocivil %in% c("solteiro", "separacao judicial", "viuvo") ~ "Sem Companheiro",
      TRUE ~ NA_character_
    ),
    resposta_tratamento = case_when(
      estadofinal %in% c("remissao completa", "remissao parcial") ~ "Satisfatória",
      
      estadofinal %in% c("doenca estavel", "doenca progressao", 
                         "obito", "suporte terapeutico") ~ "Insatisfatória",
      
      # Todo o resto vira NA
      TRUE ~ NA_character_ 
    ),
    tipo_exame_agrupado = case_when(
      # Grupo 1: Exame Bioquímico
      examesdiagn == "marcadores tumorais" ~ "Marcadores Tumorais",
      
      # Grupo 2: Exame Tecidual (Biópsia)
      examesdiagn == "anatomia patologica" ~ "Anatomia Patologica",
      
      # Grupo 3: Todo o resto vira NA (ou "Outros" se preferir não perder linhas)
      # Optei por NA pois <100 casos em 24mil não geram inferência segura
      TRUE ~ NA_character_ 
    ),
    escolaridade_agrupada = case_when(
      # Nível 1: Baixa (Analfabeto funcional ou Primário incompleto)
      escolaridade %in% c("nenhuma", "fund. incompleto") ~ "Baixa Escolaridade",
      
      # Nível 2: Média (Fundamental completo até Médio)
      escolaridade %in% c("fund. completo", "nivel medio") ~ "Media Escolaridade",
      
      # Nível 3: Alta (Entrou na faculdade)
      escolaridade %in% c("superior incompleto", "superior completo") ~ "Alta Escolaridade",
      
      # Tratamento de Missing
      escolaridade == "s/info" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    tratamento_agrupado = case_when(
      # Os três grandes pilares do tratamento
      primeirotrathosp == "cirurgia" ~ "Cirurgia",
      primeirotrathosp == "radioterapia" ~ "Radioterapia",
      primeirotrathosp == "hormonioterapia" ~ "Hormonioterapia",
      
      # O grande grupo dos "Não tratados" (Vigilância ou Encaminhados)
      primeirotrathosp %in% c("NTI", "nenhum") ~ "Sem Tratamento Inicial",
      
      # Agrupando o "Resto" (Quimio é minoria aqui, Imuno é traço)
      primeirotrathosp %in% c("quimioterapia", "imunoterapia", "outros") ~ "Quimio/Outros",
      
      # Tratamento de Missing
      primeirotrathosp == "s/info" ~ NA_character_,
      TRUE ~ NA_character_
    ),
    # Normalizar texto para minusculo e sem acentos ajuda no 'match'
    ocupacao_temp = iconv(tolower(descricao), to="ASCII//TRANSLIT"),
    
    classe_trabalho = case_when(
      
      # --- GRUPO 1: RURAL / AGRO (Prioridade alta para nao confundir com manual urbano) ---
      str_detect(ocupacao_temp, "agro|rural|lavrador|pesca|agricola|florestal|cultura de|pecuaria|trabalhador da cultura") ~ "Rural/Agropecuaria",
      
      # --- GRUPO 2: ELITE / SUPERIOR / DIRIGENTES ---
      str_detect(ocupacao_temp, "engenheir|medic|advogad|diretor|gerente|professo|arquitet|economista|analista|biolog|farmaceut|dentista|oficiais|universit|geolog|superiores|empresario|pesquisador|quimic") ~ "Superior/Dirigente",
      
      # --- GRUPO 3: MANUAL / OPERACIONAL / INDUSTRIAL ---
      str_detect(ocupacao_temp, "pedreiro|servente|braçal|operador|mecanic|metalurg|soldador|montador|eletricista|pintor|carpinteiro|marceneiro|costureir|maquinista|motorista|condutor|ajustador|ferramenteiro|tecelao|trefilador|vidreiro|sapateiro|padeiro|acougueiro") ~ "Manual/Operacional",
      
      # --- GRUPO 4: TÉCNICOS E ADMINISTRATIVOS ---
      str_detect(ocupacao_temp, "tecnico|administra|escritorio|secretari|auxiliar|contabil|bancario|recepcionista|desenhista|fiscal|inspetor|agente|assistente|datilografo") ~ "Tecnico/Administrativo",
      
      # --- GRUPO 5: SERVIÇOS / COMÉRCIO / SEGURANÇA ---
      str_detect(ocupacao_temp, "vendedor|comerci|atendente|vigia|policia|bombeiro|militar|seguranca|zelador|porteiro|limpeza|domestica|cozinheir|garcom|barbeiro|cabeleireiro|manicure|frentista|carteiro|telefonista") ~ "Servicos/Comercio",
      
      # --- TRATAMENTO DE MISSING / RESTO ---
      str_detect(ocupacao_temp, "nao se aplica|sem info|ignorado") ~ NA_character_,
      
      # Categoria de segurança para o que sobrar
      TRUE ~ "Outros/Nao Classificado"
    ),
    motivo_nao_tratamento = case_when(
      rzntr2 %in% c("abandono", "recusa") ~ "Recusa/Abandono",
      rzntr2 %in% c("doenca avancada", "obito", "complicacoes") ~ "Condicao Clinica Ruim",
      rzntr2 == "realizado fora" ~ "Transferido",
      TRUE ~ "Tratado/Nao se aplica"
    ),
    alcoolismo_agrupado = case_when(
      # Grupo de Referência (Protegido/Baixo Risco)
      alcoolismo == "nunca" ~ "Nunca Bebeu",
      
      # Grupo de Risco Passado
      alcoolismo == "ex-consumidor" ~ "Ex-Consumidor",
      
      # Grupo de Risco Ativo
      alcoolismo == "sim" ~ "Consumidor Ativo",
      
      # Tratamento de Missing (s/info, nao avaliado, nti)
      TRUE ~ NA_character_
    ),
    tabagismo_agrupado = case_when(
      # Grupo de Referência (Protegido/Baixo Risco)
      tabagismo == "nunca" ~ "Nunca Fumou",
      
      # Grupo de Risco Passado
      tabagismo == "ex-consumidor" ~ "Ex-Consumidor",
      
      # Grupo de Risco Ativo
      tabagismo == "sim" ~ "Consumidor Ativo",
      
      # Tratamento de Missing (s/info, nao avaliado, nti)
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::select(-basediag,-estadocivil,-estadofinal,-examesdiagn,-escolaridade,-primeirotrathosp,-ocupacao_temp,-descricao,-faixa_idade,
         -origemencaminhamento,-racacor2,-rzntr2,-alcoolismo,-tabagismo,-idade1consulta) %>% 
  mutate_if(is.character,as.factor)


# 2. ANÁLISE DESCRITIVA ----------------------------------------------------------

summary(dados_clean)

# 3. MODELAGEM ESTATÍSTICA -------------------------------------------------------

dados_modelo <- dados_clean %>%
  # 1. Remover quem não tem a informação do desfecho (resposta)
  filter(!is.na(resposta_tratamento)) %>%
  
  mutate(
    # 2. Criar a variável binária (Y)
    # 1 = Sucesso (Remissão), 0 = Falha (Óbito/Estável/Piora)
    desfecho_binario = ifelse(resposta_tratamento == "Satisfatória", 1, 0),
    
    # 3. Ajustar as Referências (Baselines)
    # Isso define quem é o "1.0" na comparação de risco.
    
    # Sexo/Raça (Geralmente a maioria ou o grupo de risco padrão)
    corbranca = relevel(as.factor(corbranca), ref = "Sim"), 
    
    # Idade (Se estiver usando contínua, ok. Se for faixa, defina a menor)
    # (Assumindo que você manteve idade1consulta como numérica)
    
    # Escolaridade: Referência = Baixa
    escolaridade_agrupada = relevel(as.factor(escolaridade_agrupada), ref = "Alta Escolaridade"),
    
    # Estádio: Referência = Inicial
    estadiam_bin = relevel(as.factor(estadiam_bin), ref = "Avançado"),
    
    # Tratamento: Referência = Sem Tratamento (Para ver o quanto a Cirurgia protege)
    # OU Referência = Cirurgia (Para ver o quanto não tratar piora) -> Vou sugerir Cirurgia
    tratamento_agrupado = relevel(as.factor(tratamento_agrupado), ref = "Sem Tratamento Inicial"),
    
    # Vícios: Referência = Nunca
    alcoolismo_agrupado = relevel(as.factor(alcoolismo_agrupado), ref = "Consumidor Ativo"),
    tabagismo_agrupado = relevel(as.factor(tabagismo_agrupado), ref = "Consumidor Ativo"),
    
    # Classe Trabalho: Referência = Manual ou Rural
    classe_trabalho = relevel(as.factor(classe_trabalho), ref = "Outros/Nao Classificado"),
    
    # Classe Trabalho: Referência = Manual ou Rural
    origemenSUS = relevel(as.factor(origemenSUS), ref = "Sim")
  )

## MODELO 

modelo_logistico <- glm(
  desfecho_binario ~ 
    #faixa_etaria_prim_cons +   # Faixa Etária na Primeira Consulta
    corbranca +                # Raça
    status_conjugal_bin +      # Suporte social
    escolaridade_agrupada +    # Nível socioeconômico
    classe_trabalho +          # Exposição ocupacional
    estadiam_bin +             # Gravidade da doença (Usei a binária Inicial/Avançado)
    tratamento_agrupado +      # O que foi feito
    alcoolismo_agrupado +      # Comorbidade 1
    tabagismo_agrupado +       # Comorbidade 2
    origemenSUS,               # Origem do paciente
  
  family = binomial(link = "logit"),
  data = dados_modelo
)

# Resumo estatístico bruto
summary(modelo_logistico) # O MODELO COMPLETO É TB O MELHOR MODELO. TODOS OS VIFs DERAM MENOR QUE 5

# Gerar tabela
tabela_publicacao <- tbl_regression(modelo_logistico, 
                                    exponentiate = TRUE, # CRUCIAL: Converte para Odds Ratio
                                    pvalue_fun = ~style_pvalue(.x, digits = 3),
                                    label = list(
                                      #faixa_etaria_prim_cons ~ "Faixa Etária (anos)",
                                      corbranca ~ "Raça Branca",
                                      status_conjugal_bin ~ "Situação Conjugal",
                                      escolaridade_agrupada ~ "Escolaridade",
                                      classe_trabalho ~ "Ocupação",
                                      estadiam_bin ~ "Estadiamento",
                                      tratamento_agrupado ~ "Tratamento Realizado",
                                      alcoolismo_agrupado ~ "Histórico de Álcool",
                                      tabagismo_agrupado ~ "Histórico de Fumo",
                                      origemenSUS ~ "Encaminhado pelo SUS"
                                    )) %>%
  bold_p(t = 0.05) %>% # Negrita quem for significativo
  bold_labels() %>%    # Negrita os nomes das variáveis
  italicize_levels()   # Coloca as categorias em itálico

# Visualizar
tabela_publicacao

