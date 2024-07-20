###########################################
# Instalar e carregar pacotes necessários
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, survey, vroom, gtsummary, ggplot2, readxl, rio, censobr)

# Definir o diretório de trabalho (ajuste se necessário)
setwd("C:/Users/suelt/OneDrive/Área de Trabalho/Métodos Quantitativos/Tudo")

# Carregar o dicionário de dados do Censo 2010
dic_censo2010 <- rio::import("Layout_microdados_Amostra.xls",
                             sheet = "PESS", range = "A2:I246") %>%
  rename(varname = 'VAR', 
         inicio = 'POSIÇÃO INICIAL', 
         fim = 'POSIÇÃO FINAL') %>%
  filter(!is.na(inicio)) %>%
  select(varname, inicio, fim)
# Nome do arquivo de dados
arquivo_dados <- "Amostra_Pessoas_21.txt"
# Carregar os dados usando o dicionário de dados
dados_censo2010 <- vroom::vroom_fwf(
  file = arquivo_dados,
  col_positions = fwf_positions(dic_censo2010$inicio, dic_censo2010$fim, col_names = dic_censo2010$varname),
  col_types = cols(V0010 = col_double())
)
# Filtrar população residente em domicílios particulares permanentes vivendo em união conjugal
dados_censo2010 <- dados_censo2010 %>%
  filter(V0637 == 1)
# Selecionar apenas as variáveis especificadas
variaveis_socioeconomicas <- c("V0601","V6036", "V6531", "V6532", "V1006", "V0606",
                               "V6400", "V6121", "V0657", "V0639")

dados_censo2010 <- dados_censo2010 %>%
  mutate(V6036 = as.numeric(V6036),
         V6531 = as.numeric(V6531),
         V6532 = as.numeric(V6532))

# Substituir valores na variáveis
dados_censo2010 <- dados_censo2010 %>%
  mutate(V0601 = recode(V0601, `1` = "Masculino", `2` = "Feminino")) %>%
  mutate(V1006 = recode(V1006, `1` = "Urbano", `2` = "Rural")) %>%
  mutate(V0606 = recode(V0606, `1` = "Branca", `2` = "Preta", '3' = "Amarela", '4' = "Parda", 
                        '5' = "Indígena", '9' = "Ignorado")) %>%
  mutate(V6400 = recode(V6400, '1'= "Sem instrução e fundamental incompleto", 
                        '2'= "Fundamental completo e médio incompleto", 
                        '3'= "Médio completo e superior incompleto", 
                        '4'= "Superior completo", 
                        '5'= "Não determinado")) %>%
  mutate(V0657 = recode(V0657, '1'= "Sim", '0'= "Não", '9'= "Ignorado")) %>%
  mutate(V0639 = recode(V0639,'1'= "Casamento civil e religioso", '2'= "Só casamento civil",
                        '3'= "Só casamento religioso",'4'= "União consensual"))

dados_censo2010 = dados_censo2010 %>% 
  mutate(V6121 = case_when(as.numeric(V6121) %in% (0:2) ~ "Sem religião, Agnóstico e Ateu",
                           as.numeric(V6121) == 110 ~ "Católico",
                           as.numeric(V6121) %in% (210:529) ~ "Evangélico",
                           as.numeric(V6121) %in% (990:999) ~ "Não declararam",
                           TRUE ~ "Outra religião"))

# Renomear as variáveis
dados_censo2010 <- dados_censo2010 %>%
  rename(
    Sexo = V0601,
    Idade = V6036,
    Renda_Percapita = V6531,
    Num_de_Sal_Min = V6532,
    Zona = V1006,
    Etinia = V0606,
    Escolaridade = V6400,
    Religiao = V6121,
    Recebe_Beneficio_do_Governo = V0657,
    Natureza_da_Uniao = V0639
  )

# Criar a variável grupo etário
dados_censo2010 <- dados_censo2010 %>%
  mutate(Grupo_Etario = cut(Idade, 
                            breaks = c(0, 17, 29, 44, 59, Inf), 
                            labels = c("0-17", "18-29", "30-44", "45-59", "60+")))

# Criar a variável faixa de renda
dados_censo2010 <- dados_censo2010 %>%
  mutate(Faixa_Renda = cut(Num_de_Sal_Min, 
                           breaks = c(-Inf, 1, 2, 3, 5, Inf), 
                           labels = c("Até 1 SM", "1-2 SM", "2-3 SM", "3-5 SM", 
                                      "Mais de 5 SM")))

# Tabela descritiva para todas as variáveis
tabela_descritiva <- dados_censo2010 %>%
  select(Sexo, Idade, Grupo_Etario, Renda_Percapita, Num_de_Sal_Min, 
         Faixa_Renda, Zona, Etinia, Escolaridade, Religiao, 
         Recebe_Beneficio_do_Governo, Natureza_da_Uniao) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_ci() # Adicionar intervalo de confiança de 95%

# Exibir a tabela
print(tabela_descritiva)
##############################################################
# Função para gerar tabela descritiva de uma variável
tabela_descritiva <- function(variavel) {
  tabela <- dados_censo2010 %>%
    select(all_of(variavel)) %>%
    tbl_summary(
      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
      digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
    ) %>%
    add_ci() # Adicionar intervalo de confiança de 95%
  
  print(tabela)
}

# Chame a função para cada variável individualmente
tabela_descritiva("Sexo")
tabela_descritiva("Idade")
tabela_descritiva("Grupo_Etario")
tabela_descritiva("Renda_Percapita")
tabela_descritiva("Num_de_Sal_Min")
tabela_descritiva("Faixa_Renda")
tabela_descritiva("Zona")
tabela_descritiva("Etinia")
tabela_descritiva("Escolaridade")
tabela_descritiva("Religiao")
tabela_descritiva("Recebe_Beneficio_do_Governo")
tabela_descritiva("Natureza_da_Uniao")
##############################################################
# Verificar associação entre características sociodemográficas e status de união consensual

# Teste qui-quadrado para variáveis categóricas
resultado_chisq <- dados_censo2010 %>%
  select(Sexo, Zona, Etinia, Escolaridade, Religiao, Recebe_Beneficio_do_Governo, Natureza_da_Uniao) %>%
  tbl_cross(row = Natureza_da_Uniao, col = Sexo, percent = "row") %>%
  add_p(test = "chisq.test") %>%
  add_ci() # Adicionar intervalo de confiança de 95%

# Exibir resultados do teste qui-quadrado
print(resultado_chisq)

# Teste t para variáveis contínuas
resultado_ttest <- dados_censo2010 %>%
  select(Natureza_da_Uniao, Idade, Renda_Percapita) %>%
  group_by(Natureza_da_Uniao) %>%
  summarise(across(c(Idade, Renda_Percapita), list(mean = ~ mean(.), sd = ~ sd(.), t_test = ~ t.test(.)$p.value)))

# Exibir resultados do teste t
print(resultado_ttest)

# Criação de gráficos

# Gráfico de distribuição por sexo
ggplot(dados_censo2010, aes(x = Sexo)) +
  geom_bar(aes(fill = Sexo)) +
  labs(title = "Distribuição por Sexo", x = "Sexo", y = "Contagem")

# Gráfico de distribuição de idade
ggplot(dados_censo2010, aes(x = Idade)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribuição de Idade", x = "Idade", y = "Contagem")

# Gráfico de renda domiciliar per capita
ggplot(dados_censo2010, aes(x = Renda_Percapita)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Distribuição de Renda Domiciliar Per Capita", x = "Renda per capita", y = "Contagem")

# Gráfico de raça/cor
ggplot(dados_censo2010, aes(x = Etinia)) +
  geom_bar(aes(fill = Etinia)) +
  labs(title = "Distribuição por Raça/Cor", x = "Raça/Cor", y = "Contagem")

# Gráfico de nível de instrução
#ggplot(dados_censo2010, aes(x = Escolaridade)) +
#  geom_bar(aes(fill = Escolaridade)) +
#  labs(title = "Distribuição por Nível de Instrução", x = "Nível de Instrução", y = "Contagem")
#tirando a legenda
ggplot(dados_censo2010, aes(x = Escolaridade)) +
  geom_bar(aes(fill = Escolaridade)) +
  labs(title = "Distribuição por Nível de Instrução", x = "Nível de Instrução", y = "Contagem") +
  theme(legend.position = "none")
###########################################


