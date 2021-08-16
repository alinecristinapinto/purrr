library(tidyverse)

# 1. Lendo arquivos ----
# Funções abordadas: map, set_names

nomes_arquivos <- c("Pagamento", "Passagem", "Trecho", "Viagem")

arquivos <- glue::glue('Aulas Pacotes/2021_{nomes_arquivos}.csv') %>% 
  map(data.table::fread) %>% 
  set_names(nomes_arquivos)

# 2. Inspecionando e juntando bancos ----
# Funções abordadas: pluck, walk, reduce, imap_dfc

walk(nomes_arquivos,
     ~View(pluck(arquivos, .x), title = .x))

nomes_colunas <- map(arquivos, colnames)
reduce(nomes_colunas, intersect) # Apenas pra ver qual coluna está em todas
arquivos %>% imap_dfc( # Apenas pra ver o número de linhas de cada banco
  ~nrow(pluck(arquivos, .y)))

dados_juntos <- reduce(arquivos, inner_join)

# 3. Manipulando o banco único ----

dados_juntos <- dados_juntos %>% 
  select(c(1, 4, 6, 9:24, 26, 40, 42, 44))

dados_juntos <- dados_juntos %>% 
  distinct(.keep_all = T)

# 4. Análises de viagens e rotas ----
# Funções abordadas: modify, imap, slowly, rate_delay, map2, cross2

dados_viagem_gasto <- dados_juntos %>% 
  filter(`Tipo de pagamento` == "PASSAGEM") %>% 
  select(c(1:3, 5:12))

freq_viagem <- dados_viagem_gasto %>% # Entendendo os valores possíveis (e quantidade) para cada categoria
  select(c(1, 5, 6, 9)) %>%
  distinct(.keep_all = T) %>% select(-1) %>% 
  map(~count(
    as_tibble(.x), across(everything())
  ))
# tidyselect::everything e dplyr::across, mas talvez seja interessante mencionar que 
# elas ajudam a manter um código que tem abordagem mais funcional e menos estruturada,
# como é o caso do purrr

# 4.1 Viagens mais comuns

freq_viagem <- freq_viagem %>% 
  modify(~{filter(.x, value != "Brasil") %>% top_n(10)})

graficos_freq_viagem <- function (dados, nomes) {
  dados %>% 
    ggplot(aes(fct_infreq(value), n)) + 
    geom_col() +
    coord_flip() +
    labs(x = nomes,
         y = "Contagem",
         title = glue::glue("Contagem de {nomes}"))
}

graficos_finais <- imap(freq_viagem, graficos_freq_viagem)

walk(graficos_finais, slowly(print, rate = rate_delay(5)))

# 4.2 Viagens de ida e volta

gasto_porviagem <- dados_viagem_gasto %>% 
  mutate(Valor = as.numeric(str_replace(Valor, ",", "."))) %>% 
  group_by(`Identificador do processo de viagem`) %>%
  summarise(across(everything(), 
                   ~ifelse(is.numeric(.x), sum(.x), first(.x)))) %>% 
  mutate(`Tipo de viagem` = ifelse(
    `País - Origem ida` != `País - Destino ida`, "Internacional",
    ifelse(`UF - Origem ida` != `UF - Destino ida`, "Interestadual",
           "Intraestadual"))) %>% 
  ungroup() %>% 
  select(-c(6:11))

variaveis_analise_gasto <- c("Nome do órgão superior", "Meio de transporte", "Tipo de viagem")
funcoes_analise_gasto <- list(geom_violin(), geom_boxplot(), geom_jitter())
combinacoes_analise_gasto <- cross2(funcoes_analise_gasto,
                                    variaveis_analise_gasto)

graficos_gasto_porviagem <- function (variavel, funcao_geom) {
  gasto_porviagem %>% 
    ggplot(aes(get(variavel), Valor)) +
    labs(x = variavel,
         y = "Gasto da viagem",
         title = glue::glue("Gasto da viagem de acordo com {str_to_lower(variavel)}")) +
    funcao_geom
}

graficos_finais_gasto_porviagem <- map2(
  .x = map(combinacoes_analise_gasto, 1), 
  .y = map(combinacoes_analise_gasto, 2),
  ~graficos_gasto_porviagem(.y, .x))

walk(graficos_finais_gasto_porviagem, slowly(print, rate = rate_delay(5)))
