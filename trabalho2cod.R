library(tidyverse)

# 1. Lendo arquivos ----
# Funções abordadas: map, set_names

nomes_arquivos <- c("Pagamento", "Passagem", "Trecho", "Viagem")

arquivos <- glue::glue('2021_{nomes_arquivos}.csv') %>% 
  map(~vroom::vroom(.x, locale = locale(encoding = "ISO-8859-1"))) %>% 
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

#5. Comparando as funções do pacote purrr com o R base ----

#Map, Map2, Pmap e Imap----
# A função map aplica uma determinada função à cada elemento de uma lista e retorna 
# uma nova lista com os resultados.
# Por exemplo, usamos o map anteriormente para criar a lista nomes_colunas 
# aplicando a função colnames à lista com os arquivos. 

nomes_colunas <- map(arquivos, colnames)

#Fazendo o mesmo com o R base:

nomes_colunas_base <- list()
for (i in seq_along(arquivos)) {
  nomes_colunas_base[i] <- list(colnames(arquivos[[i]])) 
}
names(nomes_colunas_base) <- c("Pagamento", "Passagem", "Trecho", "Viagem")

#E agora com o Lapply
nomes_colunas_apply <- lapply(arquivos, colnames)

#O map2 aplica a função em duas listas diferentes e retorna uma única lista com o 
#resultado. Por exemplo a função seguinte compara o tamanho de dois objetos e vamos 
#aplicá-la nas listas nomes_coluna e nomes_coluna_base

igual_ou_diferente <- function(x, y) {
  if (length(x) == length(y)) return(T)
  else return(F)
}

#Usando map2:

resultado2purrr <- map2(nomes_colunas, nomes_colunas_base, igual_ou_diferente)

#Usando R base:

resultado2base <- list()
for (i in seq_along(nomes_colunas)) {
  resultado2base[i] <- igual_ou_diferente(nomes_colunas[i], nomes_colunas_base[i]) 
}

#Usando o apply
resultado2apply <- lapply(nomes_colunas, nomes_colunas_apply, FUN = igual_ou_diferente)

#O pmap funciona da mesma forma, porém aceitando tres ou mais listas para aplicar
#a função determinada.

#Uma nova função para comparar o tamanho de tres listas
igual_ou_diferente3 <- function(x, y, z) {
  if (length(x) == length(y) || length(x) == length(z)) return(T)
  else return(F)
}


resultado3purrr <- pmap(list(nomes_colunas, nomes_colunas_apply, nomes_colunas_base), igual_ou_diferente3)

#Usando o R base
resultado3base <- list()
for (i in seq_along(nomes_colunas)) {
  resultado3base[i] <- igual_ou_diferente3(nomes_colunas[i], nomes_colunas_base[i], nomes_colunas_apply[i]) 
}

#Usando o apply
resultado3apply <- lapply(nomes_colunas, nomes_colunas_apply, nomes_colunas_base, FUN = igual_ou_diferente3)


#O imap é um caso particular do map2, quando uma função é aplicada a uma lista e seu
#respectivo índice, que pode ser considerado uma segunda lista.

ordem_do_transporte_purrr <- imap(freq_viagem[[1]][[1]], ~paste(.x, .y, sep = "/"))

#Fazendo o mesmo com R base




#Walk----
#A função walk é muito similar à função map contudo ela não retorna nada na tela, 
#ideal para quando necessitamos de funções que queremos apenas o efeito colateral.
#Por exemplo, podemos usar 

walk(freq_viagem, print)

#para visualizar as tabelas que resumem a frequencia do meio de transporte, origem
#e destino das viagens registradas em 2021. 
#Podemos ter o mesmo resultado com o R base com:

for (i in seq_along(freq_viagem)){
  print(freq_viagem[i])
}

#A função walk possui variações como walk2, pwalk, iwalk, etc que seguem a mesma 
#lógica das variações da função map.



