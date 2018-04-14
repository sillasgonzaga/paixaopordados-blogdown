#### analise me poupe --------------

#### config inicial ####

library(reticulate)
reticulate::use_python("/home/sillas/anaconda3/bin/python", required = TRUE)
py_config()
#reticulate::use_condaenv("conda 4.4.9")
library(lexiconPT)
library(tidytext)
library(tidyverse)
library(magrittr)

#### 01 - baixar captions dos videos ####

# montar query do youtube-dl
fields_raw <- c("id", "title", "alt_title", "creator", "release_date", "timestamp",
                "upload_date", "duration", "view_count", "like_count", "dislike_count",
                "comment_count")

fields <- fields_raw %>% 
  map_chr(~paste0("%(", ., ")s")) %>% 
  paste0(collapse = "&&&") %>% 
  paste0('"', ., '"')

# canal do me poupe
channel_url <- "https://www.youtube.com/channel/UC8mDF5mWNGE-Kpfcvnn0bUg"

cmd_ytdl <- str_glue("youtube-dl -o {fields} -i -v -w --skip-download --write-auto-sub --sub-lang pt {channel_url}")
# acrescentar diretorio
pasta_captions <- "/home/sillas/R/Projetos/paixaopordados-blogdown/data/mepoupe"
fs::dir_create(pasta_captions)
cmd <- str_glue("cd {pasta_captions} && {cmd_ytdl}")
# imprimir comando
cat(cmd)

#### 02 - Ler captions e converter para dataframe ####

arquivos_captions <- dir(pasta_captions, pattern = '*.vtt', full.names = TRUE)
length(arquivos_captions)

# carregar funcao para converter de caption para texto limp
source_python("rascunho/youtube_caption_to_text.py")
limpar_caption <- function(arquivo){
  caption_raw <- caption_to_vector(arquivo)
  # remover string apos \n
  n <- length(caption_raw)
  caption <- c(str_remove_all(caption_raw[-n], "[\n].*"), caption_raw[n])
  caption <- unique(caption)
  caption <- iconv(caption, from = "UTF-8", to = "ASCII//TRANSLIT")
  caption <- paste0(caption, collapse = "\n")
  caption
}

# funcao para extrair metadados do video baseado no titulo
extrair_metadados <- function(arquivo, pasta = pasta_captions, fields = fields_raw){
  mat <- str_split(arquivo, "&{3}", simplify = TRUE)
  # substituir elemento da primeira coluna por id (remover pasta do nome)
  mat[1,1] <- mat[1,1] %>% str_remove_all(pasta) %>% str_remove_all("/")
  
  # renomear colunas
  cols <- fields[1:ncol(mat)]
  colnames(mat) <- cols
  as.tibble(mat)
}

# funcao para juntar tudo num dataframe so
caption_to_df <- function(arquivo, ...){
  
  caption <- limpar_caption(arquivo)
  meta <- extrair_metadados(arquivo, ...)
  meta <- meta %>% mutate(caption = caption)
  
  meta
}

### gerar dataframe para todos os videos
df <- arquivos_captions %>% 
  map_df(caption_to_df)

# remover colunas que nao tem nada de util
df <- df %>% 
  select(-alt_title, -creator, -release_date, -timestamp, -comment_count)

write_rds(df, "data/df_me_poupe_clean.Rds")

#### 03 - analise dos dados ####
library(lubridate)
library(magrittr)
library(lexiconPT)
library(tidytext)
library(tidyverse)
df <- read_rds("data/df_me_poupe_clean.Rds")
# modificar tipos das colunas
df$upload_date %<>% ymd()
df$duration %<>% as.numeric()
df$view_count %<>% as.numeric()
df$like_count %<>% as.numeric()
df$dislike_count %<>% as.numeric()




head(df_tokens, 20)

# carregar lexico de sentimentos
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")
dict <- unique(sentiLex_lem_PT02)

# separar em tokens
df_tokens <- df %>% 
  unnest_tokens(palavra, caption) %>% 
  distinct(id, palavra)


df_tokens <- df_tokens %>% 
  inner_join(dict, by = c("palavra" = "term"))


head(df_tokens)
# mudar polaridade da palavra investir
df_tokens$polarity[df_tokens$palavra == "investir"] <- 1

# somar polaridade dos videos para obter sentimento
df_tokens_agg <- df_tokens %>% 
  #filter(polarity != 0) %>% 
  group_by(id) %>% 
  summarise(
    pct_tokens_pos = mean(polarity == 1),
    pct_tokens_neg = mean(polarity == -1),
    sentimento = sum(polarity),
    qtd_palavras = n()
    ) %>% 
  mutate(sentimento_norm = sentimento/qtd_palavras) %>% 
  # filtrar videos com pelo menos 10 palavras polarizadas
  filter(qtd_palavras >= 10)

# acrescentar dados ao dataframe principal
df_sent <- inner_join(df, df_tokens_agg, by = "id")

df_sent %>% glimpse()
hist(df_sent$pct_tokens_neg)

df_sent %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  filter(duration  <= 1000) %>% 
  cor()

df_sent %>% 
  na.omit() %>% 
  #filter(duration <= 1000) %>% 
  ggplot(aes(x = duration, y = pct_tokens_pos)) +
  geom_point()

# sentimentos mais negativos
df_sent %>% 
  ungroup() %>% 
  arrange(desc(pct_tokens_neg)) %>% 
  select(id, title)
  

df_sent %>% 
  select(d = upload_date, starts_with("pct")) %>% 
  gather(tipo, sent, -d) %>% 
  ggplot(aes(x = d, y = sent, color = tipo)) + 
  geom_line()



#### topic modeling ####
library(stm)
library(tm)

# palavras mais comuns
df_top_palavras <- df %>% 
  select(id, caption) %>% 
  unnest_tokens(palavra, caption) %>% 
  count(palavra, sort = TRUE)

df_top_palavras %>% 
  filter(!palavra %in% stopwords_pt)

minhas_stopwords <- c("voce", "pra", "gent", "gente", "aqui",
                      "vai", "la", "so", "ai")
stopwords_pt <- tm::stopwords("pt")
sw_pt <- c(stopwords_pt, minhas_stopwords)

processed <- stm::textProcessor(df$caption, metadata = df, language = "portuguese",
                                customstopwords = sw_pt)

plotRemoved(processed$documents, lower.thresh = c(1, 5, 10, 15, 20, 25))

out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                          lower.thresh = 15)

system.time({ # 563 segundos
  storage <- stm::searchK(out$documents, out$vocab, K = c(3:15),
                          data = out$meta)
})


storage$results %>% 
  select(K:semcoh) %>% 
  gather(measure, value, -K) %>% 
  ggplot(aes(x = K, y = value)) + 
  geom_line() + geom_point() +
  facet_wrap(~ measure, ncol = 1, scales = "free_y")


# K otimo: 18

## estimate
fit <- stm(
  documents = out$documents, vocab = out$vocab, data = out$meta,  K = 5,
  max.em.its = 75, init.type = "Spectral"
  )

plot.STM(fit, type = "summary")
plot.STM(fit, type = "labels")
plot.STM(fit, type = "perspectives", topics = c(14, 1))

topic_prevalence <- fit$theta
colnames(topic_prevalence) <- c("mulher", "rj_sp", "despesas", "renda_fixa", "cartao")
# extrair o nome do maximo por linha
topico_video <- colnames(topic_prevalence)[apply(topic_prevalence, 1, which.max)]

df_topico <- df %>% mutate(topico = topico_video)

# intersecao topico com sentimento
df_sent_stm <- inner_join(
  df_sent,
  df_topico %>% select(id, topico)
)


df_sent_stm %>% count(topico)
df_sent_stm %>% 
  ggplot(aes(x = sentimento_norm)) +
  geom_density(aes(fill = topico), alpha = 0.5)# +  facet_wrap(~ topico, ncol = 1)


#### network analysis ####
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

minhas_stopwords <- c("voce", "pra", "gent", "gente", "aqui",
                      "vai", "la", "so", "ai")
stopwords_pt <- tm::stopwords("pt")
sw_pt <- c(stopwords_pt, minhas_stopwords)


df <- read_rds("data/df_me_poupe_clean.Rds")
mepoupe_2gram <- df %>% 
  unnest_tokens(bigram, caption, token = "ngrams", n = 2) %>% 
  separate(bigram, into = c("p1", "p2"), sep = " ")

mepoupe_2gram %>% count(p1, p2, sort = TRUE)

# remover stopwords
mepoupe_2gram <- mepoupe_2gram %>% 
  filter(!p1 %in% sw_pt) %>% 
  filter(!p2 %in% sw_pt)


# top_100_palavras <- df %>%
#   unnest_tokens()

mepoupe_2gram_cnt <- mepoupe_2gram %>%
  distinct(id, p1, p2) %>%
  count(p1, p2, sort = TRUE) %>%
  filter(n >= 20)

grafo <- mepoupe_2gram_cnt %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

grafo %>%
  ggraph("fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), hjust = 1, vjust = 1, size = 3) +
  theme_void()

cm <- edge.betweenness.community(grafo)
plot(cm, grafo, vertex.label = NA, vertex.size = 1,
     edge.arrow.size = .2)

##

## cadeia de markov ####
library(tidyverse)
library(tidytext)
library(markovchain)

df <- read_rds("data/df_me_poupe_clean.Rds")
# mepoupe_2gram <- df %>% 
#   unnest_tokens(bigram, caption, token = "ngrams", n = 2) %>% 
#   separate(bigram, into = c("p1", "p2"), sep = " ")

top_videos <- df %>% 
  mutate(view_count = as.numeric(view_count)) %>% 
  top_n(60, view_count) %>% 
  select(id, title, view_count) %>% 
  arrange(desc(view_count))
top_videos



# matrix_me_poupe <- df %>% 
#   unnest_tokens(palavra, caption) %>% 
#   filter(palavra %in% top_palavras) %>% 
#   mutate(palavra_posterior = lead(palavra)) %>% 
#   filter(palavra != palavra_posterior) %>% 
#   count(palavra, palavra_posterior, sort = TRUE)

library(markovchain)
vetor_palavras <- df %>% 
  unnest_tokens(palavra, caption) %>% 
  filter(id %in% top_videos$id) %>% 
  pull(palavra)
length(vetor_palavras)  

system.time(mc <- markovchainFit(vetor_palavras))



palavra_inicial <- "mulher"
predict(mc$estimate, newdata = palavra_inicial, n.ahead = 50)




eh_algarismo <- function(x){
  !is.na(as.numeric(x))
}
