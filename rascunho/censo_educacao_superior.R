library(data.table)
library(readxl)
library(tidyverse)
library(stringi)
library(magrittr)
library(sf)
library(geosphere)

#### Importacao dos dados ####
# arquivos originais extraidos do endereco abaixo
# http://dados.gov.br/dataset/microdados-do-censo-da-educacao-superior/resource/572ea703-014a-47eb-8fd6-cdc1d0e2d60c
arq_aluno <- "/home/sillas/R/data/DM_ALUNO.CSV"
arq_ies <- "/home/sillas/R/data/DM_IES.CSV"

df_aluno <- fread(arq_aluno,
                  select = c("CO_IES", "CO_UF_NASCIMENTO",
                             "CO_MUNICIPIO_NASCIMENTO", "ANO_INGRESSO"))


df_ies <- fread(arq_ies) %>% 
  select(CO_IES, NO_IES, SGL_IES, DS_CATEGORIA_ADMINISTRATIVA, CO_MUNICIPIO_IES, 
         NO_MUNICIPIO_IES, CO_UF_IES, SGL_UF_IES, NO_REGIAO_IES)

# tabela de municipios
# url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/redes_e_fluxos_geograficos/gestao_do_territorio/bases_de_dados/xls/Base_de_dados_dos_municipios.xls"
# download.file(url,
#               destfile = "municipios.xls",
#               method = "curl")
# 
# df_cidades <- read_excel("municipios.xls") %>%
#   select(CodUF, Codmundv, NomeMunic) %>% 
#   purrr::set_names(c("CO_UF", "CO_MUNICIPIO", "NO_MUNICIPIO")) %>% 
#   mutate(CO_UF = as.integer(CO_UF))
df_coord <- read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/Municipios_Brasileiros.csv") %>% 
  select(-Estado) %>% 
  set_names(c("CO_MUNICIPIO", "NO_MUNICIPIO", "CO_UF", "SGL_UF", "LATITUDE",
              "LONGITUDE"))

# criar tabela de regioes
df_uf_regiao <- df_ies %>% 
  distinct(SGL_UF_IES, NO_REGIAO_IES) %>% 
  rename(SGL_UF = SGL_UF_IES, REGIAO = NO_REGIAO_IES)

# df_cidades <- df_coord %>% 
#   select(CO_UF, CO_MUNICIPIO, NO_MUNICIPIO)
# 
# df_uf <- df_ies %>% 
#   distinct(CO_UF_IES, SGL_UF_IES) %>% 
#   purrr::set_names(c("CO_UF", "SGL_UF"))


# Importar o shapefile
link <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2016/Brasil/BR/"
dir.create("data/base_mapa")
arquivo <- "data/base_mapa/shp_uf.zip"
link_estados = paste0(link, "br_unidades_da_federacao.zip")
download.file(link_estados, arquivo)
unzip(arquivo, exdir = "data/base_mapa")

# 02 - Importar arquivos de shapefile com a funcao sf::st_read()
estados <- sf::read_sf("data/base_mapa/BRUFE250GC_SIR.shp")

#### Juntar bases ####
df_principal <- inner_join(df_aluno, df_ies, by = "CO_IES") %>% 
  inner_join(df_coord,
             by = c("CO_MUNICIPIO_NASCIMENTO" = "CO_MUNICIPIO")) %>% 
  # renomear colunas referentes aos alunos
  rename(SGL_UF_NASCIMENTO = SGL_UF, 
         NO_MUNICIPIO_NASCIMENTO = NO_MUNICIPIO) %>% 
  select(-CO_UF_IES) %>% 
  # concatenar colunas de municipio e UF
  mutate(MUNICIPIO_ALUNO = paste0(NO_MUNICIPIO_NASCIMENTO, " (", SGL_UF_NASCIMENTO, ")"),
         MUNICIPIO_IES = paste0(NO_MUNICIPIO_IES, " (", SGL_UF_IES, ")")) %>% 
  # corrigir encoding da coluna municipio ies
  mutate(MUNICIPIO_IES = iconv(MUNICIPIO_IES, "ISO-8859-1", "UTF-8")) %>% 
  # filtrar casos onde o municipio do aluno eh diferente da ies
  filter(MUNICIPIO_ALUNO != MUNICIPIO_IES)
  
# padronizar nome das colunas de municipio
limpar_nome_cidade <- . %>% 
  # remover acentos
  stri_trans_general("Latin-ASCII") %>% 
  # converter para maiusculo
  str_to_upper()

df_principal <- df_principal %>% 
  mutate_at(vars(MUNICIPIO_ALUNO, MUNICIPIO_IES), limpar_nome_cidade)

# bkp <- df_principal

  # acrescentar colunas de regiao do aluno
df_principal <- df_principal %>% 
  left_join(df_uf_regiao, by = c("SGL_UF_NASCIMENTO" = "SGL_UF")) %>% 
  rename(NO_REGIAO_ALUNO = REGIAO)

# agregar dados para contar fluxos mais comuns
# df_agg <- df_principal %>%
#   select(cod_mun_aluno = CO_MUNICIPIO_NASCIMENTO, nome_mun_aluno =  MUNICIPIO_ALUNO,
#          cod_mun_ies = CO_MUNICIPIO_IES, nome_mun_ies = MUNICIPIO_IES) %>%
#   group_by_all() %>%
#   summarise(qtd = n()) %>%
#   ungroup() %>%
#   arrange(desc(qtd))
write_rds(df_principal, "/home/sillas/R/data/df_principal_censo_educ_superior.rds")

#### Até acima o codigo é o mesmo do gist ####
system.time(df_principal <- read_rds("/home/sillas/R/data/df_principal_censo_educ_superior.rds"))

# Novidade: fazer o descrito acima para cada regiao
df_agg_regiao_ies <- df_principal %>% 
  split(.$NO_REGIAO_IES)
  
agregar_dados <- . %>% 
  select(cod_mun_aluno = CO_MUNICIPIO_NASCIMENTO,
         nome_mun_aluno =  MUNICIPIO_ALUNO,
         cod_mun_ies = CO_MUNICIPIO_IES,
         nome_mun_ies = MUNICIPIO_IES) %>% 
  group_by_all() %>% 
  summarise(qtd = n()) %>% 
  ungroup() %>% 
  arrange(desc(qtd))

df_agg_regiao_ies <- df_agg_regiao_ies %>% 
  map(agregar_dados)

#### Criacao do mapa ####
myPng <- function(..., width=13, height=13, res=300) {
  png(..., width=width*res, height=height*res, res=res)
  }


# fazer processo abaixo para cada regiao
for (j in seq_along(df_agg_regiao_ies)){
  print(j)
  df_agg <- df_agg_regiao_ies[[j]]
  
  # Criar dataframes separados para os municipios do aluno (origem ou org)
  # e da IES (destino ou dest)
  df_org <- select(df_agg, org = cod_mun_aluno)
  df_dest <- select(df_agg, dest = cod_mun_ies)
  # criar variáveis para a quantidade de cada fluxo
  qtd <- df_agg$qtd
  maxqtd <- max(qtd)
  # acrescentar latitude e longitude para cada cidade dois dataframes
  df_org %<>% left_join(df_coord, by = c('org' = 'CO_MUNICIPIO'))
  df_dest %<>% left_join(df_coord, by = c('dest' = 'CO_MUNICIPIO'))
  
  # inicializar mapa
  nome_regiao_loop <- names(df_agg_regiao_ies)[j]
  nome_arquivo_mapa <- paste0(nome_regiao_loop, ".png")
  myPng(nome_arquivo_mapa)
  
  par(bg = "#000000")
  plot(st_geometry(estados), col="#191919")
  
  # além de um escala que vai de preto (valores menores) a azul (valores maiores)
  pal <- colorRampPalette(c("#333333", "white", "#1292db"))
  colors <- pal(100)
  # título
  titulo <- str_glue(paste0(
    "Mapeamento do movimento migratório", "\n",
    "universitário nas IES da região {nome_regiao_loop}"
  ))
  
  title(as.character(titulo), col.main = "white", cex.main = 1)
  
  # referencia abaixo do mapa
  mtext("Fonte: Censo do Ensino Superior 2014 \n Autor: Sillas Gonzaga (sillasgonzaga.com)",
        col = "white", side = 1, line = 1, cex = 1)
  # Para gerar as linhas, a função gcIntermediate é necessária,
  # porém dois dos argumentos dela, p1 e p2, só podem ser vetores de tamanho 2
  # portanto, preciamos fazer um for loop para plotar cada linha de df_org e df_dest individualmente
  # ao final do loop, todas as linhas estarão plotadas no mapa
  n <- nrow(df_agg)
  #n <- 100
  
  for (i in 1:n) {
    p1 = c(df_org[i, ]$lon, df_org[i, ]$lat)
    p2 = c(df_dest[i, ]$lon, df_dest[i, ]$lat)
    inter <- geosphere::gcIntermediate(p1, p2, n = 100, addStartEnd = TRUE)
    # determinar cor de cada fluxo
    colindex <- ((qtd[i]/maxqtd)*length(colors)) %>% round
    mycol = colors[colindex]
    lines(inter, col = mycol, lwd = 0.8)
  }
  
  dev.off()
  
}

