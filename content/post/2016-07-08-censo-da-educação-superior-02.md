---
title: 'Censo da Educação Superior (2): Como plotar o movimento migratório universitário
  no Brasil em um mapa'
author: ''
date: '2016-07-08'
slug: censo-da-educação-superior-02
categories:
  - R
tags:
  - censo-educacao
description: ''
topics: []
---

No primeiro post sobre os microdados do Censo da Educação Superior, falei sobre as cidades e estados que mais atraem universitários de fora. Neste segundo post, discutirei mais a fundo este movimento migratório universitário, incluindo a elaboração de um rebusco mapa de fluxo migratório.


```r
library(stringr)
library(tidyr)
library(magrittr)
library(dplyr)
library(feather)
library(maptools)
library(maps)
library(geosphere)
library(knitr)
```

# Importação dos dados
Para plotar em um mapa os universitários que estudam em uma cidade diferente da que nasceram, precisamos de dois tipos de dados:
- Dados espaciais dos municípios brasileiros;  
- Um [shapefile](https://pt.wikipedia.org/wiki/Shapefile) dos estados brasileiros
- Dados de universitários que estudam em uma cidade diferente da que nasceram;  

## Dados espaciais

Para poder localizar os municípios brasileiros em um mapa, precisamos de dados sobre suas latitudes e longitudes. Em minhas pesquisas, a melhor fonte que eu encontrei foi [neste site](http://www.monolitonimbus.com.br/coordenadas-geograficas-das-cidades-do-brasil/), que fornece o link para baixar um arquivo KML contendo os dados que precisamos. Para fazer a conversão de KML para um formato tratável pelo R, usei comandos em linux, como indicado pelo próprio site.

**ATENÇÃO**: Eu baixei o KML descrito acima em 08/06/2016, mas no momento que escrevo este post, o ftp do IBGE para baixar esse arquivo está fora do ar. Por isso, eu disponibilizei os arquivos KML e CSV no [meu Github](https://github.com/sillasgonzaga/sillasgonzaga.github.io/tree/master/data). 



{% highlight r %}
df_coord <- read.csv2("https://raw.githubusercontent.com/sillasgonzaga/sillasgonzaga.github.io/master/data/coordenadas_BR.csv",
                      stringsAsFactors = FALSE, header = FALSE)

# Ver estrutura do arquivo
head(df_coord, 10)
```



{% highlight text %}
##                   V1                                             V2
## 1                                                                  
## 2                                                                  
## 3    BARRA DO QUARAÍ                              RIO GRANDE DO SUL
## 4         URUGUAIANA                              RIO GRANDE DO SUL
## 5             QUARAÍ                              RIO GRANDE DO SUL
## 6          SANT&apos                              ANA DO LIVRAMENTO
## 7  ANA DO LIVRAMENTO -55.5348142679597,-30.8893840103812,200.677824
## 8             ITAQUI                              RIO GRANDE DO SUL
## 9           ALEGRETE                              RIO GRANDE DO SUL
## 10         MAÇAMBARÁ                              RIO GRANDE DO SUL
##                   V3
## 1                   
## 2                   
## 3    BARRA DO QUARAÍ
## 4         URUGUAIANA
## 5             QUARAÍ
## 6  RIO GRANDE DO SUL
## 7                   
## 8             ITAQUI
## 9           ALEGRETE
## 10         MAÇAMBARÁ
##                                                        V4
## 1                                                        
## 2                                                        
## 3             -57.5570603248122,-30.2110754071007,42.0408
## 4   -57.0818249090229,-29.7598231712009,78.23018999999999
## 5          -56.4536470403836,-30.3828679600575,118.674261
## 6                                               SANT&apos
## 7                                                        
## 8           -56.55713349703041,-29.128636898258,62.084645
## 9  -55.7958701453331,-29.78204320831051,94.73120299999999
## 10         -56.06361348173561,-29.146144198381,104.458392
```



{% highlight r %}
# Acrescentar nomes de colunas
names(df_coord) <- c("municipio", "uf", "municipio_localidade", "coordenadas")
# Remover duas primeiras linhas
df_coord <- df_coord[-(1:2),]
# Remover linha na coluna uf se contém número
df_coord <- df_coord[!grepl("\\d", df_coord$uf),]
# Remover linha na coluna coordenadas se NAO contem numero
df_coord <- df_coord[grepl("\\d", df_coord$coordenadas),]
# Separar coluna de coordenadas em três e remover a última
df_coord %<>% separate(coordenadas, into = c('x1', 'x2', 'x3'), sep = ",")

df_coord %<>% filter(municipio == municipio_localidade)
# remover última coluna
df_coord %<>% select(-x3, -municipio_localidade, -uf) %>% rename(lat = x2, lon = x1)
# remover acentos da coluna de municipio_localidade
df_coord$municipio %<>% iconv(to = "ASCII//TRANSLIT")
# remover duplicatas
df_coord %<>% distinct(municipio)
head(df_coord)
```



{% highlight text %}
##         municipio                lon                lat
## 1 BARRA DO QUARAI  -57.5570603248122  -30.2110754071007
## 2      URUGUAIANA  -57.0818249090229  -29.7598231712009
## 3          QUARAI  -56.4536470403836  -30.3828679600575
## 4          ITAQUI -56.55713349703041   -29.128636898258
## 5        ALEGRETE  -55.7958701453331 -29.78204320831051
## 6       MACAMBARA -56.06361348173561   -29.146144198381
```



{% highlight r %}
# Precisamos de uma coluna com o código do município.
# Para isso, usamos o arquivo df_cidades usado no post1 que eu disponibilizei tbm no github
df_cidades <- read.csv2("https://raw.githubusercontent.com/sillasgonzaga/sillasgonzaga.github.io/master/data/municipiosBR.csv")
names(df_cidades) <- c("uf", "cod_municipio", "municipio")  
df_cidades$municipio  %<>% iconv(to = "ASCII//TRANSLIT") %>% str_to_upper()
df_cidades %<>% left_join(df_coord, by = 'municipio')
df_cidades %<>% na.omit()
df_cidades$lon %<>% as.numeric()
df_cidades$lat %<>% as.numeric()
head(df_cidades)
```



{% highlight text %}
##   uf cod_municipio               municipio       lon        lat
## 2 RO       1100379 ALTO ALEGRE DOS PARECIS -61.85308 -12.131777
## 3 RO       1100403            ALTO PARAISO -53.73289 -23.508131
## 5 RO       1100023               ARIQUEMES -63.03327  -9.908463
## 6 RO       1100452                 BURITIS -63.82968 -10.209805
## 7 RO       1100031                  CABIXI -60.54431 -13.499763
## 8 RO       1100601             CACAULANDIA -62.90319 -10.338873
```



{% highlight r %}
# Importar o shapefile
# disponível em https://github.com/sillasgonzaga/sillasgonzaga.github.io/raw/master/data/estados_2010.shp
estados <- readShapePoly("/home/sillas/R/Projetos/CensoEducSuperior/Dados/shapefiles/estados_2010/estados_2010.shp")
```

## Dados de universitários

O `DM_ALUNO.csv`, tratado no post anterior, e filtrado para os casos em que o município de nascimento não é o mesmo do município da IES:



{% highlight r %}
# importar df original
system.time(df <- read_feather("/home/sillas/R/Projetos/CensoEducSuperior/Dados/dm_aluno_tratado.feather"))
```



{% highlight text %}
##    user  system elapsed 
##   4.516   0.892   6.141
```



{% highlight r %}
df %<>% filter(municipio_diferente == 1)
# excluir cidades que não estão presentes no df_cidades
df %<>% filter(CO_MUNICIPIO_NASCIMENTO %in% df_cidades$cod_municipio & CO_MUNICIPIO_IES %in% df_cidades$cod_municipio)
```


{% highlight r %}
# selecionar apenas colunas referentes às cidades
df_agg <- df %>%
  select(cod_mun_aluno = CO_MUNICIPIO_NASCIMENTO, nome_mun_aluno =  municipioNascimento,
         cod_mun_ies = CO_MUNICIPIO_IES, nome_mun_ies = municipioIES) %>%
  group_by(cod_mun_aluno, nome_mun_aluno, cod_mun_ies, nome_mun_ies) %>%
  tally(sort = TRUE) %>%
  ungroup() %>%
  rename(qtd = n)

# Os 10 fluxos migratórios mais comuns
df_agg %>% top_n(n = 10, wt = qtd) %>% select(nome_mun_aluno, nome_mun_ies, qtd) %>% kable()
```



|nome_mun_aluno             |nome_mun_ies               |   qtd|
|:--------------------------|:--------------------------|-----:|
|Aracaju (SE)               |Săo Cristóvăo (SE)         | 17574|
|Contagem (MG)              |Belo Horizonte (MG)        | 11023|
|Niterói (RJ)               |Rio de Janeiro (RJ)        | 16817|
|Rio de Janeiro (RJ)        |Duque de Caxias (RJ)       | 12676|
|Rio de Janeiro (RJ)        |Săo Gonçalo (RJ)           | 11029|
|São Bernardo do Campo (SP) |Săo Paulo (SP)             | 10811|
|São Paulo (SP)             |Guarulhos (SP)             | 14762|
|São Paulo (SP)             |Săo Bernardo do Campo (SP) | 15408|
|Porto Alegre (RS)          |Indaial (SC)               | 11902|
|Porto Alegre (RS)          |Canoas (RS)                | 12571|


Uma curiosidade sobre o primeiro lugar da lista: o fluxo Aracaju > São Cristóvão é devido ao fato de a única universidade pública de Sergipe, a Universidade Federal de Sergipe (UFS), estar localizado em São Cristóvão, mas como a UFS é muito próxima a Sergipe, os universitários aracajuanos vão à UFS e voltam para casa no mesmo dia.

Finalmente, vamos o código para plotar o mapa. O código abaixo foi "inspirado" [deste post do Flowing Data](http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/), um ótimo blog sobre visualização de dados. Adaptar o código do artigo não foi tão direto como eu imaginava, por isso fiz questão de documentar todos os passos e explicar o que eles fazem. 


{% highlight r %}
# Para deixar mais fortes os fluxos mais frequentes, precisamos classificar os dados em ordem crescente de frequência
df_agg %<>% arrange(qtd)

# Criar dataframes separados para os municipios do aluno (origem ou org) e da IES (destino ou dest)
df_org <- select(df_agg, org = cod_mun_aluno)
df_dest <- select(df_agg, dest = cod_mun_ies)
# criar variáveis para a quantidade de cada fluxo
qtd <- df_agg$qtd
maxqtd <- max(qtd)
# acrescentar latitude e longitude para cada cidade dois dataframes
df_org %<>% left_join(df_cidades, by = c('org' = 'cod_municipio'))
df_dest %<>% left_join(df_cidades, by = c('dest' = 'cod_municipio'))

# para ver quanto tempo levou para gerar o mapa
t1 <- proc.time()

# Para salvar em um PNG de alta resolução, desmarque as duas linhas abaixo, além da dev.off()
#myPng <- function(..., width=13, height=13, res=300) {png(..., width=width*res, height=height*res, res=res)}
#myPng("mapa.png")

# Para deixar o gráfico bonito, usaremos um fundo preto
map(estados, col="#191919", fill=TRUE, bg="#000000")
# além de um escala que vai de preto (valores menores) a azul (valores maiores)
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)
# título
title("Mapeamento do movimento migratório \nuniversitário no Brasil", col.main = "white", cex.main = 1)
# referencia abaixo do mapa
mtext("Fonte: Censo do Ensino Superior 2014 \n Autor: Sillas Gonzaga (sillasgonzaga.github.io)",
      col = "white", side = 1, line = 1, cex = 1)
# Para gerar as linhas, a função gcIntermediate é necessária,
# porém dois dos argumentos dela, p1 e p2, só podem ser vetores de tamanho 2
# portanto, preciamos fazer um for loop para plotar cada linha de df_org e df_dest individualmente
# ao final do loop, todas as linhas estarão plotadas no mapa
for (i in 1:nrow(df_agg)) {
  p1 = c(df_org[i, ]$lon, df_org[i, ]$lat)
  p2 = c(df_dest[i, ]$lon, df_dest[i, ]$lat)
  inter <- gcIntermediate(p1, p2, n = 100, addStartEnd = TRUE)
  # determinar cor de cada fluxo
  colindex <- ((qtd[i]/maxqtd)*length(colors)) %>% round
  mycol = colors[colindex]
  lines(inter, col = mycol, lwd = 0.8)
}
```

<img src="/figs/censo_educ_superior2/plotar mapa-2.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
#dev.off()
t2 <- proc.time()
# Tempo necessário para construir o mapa (em segundos)
t2 - t1
```



{% highlight text %}
##    user  system elapsed 
## 322.500   3.436 375.426
```


