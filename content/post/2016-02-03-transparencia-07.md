---
title: 'Transparência (7): Os famosos Cargos Comissionados'
author: ''
date: '2016-02-03'
slug: transparencia-07
categories:
  - R
tags:
  - servidores publicos
  - transparencia
description: ''
topics: []
---

# Transparência (7): Os famosos Cargos Comissionados

No [quarto post](http://sillasgonzaga.github.io/blog/transparenciaParte4/) da minha série sobre dados do Portal da Transparência, eu introduzi um tema interessante a ser olhado a fundo: os servidores cujo vínculo com o Estado é descrito como cargo comissionado. Vimos que, no Ceará, o salário médio de um servidor é muito alto. E nos outros estados?


```r
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(reshape2)
df <- read.csv2("/home/sillas/R/data/transparenciaComSalarios.csv", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-15")

cor1 <- "#C10534" #cor das barras
```

Para começar, quais são os 10 tipos de vínculo mais comuns?


```r
df %>%
  group_by(SITUACAO_VINCULO) %>%
  summarise(servidores = n()) %>%
  arrange(desc(servidores)) %>%
  top_n(10)
```



```r
## Source: local data frame [10 x 2]
## 
##        SITUACAO_VINCULO servidores
##                   (chr)      (int)
## 1      ATIVO PERMANENTE     461963
## 2   CONTRATO TEMPORARIO      11946
## 3  CONT.PROF.SUBSTITUTO      10512
## 4  NOMEADO CARGO COMIS.       7445
## 5           REQUISITADO       6459
## 6           SEM VINCULO       4117
## 7  EXERC DESCENT CARREI       3870
## 8  EXERC.÷7º ART93 8112       2475
## 9            APOSENTADO       2294
## 10 REQ.DE OUTROS ORGAOS       1570
```

Felizmente, a maioria é composta por servidores ativos, enquanto que cargo comissionado é o quarto vínculo mais comum.

Antes de adentrarmos a questão dos CCs, vamos ver qual o tipo de vínculo que possui os maiores salários:


```r
df %>%
  group_by(SITUACAO_VINCULO) %>%
  summarise(servidores = n(),
            salario = median(SALARIO)) %>%
  arrange(desc(salario)) %>%
  top_n(10, salario)
```



```r
## Source: local data frame [10 x 3]
## 
##        SITUACAO_VINCULO servidores  salario
##                   (chr)      (int)    (dbl)
## 1     NATUREZA ESPECIAL         40 30934.70
## 2  QUADRO ESPEC.-QE/MRE         44 21961.89
## 3  EXERC DESCENT CARREI       3870 20429.09
## 4                CEDIDO        102 19946.32
## 5            APOSENTADO       2294 17923.85
## 6      RESERVA CBM / PM          1 17348.72
## 7  APOSENTADO TCU733/94          1 11650.25
## 8   EXCEDENTE A LOT/MRE          8 11005.08
## 9   CELETISTA/EMPREGADO        408 10796.80
## 10      COLABORADOR ICT         46 10208.36
```

Temos algumas surpresas aqui. Alguns termos são novos para mim, por isso postei a definição deles abaixo:

* [NATUREZA ESPECIAL](https://pt.wikipedia.org/wiki/Cargo_de_Natureza_Especial): Cargo de Natureza Especial (CNE) são cargos públicos que dispensam concursos públicos para sua efetivação. No Brasil estes cargos estão vinculados a entidades públicas que têm o direito de contratar funcionários de sua confiança, podendo os salários variarem de 1.200 reais a mais de 8.000 reais. Segue um exemplo: o Presidente da Câmara dos Deputados do Congresso Nacional tem o direito a contratar 46 pessoas na forma de CNE, e cada um dos 7 membros da mesa diretora da Câmara tem direito a 33 cargos, além de 11 cargos para cada um dos 4 suplentes da mesa, perfazendo um total de 321 CNEs. Com base neste exemplo fica evidente a importância da sociedade fiscalizar os critérios de nomeação, a justificativa dos gastos e o desempenho dos CNEs, pois infelizmente ainda são muito utilizados para atender a interesses restritos de quem nomeia e do pequeno grupo favorecido, ao invés de suprirem alguma demanda técnica da administração pública.  
* QUADRO ESPEC.-QE/MRE: Não encontrei uma definição precisa mas aparentam ser algo relacionados a diplomacia.  
* EXERC DESCENT CARREI: Servidores das carreiras típicas de Estado vinculadas aos Ministérios do Planejamento, Orçamento e Gestão e Ministério da Fazenda que exercem as suas atividades na UJ mediante exercício descentralizado de atividade.  
* [CEDIDO](https://jus.com.br/artigos/21640/cessao-e-requisicao-de-servidor-publico-federal): O servidor da Administração Pública Federal poderá ser cedido a outro órgão ou entidade de qualquer ente federativo, incuindo as empresas públicas e sociedades de economia mista, para o exercício de cargo em comissão ou função de confiança e, ainda, nos termos de leis específicas.
* COLABORADOR ICT: Também não encontrei informações sobre, mas parece estar relacionado à Inovação, Ciência e Tecnologia.

Voltando aos nossos CCs: existe diferença na distribuição de salários entre CCs e servidores ativos?


```r
df2 <- filter(df, SITUACAO_VINCULO %in% c("ATIVO PERMANENTE", "NOMEADO CARGO COMIS."))
  
ggplot(df2, aes(SALARIO)) +
  geom_histogram(binwidth = 1000, fill = cor1) +
  facet_grid(SITUACAO_VINCULO ~ ., scales = "free_y") +
  xlim(0, 35000) +
  labs(title = "Distribuição dos salários de acordo com o vínculo", x = "Salário", y = "Frequência") +
  theme_bw()
```

![center](/figs/transparenciaParte7/unnamed-chunk-4-1.png) 

O interessante aqui é que, sob nenhuma hipótese, é possível afirmar que a distribuição dos salários para os CCs é normal.

Próxima pergunta: existe uma relação entre o número de cargos comissionados e o número total de servidores por estado?


```r
df2 %>%
  group_by(UF_EXERCICIO, SITUACAO_VINCULO) %>%
  summarise(quantidade = n()) %>%
  ggplot(aes(x = UF_EXERCICIO, y = quantidade)) +
    geom_bar(stat = "identity", fill = cor1) +
    facet_grid(SITUACAO_VINCULO ~ ., scales = "free_y") +
    labs(title = "Quantidade de servidores por estado e por vínculo", x = "", y = "Quantidade de servidores")
```

![center](/figs/transparenciaParte7/unnamed-chunk-5-1.png) 

Deu para perceber a aberração que existe no Distrito Federal, não deu? O DF possui mais de 5000 CCs, enquanto que o segundo estado com mais servidores do tipo, o RJ, tem cerca de 500.

E em relação aos salários?


```r
df2 %>%
  group_by(REGIAO, UF_EXERCICIO, SITUACAO_VINCULO) %>%
  summarise(salario = median(SALARIO)) %>%
  ggplot(aes(x = UF_EXERCICIO, y = salario, fill = REGIAO)) +
    geom_bar(stat = "identity") +
    facet_grid(SITUACAO_VINCULO ~ ., scales = "free_y") +
    labs(title = "Salário mediano por estado\n e vínculo do servidor", x = "", y = "Salário (R$)") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
```

![center](/figs/transparenciaParte7/unnamed-chunk-6-1.png) 

Como já havia comentado no terceiro post da série, a situação dos CCs no Ceará é estranha: lá, eles têm o maior salário mediano (R$ 8554, 70) dentre os CCs do Brasil, mais de R$ 3000,00 de diferença para o segundo lugar, Sergipe.

**Por hoje é só!**




