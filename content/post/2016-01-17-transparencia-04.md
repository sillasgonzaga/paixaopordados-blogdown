---
title: 'Transparência(4): Análise de salários usando Treemaps'
author: ''
date: '2016-01-17'
slug: transparencia-04
categories:
  - R
tags:
  - transparencia
  - servidores publicos
---

# Transparência(4): Análise de salários usando Treemaps


```r
library(treemap)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
df <- read.csv2("/home/sillas/R/data/transparenciaComSalarios.csv", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-15")
```


Para este post, continuaremos analisando os salários dos servidores federais, mas agora usando uma visualização chamada Treemap ou Mapa de árvores.


Por exemplo, o gráfico abaixo compara diferentes órgãos públicos de acordo com a quantidade de servidores e o salário médio dos mesmos.


```r
aggSetor <-df %>%
  group_by(ORG_LOTACAO) %>%
  summarise(quantidade = n(),
            salarioMedio = median(SALARIO))

aggSetor$escala <- scale(aggSetor$salarioMedio) #necessário para criar valores negativos para deixar as disparidades mais evidentes

x <- treemap(aggSetor, index = "ORG_LOTACAO", vSize = "quantidade", vColor = "escala",
        type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
        title  =  "Treemap dos salários dos órgãos federais brasileiros")
```

![center](/figs/transparenciaParte4/unnamed-chunk-2-1.png) 

__Interpretação__: Com o gráfico acima, aprendemos que:  
* O Ministério da Saúde tem muitos servidores mas salários muito baixos.  
* O Ministério da Fazenda, a Advocacia-Geral da União e o Banco Central do Brasil são os que possuem os maiores salários.

O treemap é chamado assim por permitir uma visualização fácil de hierarquias, isto é, de variáveis categóricas e seus respectivos subníveis. Além disso, ele é excelente para representar visualmente relações entre duas ou mais variáveis categóricas.
Por exemplo, será que existe alguma relação interessante entre o UF e o vínculo do servidor?


```r
treemap(df, index = c("UF_EXERCICIO", "SITUACAO_VINCULO"), vSize = "x")
```

![center](/figs/transparenciaParte4/unnamed-chunk-3-1.png) 

Aparentemente, tem sim! O número de servidores de Contrário Temporário no RJ e de Cargo Comissionado no DF parecem ser muito grandes. Podemos ratificar isso filtrando fora os servidores ativos:


```r
treemap(subset(df, SITUACAO_VINCULO != "ATIVO PERMANENTE"), index = c("UF_EXERCICIO", "SITUACAO_VINCULO"), vSize = "x")
```

![center](/figs/transparenciaParte4/unnamed-chunk-4-1.png) 

Vamos conferir essa informação com um gráfico de dispersão:


```r
df %>%
  filter(SITUACAO_VINCULO == "NOMEADO CARGO COMIS.") %>%
  group_by(UF_EXERCICIO) %>%
  summarise(servidores = n(),
            salario = median(SALARIO))  %>%
  ggplot(aes(servidores, salario)) +
    geom_point() +
    geom_text_repel(aes(label = UF_EXERCICIO)) +
    labs(title = "Cargos comissionados de cada estado", x = "Quantidade de servidores", y = "Salário médio") +
    theme_few()
```

![center](/figs/transparenciaParte4/unnamed-chunk-5-1.png) 

Duas grandes descobertas aqui:  
* O __DF__ tem um número assustadoramente grande de CCs (5384), tanto que chega a distorcer o gráfico.  
* Os CCs do __CE__ tem um salário médio assustadoramente alto (R$8554,70).

__Por hoje é só!__
