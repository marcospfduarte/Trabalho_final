##pacotes necessários

library(ggplot2)
library(dplyr)
library(geobr)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(tidyr)



mun <- read_municipality(code_muni="all", year=c(2020)) ## Banco de municipios atraves do geobr
#p = st_collection_extract(mun, "POLYGON")
#st_write(p, "mun.shp")


banco <- read.csv("/Users/marcospauloferreiraduarte/Downloads/trabalho_final/banco.csv") # banco disponível no computar
banco_2019 <- filter(banco, ano == 2019) #filtro, pegando apeas um ano, análogo abaixo
banco_2017 <- filter(banco, ano == 2017)
banco_2015 <- filter(banco, ano == 2015)
banco_2013 <- filter(banco, ano == 2013)
banco_2019 <- dplyr::left_join(mun, banco_2019, by = c("code_muni"="id_municipio"))
banco_2017 <- dplyr::left_join(mun, banco_2017, by = c("code_muni"="id_municipio"))
banco_2015 <- dplyr::left_join(mun, banco_2015, by = c("code_muni"="id_municipio"))
banco_2013 <- dplyr::left_join(mun, banco_2013, by = c("code_muni"="id_municipio"))
banco <- rbind(banco_2019, banco_2017,banco_2015,banco_2013) #O join direto gera problemas por conta do ano

 
#### GRÁFICO PARA MOSTRAR OS DADOS QUE TEMOS POR ANO

## DEfinindo uma função para produzir o gráfico de mesmo padrão, alterando apenas o ano
mapa_educ <- function(banco, titulo){
    mapa <- banco %>% ggplot() +
      geom_sf(aes(fill=ideb_final), color= NA, size=.20)+
      labs(title=titulo)+
      scale_fill_distiller(palette =  "RdYlGn",trans = "reverse",limits = c(8,2),
                         name = "")+
      theme(panel.background =  element_blank(), panel.grid.major = element_line(color = "transparent"),
          axis.text = element_blank(),
          axis.ticks = element_blank())
    
  return(mapa)
}

mapa_educ(banco_2019, "IDEB - Finais - 2019")

mapa_educ_4 <-mapa_educ(banco_2019, "IDEB - Finais - 2019")

mapa_educ_3 <-mapa_educ(banco_2017, "IDEB - Finais - 2017")

mapa_educ_2 <-mapa_educ(banco_2015, "IDEB - Finais - 2015")

mapa_educ_1 <-mapa_educ(banco_2013, "IDEB - Finais - 2013")

grid.arrange(mapa_educ_1,mapa_educ_2,mapa_educ_3,mapa_educ_4)



#### GRÁFICO PARA ANOS O IDEB DE ANOS FINAIS POR ANO

box_ideb_final <- function(banco,ano_numerico, ano_string){ 
  grafico <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))%>%
    ggplot(aes(x=name_region, y=ideb_final, fill=name_region)) +
    geom_boxplot(fill='palegreen4') + 
    coord_cartesian(ylim = c(2, 8)) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(ano_string) +
    xlab("Regiões") +
    ylab("IDEB")
  
  return(grafico)
}

grafico1 <- box_ideb_final(banco, 2013, "2013")

grafico2 <- box_ideb_final(banco, 2015, "2015")

grafico3 <- box_ideb_final(banco, 2017, "2017")

grafico4 <- box_ideb_final(banco, 2019, "2019")

grid.arrange(grafico1,grafico2,grafico3,grafico4, ncol=2, top = "Notas do IDEB para Ensino Fundamental Anos Finais ")


## GRÁFICO PARA ANOS INICIAIS

box_ideb_inicial <- function(banco,ano_numerico, ano_string){ 
  grafico <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))%>%
    ggplot(aes(x=name_region, y=ideb_inicial, fill=name_region)) +
    geom_boxplot(fill='palegreen4') + 
    coord_cartesian(ylim = c(2, 8)) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(ano_string) +
    xlab("Regiões") +
    ylab("IDEB")
  
  return(grafico)
}

grafico1 <- box_ideb_inicial(banco, 2013,"2013")

grafico2 <-  box_ideb_inicial(banco, 2015,"2015")

grafico3 <-  box_ideb_inicial(banco, 2017,"2017")

grafico4 <-  box_ideb_inicial(banco, 2019,"2019")

grid.arrange(grafico1,grafico2,grafico3,grafico4, ncol=2, top = "Notas do IDEB para Ensino Fundamental Anos Iniciais ")

## GRÁFICO PARA ANOS INICIAIS PONDERADO PELA POPULAÇÃO DE 0 A 15 ANOS

box_ideb_inicial_p <- function(banco, ano_numerico, ano_string){ 
  p <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))%>%
  ggplot(aes(x=name_region, y=ideb_inicial, fill=name_region, weight =populacao_0_15_anos )) +
  geom_boxplot(fill='palegreen4') + 
  coord_cartesian(ylim = c(2, 9)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle(ano_string) +
  xlab("Regiões") +
  ylab("IDEB")
  
  return(p)
}

grafico1 <- box_ideb_inicial_p(banco, 2013,"2013")

grafico2 <-  box_ideb_inicial_p(banco, 2015,"2015")

grafico3 <-  box_ideb_inicial_p(banco, 2017,"2017")

grafico4 <-  box_ideb_inicial_p(banco, 2019,"2019")

grid.arrange(grafico1,grafico2,grafico3,grafico4, ncol=2, top = "Notas do IDEB para Ensino Fundamental Anos Iniciais - Ponderado ")


## GRÁFICO PARA ANOS FINAIS PONDERADO PELA POPULAÇÃO DE 0 A 15 ANOS
box_ideb_final_p <- function(banco, ano_numerico, ano_string){ 
  p <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))%>%
    ggplot(aes(x=name_region, y=ideb_final, fill=name_region, weight =populacao_0_15_anos )) +
    geom_boxplot(fill='palegreen4') + 
    coord_cartesian(ylim = c(2, 9)) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(ano_string) +
    xlab("Regiões") +
    ylab("IDEB")
  
  return(p)
}

grafico1 <- box_ideb_final_p(banco, 2013, "2013")

grafico2 <- box_ideb_final_p(banco, 2015, "2015")

grafico3 <- box_ideb_final_p(banco, 2017, "2017")

grafico4 <- box_ideb_final_p(banco, 2019, "2019")

grid.arrange(grafico1,grafico2,grafico3,grafico4, ncol=2, top = "Notas do IDEB para Ensino Fundamental Anos Finais - Ponderado ")


## GRÁFICO PARA INSE
inse <- dplyr::filter(banco, ano == 2019 & (!is.na(name_region)))%>%
  ggplot(aes(x=name_region, y=inse_2019, fill=name_region, weight =populacao_0_15_anos)) +
  geom_boxplot(fill='palegreen4') + 
  coord_cartesian(ylim = c(2, 9)) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("INSE - 2019") +
  xlab("Regiões") +
  ylab("Média INSE")


### GRÁFICO DISPERSÃO - DESPESAS/PIB X IDEB 
lista_nordeste = c('Norte', "Sudeste", "Sul", "Centro-Oeste")

dispersao_educ_ideb_2013 <- dplyr::filter(banco, (ano == 2013) & (d_educacao != 0)) %>%
  ggplot(aes(x = (d_educacao/pib), y = (ideb_final), color  = name_region)) +
  geom_point( mapping = aes(x = (d_educacao/pib), y = (ideb_final)),color = "grey", alpha = 0.7, size = 1)+ 
  theme_classic() + 
  coord_cartesian(xlim = c(0,0.35), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "Propoção do Gasto com Educação por PIB",
        y = "IDEB - Fundamental, Anos Finais",
        title = "2013")

dispersao_educ_ideb_2015 <- dplyr::filter(banco, (ano == 2015) & (d_educacao != 0)) %>%
  ggplot(aes(x = (d_educacao/pib), y = (ideb_final), color  = name_region)) +
  geom_point(mapping = aes(x = (d_educacao/pib), y = (ideb_final)) ,color = "grey", alpha = 0.7, size = 1)+ 
  theme_classic() + 
  coord_cartesian(xlim = c(0,0.35), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "Propoção do Gasto com Educação por PIB",
        y = "IDEB - Fundamental, Anos Finais",
        title = "2015")
banco$Região <- banco$name_region
dispersao_educ_ideb_2017 <- dplyr::filter(banco, (ano == 2017) & (d_educacao != 0)) %>%
  ggplot(aes(x = (d_educacao/pib), y = (ideb_final), color  = Região)) +
  geom_point( mapping = aes(x = (d_educacao/pib), y = (ideb_final)),color = "grey", alpha = 0.7, size = 1)+
  theme_classic() + 
  labs(title = "Região") +
  coord_cartesian(xlim = c(0,0.35), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "Propoção do Gasto com Educação por PIB",
        y = "IDEB - Fundamental, Anos Finais",
        title = "2017")+
  theme(legend.position="bottom")



dispersao_educ_ideb_2019 <- dplyr::filter(banco, (ano == 2019) & (d_educacao != 0)) %>%
  ggplot(aes(x = (d_educacao/pib), y = (ideb_final), color  = name_region)) +
  geom_point(mapping = aes(x = (d_educacao/pib), y = (ideb_final)),color = "grey", alpha = 0.7, size = 1)+
  theme_classic() + 
  coord_cartesian(xlim = c(0,0.35), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "Propoção do Gasto com Educação por PIB",
        y = "IDEB - Fundamental, Anos Finais",
        title = "2019")



g_legend<-function(a.gplot){ #essa função tem objetivo de fazer a legenda ser única para todos os gráficos
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(dispersao_inse_ideb_2017)


grid.arrange(arrangeGrob(dispersao_educ_ideb_2013 + theme(legend.position="none"),
                         dispersao_educ_ideb_2015 + theme(legend.position="none"),
                         dispersao_educ_ideb_2017 + theme(legend.position="none"),
                         dispersao_educ_ideb_2019 + theme(legend.position="none"),
                         nrow=2),
             mylegend, nrow=2,heights=c(10, 1), top ="")

### GRÁFICO DE INSE X IDEB

disper_inse_ideb <- dplyr::filter(banco, (ano == 2019)) %>%
  ggplot(aes(x = (inse_2019), y = (ideb_final))) +
  geom_point(mapping = aes(x = (inse_2019), y = (ideb_final)),color = "palegreen4", alpha = 0.7, size = 1)+
  theme_classic() + 
 coord_cartesian(xlim = c(3,7), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "INSE",
        y = "IDEB - Fundamental, Anos Finais",
        title = "")

disper_inse_ideb_i <- dplyr::filter(banco, (ano == 2019)) %>%
  ggplot(aes(x = (inse_2019), y = (ideb_inicial))) +
  geom_point(mapping = aes(x = (inse_2019), y = (ideb_inicial)),color = "palegreen4", alpha = 0.7, size = 1)+
  theme_classic() + 
  coord_cartesian(xlim = c(3,7), ylim=c(0,9)) +
  geom_smooth(method = lm, se = F) +
  labs( x = "INSE",
        y = "IDEB - Fundamental, Anos Iniciais",
        title = "")

grid.arrange(disper_inse_ideb_i,disper_inse_ideb, ncol=2)

#### Despesa/PIB é pouco correlacionada ao IDEB, mas e a despesa per capita ?

per_despesa_ideb_final <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(x = (d_educacao/populacao_estimada), y = (ideb_final))) +
    geom_point(mapping = aes(x = (d_educacao/populacao_estimada), y = (ideb_final)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian(xlim = c(0,4000), ylim=c(0,9)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "Propoção do Gasto com Educação per capita",
          y = "IDEB - Fundamental, Anos Finais",
          title = periodo_string)
  return(grafico)
}
per_despesa_ideb_inicial <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(x = (d_educacao/populacao_estimada), y = (ideb_inicial))) +
    geom_point(mapping = aes(x = (d_educacao/populacao_estimada), y = (ideb_inicial)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian(xlim = c(0,4000), ylim=c(0,9)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "Propoção do Gasto com Educação per capita",
          y = "IDEB - Fundamental, Anos Finais",
          title = periodo_string)
  return(grafico)
}


dispersao_educ_ideb_2013 <- per_despesa_ideb_final(2013,"2013")

dispersao_educ_ideb_2013_inicial <- per_despesa_ideb_inicial(2013,"2013")

dispersao_educ_ideb_2015 <- per_despesa_ideb_final(2015,"2015")

dispersao_educ_ideb_2015_inicial <- per_despesa_ideb_inicial(2015,"2015")

dispersao_educ_ideb_2017 <- per_despesa_ideb_final(2017,"2017")

dispersao_educ_ideb_2017_inicial <-per_despesa_ideb_inicial(2017,"2017")


dispersao_educ_ideb_2019 <- per_despesa_ideb_final(2019,"2019")

dispersao_educ_ideb_2019_inicial <- per_despesa_ideb_inicial(2019,"2019")



grid.arrange(dispersao_educ_ideb_2013,
             dispersao_educ_ideb_2015,
             dispersao_educ_ideb_2017,
             dispersao_educ_ideb_2019, ncol=2)

grid.arrange(dispersao_educ_ideb_2013_inicial,
             dispersao_educ_ideb_2015_inicial,
             dispersao_educ_ideb_2017_inicial,
             dispersao_educ_ideb_2019_inicial, ncol=2)

#### E para idade de 0 a 15 anos ?
banco$populacao_0_15_anos
per_0_15_despesa_ideb_final <- function(periodo, periodo_string) {
    grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(x = (d_educacao/populacao_0_15_anos), y = (ideb_final))) +
    geom_point(mapping = aes(x = (d_educacao/populacao_0_15_anos), y = (ideb_final)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian(ylim=c(0,9)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "Propoção do Gasto com Educação por 0 a 15 anos",
          y = "IDEB - Fundamental, Anos Finais",
          title = periodo_string)
  return(grafico)
}
per_0_15_despesa_ideb_inicial <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(x = (d_educacao/populacao_0_15_anos), y = (ideb_inicial))) +
    geom_point(mapping = aes(x = (d_educacao/populacao_0_15_anos), y = (ideb_inicial)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian( ylim=c(0,9)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "Propoção do Gasto com Educação por 0 a 15 anos",
          y = "IDEB - Fundamental, Anos Iniciais",
          title = periodo_string)
  return(grafico)
}



dispersao_educ_ideb_2013 <- per_0_15_despesa_ideb_final(2013,"2013")

dispersao_educ_ideb_2013_inicial <- per_0_15_despesa_ideb_inicial(2013,"2013")

dispersao_educ_ideb_2015 <- per_0_15_despesa_ideb_final(2015,"2015")

dispersao_educ_ideb_2015_inicial <- per_0_15_despesa_ideb_inicial(2015,"2015")

dispersao_educ_ideb_2017 <- per_0_15_despesa_ideb_final(2017,"2017")

dispersao_educ_ideb_2017_inicial <-per_0_15_despesa_ideb_inicial(2017,"2017")


dispersao_educ_ideb_2019 <- per_0_15_despesa_ideb_final(2019,"2019")

dispersao_educ_ideb_2019_inicial <- per_0_15_despesa_ideb_inicial(2019,"2019")

grid.arrange(dispersao_educ_ideb_2013,
             dispersao_educ_ideb_2015,
             dispersao_educ_ideb_2017,
             dispersao_educ_ideb_2019, ncol=2)

grid.arrange(dispersao_educ_ideb_2013_inicial,
             dispersao_educ_ideb_2015_inicial,
             dispersao_educ_ideb_2017_inicial,
             dispersao_educ_ideb_2019_inicial, ncol=2)

### Vemos que há uma correlação positiva entre o gasto que parece ser mais importante
### como esse gasto se comporta com o inse ? Hipótese : INSE constante

inse_0_15 <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(x = (d_educacao/populacao_0_15_anos), y = (inse_2019))) +
    geom_point(mapping = aes(x = (d_educacao/populacao_0_15_anos), y = (inse_2019)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian(ylim=c(0,9), xlim = c(0,20000)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "Propoção",
          y = "INSE",
          title = periodo_string)
  return(grafico)
}

grafico1 <- inse_0_15(2013,"2013")
grafico2 <- inse_0_15(2015,"2015")
grafico3 <- inse_0_15(2017,"2018")
grafico4 <- inse_0_15(2019,"2019")

grid.arrange(grafico1,grafico2,
             grafico3,grafico4, ncol=2)


### Box-plot dos gastos por 0 a 15

box_gastos <- function(banco,ano_numerico, ano_string){ 
  dados <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))
  dados$dif_educ <-  dados$d_educacao/dados$populacao_0_15_anos - mean(na.omit(dados$d_educacao/dados$populacao_0_15_anos))
  grafico <- dados%>%
    ggplot(aes(x=name_region, y=dif_educ, fill=name_region)) +
    geom_boxplot(fill='palegreen4') + 
    #coord_cartesian(ylim = c(2, 8)) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(ano_string) +
    xlab("Regiões") +
    ylab("Desvio")
  
  return(grafico)
}

grafico1 <- box_gastos(banco, 2013, "2013")
grafico2 <- box_gastos(banco, 2015, "2015")
grafico3 <- box_gastos(banco, 2017, "2017")
grafico4 <- box_gastos(banco, 2019, "2019")

grid.arrange(grafico1,grafico2,
             grafico3,grafico4, ncol=2, top = "Desvio da Média Nacional dos Gastos com Educação por População de 0 a 15 anos")


### E o ponderado ? 

box_gastos_p <- function(banco,ano_numerico, ano_string){ 
  dados <- dplyr::filter(banco, ano == ano_numerico & (!is.na(name_region)))
  dados$dif_educ <-  dados$d_educacao/dados$populacao_0_15_anos - mean(na.omit(dados$d_educacao/dados$populacao_0_15_anos))
  grafico <- dados%>%
    ggplot(aes(x=name_region, y=dif_educ, fill=name_region, weight = populacao_0_15_anos)) +
    geom_boxplot(fill='palegreen4') + 
    #coord_cartesian(ylim = c(2, 8)) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle(ano_string) +
    xlab("Regiões") +
    ylab("Desvio")
  
  return(grafico)
}

grafico1 <- box_gastos_p(banco, 2013, "2013")
grafico2 <- box_gastos_p(banco, 2015, "2015")
grafico3 <- box_gastos_p(banco, 2017, "2017")
grafico4 <- box_gastos_p(banco, 2019, "2019")

grid.arrange(grafico1,grafico2,
             grafico3,grafico4, ncol=2,  top = "Desvio da Média Nacional dos Gastos com Educação por População de 0 a 15 anos - Ponderado" )




#### GRÁFICOS NÃO UTILIZADOS PARA A PRODUÇÃO DO PDF



pib_gasto <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(y = (d_educacao/pib), x = log(pib))) +
    geom_point(mapping = aes(y = (d_educacao/pib), x = log(pib)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    coord_cartesian(ylim=c(0,0.5)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "PIB",
          y = "Gasto de Educação por PIB",
          title = periodo_string)
  return(grafico)
}

pib_2019 <- pib_gasto(2019,"2019")
pib_2017 <- pib_gasto(2017,"2017")
pib_2015 <- pib_gasto(2015,"2015")
pib_2013 <- pib_gasto(2013,"2013")

grid.arrange(pib_2013,pib_2015,
             pib_2017,pib_2019, ncol=2,  top = "PIB x Gasto por PIB" )

pib_inse <- function(periodo, periodo_string) {
  grafico <- dplyr::filter(banco, (ano == periodo) & (d_educacao != 0)) %>%
    ggplot(aes(y = (inse_2019), x = log(pib))) +
    geom_point(mapping = aes(y = (inse_2019), x = log(pib)),color = "palegreen4", alpha = 0.7, size = 1)+
    theme_classic() + 
    #coord_cartesian(ylim=c(0,0.5)) +
    geom_smooth(method = lm, se = F) +
    labs( x = "PIB",
          y = "Gasto de Educação por PIB",
          title = periodo_string)
  return(grafico)
}
pib_inse(2019,"2019")


