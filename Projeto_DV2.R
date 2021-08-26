##Visualizar diretório
getwd()
##Setar um diretório específico
setwd("42kkkkkaren/data-visualization-graficos-com-multivariaveis")

##Chamada das bibliotecas a serem utilizadas
##install.packages("data.table")
library(data.table)
library(dplyr)
library(ggplot2)

#Carregamento de dados
enem_2010 <- fread("enem_2010.csv", encoding = "UTF-8")
enem_2011 <- fread("enem_2011.csv", encoding = "UTF-8")
enem_2012 <- fread("enem_2012.csv", encoding = "UTF-8")
enem_2013 <- fread("enem_2013.csv", encoding = "UTF-8")
enem_2014 <- fread("enem_2014.csv", encoding = "UTF-8")
enem_2015 <- fread("enem_2015.csv", encoding = "UTF-8")
enem_2016 <- fread("enem_2016.csv", encoding = "UTF-8")
enem_2017 <- fread("enem_2017.csv", encoding = "UTF-8")

##Trabalhar apenas com um objeto
##Merge por linhas e colunas, portanto, objetos devem ter valores referentes iguais
merge_enem <- rbind(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015, enem_2016, enem_2017, fill = TRUE)
##Eliminar registros anteriores - Gasto de memória.
rm(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015, enem_2016, enem_2017)

colunas <- c("NUMERO_INSCRICAO","ANO","CO_MUNICIPIO_RESIDENCIA","MUNICIPIO_RESIDENCIA",
             "UF_RESIDENCIA","UF_ESCOLA","IDADE","SEXO","SITUACAO_CONCLUSAO","BRAILLE","MUNICIPIO_PROVA",
             "UF_PROVA","PRESENCA_CIENCIAS_NATUREZA","PRESENCA_CIENCIAS_HUMANAS","PRESENCA_LINGUAGENS_CODIGOS",
             "PRESENCA_MATEMATICA","NOTA_CIENCIAS_NATUREZA","NOTA_CIENCIAS_HUMANAS",
             "NOTA_LINGUAGENS_CODIGOS","NOTA_MATEMATICA","TIPO_LINGUA","STATUS_REDACAO","NOTA_REDACAO")

enem <- merge_enem %>% select_(.dots = colunas)
rm(merge_enem)
str(enem)

##Verificar colunas e valores nela armazenado
table(enem$SEXO)
##0  1  F  M - Transformar para valores se tornarem únicos (1/F e 0/M)
##gsub("O que você tá procurando", "Vai ser trocado por", local armazenado)
enem$SEXO <- gsub("1", "FEMININO", enem$SEXO)
enem$SEXO <- gsub("^F$", "FEMININO", enem$SEXO)
enem$SEXO <- gsub("0", "MASCULINO", enem$SEXO)
enem$SEXO <- gsub("^M$", "MASCULINO", enem$SEXO)

table(enem$TIPO_LINGUA)
##.  0   1 
enem$TIPO_LINGUA <- gsub("0", "INGLÊS", enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub("1", "ESPANHOL", enem$TIPO_LINGUA)

length(table(enem$UF_PROVA))
##28, mas só temos 27 UFs

table(enem$SITUACAO_CONCLUSAO)
## 1  2  3  4 
enem$SITUACAO_CONCLUSAO <- gsub("1", "CONCLUÍDO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("2", "CONCLUIRÁ NO ANO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("3", "CONCLUIRÁ APÓS(ANO)", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("4", "NÃO CONCLUÍDO/NÃO CURSANDO", enem$SITUACAO_CONCLUSAO)

summary(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_HUMANAS <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_NATUREZA <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_MATEMATICA <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_REDACAO <- as.numeric(enem$NOTA_REDACAO)

str(enem)

ggplot(data = enem) + geom_bar(aes(x = TIPO_LINGUA), stat = 'count')

tp_lingua_sexo <-  enem %>% 
                      filter(TIPO_LINGUA != '.') %>% 
                      select_(.dots = c('SEXO', 'TIPO_LINGUA'))
##position_dodge() - Separa barras lado a lado
plot_idioma_sexo <- ggplot(data = tp_lingua_sexo) + 
                      geom_bar(aes(x = SEXO, fill = TIPO_LINGUA), stat = 'count', position = position_dodge())
p <- plot_idioma_sexo +
      ggtitle('Idioma por Sexo') +
      xlab('Sexo') + ylab('Quantidade')

p <- p + theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5))

plot_idioma_sexo <- p

ggplot(data = enem) +
  geom_bar(aes(x = UF_PROVA), stat = 'count')
##Eliminar notação científica
options(scipen = 9999)

uf_prova <- enem %>%
              filter(UF_PROVA != '') %>% 
              select_(.dots = c('UF_PROVA', 'SITUACAO_CONCLUSAO'))

##fill define cores distintas
##~. final, pegará somente a devida coluna
plot_uf_conclusao <- ggplot(data = uf_prova) +
                      geom_bar(aes(x = UF_PROVA, fill = SITUACAO_CONCLUSAO), 
                               position = position_dodge()) +
                      facet_grid(SITUACAO_CONCLUSAO~.)

p <- plot_uf_conclusao + 
      ggtitle('Situação Escolar por Estado') +
      xlab('Estado') + ylab('Quantidade')

p <- p + theme_linedraw() +
      labs(fill = 'Situação') +
      theme(plot.title = element_text(hjust = 0.5))

plot_uf_conclusao <- p

summary(enem$IDADE)
idade_uf <- enem %>% 
              filter(!is.na(IDADE))
summary(idade_uf$IDADE)

media_idade_sexo_uf <- idade_uf %>% 
                        group_by(UF_PROVA, SEXO) %>%
                        summarise(media = mean(IDADE))
media_idade_sexo_uf <- media_idade_sexo_uf %>%
                        filter(UF_PROVA != "")
View(media_idade_sexo_uf)

ggplot(data = media_idade_sexo_uf) +
  geom_bar(aes(x = UF_PROVA, y = media, fill = SEXO),
  position = position_dodge(), stat = 'identity') +
  coord_flip()

##Gráfico de pirâmide
##Não há função específica para gráfico de pirâmide
plot_piram_idade <- ggplot(data = media_idade_sexo_uf, 
                     aes(x = reorder(UF_PROVA, -media), 
                         y = ifelse(SEXO == 'MASCULINO', -media, media),
                         fill = SEXO)) +
                geom_bar(stat = 'identity') + 
                coord_flip()
##Alterar valor das médias negativas
plot_piram_idade <- plot_piram_idade + scale_y_continuous(labels = abs)

p <- plot_piram_idade +
        ggtitle('Média de Idade por UF e Sexo') +
        xlab('Estado') + ylab('Média Idade') +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))

p <- p + scale_fill_manual(values = c('hotpink', 'dodgerblue3'))

##Colocar rótulos em cada barra demonstrando o que a mesma representa
p <- p + geom_text(aes(label = round(media, digits = 2),
                    hjust = 0.5),
                    size = 4.5,
                    colour = 'black',
                    fontface = 'bold')

plot_piram_idade <- p
plot_piram_idade

notas_ciencias_humanas <- enem %>% 
                            filter(!is.na(NOTA_CIENCIAS_HUMANAS) &
                                     !is.na(IDADE) & 
                                     IDADE > 17)
notas_ciencias_humanas_idade <- notas_ciencias_humanas %>% 
                                group_by(IDADE) %>%
                                summarise(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))
##Gráfico de pontos
ggplot(data = notas_ciencias_humanas_idade) +
  geom_point(aes(x = IDADE, y = media_nota_ciencias_humanas))

notas_mt <- enem %>%
              filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE > 17)
notas_matematica_idade <- notas_mt %>%
                            group_by(IDADE) %>%
                            summarise(media_nota_matematica = mean(NOTA_MATEMATICA))
ggplot(data = notas_matematica_idade) +
  geom_point(aes(x = IDADE, y = media_nota_matematica))

notas_ciencias_humanas_idade
notas_matematica_idade
##Tem em comum o campo idade
notas_ciencias_humanas_matematica_idade <- merge(notas_ciencias_humanas_idade,
                                                 notas_matematica_idade, all = T)
View(notas_ciencias_humanas_matematica_idade)

##Converter coluna em linhas
##install.packages("reshape2")
library(reshape2)

##id.vars indica qual coluna vai ser mantida e não será transformada em linhas
notas_ciencias_humanas_matematica_idade <- melt(notas_ciencias_humanas_matematica_idade,
                                                id.vars = 'IDADE')
plot_scatter_mt_ch <- ggplot(data = notas_ciencias_humanas_matematica_idade) +
                        geom_point(aes(IDADE, value, color = variable))
p <- plot_scatter_mt_ch +
      ggtitle('Média Notas por Idade e Matéria') +
      xlab('Idade') + ylab('Notas (média)')
p <- p + theme_bw()
p <- p + scale_color_manual(name = 'Matéria',
                            values = c('blue', 'red'),
                            labels = c('Ciências\nHumanas', 'Matemática'))
plot_scatter_mt_ch <- p

##Gráficos até agora
plot_idioma_sexo
plot_uf_conclusao
plot_piram_idade
plot_scatter_mt_ch

media_anos <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS) &
                              !is.na(NOTA_CIENCIAS_NATUREZA) & 
                              !is.na(NOTA_LINGUAGENS_CODIGOS) &
                              !is.na(NOTA_MATEMATICA) &
                              !is.na(NOTA_REDACAO)) %>%
                        group_by(ANO) %>%
                          summarise(media_ch = mean(NOTA_CIENCIAS_HUMANAS),
                                    media_cn = mean(NOTA_CIENCIAS_NATUREZA),
                                    media_lc = mean(NOTA_LINGUAGENS_CODIGOS),
                                    media_mt = mean(NOTA_MATEMATICA),
                                    media_red = mean(NOTA_REDACAO))
View(media_anos)

ggplot(data = media_anos) +
  geom_line(aes(x = ANO, y = media_cn), color = 'green') +
  geom_line(aes(x = ANO, y = media_ch), color = 'blue') 

media_anos_2 <- melt(data = media_anos, id.vars = 'ANO')

plot_line_notas <- ggplot(data = media_anos_2) +
                    geom_line(aes(x = ANO, y = value, color = variable))

##Inserir pontos em locais que indicam valores
p <- plot_line_notas +
        ggtitle("Média Notas por Matéria") +
                  ylab('Média') +
                  geom_point(aes(ANO, value, color = variable), size = 3)
##Inserir rótulos em pontos
p <- p + geom_text(aes(x = ANO, y = value, color = variable, 
                  label = round(value, digits = 2),
                  hjust = -0.15,
                  vjust = 0.2))
p <- p + scale_color_discrete(name = 'Matérias', labels = c("Ciência da Natureza",
                                                       "Ciências Humanas",
                                                       "Matemática", "Linguagens e Códigos",
                                                       "Redação")) + theme_bw()
plot_line_notas <- p
plot_line_notas

notas_matematica_redacao <- enem %>% 
                              filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_MATEMATICA) & 
                                       !is.na(NOTA_REDACAO) & !is.na(IDADE) & IDADE > 17 & 
                                       UF_PROVA %in% c('CE', 'DF', 'MG', 'RS')) %>%
                              group_by(IDADE, UF_PROVA) %>%
                              summarise(media_nota_matematica = mean(NOTA_MATEMATICA),
                                        media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS),
                                        media_nota_reda = mean(NOTA_REDACAO))
View(notas_matematica_redacao)

##alpha deixa cores mais suaves
plot_bolhas_uf_notas <- ggplot(data = notas_matematica_redacao) +
                          geom_point(aes(x = media_nota_ciencias_humanas, y = media_nota_matematica,
                                         color = UF_PROVA, size = media_nota_reda),
                                     alpha = .5)
p <- plot_bolhas_uf_notas +
      ggtitle('Médias Matemática, Ciências Humanas e Redação') +
      xlab('Média de Notas Ciências Humanas') + ylab('Média de Notas Matemática')
p <- p + labs(color = 'UF Prova', size = 'Média da Nota Redação')
p <- p + theme_bw() + theme(legend.position = 'bottom')

plot_bolhas_uf_notas <- p

notas_redacao_uf <- enem %>% 
                      filter(UF_PROVA != '' & !is.na(NOTA_REDACAO)) %>%
                      select_(.dots = c('UF_PROVA', 'NOTA_REDACAO'))
View(notas_redacao_uf)

plot_box_uf_redacao <- ggplot(data = notas_redacao_uf) +
                          geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO))

dados <- plot_box_uf_redacao$data
View(dados)

dados <- dados %>%
          mutate(filial = if_else(UF_PROVA %in% c('CE', 'DF', 'MG', 'RS'), T, F))

p <- ggplot(data = dados) +
        geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO, fill = filial),
                     outlier.colour = 'red', outlier.size = 3.5)

p <- p + xlab('UF Prova') + ylab('Nota Redação') +
         theme_bw()

p <- p + scale_fill_manual(name = '', values = c('chocolate3', 'chartreuse3'),
                                      labels = c('Sem Filial', 'Com Filial'))

plot_box_uf_redacao <- p

media_redacao <- enem %>% 
                  filter(UF_PROVA != "" & !is.na(NOTA_REDACAO)) %>%
                  mutate(media_nacional = mean(NOTA_REDACAO)) %>%
                  group_by(UF_PROVA, media_nacional) %>%
                  summarise(media_uf = mean(NOTA_REDACAO))

plot_bar_erro <- ggplot(data = media_redacao, aes(x = reorder(UF_PROVA, media_uf), y = media_uf)) +
                  geom_errorbar(aes(ymin = media_nacional/2, ymax = media_nacional), size = 1) +
                  geom_bar(stat = 'identity') + coord_flip()
                  
dados <- plot_bar_erro$data

dados <- dados %>% 
          mutate(filial = if_else(UF_PROVA %in% c('CE', 'DF', 'MG', 'RS'), T, F))

plot_bar_erro <- ggplot(data = dados, aes(x = reorder(UF_PROVA, media_uf), y = media_uf)) +
                  geom_errorbar(aes(ymin = media_nacional/2, ymax = media_nacional), size = 1) +
                  geom_bar(aes(fill = filial), stat = 'identity') + coord_flip() +
                  guides(fill = F) +
                  ggtitle('Média de Nota Redação por UF/Nacional') +
                  xlab('UF Prova') + ylab('Média Redação') +
                  theme_bw()

plot_idioma_sexo 
plot_uf_conclusao
plot_piram_idade
plot_scatter_mt_ch 
plot_line_notas 
plot_bolhas_uf_notas 
plot_box_uf_redacao 
plot_bar_erro 

library(gridExtra)
library(grid)

grid.arrange(plot_idioma_sexo,
             plot_uf_conclusao,
             plot_piram_idade,
             plot_scatter_mt_ch,
             plot_line_notas,
             plot_bolhas_uf_notas,
             plot_box_uf_redacao,
             plot_bar_erro)

lay <- rbind(c(1,2,3,4),
             c(5,6,7,8))

grid.arrange(plot_idioma_sexo,plot_uf_conclusao,
             plot_piram_idade,plot_scatter_mt_ch,
             plot_line_notas,plot_bolhas_uf_notas,
             plot_box_uf_redacao,plot_bar_erro,
             layout_matrix = lay)

lay <- rbind(c(1,1),
             c(2,3))

grid.arrange(plot_line_notas,
             plot_box_uf_redacao,plot_bar_erro,
             layout_matrix = lay)

lay <- rbind(c(1,2,3),
             c(4,4,5))

grid.arrange(plot_scatter_mt_ch, plot_bolhas_uf_notas, plot_idioma_sexo,
             plot_uf_conclusao,plot_piram_idade,
             layout_matrix = lay)
