
# Versão do R utilizada na execução das análises: 4.3.0

## Codigo de replicacao para a classificação de estabelecimentos religiosos
## descritas na seguinte Nota técnica:
## Surgimento, trajetória e expansão das Igrejas Evangélicas no território Brasileiro 
## ao longo do último século (1920-2019)



## Os pacotes necessários para a execucação da análise são os seguintes:
library(tidyverse) ## funções gerais para preparação e consolidação dos dados
library(stringi) ## funções para a deteção e classificação de variáveis de texto
library(foreign) ## converte dataframes em outros formatos (e.g., CSV)
library(geobr)  ## disponibilizar shapefiles para plotar informações no território brasileiro
library(ggpubr) ## permite combinar em uma única figura arquivos do ggplot



## Selecione o diretório onde o arquivo em CSV está salvo no seu computador
setwd("")

## Uploading o dataframe com as informações sobre os estabelecimentos religiosos
df_cnpjs <- read.csv("df_igrejas_nomes.csv", sep =",", header = TRUE)


## A classificação das igrejas segue duas etapas:
# Na primeira, identificamos as denominações evangélicas utilizando termos chave
# Na segunda, agregamos essas denominações em uma única categoria

## Por exemplo, no primeiro caso, as denominações Batista, Metodista,
 # Presbiteriana, Luterana, Anglicana, Episcopal, Reformada, Menonita
 # e Adventista são agrupadas na categoria "Missionárias"


## Categorias restritivas (Missionárias)
df_cnpjs$batista <-str_detect(df_cnpjs$razao_social, "BATISTA")
df_cnpjs$metodista <-str_detect(df_cnpjs$razao_social, "METODISTA")
df_cnpjs$presbiteriana <-str_detect(df_cnpjs$razao_social, "PRESBITERIANA")
df_cnpjs$luterana <-str_detect(df_cnpjs$razao_social, "LUTERANA")
df_cnpjs$anglicana <-str_detect(df_cnpjs$razao_social, "ANGLICANA")
df_cnpjs$episcopal <-str_detect(df_cnpjs$razao_social, "EPISCOPAL")
df_cnpjs$reformada <-str_detect(df_cnpjs$razao_social, "REFORMADA")
df_cnpjs$menonitas <-str_detect(df_cnpjs$razao_social, "MENONITA")
df_cnpjs$menonitas2 <-str_detect(df_cnpjs$razao_social, "MENONITAS")
df_cnpjs$adventista <-str_detect(df_cnpjs$razao_social, "ADVENTISTA")


df_cnpjs$missionarias = ifelse(df_cnpjs$batista == "TRUE"|
                                 df_cnpjs$metodista  == "TRUE"|
                                 df_cnpjs$presbiteriana == "TRUE"|
                                 df_cnpjs$luterana == "TRUE"|
                                 df_cnpjs$anglicana == "TRUE"|
                                 df_cnpjs$episcopal == "TRUE"|
                                 df_cnpjs$reformada == "TRUE"|
                                 df_cnpjs$menonitas == "TRUE"|
                                 df_cnpjs$menonitas2 == "TRUE"|
                                 df_cnpjs$adventista == "TRUE", 1, 0)



## Categorias restritivas (pentecostais)
df_cnpjs$pentecostal <-str_detect(df_cnpjs$razao_social, "PENTECOSTAL")
df_cnpjs$assembleia <-str_detect(df_cnpjs$razao_social, "ASSEMBLEIA")
df_cnpjs$quadrangular <-str_detect(df_cnpjs$razao_social, "QUADRANGULAR")
df_cnpjs$deuseamor <-str_detect(df_cnpjs$razao_social, "IGREJA PENTECOSTAL DEUS E AMOR")
df_cnpjs$maranata <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA MARANATA")
df_cnpjs$deuseamor2 <-str_detect(df_cnpjs$razao_social, "DEUS E AMOR")
df_cnpjs$igrejadedeusbr <-str_detect(df_cnpjs$razao_social, "IGREJA DE DEUS NO BRASIL")
df_cnpjs$cristanobr <-str_detect(df_cnpjs$razao_social, "CONGREGACAO CRISTA NO BRASIL")
df_cnpjs$brparacristo <-str_detect(df_cnpjs$razao_social, "O BRASIL PARA CRISTO")
df_cnpjs$nazareno <-str_detect(df_cnpjs$razao_social, "IGREJA DO NAZARENO")




df_cnpjs$pentecostais = ifelse(df_cnpjs$pentecostal  == "TRUE"|
                                 df_cnpjs$assembleia   == "TRUE"|
                                 df_cnpjs$quadrangular  == "TRUE"|
                                 df_cnpjs$deuseamor == "TRUE"|
                                 df_cnpjs$deuseamor2 == "TRUE"|
                                 df_cnpjs$igrejadedeusbr == "TRUE"|
                                 df_cnpjs$igrejadedeusbr == "TRUE"|
                                 df_cnpjs$cristanobr == "TRUE"|
                                 df_cnpjs$brparacristo == "TRUE"|
                                 df_cnpjs$nazareno == "TRUE", 1, 0)




## Categorias restritivas (neopentecostais)
df_cnpjs$iurd <-str_detect(df_cnpjs$razao_social, "IGREJA UNIVERSAL DO REINO DE DEUS")
df_cnpjs$bola <-str_detect(df_cnpjs$razao_social, "BOLA DE NEVE")
df_cnpjs$sara <-str_detect(df_cnpjs$razao_social, "SARA NOSSA TERRA")
df_cnpjs$mundial <-str_detect(df_cnpjs$razao_social, "IGREJA MUNDIAL DO PODER DE DEUS")
df_cnpjs$renascer <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA APOSTOLICA RENASCER EM CRISTO")
df_cnpjs$internacional <-str_detect(df_cnpjs$razao_social, "IGREJA INTERNACIONAL DA GRACA DE DEUS")
df_cnpjs$casabencao <-str_detect(df_cnpjs$razao_social, "IGREJA TABERNACULO EVANGELICO DE JESUS")



df_cnpjs$neopente = ifelse(df_cnpjs$iurd == "TRUE"|
                             df_cnpjs$bola == "TRUE"|
                             df_cnpjs$sara == "TRUE"|
                             df_cnpjs$mundial  == "TRUE"|
                             df_cnpjs$renascer == "TRUE"|
                             df_cnpjs$internacional == "TRUE"|
                             df_cnpjs$casabencao == "TRUE", 1, 0)


## Igrejas de classificação não determinada 
df_cnpjs$igreja1 <-str_detect(df_cnpjs$razao_social, "IGREJA EM")
df_cnpjs$igreja2 <-str_detect(df_cnpjs$razao_social, "IGREJA APOSTOLICA FONTE DA VIDA")
df_cnpjs$igreja3 <-str_detect(df_cnpjs$razao_social, "COMUNIDADE CRISTA PAZ E VIDA")
df_cnpjs$igreja4 <-str_detect(df_cnpjs$razao_social, "IGREJA BIBLICA")
df_cnpjs$igreja5 <-str_detect(df_cnpjs$razao_social, "IGREJA NACIONAL DO SENHOR JESUS CRISTO")
df_cnpjs$igreja6 <-str_detect(df_cnpjs$razao_social, "ASSOCIACAO MISSIONARIA EVANGELICA VIDA")
df_cnpjs$igreja7 <-str_detect(df_cnpjs$razao_social, "COMUNIDADE EVANGELICA")
df_cnpjs$igreja8 <-str_detect(df_cnpjs$razao_social, "CASA DE ORACAO")
df_cnpjs$igreja9 <-str_detect(df_cnpjs$razao_social, "CEO COMUNIDADE DE EVANGELIZACAO E ORACAO")
df_cnpjs$igreja10 <-str_detect(df_cnpjs$razao_social, "COMUNIDADE CRISTA")
df_cnpjs$igreja11 <-str_detect(df_cnpjs$razao_social, "COMUNIDADE DA GRACA")
df_cnpjs$igreja12 <-str_detect(df_cnpjs$razao_social, "ALIANCA BIBLICA DE AVIVAMENTO")
df_cnpjs$igreja13 <-str_detect(df_cnpjs$razao_social, "IGREJA APOSTOLICA")
df_cnpjs$igreja14 <-str_detect(df_cnpjs$razao_social, "IGREJA BETEL")
df_cnpjs$igreja15 <-str_detect(df_cnpjs$razao_social, "IGREJA CATEDRAL DE ADORACAO E MILAGRES")
df_cnpjs$igreja16 <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA EVANGELICA")
df_cnpjs$igreja17 <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA")
df_cnpjs$igreja18 <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA FILADELFIA")
df_cnpjs$igreja19 <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA PRIMITIVA")
df_cnpjs$igreja20 <-str_detect(df_cnpjs$razao_social, "IGREJA CRISTA NOVA ALIANCA")
df_cnpjs$igreja21 <-str_detect(df_cnpjs$razao_social, "IGREJA DA BENCAO")
df_cnpjs$igreja22 <-str_detect(df_cnpjs$razao_social, "IGREJA DA PAZ")
df_cnpjs$igreja23 <-str_detect(df_cnpjs$razao_social, "IGREJA DE CRISTO")
df_cnpjs$igreja24 <-str_detect(df_cnpjs$razao_social, "IGREJA DE DEUS")
df_cnpjs$igreja25 <-str_detect(df_cnpjs$razao_social, "IGREJA DE NOVA VIDA")
df_cnpjs$igreja26 <-str_detect(df_cnpjs$razao_social, "IGREJA DO EVANGELHO PLENO")
df_cnpjs$igreja27 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA A PALAVRA DE CRISTO NO BRASIL")
df_cnpjs$igreja28 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA A VOZ DE DEUS")
df_cnpjs$igreja29 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ADONAI")
df_cnpjs$igreja30 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA AGAPE")
df_cnpjs$igreja31 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA AGUA DA VIDA")
df_cnpjs$igreja32 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ALIANCA COM CRISTO")
df_cnpjs$igreja33 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ALIANCA COM DEUS")
df_cnpjs$igreja34 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ALIANCA ETERNA")
df_cnpjs$igreja35 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ALIANCA RENOVADA")
df_cnpjs$igreja36 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA APOSTOLICA")
df_cnpjs$igreja37 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA ARCA DA ALIANCA")
df_cnpjs$igreja38 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA AVIVAMENTO BIBLICO")
df_cnpjs$igreja39 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA BETANIA")
df_cnpjs$igreja40 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA BETEL")
df_cnpjs$igreja41 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA BETHEL")
df_cnpjs$igreja42 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA COMUNIDADE")
df_cnpjs$igreja43 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA CONGREGACIONAL")
df_cnpjs$igreja44 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA CASA DE ORACAO")
df_cnpjs$igreja45 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA CRISTA")
df_cnpjs$igreja46 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA CRISTO")
df_cnpjs$igreja47 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DA PAZ")
df_cnpjs$igreja48 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DE JESUS")
df_cnpjs$igreja49 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DEUS")
df_cnpjs$igreja50 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DO DEUS VIVO")
df_cnpjs$igreja51 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DO FUNDAMENTO BIBLICO")
df_cnpjs$igreja52 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DO PODER DE DEUS")
df_cnpjs$igreja53 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DO SENHOR JESUS")
df_cnpjs$igreja54 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA DOS IRMAOS")
df_cnpjs$igreja55 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA EBENEZER")
df_cnpjs$igreja56 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA EDIFICACAO EM CRISTO")
df_cnpjs$igreja57 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA FAMILIA DE DEUS")
df_cnpjs$igreja58 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA FILADELFIA")
df_cnpjs$igreja59 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA FONTE")
df_cnpjs$igreja60 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA GRACA E ")
df_cnpjs$igreja61 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA INTERNACIONAL")
df_cnpjs$igreja62 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA JESUS CRISTO E")
df_cnpjs$igreja63 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA JUVENTUDE DE CRISTO")
df_cnpjs$igreja64 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA LUZ")
df_cnpjs$igreja65 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA MINISTERIO")
df_cnpjs$igreja66 <-str_detect(df_cnpjs$razao_social, "IGREJA MINISTERIO")
df_cnpjs$igreja67 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA MISSAO")
df_cnpjs$igreja68 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA MISSIONARIA")
df_cnpjs$igreja69 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA MISSIONARIA SO O SENHOR E DEUS")
df_cnpjs$igreja70 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA MONTE")
df_cnpjs$igreja71 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA NOVA")
df_cnpjs$igreja72 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA PALAVRA")
df_cnpjs$igreja73 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA REMIDOS POR CRISTO")
df_cnpjs$igreja74 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA RESTAURACAO")
df_cnpjs$igreja75 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA REVIVER EM")
df_cnpjs$igreja76 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA TABERNACULO")
df_cnpjs$igreja77 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA TEMPLO")
df_cnpjs$igreja78 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA TRANSCULTURAL")
df_cnpjs$igreja79 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA VERBO DA VIDA")
df_cnpjs$igreja80 <-str_detect(df_cnpjs$razao_social, "IGREJA EVANGELICA VIDA")
df_cnpjs$igreja81 <-str_detect(df_cnpjs$razao_social, "IGREJA INTERNACIONAL")
df_cnpjs$igreja82 <-str_detect(df_cnpjs$razao_social, "IGREJA MISSIONARIA")
df_cnpjs$igreja83 <-str_detect(df_cnpjs$razao_social, "IGREJA MUNDIAL")
df_cnpjs$igreja84 <-str_detect(df_cnpjs$razao_social, "IGREJA NACIONAL")
df_cnpjs$igreja85 <-str_detect(df_cnpjs$razao_social, "IGREJA NOVA")
df_cnpjs$igreja86 <-str_detect(df_cnpjs$razao_social, "IGREJA VIDEIRA")
df_cnpjs$igreja87 <-str_detect(df_cnpjs$razao_social, "LUZ PARA OS POVOS")
df_cnpjs$igreja88 <-str_detect(df_cnpjs$razao_social, "MINISTERIO APOSTOLICO")
df_cnpjs$igreja89 <-str_detect(df_cnpjs$razao_social, "MINISTERIO COMUNIDADE CRISTA")
df_cnpjs$igreja90 <-str_detect(df_cnpjs$razao_social, "MINISTERIO EVANGELICO")
df_cnpjs$igreja91 <-str_detect(df_cnpjs$razao_social, "MINISTERIO MUDANCA DE VIDA")
df_cnpjs$igreja92 <-str_detect(df_cnpjs$razao_social, "MINISTERIO PALAVRA")
df_cnpjs$igreja93 <-str_detect(df_cnpjs$razao_social, "PRIMEIRA IGREJA EVANGELICA")




df_cnpjs$naodeterm = ifelse(df_cnpjs$igreja1 == "TRUE"|
                              df_cnpjs$igreja2 == "TRUE"|
                              df_cnpjs$igreja3 == "TRUE"|
                              df_cnpjs$igreja4  == "TRUE"|
                              df_cnpjs$igreja5 == "TRUE"|
                              df_cnpjs$igreja6 == "TRUE"|
                              df_cnpjs$igreja6 == "TRUE"|
                              df_cnpjs$igreja7 == "TRUE"|
                              df_cnpjs$igreja8 == "TRUE"|
                              df_cnpjs$igreja9 == "TRUE"|
                              df_cnpjs$igreja10 == "TRUE"|
                              df_cnpjs$igreja11 == "TRUE"|
                              df_cnpjs$igreja12 == "TRUE"|
                              df_cnpjs$igreja13 == "TRUE"|
                              df_cnpjs$igreja14 == "TRUE"|
                              df_cnpjs$igreja15 == "TRUE"|
                              df_cnpjs$igreja16 == "TRUE"|
                              df_cnpjs$igreja17 == "TRUE"|
                              df_cnpjs$igreja18 == "TRUE"|
                              df_cnpjs$igreja19 == "TRUE"|
                              df_cnpjs$igreja20 == "TRUE"|
                              df_cnpjs$igreja21 == "TRUE"|
                              df_cnpjs$igreja22 == "TRUE"|
                              df_cnpjs$igreja23 == "TRUE"|
                              df_cnpjs$igreja24 == "TRUE"|
                              df_cnpjs$igreja25 == "TRUE"|
                              df_cnpjs$igreja26 == "TRUE"|
                              df_cnpjs$igreja27 == "TRUE"|
                              df_cnpjs$igreja28 == "TRUE"|
                              df_cnpjs$igreja29 == "TRUE"|
                              df_cnpjs$igreja30 == "TRUE"|
                              df_cnpjs$igreja31 == "TRUE"|
                              df_cnpjs$igreja32 == "TRUE"|
                              df_cnpjs$igreja33 == "TRUE"|
                              df_cnpjs$igreja34 == "TRUE"|
                              df_cnpjs$igreja35 == "TRUE"|
                              df_cnpjs$igreja36 == "TRUE"|
                              df_cnpjs$igreja37 == "TRUE"|
                              df_cnpjs$igreja38 == "TRUE"|
                              df_cnpjs$igreja39 == "TRUE"|
                              df_cnpjs$igreja40 == "TRUE"|
                              df_cnpjs$igreja41 == "TRUE"|
                              df_cnpjs$igreja42 == "TRUE"|
                              df_cnpjs$igreja43 == "TRUE"|
                              df_cnpjs$igreja44 == "TRUE"|
                              df_cnpjs$igreja45 == "TRUE"|
                              df_cnpjs$igreja46 == "TRUE"|
                              df_cnpjs$igreja47 == "TRUE"|
                              df_cnpjs$igreja48 == "TRUE"|
                              df_cnpjs$igreja49 == "TRUE"|
                              df_cnpjs$igreja50 == "TRUE"|
                              df_cnpjs$igreja51 == "TRUE"|
                              df_cnpjs$igreja52 == "TRUE"|
                              df_cnpjs$igreja53 == "TRUE"|
                              df_cnpjs$igreja54 == "TRUE"|
                              df_cnpjs$igreja55 == "TRUE"|
                              df_cnpjs$igreja56 == "TRUE"|
                              df_cnpjs$igreja57 == "TRUE"|
                              df_cnpjs$igreja58 == "TRUE"|
                              df_cnpjs$igreja59 == "TRUE"|
                              df_cnpjs$igreja60 == "TRUE"|
                              df_cnpjs$igreja61 == "TRUE"|
                              df_cnpjs$igreja62 == "TRUE"|
                              df_cnpjs$igreja63 == "TRUE"|
                              df_cnpjs$igreja64 == "TRUE"|
                              df_cnpjs$igreja65 == "TRUE"|
                              df_cnpjs$igreja66 == "TRUE"|
                              df_cnpjs$igreja67 == "TRUE"|
                              df_cnpjs$igreja68 == "TRUE"|
                              df_cnpjs$igreja69 == "TRUE"|
                              df_cnpjs$igreja70 == "TRUE"|
                              df_cnpjs$igreja71 == "TRUE"|
                              df_cnpjs$igreja72 == "TRUE"|
                              df_cnpjs$igreja73 == "TRUE"|
                              df_cnpjs$igreja74 == "TRUE"|
                              df_cnpjs$igreja75 == "TRUE"|
                              df_cnpjs$igreja76 == "TRUE"|
                              df_cnpjs$igreja77 == "TRUE"|
                              df_cnpjs$igreja78 == "TRUE"|
                              df_cnpjs$igreja79 == "TRUE"|
                              df_cnpjs$igreja80 == "TRUE"|
                              df_cnpjs$igreja81 == "TRUE"|
                              df_cnpjs$igreja82 == "TRUE"|
                              df_cnpjs$igreja83 == "TRUE"|
                              df_cnpjs$igreja84 == "TRUE"|
                              df_cnpjs$igreja85 == "TRUE"|
                              df_cnpjs$igreja86 == "TRUE"|
                              df_cnpjs$igreja87 == "TRUE"|
                              df_cnpjs$igreja88 == "TRUE"|
                              df_cnpjs$igreja89 == "TRUE"|
                              df_cnpjs$igreja90 == "TRUE"|
                              df_cnpjs$igreja91 == "TRUE"|
                              df_cnpjs$igreja92 == "TRUE"|
                              df_cnpjs$igreja93 == "TRUE", 1, 0)


## O código abaixo permite colocar todas as denominações evangélicas numa única categoria "evangélicas ###

df_cnpjs$evangelicas = ifelse(df_cnpjs$missionarias == "1" |
                                df_cnpjs$pentecostais  == "1" |
                                df_cnpjs$neopente == "1"|
                                df_cnpjs$naodeterm == "1", 1, 0)



##########################################################
## Estimando o número de igrejas ativas, por UF e ANO (1960-2019)
##########################################################

## Neste caso, executamos esse procedimento para todas as igrejas
## evangélicas, e depois para cada um dos grupos denominacionais:
## Missionárias, Pentecostais, Neopentecostais e de classificaçõa não determinada


################################
## Todas as Igrejas Evangélicas
###############################

## Para realizar a contagem, criamos um dataframe com todas as igrejas evangélicas 
  ## isto é, excluindo outros tipos de estabelecimentos religiosos

df_evangelicas <- filter(df_cnpjs, evangelicas == 1)


## Contando o número de igrejas ativas, em cada UF, em 1960

## 1960
count_1960 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1960 = sum(count_1960))
## deixando apenas as igrejas ativas no Brasil
count_1960 <- filter(count_1960, uf !="EX")

## O mesmo procedimento é executado, mas para o ano de 1961

## 1961
count_1961 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1961 = sum(count_1961))
# deixando apenas as igrejas ativas no Brasil
count_1961 <- filter(count_1961, uf !="EX")


## 1962
count_1962 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1962 = sum(count_1962))
# deixando apenas as igrejas ativas no Brasil
count_1962 <- filter(count_1962, uf !="EX")


## 1963
count_1963 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1963 = sum(count_1963))
# deixando apenas as igrejas ativas no Brasil
count_1963 <- filter(count_1963, uf !="EX")


## 1964
count_1964 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1964 = sum(count_1964))
# deixando apenas as igrejas ativas no Brasil
count_1964 <- filter(count_1964, uf !="EX")


## 1965
count_1965 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1965 = sum(count_1965))
# deixando apenas as igrejas ativas no Brasil
count_1965 <- filter(count_1965, uf !="EX")


## 1966
count_1966 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1966 = sum(count_1966))
# deixando apenas as igrejas ativas no Brasil
count_1966 <- filter(count_1966, uf !="EX")


## 1967
count_1967 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1967 = sum(count_1967))
# deixando apenas as igrejas ativas no Brasil
count_1967 <- filter(count_1967, uf !="EX")


## 1968
count_1968 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1968 = sum(count_1968))
# deixando apenas as igrejas ativas no Brasil
count_1968 <- filter(count_1968, uf !="EX")


## 1969
count_1969 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1969 = sum(count_1969))
# deixando apenas as igrejas ativas no Brasil
count_1969 <- filter(count_1969, uf !="EX")


## 1970
count_1970 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1970 = sum(count_1970))
# deixando apenas as igrejas ativas no Brasil
count_1970 <- filter(count_1970, uf !="EX")


## 1971
count_1971 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1971 = sum(count_1971))
# deixando apenas as igrejas ativas no Brasil
count_1971 <- filter(count_1971, uf !="EX")

## 1972
count_1972 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1972 = sum(count_1972))
# deixando apenas as igrejas ativas no Brasil
count_1972 <- filter(count_1972, uf !="EX")

## 1973
count_1973 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1973 = sum(count_1973))
# deixando apenas as igrejas ativas no Brasil
count_1973 <- filter(count_1973, uf !="EX")


## 1974
count_1974 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1974 = sum(count_1974))
# deixando apenas as igrejas ativas no Brasil
count_1974 <- filter(count_1974, uf !="EX")


## 1975
count_1975 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1975 = sum(count_1975))
# deixando apenas as igrejas ativas no Brasil
count_1975 <- filter(count_1975, uf !="EX")


## 1976
count_1976 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1976 = sum(count_1976))
# deixando apenas as igrejas ativas no Brasil
count_1976 <- filter(count_1976, uf !="EX")


## 1977
count_1977 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1977 = sum(count_1977))
# deixando apenas as igrejas ativas no Brasil
count_1977 <- filter(count_1977, uf !="EX")


## 1978
count_1978 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1978 = sum(count_1978))
# deixando apenas as igrejas ativas no Brasil
count_1978 <- filter(count_1978, uf !="EX")


## 1979
count_1979 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1979 = sum(count_1979))
# deixando apenas as igrejas ativas no Brasil
count_1979 <- filter(count_1979, uf !="EX")


## 1980
count_1980 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1980 = sum(count_1980))
# deixando apenas as igrejas ativas no Brasil
count_1980 <- filter(count_1980, uf !="EX")


## 1981
count_1981 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1981 = sum(count_1981))
# deixando apenas as igrejas ativas no Brasil
count_1981 <- filter(count_1981, uf !="EX")


## 1982
count_1982 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1982 = sum(count_1982))
# deixando apenas as igrejas ativas no Brasil
count_1982 <- filter(count_1982, uf !="EX")


## 1983
count_1983 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1983 = sum(count_1983))
# deixando apenas as igrejas ativas no Brasil
count_1983 <- filter(count_1983, uf !="EX")


## 1984
count_1984 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1984 = sum(count_1984))
# deixando apenas as igrejas ativas no Brasil
count_1984 <- filter(count_1984, uf !="EX")


## 1985
count_1985 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1985 = sum(count_1985))
# deixando apenas as igrejas ativas no Brasil
count_1985 <- filter(count_1985, uf !="EX")


## 1986
count_1986 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1986 = sum(count_1986))
# deixando apenas as igrejas ativas no Brasil
count_1986 <- filter(count_1986, uf !="EX")


## 1987
count_1987 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1987 = sum(count_1987))
# deixando apenas as igrejas ativas no Brasil
count_1987 <- filter(count_1987, uf !="EX")


## 1988
count_1988 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1988 = sum(count_1988))
# deixando apenas as igrejas ativas no Brasil
count_1988 <- filter(count_1988, uf !="EX")


## 1989
count_1989 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1989 = sum(count_1989))
# deixando apenas as igrejas ativas no Brasil
count_1989 <- filter(count_1989, uf !="EX")


## 1990
count_1990 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1990 = sum(count_1990))
# deixando apenas as igrejas ativas no Brasil
count_1990 <- filter(count_1990, uf !="EX")


## 1991
count_1991 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1991 = sum(count_1991))
# deixando apenas as igrejas ativas no Brasil
count_1991 <- filter(count_1991, uf !="EX")


## 1992
count_1992 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1992 = sum(count_1992))
# deixando apenas as igrejas ativas no Brasil
count_1992 <- filter(count_1992, uf !="EX")


## 1993
count_1993 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1993 = sum(count_1993))
# deixando apenas as igrejas ativas no Brasil
count_1993 <- filter(count_1993, uf !="EX")


## 1994
count_1994 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1994 = sum(count_1994))
# deixando apenas as igrejas ativas no Brasil
count_1994 <- filter(count_1994, uf !="EX")


## 1995
count_1995 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1995 = sum(count_1995))
# deixando apenas as igrejas ativas no Brasil
count_1995 <- filter(count_1995, uf !="EX")


## 1996
count_1996 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1996 = sum(count_1996))
# deixando apenas as igrejas ativas no Brasil
count_1996 <- filter(count_1996, uf !="EX")


## 1997
count_1997 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1997 = sum(count_1997))
# deixando apenas as igrejas ativas no Brasil
count_1997 <- filter(count_1997, uf !="EX")


## 1998
count_1998 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1998 = sum(count_1998))
# deixando apenas as igrejas ativas no Brasil
count_1998 <- filter(count_1998, uf !="EX")


## 1999
count_1999 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_1999 = sum(count_1999))
# deixando apenas as igrejas ativas no Brasil
count_1999 <- filter(count_1999, uf !="EX")


## 2000
count_2000 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2000 = sum(count_2000))
# deixando apenas as igrejas ativas no Brasil
count_2000 <- filter(count_2000, uf !="EX")


## 2001
count_2001 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2001 = sum(count_2001))
# deixando apenas as igrejas ativas no Brasil
count_2001 <- filter(count_2001, uf !="EX")


## 2002
count_2002 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2002 = sum(count_2002))
# deixando apenas as igrejas ativas no Brasil
count_2002 <- filter(count_2002, uf !="EX")


## 2003
count_2003 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2003 = sum(count_2003))
# deixando apenas as igrejas ativas no Brasil
count_2003 <- filter(count_2003, uf !="EX")



## 2004
count_2004 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2004 = sum(count_2004))
# deixando apenas as igrejas ativas no Brasil
count_2004 <- filter(count_2004, uf !="EX")



## 2005
count_2005 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2005 = sum(count_2005))
# deixando apenas as igrejas ativas no Brasil
count_2005 <- filter(count_2005, uf !="EX")



## 2006
count_2006 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2006 = sum(count_2006))
# deixando apenas as igrejas ativas no Brasil
count_2006 <- filter(count_2006, uf !="EX")



## 2007
count_2007 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2007 = sum(count_2007))
# deixando apenas as igrejas ativas no Brasil
count_2007 <- filter(count_2007, uf !="EX")



## 2008
count_2008 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2008 = sum(count_2008))
# deixando apenas as igrejas ativas no Brasil
count_2008 <- filter(count_2008, uf !="EX")



## 2009
count_2009 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2009 = sum(count_2009))
# deixando apenas as igrejas ativas no Brasil
count_2009 <- filter(count_2009, uf !="EX")



## 2010
count_2010 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2010 = sum(count_2010))
# deixando apenas as igrejas ativas no Brasil
count_2010 <- filter(count_2010, uf !="EX")


## 2011
count_2011 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2011 = sum(count_2011))
# deixando apenas as igrejas ativas no Brasil
count_2011 <- filter(count_2011, uf !="EX")


## 2012
count_2012 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2012 = sum(count_2012))
# deixando apenas as igrejas ativas no Brasil
count_2012 <- filter(count_2012, uf !="EX")


## 2013
count_2013 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2013 = sum(count_2013))
# deixando apenas as igrejas ativas no Brasil
count_2013 <- filter(count_2013, uf !="EX")

## 2014
count_2014 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2014 = sum(count_2014))
# deixando apenas as igrejas ativas no Brasil
count_2014 <- filter(count_2014, uf !="EX")


## 2015
count_2015 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2015 = sum(count_2015))
# deixando apenas as igrejas ativas no Brasil
count_2015 <- filter(count_2015, uf !="EX")


## 2016
count_2016 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2016 = sum(count_2016))
# deixando apenas as igrejas ativas no Brasil
count_2016 <- filter(count_2016, uf !="EX")


## 2017
count_2017 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2017 = sum(count_2017))
# deixando apenas as igrejas ativas no Brasil
count_2017 <- filter(count_2017, uf !="EX")


## 2018
count_2018 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2018 = sum(count_2018))
# deixando apenas as igrejas ativas no Brasil
count_2018 <- filter(count_2018, uf !="EX")


## 2019
count_2019 <- df_evangelicas %>%
  group_by (uf) %>% 
  summarise(ativas_all_2019 = sum(count_2019))
# deixando apenas as igrejas ativas no Brasil
count_2019 <- filter(count_2019, uf !="EX")


## Juntando todos os anos em um único dataframe

comb_ger <- count_1960 %>% 
  left_join(count_1961, by = c("uf")) %>%
  left_join(count_1962, by = c("uf")) %>%
  left_join(count_1963, by = c("uf")) %>%
  left_join(count_1964, by = c("uf")) %>%
  left_join(count_1965, by = c("uf")) %>%
  left_join(count_1966, by = c("uf")) %>%
  left_join(count_1967, by = c("uf")) %>%
  left_join(count_1968, by = c("uf")) %>%
  left_join(count_1969, by = c("uf")) %>%
  left_join(count_1970, by = c("uf")) %>%
  left_join(count_1971, by = c("uf")) %>%
  left_join(count_1972, by = c("uf")) %>%
  left_join(count_1973, by = c("uf")) %>%
  left_join(count_1974, by = c("uf")) %>%
  left_join(count_1975, by = c("uf")) %>%
  left_join(count_1976, by = c("uf")) %>%
  left_join(count_1977, by = c("uf")) %>%
  left_join(count_1978, by = c("uf")) %>%
  left_join(count_1979, by = c("uf")) %>%
  left_join(count_1980, by = c("uf")) %>%
  left_join(count_1981, by = c("uf")) %>%
  left_join(count_1982, by = c("uf")) %>%
  left_join(count_1983, by = c("uf")) %>%
  left_join(count_1984, by = c("uf")) %>%
  left_join(count_1985, by = c("uf")) %>%
  left_join(count_1986, by = c("uf")) %>%
  left_join(count_1987, by = c("uf")) %>%
  left_join(count_1988, by = c("uf")) %>%
  left_join(count_1989, by = c("uf")) %>%
  left_join(count_1990, by = c("uf")) %>%
  left_join(count_1991, by = c("uf")) %>%
  left_join(count_1992, by = c("uf")) %>%
  left_join(count_1993, by = c("uf")) %>%
  left_join(count_1994, by = c("uf")) %>%
  left_join(count_1995, by = c("uf")) %>%
  left_join(count_1996, by = c("uf")) %>%
  left_join(count_1997, by = c("uf")) %>%
  left_join(count_1998, by = c("uf")) %>%
  left_join(count_1999, by = c("uf")) %>%
  left_join(count_2000, by = c("uf")) %>%
  left_join(count_2001, by = c("uf")) %>%
  left_join(count_2002, by = c("uf")) %>%
  left_join(count_2003, by = c("uf")) %>%
  left_join(count_2004, by = c("uf")) %>%
  left_join(count_2005, by = c("uf")) %>%
  left_join(count_2006, by = c("uf")) %>%
  left_join(count_2007, by = c("uf")) %>%
  left_join(count_2008, by = c("uf")) %>%
  left_join(count_2009, by = c("uf")) %>%
  left_join(count_2010, by = c("uf")) %>%
  left_join(count_2011, by = c("uf")) %>%
  left_join(count_2012, by = c("uf")) %>%
  left_join(count_2013, by = c("uf")) %>%
  left_join(count_2014, by = c("uf")) %>%
  left_join(count_2015, by = c("uf")) %>%
  left_join(count_2016, by = c("uf")) %>%
  left_join(count_2017, by = c("uf")) %>%
  left_join(count_2018, by = c("uf")) %>%
  left_join(count_2019, by = c("uf"))



####################################
## Igrejas Evangélicas Missionárias
###################################

## Para realizar a contagem, criamos um dataframe apenas com as Igrejas
  ## Evangélicas Missionárias

df_missionarias <- filter(df_cnpjs, missionarias == 1)


## Contando o número de igrejas ativas, em cada UF, em 1960

## 1960
count_1960 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1960 = sum(count_1960))
## deixando apenas as igrejas ativas no Brasil
count_1960 <- filter(count_1960, uf !="EX")


## 1961
count_1961 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1961 = sum(count_1961))
# deixando apenas as igrejas ativas no Brasil
count_1961 <- filter(count_1961, uf !="EX")


## 1962
count_1962 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1962 = sum(count_1962))
# deixando apenas as igrejas ativas no Brasil
count_1962 <- filter(count_1962, uf !="EX")


## 1963
count_1963 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1963 = sum(count_1963))
# deixando apenas as igrejas ativas no Brasil
count_1963 <- filter(count_1963, uf !="EX")


## 1964
count_1964 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1964 = sum(count_1964))
# deixando apenas as igrejas ativas no Brasil
count_1964 <- filter(count_1964, uf !="EX")


## 1965
count_1965 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1965 = sum(count_1965))
# deixando apenas as igrejas ativas no Brasil
count_1965 <- filter(count_1965, uf !="EX")


## 1966
count_1966 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1966 = sum(count_1966))
# deixando apenas as igrejas ativas no Brasil
count_1966 <- filter(count_1966, uf !="EX")


## 1967
count_1967 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1967 = sum(count_1967))
# deixando apenas as igrejas ativas no Brasil
count_1967 <- filter(count_1967, uf !="EX")


## 1968
count_1968 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1968 = sum(count_1968))
# deixando apenas as igrejas ativas no Brasil
count_1968 <- filter(count_1968, uf !="EX")


## 1969
count_1969 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1969 = sum(count_1969))
# deixando apenas as igrejas ativas no Brasil
count_1969 <- filter(count_1969, uf !="EX")


## 1970
count_1970 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1970 = sum(count_1970))
# deixando apenas as igrejas ativas no Brasil
count_1970 <- filter(count_1970, uf !="EX")


## 1971
count_1971 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1971 = sum(count_1971))
# deixando apenas as igrejas ativas no Brasil
count_1971 <- filter(count_1971, uf !="EX")

## 1972
count_1972 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1972 = sum(count_1972))
# deixando apenas as igrejas ativas no Brasil
count_1972 <- filter(count_1972, uf !="EX")

## 1973
count_1973 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1973 = sum(count_1973))
# deixando apenas as igrejas ativas no Brasil
count_1973 <- filter(count_1973, uf !="EX")


## 1974
count_1974 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1974 = sum(count_1974))
# deixando apenas as igrejas ativas no Brasil
count_1974 <- filter(count_1974, uf !="EX")


## 1975
count_1975 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1975 = sum(count_1975))
# deixando apenas as igrejas ativas no Brasil
count_1975 <- filter(count_1975, uf !="EX")


## 1976
count_1976 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1976 = sum(count_1976))
# deixando apenas as igrejas ativas no Brasil
count_1976 <- filter(count_1976, uf !="EX")


## 1977
count_1977 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1977 = sum(count_1977))
# deixando apenas as igrejas ativas no Brasil
count_1977 <- filter(count_1977, uf !="EX")


## 1978
count_1978 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1978 = sum(count_1978))
# deixando apenas as igrejas ativas no Brasil
count_1978 <- filter(count_1978, uf !="EX")


## 1979
count_1979 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1979 = sum(count_1979))
# deixando apenas as igrejas ativas no Brasil
count_1979 <- filter(count_1979, uf !="EX")


## 1980
count_1980 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1980 = sum(count_1980))
# deixando apenas as igrejas ativas no Brasil
count_1980 <- filter(count_1980, uf !="EX")


## 1981
count_1981 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1981 = sum(count_1981))
# deixando apenas as igrejas ativas no Brasil
count_1981 <- filter(count_1981, uf !="EX")


## 1982
count_1982 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1982 = sum(count_1982))
# deixando apenas as igrejas ativas no Brasil
count_1982 <- filter(count_1982, uf !="EX")


## 1983
count_1983 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1983 = sum(count_1983))
# deixando apenas as igrejas ativas no Brasil
count_1983 <- filter(count_1983, uf !="EX")


## 1984
count_1984 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1984 = sum(count_1984))
# deixando apenas as igrejas ativas no Brasil
count_1984 <- filter(count_1984, uf !="EX")


## 1985
count_1985 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1985 = sum(count_1985))
# deixando apenas as igrejas ativas no Brasil
count_1985 <- filter(count_1985, uf !="EX")


## 1986
count_1986 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1986 = sum(count_1986))
# deixando apenas as igrejas ativas no Brasil
count_1986 <- filter(count_1986, uf !="EX")


## 1987
count_1987 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1987 = sum(count_1987))
# deixando apenas as igrejas ativas no Brasil
count_1987 <- filter(count_1987, uf !="EX")


## 1988
count_1988 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1988 = sum(count_1988))
# deixando apenas as igrejas ativas no Brasil
count_1988 <- filter(count_1988, uf !="EX")


## 1989
count_1989 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1989 = sum(count_1989))
# deixando apenas as igrejas ativas no Brasil
count_1989 <- filter(count_1989, uf !="EX")


## 1990
count_1990 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1990 = sum(count_1990))
# deixando apenas as igrejas ativas no Brasil
count_1990 <- filter(count_1990, uf !="EX")


## 1991
count_1991 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1991 = sum(count_1991))
# deixando apenas as igrejas ativas no Brasil
count_1991 <- filter(count_1991, uf !="EX")


## 1992
count_1992 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1992 = sum(count_1992))
# deixando apenas as igrejas ativas no Brasil
count_1992 <- filter(count_1992, uf !="EX")


## 1993
count_1993 <-df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1993 = sum(count_1993))
# deixando apenas as igrejas ativas no Brasil
count_1993 <- filter(count_1993, uf !="EX")


## 1994
count_1994 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1994 = sum(count_1994))
# deixando apenas as igrejas ativas no Brasil
count_1994 <- filter(count_1994, uf !="EX")


## 1995
count_1995 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1995 = sum(count_1995))
# deixando apenas as igrejas ativas no Brasil
count_1995 <- filter(count_1995, uf !="EX")


## 1996
count_1996 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1996 = sum(count_1996))
# deixando apenas as igrejas ativas no Brasil
count_1996 <- filter(count_1996, uf !="EX")


## 1997
count_1997 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1997 = sum(count_1997))
# deixando apenas as igrejas ativas no Brasil
count_1997 <- filter(count_1997, uf !="EX")


## 1998
count_1998 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1998 = sum(count_1998))
# deixando apenas as igrejas ativas no Brasil
count_1998 <- filter(count_1998, uf !="EX")


## 1999
count_1999 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_1999 = sum(count_1999))
# deixando apenas as igrejas ativas no Brasil
count_1999 <- filter(count_1999, uf !="EX")


## 2000
count_2000 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2000 = sum(count_2000))
# deixando apenas as igrejas ativas no Brasil
count_2000 <- filter(count_2000, uf !="EX")


## 2001
count_2001 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2001 = sum(count_2001))
# deixando apenas as igrejas ativas no Brasil
count_2001 <- filter(count_2001, uf !="EX")


## 2002
count_2002 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2002 = sum(count_2002))
# deixando apenas as igrejas ativas no Brasil
count_2002 <- filter(count_2002, uf !="EX")


## 2003
count_2003 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2003 = sum(count_2003))
# deixando apenas as igrejas ativas no Brasil
count_2003 <- filter(count_2003, uf !="EX")



## 2004
count_2004 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2004 = sum(count_2004))
# deixando apenas as igrejas ativas no Brasil
count_2004 <- filter(count_2004, uf !="EX")



## 2005
count_2005 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2005 = sum(count_2005))
# deixando apenas as igrejas ativas no Brasil
count_2005 <- filter(count_2005, uf !="EX")



## 2006
count_2006 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2006 = sum(count_2006))
# deixando apenas as igrejas ativas no Brasil
count_2006 <- filter(count_2006, uf !="EX")


## 2007
count_2007 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2007 = sum(count_2007))
# deixando apenas as igrejas ativas no Brasil
count_2007 <- filter(count_2007, uf !="EX")



## 2008
count_2008 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2008 = sum(count_2008))
# deixando apenas as igrejas ativas no Brasil
count_2008 <- filter(count_2008, uf !="EX")



## 2009
count_2009 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2009 = sum(count_2009))
# deixando apenas as igrejas ativas no Brasil
count_2009 <- filter(count_2009, uf !="EX")



## 2010
count_2010 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2010 = sum(count_2010))
# deixando apenas as igrejas ativas no Brasil
count_2010 <- filter(count_2010, uf !="EX")


## 2011
count_2011 <-df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2011 = sum(count_2011))
# deixando apenas as igrejas ativas no Brasil
count_2011 <- filter(count_2011, uf !="EX")


## 2012
count_2012 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2012 = sum(count_2012))
# deixando apenas as igrejas ativas no Brasil
count_2012 <- filter(count_2012, uf !="EX")


## 2013
count_2013 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2013 = sum(count_2013))
# deixando apenas as igrejas ativas no Brasil
count_2013 <- filter(count_2013, uf !="EX")

## 2014
count_2014 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2014 = sum(count_2014))
# deixando apenas as igrejas ativas no Brasil
count_2014 <- filter(count_2014, uf !="EX")


## 2015
count_2015 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2015 = sum(count_2015))
# deixando apenas as igrejas ativas no Brasil
count_2015 <- filter(count_2015, uf !="EX")


## 2016
count_2016 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2016 = sum(count_2016))
# deixando apenas as igrejas ativas no Brasil
count_2016 <- filter(count_2016, uf !="EX")


## 2017
count_2017 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2017 = sum(count_2017))
# deixando apenas as igrejas ativas no Brasil
count_2017 <- filter(count_2017, uf !="EX")


## 2018
count_2018 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2018 = sum(count_2018))
# deixando apenas as igrejas ativas no Brasil
count_2018 <- filter(count_2018, uf !="EX")


## 2019
count_2019 <- df_missionarias %>%
  group_by (uf) %>% 
  summarise(ativas_miss_2019 = sum(count_2019))
# deixando apenas as igrejas ativas no Brasil
count_2019 <- filter(count_2019, uf !="EX")



comb_mis <- count_1960 %>% 
  left_join(count_1961, by = c("uf")) %>%
  left_join(count_1962, by = c("uf")) %>%
  left_join(count_1963, by = c("uf")) %>%
  left_join(count_1964, by = c("uf")) %>%
  left_join(count_1965, by = c("uf")) %>%
  left_join(count_1966, by = c("uf")) %>%
  left_join(count_1967, by = c("uf")) %>%
  left_join(count_1968, by = c("uf")) %>%
  left_join(count_1969, by = c("uf")) %>%
  left_join(count_1970, by = c("uf")) %>%
  left_join(count_1971, by = c("uf")) %>%
  left_join(count_1972, by = c("uf")) %>%
  left_join(count_1973, by = c("uf")) %>%
  left_join(count_1974, by = c("uf")) %>%
  left_join(count_1975, by = c("uf")) %>%
  left_join(count_1976, by = c("uf")) %>%
  left_join(count_1977, by = c("uf")) %>%
  left_join(count_1978, by = c("uf")) %>%
  left_join(count_1979, by = c("uf")) %>%
  left_join(count_1980, by = c("uf")) %>%
  left_join(count_1981, by = c("uf")) %>%
  left_join(count_1982, by = c("uf")) %>%
  left_join(count_1983, by = c("uf")) %>%
  left_join(count_1984, by = c("uf")) %>%
  left_join(count_1985, by = c("uf")) %>%
  left_join(count_1986, by = c("uf")) %>%
  left_join(count_1987, by = c("uf")) %>%
  left_join(count_1988, by = c("uf")) %>%
  left_join(count_1989, by = c("uf")) %>%
  left_join(count_1990, by = c("uf")) %>%
  left_join(count_1991, by = c("uf")) %>%
  left_join(count_1992, by = c("uf")) %>%
  left_join(count_1993, by = c("uf")) %>%
  left_join(count_1994, by = c("uf")) %>%
  left_join(count_1995, by = c("uf")) %>%
  left_join(count_1996, by = c("uf")) %>%
  left_join(count_1997, by = c("uf")) %>%
  left_join(count_1998, by = c("uf")) %>%
  left_join(count_1999, by = c("uf")) %>%
  left_join(count_2000, by = c("uf")) %>%
  left_join(count_2001, by = c("uf")) %>%
  left_join(count_2002, by = c("uf")) %>%
  left_join(count_2003, by = c("uf")) %>%
  left_join(count_2004, by = c("uf")) %>%
  left_join(count_2005, by = c("uf")) %>%
  left_join(count_2006, by = c("uf")) %>%
  left_join(count_2007, by = c("uf")) %>%
  left_join(count_2008, by = c("uf")) %>%
  left_join(count_2009, by = c("uf")) %>%
  left_join(count_2010, by = c("uf")) %>%
  left_join(count_2011, by = c("uf")) %>%
  left_join(count_2012, by = c("uf")) %>%
  left_join(count_2013, by = c("uf")) %>%
  left_join(count_2014, by = c("uf")) %>%
  left_join(count_2015, by = c("uf")) %>%
  left_join(count_2016, by = c("uf")) %>%
  left_join(count_2017, by = c("uf")) %>%
  left_join(count_2018, by = c("uf")) %>%
  left_join(count_2019, by = c("uf"))




####################################
## Igrejas Evangélicas Pentecostais
###################################

## Para realizar a contagem, criamos um dataframe apenas com as Igrejas
## Evangélicas Pentecostais

df_pentecostais <- filter(df_cnpjs, pentecostais == 1)

## Contando o número de igrejas ativas, em cada UF, em 1960

## 1960
count_1960 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1960 = sum(count_1960))
## deixando apenas as igrejas ativas no Brasil
count_1960 <- filter(count_1960, uf !="EX")


## 1961
count_1961 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1961 = sum(count_1961))
# deixando apenas as igrejas ativas no Brasil
count_1961 <- filter(count_1961, uf !="EX")


## 1962
count_1962 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1962 = sum(count_1962))
# deixando apenas as igrejas ativas no Brasil
count_1962 <- filter(count_1962, uf !="EX")


## 1963
count_1963 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1963 = sum(count_1963))
# deixando apenas as igrejas ativas no Brasil
count_1963 <- filter(count_1963, uf !="EX")


## 1964
count_1964 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1964 = sum(count_1964))
# deixando apenas as igrejas ativas no Brasil
count_1964 <- filter(count_1964, uf !="EX")


## 1965
count_1965 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1965 = sum(count_1965))
# deixando apenas as igrejas ativas no Brasil
count_1965 <- filter(count_1965, uf !="EX")


## 1966
count_1966 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1966 = sum(count_1966))
# deixando apenas as igrejas ativas no Brasil
count_1966 <- filter(count_1966, uf !="EX")


## 1967
count_1967 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1967 = sum(count_1967))
# deixando apenas as igrejas ativas no Brasil
count_1967 <- filter(count_1967, uf !="EX")


## 1968
count_1968 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1968 = sum(count_1968))
# deixando apenas as igrejas ativas no Brasil
count_1968 <- filter(count_1968, uf !="EX")


## 1969
count_1969 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1969 = sum(count_1969))
# deixando apenas as igrejas ativas no Brasil
count_1969 <- filter(count_1969, uf !="EX")


## 1970
count_1970 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1970 = sum(count_1970))
# deixando apenas as igrejas ativas no Brasil
count_1970 <- filter(count_1970, uf !="EX")


## 1971
count_1971 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1971 = sum(count_1971))
# deixando apenas as igrejas ativas no Brasil
count_1971 <- filter(count_1971, uf !="EX")

## 1972
count_1972 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1972 = sum(count_1972))
# deixando apenas as igrejas ativas no Brasil
count_1972 <- filter(count_1972, uf !="EX")

## 1973
count_1973 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1973 = sum(count_1973))
# deixando apenas as igrejas ativas no Brasil
count_1973 <- filter(count_1973, uf !="EX")


## 1974
count_1974 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1974 = sum(count_1974))
# deixando apenas as igrejas ativas no Brasil
count_1974 <- filter(count_1974, uf !="EX")


## 1975
count_1975 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1975 = sum(count_1975))
# deixando apenas as igrejas ativas no Brasil
count_1975 <- filter(count_1975, uf !="EX")


## 1976
count_1976 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1976 = sum(count_1976))
# deixando apenas as igrejas ativas no Brasil
count_1976 <- filter(count_1976, uf !="EX")


## 1977
count_1977 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1977 = sum(count_1977))
# deixando apenas as igrejas ativas no Brasil
count_1977 <- filter(count_1977, uf !="EX")


## 1978
count_1978 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1978 = sum(count_1978))
# deixando apenas as igrejas ativas no Brasil
count_1978 <- filter(count_1978, uf !="EX")


## 1979
count_1979 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1979 = sum(count_1979))
# deixando apenas as igrejas ativas no Brasil
count_1979 <- filter(count_1979, uf !="EX")


## 1980
count_1980 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1980 = sum(count_1980))
# deixando apenas as igrejas ativas no Brasil
count_1980 <- filter(count_1980, uf !="EX")


## 1981
count_1981 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1981 = sum(count_1981))
# deixando apenas as igrejas ativas no Brasil
count_1981 <- filter(count_1981, uf !="EX")


## 1982
count_1982 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1982 = sum(count_1982))
# deixando apenas as igrejas ativas no Brasil
count_1982 <- filter(count_1982, uf !="EX")


## 1983
count_1983 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1983 = sum(count_1983))
# deixando apenas as igrejas ativas no Brasil
count_1983 <- filter(count_1983, uf !="EX")


## 1984
count_1984 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1984 = sum(count_1984))
# deixando apenas as igrejas ativas no Brasil
count_1984 <- filter(count_1984, uf !="EX")


## 1985
count_1985 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1985 = sum(count_1985))
# deixando apenas as igrejas ativas no Brasil
count_1985 <- filter(count_1985, uf !="EX")


## 1986
count_1986 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1986 = sum(count_1986))
# deixando apenas as igrejas ativas no Brasil
count_1986 <- filter(count_1986, uf !="EX")


## 1987
count_1987 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1987 = sum(count_1987))
# deixando apenas as igrejas ativas no Brasil
count_1987 <- filter(count_1987, uf !="EX")


## 1988
count_1988 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1988 = sum(count_1988))
# deixando apenas as igrejas ativas no Brasil
count_1988 <- filter(count_1988, uf !="EX")


## 1989
count_1989 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1989 = sum(count_1989))
# deixando apenas as igrejas ativas no Brasil
count_1989 <- filter(count_1989, uf !="EX")


## 1990
count_1990 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1990 = sum(count_1990))
# deixando apenas as igrejas ativas no Brasil
count_1990 <- filter(count_1990, uf !="EX")


## 1991
count_1991 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1991 = sum(count_1991))
# deixando apenas as igrejas ativas no Brasil
count_1991 <- filter(count_1991, uf !="EX")


## 1992
count_1992 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1992 = sum(count_1992))
# deixando apenas as igrejas ativas no Brasil
count_1992 <- filter(count_1992, uf !="EX")


## 1993
count_1993 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1993 = sum(count_1993))
# deixando apenas as igrejas ativas no Brasil
count_1993 <- filter(count_1993, uf !="EX")


## 1994
count_1994 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1994 = sum(count_1994))
# deixando apenas as igrejas ativas no Brasil
count_1994 <- filter(count_1994, uf !="EX")


## 1995
count_1995 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1995 = sum(count_1995))
# deixando apenas as igrejas ativas no Brasil
count_1995 <- filter(count_1995, uf !="EX")


## 1996
count_1996 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1996 = sum(count_1996))
# deixando apenas as igrejas ativas no Brasil
count_1996 <- filter(count_1996, uf !="EX")


## 1997
count_1997 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1997 = sum(count_1997))
# deixando apenas as igrejas ativas no Brasil
count_1997 <- filter(count_1997, uf !="EX")


## 1998
count_1998 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1998 = sum(count_1998))
# deixando apenas as igrejas ativas no Brasil
count_1998 <- filter(count_1998, uf !="EX")


## 1999
count_1999 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_1999 = sum(count_1999))
# deixando apenas as igrejas ativas no Brasil
count_1999 <- filter(count_1999, uf !="EX")


## 2000
count_2000 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2000 = sum(count_2000))
# deixando apenas as igrejas ativas no Brasil
count_2000 <- filter(count_2000, uf !="EX")


## 2001
count_2001 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2001 = sum(count_2001))
# deixando apenas as igrejas ativas no Brasil
count_2001 <- filter(count_2001, uf !="EX")


## 2002
count_2002 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2002 = sum(count_2002))
# deixando apenas as igrejas ativas no Brasil
count_2002 <- filter(count_2002, uf !="EX")


## 2003
count_2003 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2003 = sum(count_2003))
# deixando apenas as igrejas ativas no Brasil
count_2003 <- filter(count_2003, uf !="EX")



## 2004
count_2004 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2004 = sum(count_2004))
# deixando apenas as igrejas ativas no Brasil
count_2004 <- filter(count_2004, uf !="EX")



## 2005
count_2005 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2005 = sum(count_2005))
# deixando apenas as igrejas ativas no Brasil
count_2005 <- filter(count_2005, uf !="EX")



## 2006
count_2006 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2006 = sum(count_2006))
# deixando apenas as igrejas ativas no Brasil
count_2006 <- filter(count_2006, uf !="EX")



## 2007
count_2007 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2007 = sum(count_2007))
# deixando apenas as igrejas ativas no Brasil
count_2007 <- filter(count_2007, uf !="EX")



## 2008
count_2008 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2008 = sum(count_2008))
# deixando apenas as igrejas ativas no Brasil
count_2008 <- filter(count_2008, uf !="EX")



## 2009
count_2009 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2009 = sum(count_2009))
# deixando apenas as igrejas ativas no Brasil
count_2009 <- filter(count_2009, uf !="EX")



## 2010
count_2010 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2010 = sum(count_2010))
# deixando apenas as igrejas ativas no Brasil
count_2010 <- filter(count_2010, uf !="EX")


## 2011
count_2011 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2011 = sum(count_2011))
# deixando apenas as igrejas ativas no Brasil
count_2011 <- filter(count_2011, uf !="EX")


## 2012
count_2012 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2012 = sum(count_2012))
# deixando apenas as igrejas ativas no Brasil
count_2012 <- filter(count_2012, uf !="EX")


## 2013
count_2013 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2013 = sum(count_2013))
# deixando apenas as igrejas ativas no Brasil
count_2013 <- filter(count_2013, uf !="EX")

## 2014
count_2014 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2014 = sum(count_2014))
# deixando apenas as igrejas ativas no Brasil
count_2014 <- filter(count_2014, uf !="EX")


## 2015
count_2015 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2015 = sum(count_2015))
# deixando apenas as igrejas ativas no Brasil
count_2015 <- filter(count_2015, uf !="EX")


## 2016
count_2016 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2016 = sum(count_2016))
# deixando apenas as igrejas ativas no Brasil
count_2016 <- filter(count_2016, uf !="EX")


## 2017
count_2017 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2017 = sum(count_2017))
# deixando apenas as igrejas ativas no Brasil
count_2017 <- filter(count_2017, uf !="EX")


## 2018
count_2018 <- df_pentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2018 = sum(count_2018))
# deixando apenas as igrejas ativas no Brasil
count_2018 <- filter(count_2018, uf !="EX")


## 2019
count_2019 <- df_pentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_pent_2019 = sum(count_2019))
# deixando apenas as igrejas ativas no Brasil
count_2019 <- filter(count_2019, uf !="EX")


comb_pen <- count_1960 %>% 
  left_join(count_1961, by = c("uf")) %>%
  left_join(count_1962, by = c("uf")) %>%
  left_join(count_1963, by = c("uf")) %>%
  left_join(count_1964, by = c("uf")) %>%
  left_join(count_1965, by = c("uf")) %>%
  left_join(count_1966, by = c("uf")) %>%
  left_join(count_1967, by = c("uf")) %>%
  left_join(count_1968, by = c("uf")) %>%
  left_join(count_1969, by = c("uf")) %>%
  left_join(count_1970, by = c("uf")) %>%
  left_join(count_1971, by = c("uf")) %>%
  left_join(count_1972, by = c("uf")) %>%
  left_join(count_1973, by = c("uf")) %>%
  left_join(count_1974, by = c("uf")) %>%
  left_join(count_1975, by = c("uf")) %>%
  left_join(count_1976, by = c("uf")) %>%
  left_join(count_1977, by = c("uf")) %>%
  left_join(count_1978, by = c("uf")) %>%
  left_join(count_1979, by = c("uf")) %>%
  left_join(count_1980, by = c("uf")) %>%
  left_join(count_1981, by = c("uf")) %>%
  left_join(count_1982, by = c("uf")) %>%
  left_join(count_1983, by = c("uf")) %>%
  left_join(count_1984, by = c("uf")) %>%
  left_join(count_1985, by = c("uf")) %>%
  left_join(count_1986, by = c("uf")) %>%
  left_join(count_1987, by = c("uf")) %>%
  left_join(count_1988, by = c("uf")) %>%
  left_join(count_1989, by = c("uf")) %>%
  left_join(count_1990, by = c("uf")) %>%
  left_join(count_1991, by = c("uf")) %>%
  left_join(count_1992, by = c("uf")) %>%
  left_join(count_1993, by = c("uf")) %>%
  left_join(count_1994, by = c("uf")) %>%
  left_join(count_1995, by = c("uf")) %>%
  left_join(count_1996, by = c("uf")) %>%
  left_join(count_1997, by = c("uf")) %>%
  left_join(count_1998, by = c("uf")) %>%
  left_join(count_1999, by = c("uf")) %>%
  left_join(count_2000, by = c("uf")) %>%
  left_join(count_2001, by = c("uf")) %>%
  left_join(count_2002, by = c("uf")) %>%
  left_join(count_2003, by = c("uf")) %>%
  left_join(count_2004, by = c("uf")) %>%
  left_join(count_2005, by = c("uf")) %>%
  left_join(count_2006, by = c("uf")) %>%
  left_join(count_2007, by = c("uf")) %>%
  left_join(count_2008, by = c("uf")) %>%
  left_join(count_2009, by = c("uf")) %>%
  left_join(count_2010, by = c("uf")) %>%
  left_join(count_2011, by = c("uf")) %>%
  left_join(count_2012, by = c("uf")) %>%
  left_join(count_2013, by = c("uf")) %>%
  left_join(count_2014, by = c("uf")) %>%
  left_join(count_2015, by = c("uf")) %>%
  left_join(count_2016, by = c("uf")) %>%
  left_join(count_2017, by = c("uf")) %>%
  left_join(count_2018, by = c("uf")) %>%
  left_join(count_2019, by = c("uf"))





####################################
## Igrejas Evangélicas Neopentecostais
###################################

## Para realizar a contagem, criamos um dataframe apenas com as Igrejas
## Evangélicas Neopentecostais

df_neopentecostais <- filter(df_cnpjs, neopente == 1)

## Contando o número de igrejas ativas, em cada UF, em 1960

## 1960
count_1960 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1960 = sum(count_1960))
## deixando apenas as igrejas ativas no Brasil
count_1960 <- filter(count_1960, uf !="EX")


## 1961
count_1961 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1961 = sum(count_1961))
# deixando apenas as igrejas ativas no Brasil
count_1961 <- filter(count_1961, uf !="EX")


## 1962
count_1962 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1962 = sum(count_1962))
# deixando apenas as igrejas ativas no Brasil
count_1962 <- filter(count_1962, uf !="EX")


## 1963
count_1963 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1963 = sum(count_1963))
# deixando apenas as igrejas ativas no Brasil
count_1963 <- filter(count_1963, uf !="EX")


## 1964
count_1964 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1964 = sum(count_1964))
# deixando apenas as igrejas ativas no Brasil
count_1964 <- filter(count_1964, uf !="EX")


## 1965
count_1965 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1965 = sum(count_1965))
# deixando apenas as igrejas ativas no Brasil
count_1965 <- filter(count_1965, uf !="EX")


## 1966
count_1966 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1966 = sum(count_1966))
# deixando apenas as igrejas ativas no Brasil
count_1966 <- filter(count_1966, uf !="EX")


## 1967
count_1967 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1967 = sum(count_1967))
# deixando apenas as igrejas ativas no Brasil
count_1967 <- filter(count_1967, uf !="EX")


## 1968
count_1968 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1968 = sum(count_1968))
# deixando apenas as igrejas ativas no Brasil
count_1968 <- filter(count_1968, uf !="EX")


## 1969
count_1969 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1969 = sum(count_1969))
# deixando apenas as igrejas ativas no Brasil
count_1969 <- filter(count_1969, uf !="EX")


## 1970
count_1970 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1970 = sum(count_1970))
# deixando apenas as igrejas ativas no Brasil
count_1970 <- filter(count_1970, uf !="EX")


## 1971
count_1971 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1971 = sum(count_1971))
# deixando apenas as igrejas ativas no Brasil
count_1971 <- filter(count_1971, uf !="EX")

## 1972
count_1972 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1972 = sum(count_1972))
# deixando apenas as igrejas ativas no Brasil
count_1972 <- filter(count_1972, uf !="EX")

## 1973
count_1973 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1973 = sum(count_1973))
# deixando apenas as igrejas ativas no Brasil
count_1973 <- filter(count_1973, uf !="EX")


## 1974
count_1974 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1974 = sum(count_1974))
# deixando apenas as igrejas ativas no Brasil
count_1974 <- filter(count_1974, uf !="EX")


## 1975
count_1975 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1975 = sum(count_1975))
# deixando apenas as igrejas ativas no Brasil
count_1975 <- filter(count_1975, uf !="EX")


## 1976
count_1976 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1976 = sum(count_1976))
# deixando apenas as igrejas ativas no Brasil
count_1976 <- filter(count_1976, uf !="EX")


## 1977
count_1977 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1977 = sum(count_1977))
# deixando apenas as igrejas ativas no Brasil
count_1977 <- filter(count_1977, uf !="EX")


## 1978
count_1978 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1978 = sum(count_1978))
# deixando apenas as igrejas ativas no Brasil
count_1978 <- filter(count_1978, uf !="EX")


## 1979
count_1979 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1979 = sum(count_1979))
# deixando apenas as igrejas ativas no Brasil
count_1979 <- filter(count_1979, uf !="EX")


## 1980
count_1980 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1980 = sum(count_1980))
# deixando apenas as igrejas ativas no Brasil
count_1980 <- filter(count_1980, uf !="EX")


## 1981
count_1981 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1981 = sum(count_1981))
# deixando apenas as igrejas ativas no Brasil
count_1981 <- filter(count_1981, uf !="EX")


## 1982
count_1982 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1982 = sum(count_1982))
# deixando apenas as igrejas ativas no Brasil
count_1982 <- filter(count_1982, uf !="EX")


## 1983
count_1983 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1983 = sum(count_1983))
# deixando apenas as igrejas ativas no Brasil
count_1983 <- filter(count_1983, uf !="EX")


## 1984
count_1984 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1984 = sum(count_1984))
# deixando apenas as igrejas ativas no Brasil
count_1984 <- filter(count_1984, uf !="EX")


## 1985
count_1985 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1985 = sum(count_1985))
# deixando apenas as igrejas ativas no Brasil
count_1985 <- filter(count_1985, uf !="EX")


## 1986
count_1986 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1986 = sum(count_1986))
# deixando apenas as igrejas ativas no Brasil
count_1986 <- filter(count_1986, uf !="EX")


## 1987
count_1987 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1987 = sum(count_1987))
# deixando apenas as igrejas ativas no Brasil
count_1987 <- filter(count_1987, uf !="EX")


## 1988
count_1988 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1988 = sum(count_1988))
# deixando apenas as igrejas ativas no Brasil
count_1988 <- filter(count_1988, uf !="EX")


## 1989
count_1989 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1989 = sum(count_1989))
# deixando apenas as igrejas ativas no Brasil
count_1989 <- filter(count_1989, uf !="EX")


## 1990
count_1990 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1990 = sum(count_1990))
# deixando apenas as igrejas ativas no Brasil
count_1990 <- filter(count_1990, uf !="EX")


## 1991
count_1991 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1991 = sum(count_1991))
# deixando apenas as igrejas ativas no Brasil
count_1991 <- filter(count_1991, uf !="EX")


## 1992
count_1992 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1992 = sum(count_1992))
# deixando apenas as igrejas ativas no Brasil
count_1992 <- filter(count_1992, uf !="EX")


## 1993
count_1993 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1993 = sum(count_1993))
# deixando apenas as igrejas ativas no Brasil
count_1993 <- filter(count_1993, uf !="EX")


## 1994
count_1994 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1994 = sum(count_1994))
# deixando apenas as igrejas ativas no Brasil
count_1994 <- filter(count_1994, uf !="EX")


## 1995
count_1995 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1995 = sum(count_1995))
# deixando apenas as igrejas ativas no Brasil
count_1995 <- filter(count_1995, uf !="EX")


## 1996
count_1996 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1996 = sum(count_1996))
# deixando apenas as igrejas ativas no Brasil
count_1996 <- filter(count_1996, uf !="EX")


## 1997
count_1997 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1997 = sum(count_1997))
# deixando apenas as igrejas ativas no Brasil
count_1997 <- filter(count_1997, uf !="EX")


## 1998
count_1998 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1998 = sum(count_1998))
# deixando apenas as igrejas ativas no Brasil
count_1998 <- filter(count_1998, uf !="EX")


## 1999
count_1999 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_1999 = sum(count_1999))
# deixando apenas as igrejas ativas no Brasil
count_1999 <- filter(count_1999, uf !="EX")


## 2000
count_2000 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2000 = sum(count_2000))
# deixando apenas as igrejas ativas no Brasil
count_2000 <- filter(count_2000, uf !="EX")


## 2001
count_2001 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2001 = sum(count_2001))
# deixando apenas as igrejas ativas no Brasil
count_2001 <- filter(count_2001, uf !="EX")


## 2002
count_2002 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2002 = sum(count_2002))
# deixando apenas as igrejas ativas no Brasil
count_2002 <- filter(count_2002, uf !="EX")


## 2003
count_2003 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2003 = sum(count_2003))
# deixando apenas as igrejas ativas no Brasil
count_2003 <- filter(count_2003, uf !="EX")



## 2004
count_2004 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2004 = sum(count_2004))
# deixando apenas as igrejas ativas no Brasil
count_2004 <- filter(count_2004, uf !="EX")



## 2005
count_2005 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2005 = sum(count_2005))
# deixando apenas as igrejas ativas no Brasil
count_2005 <- filter(count_2005, uf !="EX")



## 2006
count_2006 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2006 = sum(count_2006))
# deixando apenas as igrejas ativas no Brasil
count_2006 <- filter(count_2006, uf !="EX")



## 2007
count_2007 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2007 = sum(count_2007))
# deixando apenas as igrejas ativas no Brasil
count_2007 <- filter(count_2007, uf !="EX")



## 2008
count_2008 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2008 = sum(count_2008))
# deixando apenas as igrejas ativas no Brasil
count_2008 <- filter(count_2008, uf !="EX")



## 2009
count_2009 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2009 = sum(count_2009))
# deixando apenas as igrejas ativas no Brasil
count_2009 <- filter(count_2009, uf !="EX")



## 2010
count_2010 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2010 = sum(count_2010))
# deixando apenas as igrejas ativas no Brasil
count_2010 <- filter(count_2010, uf !="EX")


## 2011
count_2011 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2011 = sum(count_2011))
# deixando apenas as igrejas ativas no Brasil
count_2011 <- filter(count_2011, uf !="EX")


## 2012
count_2012 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2012 = sum(count_2012))
# deixando apenas as igrejas ativas no Brasil
count_2012 <- filter(count_2012, uf !="EX")


## 2013
count_2013 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2013 = sum(count_2013))
# deixando apenas as igrejas ativas no Brasil
count_2013 <- filter(count_2013, uf !="EX")

## 2014
count_2014 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2014 = sum(count_2014))
# deixando apenas as igrejas ativas no Brasil
count_2014 <- filter(count_2014, uf !="EX")


## 2015
count_2015 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2015 = sum(count_2015))
# deixando apenas as igrejas ativas no Brasil
count_2015 <- filter(count_2015, uf !="EX")


## 2016
count_2016 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2016 = sum(count_2016))
# deixando apenas as igrejas ativas no Brasil
count_2016 <- filter(count_2016, uf !="EX")


## 2017
count_2017 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2017 = sum(count_2017))
# deixando apenas as igrejas ativas no Brasil
count_2017 <- filter(count_2017, uf !="EX")


## 2018
count_2018 <- df_neopentecostais  %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2018 = sum(count_2018))
# deixando apenas as igrejas ativas no Brasil
count_2018 <- filter(count_2018, uf !="EX")


## 2019
count_2019 <- df_neopentecostais %>%
  group_by (uf) %>% 
  summarise(ativas_neopent_2019 = sum(count_2019))
# deixando apenas as igrejas ativas no Brasil
count_2019 <- filter(count_2019, uf !="EX")



comb_neo <- count_1960 %>% 
  left_join(count_1961, by = c("uf")) %>%
  left_join(count_1962, by = c("uf")) %>%
  left_join(count_1963, by = c("uf")) %>%
  left_join(count_1964, by = c("uf")) %>%
  left_join(count_1965, by = c("uf")) %>%
  left_join(count_1966, by = c("uf")) %>%
  left_join(count_1967, by = c("uf")) %>%
  left_join(count_1968, by = c("uf")) %>%
  left_join(count_1969, by = c("uf")) %>%
  left_join(count_1970, by = c("uf")) %>%
  left_join(count_1971, by = c("uf")) %>%
  left_join(count_1972, by = c("uf")) %>%
  left_join(count_1973, by = c("uf")) %>%
  left_join(count_1974, by = c("uf")) %>%
  left_join(count_1975, by = c("uf")) %>%
  left_join(count_1976, by = c("uf")) %>%
  left_join(count_1977, by = c("uf")) %>%
  left_join(count_1978, by = c("uf")) %>%
  left_join(count_1979, by = c("uf")) %>%
  left_join(count_1980, by = c("uf")) %>%
  left_join(count_1981, by = c("uf")) %>%
  left_join(count_1982, by = c("uf")) %>%
  left_join(count_1983, by = c("uf")) %>%
  left_join(count_1984, by = c("uf")) %>%
  left_join(count_1985, by = c("uf")) %>%
  left_join(count_1986, by = c("uf")) %>%
  left_join(count_1987, by = c("uf")) %>%
  left_join(count_1988, by = c("uf")) %>%
  left_join(count_1989, by = c("uf")) %>%
  left_join(count_1990, by = c("uf")) %>%
  left_join(count_1991, by = c("uf")) %>%
  left_join(count_1992, by = c("uf")) %>%
  left_join(count_1993, by = c("uf")) %>%
  left_join(count_1994, by = c("uf")) %>%
  left_join(count_1995, by = c("uf")) %>%
  left_join(count_1996, by = c("uf")) %>%
  left_join(count_1997, by = c("uf")) %>%
  left_join(count_1998, by = c("uf")) %>%
  left_join(count_1999, by = c("uf")) %>%
  left_join(count_2000, by = c("uf")) %>%
  left_join(count_2001, by = c("uf")) %>%
  left_join(count_2002, by = c("uf")) %>%
  left_join(count_2003, by = c("uf")) %>%
  left_join(count_2004, by = c("uf")) %>%
  left_join(count_2005, by = c("uf")) %>%
  left_join(count_2006, by = c("uf")) %>%
  left_join(count_2007, by = c("uf")) %>%
  left_join(count_2008, by = c("uf")) %>%
  left_join(count_2009, by = c("uf")) %>%
  left_join(count_2010, by = c("uf")) %>%
  left_join(count_2011, by = c("uf")) %>%
  left_join(count_2012, by = c("uf")) %>%
  left_join(count_2013, by = c("uf")) %>%
  left_join(count_2014, by = c("uf")) %>%
  left_join(count_2015, by = c("uf")) %>%
  left_join(count_2016, by = c("uf")) %>%
  left_join(count_2017, by = c("uf")) %>%
  left_join(count_2018, by = c("uf")) %>%
  left_join(count_2019, by = c("uf"))



############################################################
## Igrejas Evangélicas de classificação não determinada
############################################################

## Para realizar a contagem, criamos um dataframe apenas com as Igrejas
## de classificação não determinada

df_naodeterm <- filter(df_cnpjs, naodeterm == 1)

## Contando o número de igrejas ativas, em cada UF, em 1960

## 1960
count_1960 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1960 = sum(count_1960))
## deixando apenas as igrejas ativas no Brasil
count_1960 <- filter(count_1960, uf !="EX")


## 1961
count_1961 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1961 = sum(count_1961))
# deixando apenas as igrejas ativas no Brasil
count_1961 <- filter(count_1961, uf !="EX")


## 1962
count_1962 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1962 = sum(count_1962))
# deixando apenas as igrejas ativas no Brasil
count_1962 <- filter(count_1962, uf !="EX")


## 1963
count_1963 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1963 = sum(count_1963))
# deixando apenas as igrejas ativas no Brasil
count_1963 <- filter(count_1963, uf !="EX")


## 1964
count_1964 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1964 = sum(count_1964))
# deixando apenas as igrejas ativas no Brasil
count_1964 <- filter(count_1964, uf !="EX")


## 1965
count_1965 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1965 = sum(count_1965))
# deixando apenas as igrejas ativas no Brasil
count_1965 <- filter(count_1965, uf !="EX")


## 1966
count_1966 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1966 = sum(count_1966))
# deixando apenas as igrejas ativas no Brasil
count_1966 <- filter(count_1966, uf !="EX")


## 1967
count_1967 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1967 = sum(count_1967))
# deixando apenas as igrejas ativas no Brasil
count_1967 <- filter(count_1967, uf !="EX")


## 1968
count_1968 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1968 = sum(count_1968))
# deixando apenas as igrejas ativas no Brasil
count_1968 <- filter(count_1968, uf !="EX")


## 1969
count_1969 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1969 = sum(count_1969))
# deixando apenas as igrejas ativas no Brasil
count_1969 <- filter(count_1969, uf !="EX")


## 1970
count_1970 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1970 = sum(count_1970))
# deixando apenas as igrejas ativas no Brasil
count_1970 <- filter(count_1970, uf !="EX")


## 1971
count_1971 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1971 = sum(count_1971))
# deixando apenas as igrejas ativas no Brasil
count_1971 <- filter(count_1971, uf !="EX")

## 1972
count_1972 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1972 = sum(count_1972))
# deixando apenas as igrejas ativas no Brasil
count_1972 <- filter(count_1972, uf !="EX")

## 1973
count_1973 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1973 = sum(count_1973))
# deixando apenas as igrejas ativas no Brasil
count_1973 <- filter(count_1973, uf !="EX")


## 1974
count_1974 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1974 = sum(count_1974))
# deixando apenas as igrejas ativas no Brasil
count_1974 <- filter(count_1974, uf !="EX")


## 1975
count_1975 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1975 = sum(count_1975))
# deixando apenas as igrejas ativas no Brasil
count_1975 <- filter(count_1975, uf !="EX")


## 1976
count_1976 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1976 = sum(count_1976))
# deixando apenas as igrejas ativas no Brasil
count_1976 <- filter(count_1976, uf !="EX")


## 1977
count_1977 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1977 = sum(count_1977))
# deixando apenas as igrejas ativas no Brasil
count_1977 <- filter(count_1977, uf !="EX")


## 1978
count_1978 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1978 = sum(count_1978))
# deixando apenas as igrejas ativas no Brasil
count_1978 <- filter(count_1978, uf !="EX")


## 1979
count_1979 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1979 = sum(count_1979))
# deixando apenas as igrejas ativas no Brasil
count_1979 <- filter(count_1979, uf !="EX")


## 1980
count_1980 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1980 = sum(count_1980))
# deixando apenas as igrejas ativas no Brasil
count_1980 <- filter(count_1980, uf !="EX")


## 1981
count_1981 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1981 = sum(count_1981))
# deixando apenas as igrejas ativas no Brasil
count_1981 <- filter(count_1981, uf !="EX")


## 1982
count_1982 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1982 = sum(count_1982))
# deixando apenas as igrejas ativas no Brasil
count_1982 <- filter(count_1982, uf !="EX")


#### 1983
#count_1983 < df_naodeterm %>%
 #group_by (uf) %>% 
#summarise(ativas_naodet_1983 = sum(count_1983))
#deixando apenas as igrejas ativas no Brasil
#count_1983 <- filter(count_1983, uf !="EX")


## 1984
count_1984 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1984 = sum(count_1984))
# deixando apenas as igrejas ativas no Brasil
count_1984 <- filter(count_1984, uf !="EX")


## 1985
count_1985 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1985 = sum(count_1985))
# deixando apenas as igrejas ativas no Brasil
count_1985 <- filter(count_1985, uf !="EX")


## 1986
count_1986 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1986 = sum(count_1986))
# deixando apenas as igrejas ativas no Brasil
count_1986 <- filter(count_1986, uf !="EX")


## 1987
count_1987 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1987 = sum(count_1987))
# deixando apenas as igrejas ativas no Brasil
count_1987 <- filter(count_1987, uf !="EX")


## 1988
count_1988 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1988 = sum(count_1988))
# deixando apenas as igrejas ativas no Brasil
count_1988 <- filter(count_1988, uf !="EX")


## 1989
count_1989 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1989 = sum(count_1989))
# deixando apenas as igrejas ativas no Brasil
count_1989 <- filter(count_1989, uf !="EX")


## 1990
count_1990 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1990 = sum(count_1990))
# deixando apenas as igrejas ativas no Brasil
count_1990 <- filter(count_1990, uf !="EX")


## 1991
count_1991 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1991 = sum(count_1991))
# deixando apenas as igrejas ativas no Brasil
count_1991 <- filter(count_1991, uf !="EX")


## 1992
count_1992 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1992 = sum(count_1992))
# deixando apenas as igrejas ativas no Brasil
count_1992 <- filter(count_1992, uf !="EX")


## 1993
count_1993 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1993 = sum(count_1993))
# deixando apenas as igrejas ativas no Brasil
count_1993 <- filter(count_1993, uf !="EX")


## 1994
count_1994 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1994 = sum(count_1994))
# deixando apenas as igrejas ativas no Brasil
count_1994 <- filter(count_1994, uf !="EX")


## 1995
count_1995 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1995 = sum(count_1995))
# deixando apenas as igrejas ativas no Brasil
count_1995 <- filter(count_1995, uf !="EX")


## 1996
count_1996 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1996 = sum(count_1996))
# deixando apenas as igrejas ativas no Brasil
count_1996 <- filter(count_1996, uf !="EX")


## 1997
count_1997 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1997 = sum(count_1997))
# deixando apenas as igrejas ativas no Brasil
count_1997 <- filter(count_1997, uf !="EX")


## 1998
count_1998 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1998 = sum(count_1998))
# deixando apenas as igrejas ativas no Brasil
count_1998 <- filter(count_1998, uf !="EX")


## 1999
count_1999 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_1999 = sum(count_1999))
# deixando apenas as igrejas ativas no Brasil
count_1999 <- filter(count_1999, uf !="EX")


## 2000
count_2000 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2000 = sum(count_2000))
# deixando apenas as igrejas ativas no Brasil
count_2000 <- filter(count_2000, uf !="EX")


## 2001
count_2001 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2001 = sum(count_2001))
# deixando apenas as igrejas ativas no Brasil
count_2001 <- filter(count_2001, uf !="EX")


## 2002
count_2002 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2002 = sum(count_2002))
# deixando apenas as igrejas ativas no Brasil
count_2002 <- filter(count_2002, uf !="EX")


## 2003
count_2003 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2003 = sum(count_2003))
# deixando apenas as igrejas ativas no Brasil
count_2003 <- filter(count_2003, uf !="EX")



## 2004
count_2004 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2004 = sum(count_2004))
# deixando apenas as igrejas ativas no Brasil
count_2004 <- filter(count_2004, uf !="EX")



## 2005
count_2005 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2005 = sum(count_2005))
# deixando apenas as igrejas ativas no Brasil
count_2005 <- filter(count_2005, uf !="EX")



## 2006
count_2006 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2006 = sum(count_2006))
# deixando apenas as igrejas ativas no Brasil
count_2006 <- filter(count_2006, uf !="EX")



## 2007
count_2007 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2007 = sum(count_2007))
# deixando apenas as igrejas ativas no Brasil
count_2007 <- filter(count_2007, uf !="EX")



## 2008
count_2008 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2008 = sum(count_2008))
# deixando apenas as igrejas ativas no Brasil
count_2008 <- filter(count_2008, uf !="EX")



## 2009
count_2009 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2009 = sum(count_2009))
# deixando apenas as igrejas ativas no Brasil
count_2009 <- filter(count_2009, uf !="EX")



## 2010
count_2010 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2010 = sum(count_2010))
# deixando apenas as igrejas ativas no Brasil
count_2010 <- filter(count_2010, uf !="EX")


## 2011
count_2011 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2011 = sum(count_2011))
# deixando apenas as igrejas ativas no Brasil
count_2011 <- filter(count_2011, uf !="EX")


## 2012
count_2012 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2012 = sum(count_2012))
# deixando apenas as igrejas ativas no Brasil
count_2012 <- filter(count_2012, uf !="EX")


## 2013
count_2013 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2013 = sum(count_2013))
# deixando apenas as igrejas ativas no Brasil
count_2013 <- filter(count_2013, uf !="EX")

## 2014
count_2014 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2014 = sum(count_2014))
# deixando apenas as igrejas ativas no Brasil
count_2014 <- filter(count_2014, uf !="EX")


## 2015
count_2015 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2015 = sum(count_2015))
# deixando apenas as igrejas ativas no Brasil
count_2015 <- filter(count_2015, uf !="EX")


## 2016
count_2016 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2016 = sum(count_2016))
# deixando apenas as igrejas ativas no Brasil
count_2016 <- filter(count_2016, uf !="EX")


## 2017
count_2017 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2017 = sum(count_2017))
# deixando apenas as igrejas ativas no Brasil
count_2017 <- filter(count_2017, uf !="EX")


## 2018
count_2018 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2018 = sum(count_2018))
# deixando apenas as igrejas ativas no Brasil
count_2018 <- filter(count_2018, uf !="EX")


## 2019
count_2019 <- df_naodeterm %>%
  group_by (uf) %>% 
  summarise(ativas_naodet_2019 = sum(count_2019))
# deixando apenas as igrejas ativas no Brasil
count_2019 <- filter(count_2019, uf !="EX")



comb_nde <- count_1960 %>% 
  left_join(count_1961, by = c("uf")) %>%
  left_join(count_1962, by = c("uf")) %>%
  left_join(count_1963, by = c("uf")) %>%
  left_join(count_1964, by = c("uf")) %>%
  left_join(count_1965, by = c("uf")) %>%
  left_join(count_1966, by = c("uf")) %>%
  left_join(count_1967, by = c("uf")) %>%
  left_join(count_1968, by = c("uf")) %>%
  left_join(count_1969, by = c("uf")) %>%
  left_join(count_1970, by = c("uf")) %>%
  left_join(count_1971, by = c("uf")) %>%
  left_join(count_1972, by = c("uf")) %>%
  left_join(count_1973, by = c("uf")) %>%
  left_join(count_1974, by = c("uf")) %>%
  left_join(count_1975, by = c("uf")) %>%
  left_join(count_1976, by = c("uf")) %>%
  left_join(count_1977, by = c("uf")) %>%
  left_join(count_1978, by = c("uf")) %>%
  left_join(count_1979, by = c("uf")) %>%
  left_join(count_1980, by = c("uf")) %>%
  left_join(count_1981, by = c("uf")) %>%
  left_join(count_1982, by = c("uf")) %>%
  #left_join(count_1983, by = c("uf")) %>%
  left_join(count_1984, by = c("uf")) %>%
  left_join(count_1985, by = c("uf")) %>%
  left_join(count_1986, by = c("uf")) %>%
  left_join(count_1987, by = c("uf")) %>%
  left_join(count_1988, by = c("uf")) %>%
  left_join(count_1989, by = c("uf")) %>%
  left_join(count_1990, by = c("uf")) %>%
  left_join(count_1991, by = c("uf")) %>%
  left_join(count_1992, by = c("uf")) %>%
  left_join(count_1993, by = c("uf")) %>%
  left_join(count_1994, by = c("uf")) %>%
  left_join(count_1995, by = c("uf")) %>%
  left_join(count_1996, by = c("uf")) %>%
  left_join(count_1997, by = c("uf")) %>%
  left_join(count_1998, by = c("uf")) %>%
  left_join(count_1999, by = c("uf")) %>%
  left_join(count_2000, by = c("uf")) %>%
  left_join(count_2001, by = c("uf")) %>%
  left_join(count_2002, by = c("uf")) %>%
  left_join(count_2003, by = c("uf")) %>%
  left_join(count_2004, by = c("uf")) %>%
  left_join(count_2005, by = c("uf")) %>%
  left_join(count_2006, by = c("uf")) %>%
  left_join(count_2007, by = c("uf")) %>%
  left_join(count_2008, by = c("uf")) %>%
  left_join(count_2009, by = c("uf")) %>%
  left_join(count_2010, by = c("uf")) %>%
  left_join(count_2011, by = c("uf")) %>%
  left_join(count_2012, by = c("uf")) %>%
  left_join(count_2013, by = c("uf")) %>%
  left_join(count_2014, by = c("uf")) %>%
  left_join(count_2015, by = c("uf")) %>%
  left_join(count_2016, by = c("uf")) %>%
  left_join(count_2017, by = c("uf")) %>%
  left_join(count_2018, by = c("uf")) %>%
  left_join(count_2019, by = c("uf"))



## Juntando todos os grupos de igrejas (comb) em um único dataframe

comb_all <- comb_ger %>% 
  left_join(comb_mis, by = c("uf")) %>%
  left_join(comb_pen, by = c("uf")) %>%
  left_join(comb_neo, by = c("uf")) %>%
  left_join(comb_nde, by = c("uf"))


### Calculando a taxa (número) de igrejas evangélicas por 100 mil habitantes

## A taxa por 100 mil habitantes é dada pela seguinte operação:

## (Número de Igrejas na UF/População na UF)*100.000

## População na UF = estimativas populacionais do IBGE 
## Para os anos compreendidos entre 1960 e 1970, foi utilizada a estimativa do ano de 1960
## Para os anos compreendidos entre 1970 e 1980, foi utilizada a estimativa do ano de 1970
## Para os anos compreendidos entre 1980 e 1990, foi utilizada a estimativa do ano de 1980
## Para os demais anos, foram utilizadas as estimativas correspondentes a cada ano
#  As exceções foram os anos de 1996, 2007 e 2010, para os quais não existem estimativas
#  Nestes casos, foram utilizados os anos imediatamente anteriores



## Chamando para a área de programação o arquivo CSV com as estimativas populacionais do IBGE

## Estimativas populacionais por ano e UF
df_pop <- read.csv("estimativas_pop.csv", sep =",", header = TRUE)
df_pop  <- rename(df_pop, uf = Sigla)

## População evangélicas em 2010 (IBGE, 2010)
df_pop_evang <- read.csv("evangelicos_censo2010.csv", sep =",", header = TRUE)
df_pop_evang  <- rename(df_pop_evang, uf = Sigla)

## A função left_join serve para juntar os três bancos de dados
 #comb_all = o dataframe que contem a contagem de igrejas para todas as denominações
 #df_pop =  o dataframe que contem as estimativas populacionais do IBGE
 #df_pop_evang = o dataframe que contem as estimativas da população evangélicas
     # de acordo com o Censo IBGE de 2010

comb <- comb_all %>% 
  left_join(df_pop, by = c("uf")) %>% 
  left_join(df_pop_evang, by = c("uf"))


## Renomeando as variáveis referentes às estimativas populacionais dos anos utilizados

comb  <- rename(comb, pop1960 =  X1960)
comb  <- rename(comb, pop1970 =  X1970)
comb  <- rename(comb, pop1980 = X1980)
comb  <- rename(comb, pop1991 = X1991)
comb  <- rename(comb, pop1992 = X1992)
comb  <- rename(comb, pop1993 = X1993)
comb  <- rename(comb, pop1994 = X1994)
comb  <- rename(comb, pop1995 = X1995)
comb  <- rename(comb, pop1997 = X1997)
comb  <- rename(comb, pop1998 = X1998)
comb  <- rename(comb, pop1999 = X1999)
comb  <- rename(comb, pop2000 = X2000)
comb  <- rename(comb, pop2001 = X2001)
comb  <- rename(comb, pop2002 = X2002)
comb  <- rename(comb, pop2003 = X2003)
comb  <- rename(comb, pop2004 = X2004)
comb  <- rename(comb, pop2005 = X2005)
comb  <- rename(comb, pop2006 = X2006)
comb  <- rename(comb, pop2008 = X2008)
comb  <- rename(comb, pop2009 = X2009)
comb  <- rename(comb, pop2011 = X2011)
comb  <- rename(comb, pop2012 = X2012)
comb  <- rename(comb, pop2013 = X2013)
comb  <- rename(comb, pop2014 = X2014)
comb  <- rename(comb, pop2015 = X2015)
comb  <- rename(comb, pop2016 = X2016)
comb  <- rename(comb, pop2017 = X2017)
comb  <- rename(comb, pop2018 = X2018)
comb  <- rename(comb, pop2019 = X2019)


## Criando as variáveis que se referem a proporção de cada um grupo
  # evangélico em 2010 (de acordo com o Censo IBGE, 2010)

comb$evang_10 <- NA
comb$evang_10 <- (comb$evangelica10/comb$pop2009)*100

comb$mis_10 <- NA
comb$mis_10 <- (comb$missionaria10/comb$pop2009)*100

comb$pen_10 <- NA
comb$pen_10 <- (comb$pentecostal10/comb$pop2009)*100

comb$nde_10 <- NA
comb$nde_10 <- (comb$nao_determinada10/comb$pop2009)*100


### Calculando o número (taxa) de igrejas por 100mil habitantes para cada grupo

###########
## 1960
###########

## Todas as igrejas evangélicas
comb$all_100_60 <- NA
comb$all_100_60 <- (comb$ativas_all_1960/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_60 <- NA
comb$mis_100_60 <- (comb$ativas_miss_1960/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_60 <- NA
comb$pen_100_60 <- (comb$ativas_pent_1960/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_60 <- NA
comb$neo_100_60 <- (comb$ativas_neopent_1960/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_60 <- NA
comb$nde_100_60 <- (comb$ativas_naodet_1960/comb$pop1960)*100000



###########
## 1961
###########

## Todas as igrejas evangélicas
comb$all_100_61 <- NA
comb$all_100_61 <- (comb$ativas_all_1961/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_61 <- NA
comb$mis_100_61 <- (comb$ativas_miss_1961/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_61 <- NA
comb$pen_100_61 <- (comb$ativas_pent_1961/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_61 <- NA
comb$neo_100_61 <- (comb$ativas_neopent_1961/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_61 <- NA
comb$nde_100_61 <- (comb$ativas_naodet_1961/comb$pop1960)*100000



###########
## 1962
###########

## Todas as igrejas evangélicas
comb$all_100_62 <- NA
comb$all_100_62 <- (comb$ativas_all_1962/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_62 <- NA
comb$mis_100_62 <- (comb$ativas_miss_1962/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_62 <- NA
comb$pen_100_62 <- (comb$ativas_pent_1962/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_62 <- NA
comb$neo_100_62 <- (comb$ativas_neopent_1962/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_62 <- NA
comb$nde_100_62 <- (comb$ativas_naodet_1962/comb$pop1960)*100000




###########
## 1963
###########

## Todas as igrejas evangélicas
comb$all_100_63 <- NA
comb$all_100_63 <- (comb$ativas_all_1963/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_63 <- NA
comb$mis_100_63 <- (comb$ativas_miss_1963/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_63 <- NA
comb$pen_100_63 <- (comb$ativas_pent_1963/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_63 <- NA
comb$neo_100_63 <- (comb$ativas_neopent_1963/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_63 <- NA
comb$nde_100_63 <- (comb$ativas_naodet_1963/comb$pop1960)*100000



###########
## 1964
###########

## Todas as igrejas evangélicas
comb$all_100_64 <- NA
comb$all_100_64 <- (comb$ativas_all_1964/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_64 <- NA
comb$mis_100_64 <- (comb$ativas_miss_1964/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_64 <- NA
comb$pen_100_64 <- (comb$ativas_pent_1964/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_64 <- NA
comb$neo_100_64 <- (comb$ativas_neopent_1964/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_64 <- NA
comb$nde_100_64 <- (comb$ativas_naodet_1964/comb$pop1960)*100000




###########
## 1965
###########

## Todas as igrejas evangélicas
comb$all_100_65 <- NA
comb$all_100_65 <- (comb$ativas_all_1965/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_65 <- NA
comb$mis_100_65 <- (comb$ativas_miss_1965/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_65 <- NA
comb$pen_100_65 <- (comb$ativas_pent_1965/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_65 <- NA
comb$neo_100_65 <- (comb$ativas_neopent_1965/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_65 <- NA
comb$nde_100_65 <- (comb$ativas_naodet_1965/comb$pop1960)*100000




###########
## 1966
###########

## Todas as igrejas evangélicas
comb$all_100_66 <- NA
comb$all_100_66 <- (comb$ativas_all_1966/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_66 <- NA
comb$mis_100_66 <- (comb$ativas_miss_1966/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_66 <- NA
comb$pen_100_66 <- (comb$ativas_pent_1966/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_66 <- NA
comb$neo_100_66 <- (comb$ativas_neopent_1966/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_66 <- NA
comb$nde_100_66 <- (comb$ativas_naodet_1966/comb$pop1960)*100000




###########
## 1967
###########

## Todas as igrejas evangélicas
comb$all_100_67 <- NA
comb$all_100_67 <- (comb$ativas_all_1967/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_67 <- NA
comb$mis_100_67 <- (comb$ativas_miss_1967/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_67 <- NA
comb$pen_100_67 <- (comb$ativas_pent_1967/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_67 <- NA
comb$neo_100_67 <- (comb$ativas_neopent_1967/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_67 <- NA
comb$nde_100_67 <- (comb$ativas_naodet_1967/comb$pop1960)*100000



###########
## 1968
###########

## Todas as igrejas evangélicas
comb$all_100_68 <- NA
comb$all_100_68 <- (comb$ativas_all_1968/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_68 <- NA
comb$mis_100_68 <- (comb$ativas_miss_1968/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_68 <- NA
comb$pen_100_68 <- (comb$ativas_pent_1968/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_68 <- NA
comb$neo_100_68 <- (comb$ativas_neopent_1968/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_68 <- NA
comb$nde_100_68 <- (comb$ativas_naodet_1968/comb$pop1960)*100000



###########
## 1969
###########

## Todas as igrejas evangélicas
comb$all_100_69 <- NA
comb$all_100_69 <- (comb$ativas_all_1969/comb$pop1960)*100000

## Igrejas evangélicas missionárias
comb$mis_100_69 <- NA
comb$mis_100_69 <- (comb$ativas_miss_1969/comb$pop1960)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_69 <- NA
comb$pen_100_69 <- (comb$ativas_pent_1969/comb$pop1960)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_69 <- NA
comb$neo_100_69 <- (comb$ativas_neopent_1969/comb$pop1960)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_69 <- NA
comb$nde_100_69 <- (comb$ativas_naodet_1969/comb$pop1960)*100000




###########
## 1970
###########

## Todas as igrejas evangélicas
comb$all_100_70 <- NA
comb$all_100_70 <- (comb$ativas_all_1970/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_70 <- NA
comb$mis_100_70 <- (comb$ativas_miss_1970/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_70 <- NA
comb$pen_100_70 <- (comb$ativas_pent_1970/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_70 <- NA
comb$neo_100_70 <- (comb$ativas_neopent_1970/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_70 <- NA
comb$nde_100_70 <- (comb$ativas_naodet_1970/comb$pop1970)*100000




###########
## 1971
###########

## Todas as igrejas evangélicas
comb$all_100_71 <- NA
comb$all_100_71 <- (comb$ativas_all_1971/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_71 <- NA
comb$mis_100_71 <- (comb$ativas_miss_1971/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_71 <- NA
comb$pen_100_71 <- (comb$ativas_pent_1971/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_71 <- NA
comb$neo_100_71 <- (comb$ativas_neopent_1971/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_71 <- NA
comb$nde_100_71 <- (comb$ativas_naodet_1971/comb$pop1970)*100000




###########
## 1972
###########

## Todas as igrejas evangélicas
comb$all_100_72 <- NA
comb$all_100_72 <- (comb$ativas_all_1972/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_72 <- NA
comb$mis_100_72 <- (comb$ativas_miss_1972/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_72 <- NA
comb$pen_100_72 <- (comb$ativas_pent_1972/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_72 <- NA
comb$neo_100_72 <- (comb$ativas_neopent_1972/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_72 <- NA
comb$nde_100_72 <- (comb$ativas_naodet_1972/comb$pop1970)*100000



###########
## 1973
###########

## Todas as igrejas evangélicas
comb$all_100_73 <- NA
comb$all_100_73 <- (comb$ativas_all_1973/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_73 <- NA
comb$mis_100_73 <- (comb$ativas_miss_1973/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_73 <- NA
comb$pen_100_73 <- (comb$ativas_pent_1973/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_73 <- NA
comb$neo_100_73 <- (comb$ativas_neopent_1973/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_73 <- NA
comb$nde_100_73 <- (comb$ativas_naodet_1973/comb$pop1970)*100000





###########
## 1974
###########

## Todas as igrejas evangélicas
comb$all_100_74 <- NA
comb$all_100_74 <- (comb$ativas_all_1974/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_74 <- NA
comb$mis_100_74 <- (comb$ativas_miss_1974/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_74 <- NA
comb$pen_100_74 <- (comb$ativas_pent_1974/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_74 <- NA
comb$neo_100_74 <- (comb$ativas_neopent_1974/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_74 <- NA
comb$nde_100_74 <- (comb$ativas_naodet_1974/comb$pop1970)*100000





###########
## 1975
###########

## Todas as igrejas evangélicas
comb$all_100_75 <- NA
comb$all_100_75 <- (comb$ativas_all_1975/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_75 <- NA
comb$mis_100_75 <- (comb$ativas_miss_1975/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_75 <- NA
comb$pen_100_75 <- (comb$ativas_pent_1975/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_75 <- NA
comb$neo_100_75 <- (comb$ativas_neopent_1975/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_75 <- NA
comb$nde_100_75 <- (comb$ativas_naodet_1975/comb$pop1970)*100000



###########
## 1976
###########

## Todas as igrejas evangélicas
comb$all_100_76 <- NA
comb$all_100_76 <- (comb$ativas_all_1976/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_76 <- NA
comb$mis_100_76 <- (comb$ativas_miss_1976/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_76 <- NA
comb$pen_100_76 <- (comb$ativas_pent_1976/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_76 <- NA
comb$neo_100_76 <- (comb$ativas_neopent_1976/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_76 <- NA
comb$nde_100_76 <- (comb$ativas_naodet_1976/comb$pop1970)*100000




###########
## 1977
###########

## Todas as igrejas evangélicas
comb$all_100_77 <- NA
comb$all_100_77 <- (comb$ativas_all_1977/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_77 <- NA
comb$mis_100_77 <- (comb$ativas_miss_1977/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_77 <- NA
comb$pen_100_77 <- (comb$ativas_pent_1977/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_77 <- NA
comb$neo_100_77 <- (comb$ativas_neopent_1977/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_77 <- NA
comb$nde_100_77 <- (comb$ativas_naodet_1977/comb$pop1970)*100000




###########
## 1978
###########

## Todas as igrejas evangélicas
comb$all_100_78 <- NA
comb$all_100_78 <- (comb$ativas_all_1978/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_78 <- NA
comb$mis_100_78 <- (comb$ativas_miss_1978/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_78 <- NA
comb$pen_100_78 <- (comb$ativas_pent_1978/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_78 <- NA
comb$neo_100_78 <- (comb$ativas_neopent_1978/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_78 <- NA
comb$nde_100_78 <- (comb$ativas_naodet_1978/comb$pop1970)*100000



###########
## 1979
###########

## Todas as igrejas evangélicas
comb$all_100_79 <- NA
comb$all_100_79 <- (comb$ativas_all_1979/comb$pop1970)*100000

## Igrejas evangélicas missionárias
comb$mis_100_79 <- NA
comb$mis_100_79 <- (comb$ativas_miss_1979/comb$pop1970)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_79 <- NA
comb$pen_100_79 <- (comb$ativas_pent_1979/comb$pop1970)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_79 <- NA
comb$neo_100_79 <- (comb$ativas_neopent_1979/comb$pop1970)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_79 <- NA
comb$nde_100_79 <- (comb$ativas_naodet_1979/comb$pop1970)*100000




###########
## 1980
###########

## Todas as igrejas evangélicas
comb$all_100_80 <- NA
comb$all_100_80 <- (comb$ativas_all_1980/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_80 <- NA
comb$mis_100_80 <- (comb$ativas_miss_1980/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_80 <- NA
comb$pen_100_80 <- (comb$ativas_pent_1980/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_80 <- NA
comb$neo_100_80 <- (comb$ativas_neopent_1980/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_80 <- NA
comb$nde_100_80 <- (comb$ativas_naodet_1980/comb$pop1980)*100000





###########
## 1981
###########

## Todas as igrejas evangélicas
comb$all_100_81 <- NA
comb$all_100_81 <- (comb$ativas_all_1981/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_81 <- NA
comb$mis_100_81 <- (comb$ativas_miss_1981/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_81 <- NA
comb$pen_100_81 <- (comb$ativas_pent_1981/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_81 <- NA
comb$neo_100_81 <- (comb$ativas_neopent_1981/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_81 <- NA
comb$nde_100_81 <- (comb$ativas_naodet_1981/comb$pop1980)*100000






###########
## 1982
###########

## Todas as igrejas evangélicas
comb$all_100_82 <- NA
comb$all_100_82 <- (comb$ativas_all_1982/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_82 <- NA
comb$mis_100_82 <- (comb$ativas_miss_1982/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_82 <- NA
comb$pen_100_82 <- (comb$ativas_pent_1982/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_82 <- NA
comb$neo_100_82 <- (comb$ativas_neopent_1982/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_82 <- NA
comb$nde_100_82 <- (comb$ativas_naodet_1982/comb$pop1980)*100000




###########
## 1983
###########

## Todas as igrejas evangélicas
comb$all_100_83 <- NA
comb$all_100_83 <- (comb$ativas_all_1983/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_83 <- NA
comb$mis_100_83 <- (comb$ativas_miss_1983/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_83 <- NA
comb$pen_100_83 <- (comb$ativas_pent_1983/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_83 <- NA
comb$neo_100_83 <- (comb$ativas_neopent_1983/comb$pop1980)*100000

## Igrejas evangélicas de classificação não determinada
#comb$nde_100_83 <- NA
#comb$nde_100_83 <- (comb$ativas_naodet_1983/comb$pop1980)*100000




###########
## 1984
###########

## Todas as igrejas evangélicas
comb$all_100_84 <- NA
comb$all_100_84 <- (comb$ativas_all_1984/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_84 <- NA
comb$mis_100_84 <- (comb$ativas_miss_1984/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_84 <- NA
comb$pen_100_84 <- (comb$ativas_pent_1984/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_84 <- NA
comb$neo_100_84 <- (comb$ativas_neopent_1984/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_84 <- NA
comb$nde_100_84 <- (comb$ativas_naodet_1984/comb$pop1980)*100000



###########
## 1985
###########

## Todas as igrejas evangélicas
comb$all_100_85 <- NA
comb$all_100_85 <- (comb$ativas_all_1985/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_85 <- NA
comb$mis_100_85 <- (comb$ativas_miss_1985/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_85 <- NA
comb$pen_100_85 <- (comb$ativas_pent_1985/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_85 <- NA
comb$neo_100_85 <- (comb$ativas_neopent_1985/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_85 <- NA
comb$nde_100_85 <- (comb$ativas_naodet_1985/comb$pop1980)*100000



###########
## 1986
###########

## Todas as igrejas evangélicas
comb$all_100_86 <- NA
comb$all_100_86 <- (comb$ativas_all_1986/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_86 <- NA
comb$mis_100_86 <- (comb$ativas_miss_1986/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_86 <- NA
comb$pen_100_86 <- (comb$ativas_pent_1986/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_86 <- NA
comb$neo_100_86 <- (comb$ativas_neopent_1986/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_86 <- NA
comb$nde_100_86 <- (comb$ativas_naodet_1986/comb$pop1980)*100000





###########
## 1987
###########

## Todas as igrejas evangélicas
comb$all_100_87 <- NA
comb$all_100_87 <- (comb$ativas_all_1987/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_87 <- NA
comb$mis_100_87 <- (comb$ativas_miss_1987/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_87 <- NA
comb$pen_100_87 <- (comb$ativas_pent_1987/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_87 <- NA
comb$neo_100_87 <- (comb$ativas_neopent_1987/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_87 <- NA
comb$nde_100_87 <- (comb$ativas_naodet_1987/comb$pop1980)*100000



###########
## 1988
###########

## Todas as igrejas evangélicas
comb$all_100_88 <- NA
comb$all_100_88 <- (comb$ativas_all_1988/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_88 <- NA
comb$mis_100_88 <- (comb$ativas_miss_1988/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_88 <- NA
comb$pen_100_88 <- (comb$ativas_pent_1988/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_88 <- NA
comb$neo_100_88 <- (comb$ativas_neopent_1988/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_88 <- NA
comb$nde_100_88 <- (comb$ativas_naodet_1988/comb$pop1980)*100000



###########
## 1989
###########

## Todas as igrejas evangélicas
comb$all_100_89 <- NA
comb$all_100_89 <- (comb$ativas_all_1989/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_89 <- NA
comb$mis_100_89 <- (comb$ativas_miss_1989/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_89 <- NA
comb$pen_100_89 <- (comb$ativas_pent_1989/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_89 <- NA
comb$neo_100_89 <- (comb$ativas_neopent_1989/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_89 <- NA
comb$nde_100_89 <- (comb$ativas_naodet_1989/comb$pop1980)*100000




###########
## 1990
###########

## Todas as igrejas evangélicas
comb$all_100_90 <- NA
comb$all_100_90 <- (comb$ativas_all_1990/comb$pop1980)*100000

## Igrejas evangélicas missionárias
comb$mis_100_90 <- NA
comb$mis_100_90 <- (comb$ativas_miss_1990/comb$pop1980)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_90 <- NA
comb$pen_100_90 <- (comb$ativas_pent_1990/comb$pop1980)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_90 <- NA
comb$neo_100_90 <- (comb$ativas_neopent_1990/comb$pop1980)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_90 <- NA
comb$nde_100_90 <- (comb$ativas_naodet_1990/comb$pop1980)*100000





###########
## 1991
###########

## Todas as igrejas evangélicas
comb$all_100_91 <- NA
comb$all_100_91 <- (comb$ativas_all_1991/comb$pop1991)*100000

## Igrejas evangélicas missionárias
comb$mis_100_91 <- NA
comb$mis_100_91 <- (comb$ativas_miss_1991/comb$pop1991)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_91 <- NA
comb$pen_100_91 <- (comb$ativas_pent_1991/comb$pop1991)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_91 <- NA
comb$neo_100_91 <- (comb$ativas_neopent_1991/comb$pop1991)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_91 <- NA
comb$nde_100_91 <- (comb$ativas_naodet_1991/comb$pop1991)*100000




###########
## 1992
###########

## Todas as igrejas evangélicas
comb$all_100_92 <- NA
comb$all_100_92 <- (comb$ativas_all_1992/comb$pop1992)*100000

## Igrejas evangélicas missionárias
comb$mis_100_92 <- NA
comb$mis_100_92 <- (comb$ativas_miss_1992/comb$pop1992)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_92 <- NA
comb$pen_100_92 <- (comb$ativas_pent_1992/comb$pop1992)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_92 <- NA
comb$neo_100_92 <- (comb$ativas_neopent_1992/comb$pop1992)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_92 <- NA
comb$nde_100_92 <- (comb$ativas_naodet_1992/comb$pop1992)*100000





###########
## 1993
###########

## Todas as igrejas evangélicas
comb$all_100_93 <- NA
comb$all_100_93 <- (comb$ativas_all_1993/comb$pop1993)*100000

## Igrejas evangélicas missionárias
comb$mis_100_93 <- NA
comb$mis_100_93 <- (comb$ativas_miss_1993/comb$pop1993)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_93 <- NA
comb$pen_100_93 <- (comb$ativas_pent_1993/comb$pop1993)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_93 <- NA
comb$neo_100_93 <- (comb$ativas_neopent_1993/comb$pop1993)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_93 <- NA
comb$nde_100_93 <- (comb$ativas_naodet_1993/comb$pop1993)*100000





###########
## 1994
###########

## Todas as igrejas evangélicas
comb$all_100_94 <- NA
comb$all_100_94 <- (comb$ativas_all_1994/comb$pop1994)*100000

## Igrejas evangélicas missionárias
comb$mis_100_94 <- NA
comb$mis_100_94 <- (comb$ativas_miss_1994/comb$pop1994)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_94 <- NA
comb$pen_100_94 <- (comb$ativas_pent_1994/comb$pop1994)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_94 <- NA
comb$neo_100_94 <- (comb$ativas_neopent_1994/comb$pop1994)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_94 <- NA
comb$nde_100_94 <- (comb$ativas_naodet_1994/comb$pop1994)*100000





###########
## 1995
###########

## Todas as igrejas evangélicas
comb$all_100_95 <- NA
comb$all_100_95 <- (comb$ativas_all_1995/comb$pop1995)*100000

## Igrejas evangélicas missionárias
comb$mis_100_95 <- NA
comb$mis_100_95 <- (comb$ativas_miss_1995/comb$pop1995)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_95 <- NA
comb$pen_100_95 <- (comb$ativas_pent_1995/comb$pop1995)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_95 <- NA
comb$neo_100_95 <- (comb$ativas_neopent_1995/comb$pop1995)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_95 <- NA
comb$nde_100_95 <- (comb$ativas_naodet_1995/comb$pop1995)*100000





###########
## 1996
###########

## Todas as igrejas evangélicas
comb$all_100_96 <- NA
comb$all_100_96 <- (comb$ativas_all_1996/comb$pop1995)*100000

## Igrejas evangélicas missionárias
comb$mis_100_96 <- NA
comb$mis_100_96 <- (comb$ativas_miss_1996/comb$pop1995)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_96 <- NA
comb$pen_100_96 <- (comb$ativas_pent_1996/comb$pop1995)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_96 <- NA
comb$neo_100_96 <- (comb$ativas_neopent_1996/comb$pop1995)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_96 <- NA
comb$nde_100_96 <- (comb$ativas_naodet_1996/comb$pop1995)*100000





###########
## 1997
###########

## Todas as igrejas evangélicas
comb$all_100_97 <- NA
comb$all_100_97 <- (comb$ativas_all_1997/comb$pop1997)*100000

## Igrejas evangélicas missionárias
comb$mis_100_97 <- NA
comb$mis_100_97 <- (comb$ativas_miss_1997/comb$pop1997)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_97 <- NA
comb$pen_100_97 <- (comb$ativas_pent_1997/comb$pop1997)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_97 <- NA
comb$neo_100_97 <- (comb$ativas_neopent_1997/comb$pop1997)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_97 <- NA
comb$nde_100_97 <- (comb$ativas_naodet_1997/comb$pop1997)*100000





###########
## 1998
###########

## Todas as igrejas evangélicas
comb$all_100_98 <- NA
comb$all_100_98 <- (comb$ativas_all_1998/comb$pop1998)*100000

## Igrejas evangélicas missionárias
comb$mis_100_98 <- NA
comb$mis_100_98 <- (comb$ativas_miss_1998/comb$pop1998)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_98 <- NA
comb$pen_100_98 <- (comb$ativas_pent_1998/comb$pop1998)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_98 <- NA
comb$neo_100_98 <- (comb$ativas_neopent_1998/comb$pop1998)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_98 <- NA
comb$nde_100_98 <- (comb$ativas_naodet_1998/comb$pop1998)*100000




###########
## 1999
###########

## Todas as igrejas evangélicas
comb$all_100_99 <- NA
comb$all_100_99 <- (comb$ativas_all_1999/comb$pop1999)*100000

## Igrejas evangélicas missionárias
comb$mis_100_99 <- NA
comb$mis_100_99 <- (comb$ativas_miss_1999/comb$pop1999)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_99 <- NA
comb$pen_100_99 <- (comb$ativas_pent_1999/comb$pop1999)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_99 <- NA
comb$neo_100_99 <- (comb$ativas_neopent_1999/comb$pop1999)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_99 <- NA
comb$nde_100_99 <- (comb$ativas_naodet_1999/comb$pop1999)*100000





###########
## 2000
###########

## Todas as igrejas evangélicas
comb$all_100_00 <- NA
comb$all_100_00 <- (comb$ativas_all_2000/comb$pop2000)*100000

## Igrejas evangélicas missionárias
comb$mis_100_00 <- NA
comb$mis_100_00 <- (comb$ativas_miss_2000/comb$pop2000)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_00 <- NA
comb$pen_100_00 <- (comb$ativas_pent_2000/comb$pop2000)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_00 <- NA
comb$neo_100_00 <- (comb$ativas_neopent_2000/comb$pop2000)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_00 <- NA
comb$nde_100_00 <- (comb$ativas_naodet_2000/comb$pop2000)*100000





###########
## 2001
###########

## Todas as igrejas evangélicas
comb$all_100_01 <- NA
comb$all_100_01 <- (comb$ativas_all_2001/comb$pop2001)*100000

## Igrejas evangélicas missionárias
comb$mis_100_01 <- NA
comb$mis_100_01 <- (comb$ativas_miss_2001/comb$pop2001)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_01 <- NA
comb$pen_100_01 <- (comb$ativas_pent_2001/comb$pop2001)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_01 <- NA
comb$neo_100_01 <- (comb$ativas_neopent_2001/comb$pop2001)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_01 <- NA
comb$nde_100_01 <- (comb$ativas_naodet_2001/comb$pop2001)*100000




###########
## 2002
###########

## Todas as igrejas evangélicas
comb$all_100_02 <- NA
comb$all_100_02 <- (comb$ativas_all_2002/comb$pop2002)*100000

## Igrejas evangélicas missionárias
comb$mis_100_02 <- NA
comb$mis_100_02 <- (comb$ativas_miss_2002/comb$pop2002)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_02 <- NA
comb$pen_100_02 <- (comb$ativas_pent_2002/comb$pop2002)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_02 <- NA
comb$neo_100_02 <- (comb$ativas_neopent_2002/comb$pop2002)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_02 <- NA
comb$nde_100_02 <- (comb$ativas_naodet_2002/comb$pop2002)*100000



###########
## 2003
###########

## Todas as igrejas evangélicas
comb$all_100_03 <- NA
comb$all_100_03 <- (comb$ativas_all_2003/comb$pop2003)*100000

## Igrejas evangélicas missionárias
comb$mis_100_03 <- NA
comb$mis_100_03 <- (comb$ativas_miss_2003/comb$pop2003)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_03 <- NA
comb$pen_100_03 <- (comb$ativas_pent_2003/comb$pop2003)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_03 <- NA
comb$neo_100_03 <- (comb$ativas_neopent_2003/comb$pop2003)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_03 <- NA
comb$nde_100_03 <- (comb$ativas_naodet_2003/comb$pop2003)*100000





###########
## 2004
###########

## Todas as igrejas evangélicas
comb$all_100_04 <- NA
comb$all_100_04 <- (comb$ativas_all_2004/comb$pop2004)*100000

## Igrejas evangélicas missionárias
comb$mis_100_04 <- NA
comb$mis_100_04 <- (comb$ativas_miss_2004/comb$pop2004)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_04 <- NA
comb$pen_100_04 <- (comb$ativas_pent_2004/comb$pop2004)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_04 <- NA
comb$neo_100_04 <- (comb$ativas_neopent_2004/comb$pop2004)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_04 <- NA
comb$nde_100_04 <- (comb$ativas_naodet_2004/comb$pop2004)*100000





###########
## 2005
###########

## Todas as igrejas evangélicas
comb$all_100_05 <- NA
comb$all_100_05 <- (comb$ativas_all_2005/comb$pop2005)*100000

## Igrejas evangélicas missionárias
comb$mis_100_05 <- NA
comb$mis_100_05 <- (comb$ativas_miss_2005/comb$pop2005)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_05 <- NA
comb$pen_100_05 <- (comb$ativas_pent_2005/comb$pop2005)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_05 <- NA
comb$neo_100_05 <- (comb$ativas_neopent_2005/comb$pop2005)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_05 <- NA
comb$nde_100_05 <- (comb$ativas_naodet_2005/comb$pop2005)*100000




###########
## 2006
###########

## Todas as igrejas evangélicas
comb$all_100_06 <- NA
comb$all_100_06 <- (comb$ativas_all_2006/comb$pop2006)*100000

## Igrejas evangélicas missionárias
comb$mis_100_06 <- NA
comb$mis_100_06 <- (comb$ativas_miss_2006/comb$pop2006)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_06 <- NA
comb$pen_100_06 <- (comb$ativas_pent_2006/comb$pop2006)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_06 <- NA
comb$neo_100_06 <- (comb$ativas_neopent_2006/comb$pop2006)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_06 <- NA
comb$nde_100_06 <- (comb$ativas_naodet_2006/comb$pop2006)*100000




###########
## 2007
###########

## Todas as igrejas evangélicas
comb$all_100_07 <- NA
comb$all_100_07 <- (comb$ativas_all_2007/comb$pop2006)*100000

## Igrejas evangélicas missionárias
comb$mis_100_07 <- NA
comb$mis_100_07 <- (comb$ativas_miss_2007/comb$pop2006)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_07 <- NA
comb$pen_100_07 <- (comb$ativas_pent_2007/comb$pop2006)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_07 <- NA
comb$neo_100_07 <- (comb$ativas_neopent_2007/comb$pop2006)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_07 <- NA
comb$nde_100_07 <- (comb$ativas_naodet_2007/comb$pop2006)*100000




###########
## 2008
###########

## Todas as igrejas evangélicas
comb$all_100_08 <- NA
comb$all_100_08 <- (comb$ativas_all_2008/comb$pop2008)*100000

## Igrejas evangélicas missionárias
comb$mis_100_08 <- NA
comb$mis_100_08 <- (comb$ativas_miss_2008/comb$pop2008)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_08 <- NA
comb$pen_100_08 <- (comb$ativas_pent_2008/comb$pop2008)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_08 <- NA
comb$neo_100_08 <- (comb$ativas_neopent_2008/comb$pop2008)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_08 <- NA
comb$nde_100_08 <- (comb$ativas_naodet_2008/comb$pop2008)*100000





###########
## 2009
###########

## Todas as igrejas evangélicas
comb$all_100_09 <- NA
comb$all_100_09 <- (comb$ativas_all_2009/comb$pop2009)*100000

## Igrejas evangélicas missionárias
comb$mis_100_09 <- NA
comb$mis_100_09 <- (comb$ativas_miss_2009/comb$pop2009)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_09 <- NA
comb$pen_100_09 <- (comb$ativas_pent_2009/comb$pop2009)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_09 <- NA
comb$neo_100_09 <- (comb$ativas_neopent_2009/comb$pop2009)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_09 <- NA
comb$nde_100_09 <- (comb$ativas_naodet_2009/comb$pop2009)*100000




###########
## 2010
###########

## Todas as igrejas evangélicas
comb$all_100_10 <- NA
comb$all_100_10 <- (comb$ativas_all_2010/comb$pop2009)*100000

## Igrejas evangélicas missionárias
comb$mis_100_10 <- NA
comb$mis_100_10 <- (comb$ativas_miss_2010/comb$pop2009)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_10 <- NA
comb$pen_100_10 <- (comb$ativas_pent_2010/comb$pop2009)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_10 <- NA
comb$neo_100_10 <- (comb$ativas_neopent_2010/comb$pop2009)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_10 <- NA
comb$nde_100_10 <- (comb$ativas_naodet_2010/comb$pop2009)*100000



###########
## 2011
###########

## Todas as igrejas evangélicas
comb$all_100_11 <- NA
comb$all_100_11 <- (comb$ativas_all_2011/comb$pop2011)*100000

## Igrejas evangélicas missionárias
comb$mis_100_11 <- NA
comb$mis_100_11 <- (comb$ativas_miss_2011/comb$pop2011)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_11 <- NA
comb$pen_100_11 <- (comb$ativas_pent_2011/comb$pop2011)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_11 <- NA
comb$neo_100_11 <- (comb$ativas_neopent_2011/comb$pop2011)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_11 <- NA
comb$nde_100_11 <- (comb$ativas_naodet_2011/comb$pop2011)*100000




###########
## 2012
###########

## Todas as igrejas evangélicas
comb$all_100_12 <- NA
comb$all_100_12 <- (comb$ativas_all_2012/comb$pop2012)*100000

## Igrejas evangélicas missionárias
comb$mis_100_12 <- NA
comb$mis_100_12 <- (comb$ativas_miss_2012/comb$pop2012)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_12 <- NA
comb$pen_100_12 <- (comb$ativas_pent_2012/comb$pop2012)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_12 <- NA
comb$neo_100_12 <- (comb$ativas_neopent_2012/comb$pop2012)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_12 <- NA
comb$nde_100_12 <- (comb$ativas_naodet_2012/comb$pop2012)*100000





###########
## 2013
###########

## Todas as igrejas evangélicas
comb$all_100_13 <- NA
comb$all_100_13 <- (comb$ativas_all_2013/comb$pop2013)*100000

## Igrejas evangélicas missionárias
comb$mis_100_13 <- NA
comb$mis_100_13 <- (comb$ativas_miss_2013/comb$pop2013)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_13 <- NA
comb$pen_100_13 <- (comb$ativas_pent_2013/comb$pop2013)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_13 <- NA
comb$neo_100_13 <- (comb$ativas_neopent_2013/comb$pop2013)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_13 <- NA
comb$nde_100_13 <- (comb$ativas_naodet_2013/comb$pop2013)*100000





###########
## 2014
###########

## Todas as igrejas evangélicas
comb$all_100_14 <- NA
comb$all_100_14 <- (comb$ativas_all_2014/comb$pop2014)*100000

## Igrejas evangélicas missionárias
comb$mis_100_14 <- NA
comb$mis_100_14 <- (comb$ativas_miss_2014/comb$pop2014)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_14 <- NA
comb$pen_100_14 <- (comb$ativas_pent_2014/comb$pop2014)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_14 <- NA
comb$neo_100_14 <- (comb$ativas_neopent_2014/comb$pop2014)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_14 <- NA
comb$nde_100_14 <- (comb$ativas_naodet_2014/comb$pop2014)*100000




###########
## 2015
###########

## Todas as igrejas evangélicas
comb$all_100_15 <- NA
comb$all_100_15 <- (comb$ativas_all_2015/comb$pop2015)*100000

## Igrejas evangélicas missionárias
comb$mis_100_15 <- NA
comb$mis_100_15 <- (comb$ativas_miss_2015/comb$pop2015)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_15 <- NA
comb$pen_100_15 <- (comb$ativas_pent_2015/comb$pop2015)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_15 <- NA
comb$neo_100_15 <- (comb$ativas_neopent_2015/comb$pop2015)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_15 <- NA
comb$nde_100_15 <- (comb$ativas_naodet_2015/comb$pop2015)*100000





###########
## 2016
###########

## Todas as igrejas evangélicas
comb$all_100_16 <- NA
comb$all_100_16 <- (comb$ativas_all_2016/comb$pop2016)*100000

## Igrejas evangélicas missionárias
comb$mis_100_16 <- NA
comb$mis_100_16 <- (comb$ativas_miss_2016/comb$pop2016)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_16 <- NA
comb$pen_100_16 <- (comb$ativas_pent_2016/comb$pop2016)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_16 <- NA
comb$neo_100_16 <- (comb$ativas_neopent_2016/comb$pop2016)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_16 <- NA
comb$nde_100_16 <- (comb$ativas_naodet_2016/comb$pop2016)*100000




###########
## 2017
###########

## Todas as igrejas evangélicas
comb$all_100_17 <- NA
comb$all_100_17 <- (comb$ativas_all_2017/comb$pop2017)*100000

## Igrejas evangélicas missionárias
comb$mis_100_17 <- NA
comb$mis_100_17 <- (comb$ativas_miss_2017/comb$pop2017)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_17 <- NA
comb$pen_100_17 <- (comb$ativas_pent_2017/comb$pop2017)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_17 <- NA
comb$neo_100_17 <- (comb$ativas_neopent_2017/comb$pop2017)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_17 <- NA
comb$nde_100_17 <- (comb$ativas_naodet_2017/comb$pop2017)*100000




###########
## 2018
###########

## Todas as igrejas evangélicas
comb$all_100_18 <- NA
comb$all_100_18 <- (comb$ativas_all_2018/comb$pop2018)*100000

## Igrejas evangélicas missionárias
comb$mis_100_18 <- NA
comb$mis_100_18 <- (comb$ativas_miss_2018/comb$pop2018)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_18 <- NA
comb$pen_100_18 <- (comb$ativas_pent_2018/comb$pop2018)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_18 <- NA
comb$neo_100_18 <- (comb$ativas_neopent_2018/comb$pop2018)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_18 <- NA
comb$nde_100_18 <- (comb$ativas_naodet_2018/comb$pop2018)*100000




###########
## 2019
###########

## Todas as igrejas evangélicas
comb$all_100_19 <- NA
comb$all_100_19 <- (comb$ativas_all_2019/comb$pop2019)*100000

## Igrejas evangélicas missionárias
comb$mis_100_19 <- NA
comb$mis_100_19 <- (comb$ativas_miss_2019/comb$pop2019)*100000

## Igrejas evangélicas pentecostais
comb$pen_100_19 <- NA
comb$pen_100_19 <- (comb$ativas_pent_2019/comb$pop2019)*100000

## Igrejas evangélicas neopentecostais
comb$neo_100_19 <- NA
comb$neo_100_19 <- (comb$ativas_neopent_2019/comb$pop2019)*100000

# Igrejas evangélicas de classificação não determinada
comb$nde_100_19 <- NA
comb$nde_100_19 <- (comb$ativas_naodet_2019/comb$pop2019)*100000



### Salvando o dataframe com todas as variáveis criadas em formato CSV
## Se nenhum outro diretório for especificado, o banco será salvo no diretório
## original onde foram salvos os outros arquivos utilizados na classifação


write.csv(comb, file=paste("df_igrejas_UF_medidas.csv"))  




###############################################################
## Plotando o número de Igrejas por 100 mil habitantes por UF
###############################################################

#### Todas as Igrejas 

p70_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_70), y=all_100_70)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("N. igrejas por 100 mil") + xlab("") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "1970")
p70_all



p80_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_80), y=all_100_80)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("") + xlab("") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "1980")
p80_all


p90_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_90), y=all_100_90)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("N. igrejas por 100 mil") + xlab("") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "1990")
p90_all


p00_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_00), y=all_100_00)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("") + xlab("") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "2000")
p00_all

p10_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_10), y=all_100_10)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("N. igrejas por 100 mil") + xlab("UF") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "2010")
p10_all


p19_all <-ggplot(data=comb, aes(x=reorder (uf, -all_100_19), y=all_100_19)) +
  geom_bar(stat="identity") + ylim(0, max(100)) +
  ylab("") + xlab("UF") +
  theme_minimal() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("",
          subtitle = "2019")
p19_all


Figure_ufall <- ggarrange(p70_all, p80_all,p90_all,p00_all,p10_all, p19_all,
                          labels = c("A", "B", "C", "D", "E", "F"),
                          ncol = 2, nrow = 3)
Figure_ufall




#################################################
#### Competição entre denominações evangélicas
#################################################

## Todas evangélicas versus Pentecostais 

comp1 <- ggplot(comb, aes(x= pen_100_19, y=all_100_19)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method=lm)+ ylim(0, max(100)) +
  ylab("Igrejas evangélicas por 100mil") +
  xlab("Igrejas pentecostais por 100mil") + ggtitle("",
                  subtitle = "")
comp1

## Missionárias versus Pentecostais

comp2 <- ggplot(comb, aes(x= pen_100_19, y=mis_100_19)) + 
  geom_point() + theme_minimal() +
  geom_smooth(method=lm) + ylim(0, max(30)) +
  ylab("Igrejas de missão por 100mil") +
  xlab("Igrejas pentecostais por 100mil") + ggtitle("",
                    subtitle = "")
comp2

## Neopentecostais versus Pentecostais

comp3 <- ggplot(comb, aes(x= pen_100_19, y=neo_100_19)) + 
  geom_point()+ theme_minimal() +
  geom_smooth(method=lm) + ylim(0, max(30)) +
  ylab("Igrejas neopentecostais por 100mil") +
  xlab("Igrejas pentecostais por 100mil") + ggtitle("",
                  subtitle = "")
comp3


## Igrejas de classificação não determinada versus Pentecostais

comp4 <- ggplot(comb, aes(x= pen_100_19, y=nde_100_19)) + 
  geom_point()+ theme_minimal() +
  geom_smooth(method=lm) + ylim(0, max(30)) +
  ylab("Igrejas não determinadas por 100mil") +
  xlab("Igrejas pentecostais por 100mil") + ggtitle("",
                  subtitle = "")
comp4



Figure_comp <- ggarrange(comp1,           
                         ggarrange(comp2, comp3, comp4, ncol = 3, labels = c("B", "C", "D")), # Second row with box and dot plots
                         nrow = 2, 
                         labels = "A")
Figure_comp 



#####################################################################################
## Validação das medidas
## Correlação entre as medidas criados e os dados oficiais do censo IBGE (2010)
######################################################################################

## % de cristãos evangélicos versus Igrejas Evangélicas por 100mil em 2010

p_val1 <- ggplot(comb, aes(x= all_100_10, y=evang_10)) + 
  geom_point()+ theme_minimal() + 
  geom_smooth(method=lm) +
  ylab("% de cristãos evangélicos (IBGE, 2010)") +
  xlab("Igrejas Evangélicas por 100mil (2010)") + ggtitle("",
                          subtitle = "")
p_val1


## % de evangélicos missionários versus Igrejas Missinárias por 100mil em 2010

p_val2 <- ggplot(comb, aes(x= mis_100_10, y=mis_10)) + 
  geom_point()+ theme_minimal() +
  geom_smooth(method=lm) +
  ylab("% de evangélicos missionários (2010)") +
  xlab("Igrejas Missinárias por 100mil (2010)") + ggtitle("",
                            subtitle = "")
p_val2


## % de evangélicos pentecostais versus Igrejas Pentecostais por 100mil em 2010 

p_val3 <- ggplot(comb, aes(x= pen_100_10, y=pen_10)) + 
  geom_point()+ theme_minimal()  +
  geom_smooth(method=lm) +
  ylab("% de evangélicos pentecostais (2010)") +
  xlab("Igrejas Pentecostais por 100mil (2010)") + ggtitle("",
                              subtitle = "")
p_val3


## % de evangélicos de class. não determinada versus Igrejas de class. não determinada por 100mil

p_val4 <- ggplot(comb, aes(x= nde_100_10, y=nde_10)) + 
  geom_point()+ theme_minimal() +
  geom_smooth(method=lm) +
  ylab("% de evangélicos de class. não determinada (2010)") +
  xlab("Igrejas de class. não determinada por 100mil (2010)") + ggtitle("",
                              subtitle = "")
p_val4





### Mapping o número de igrejas por 100 mil habitantes em 2019
# 1960, 1980, 2000 e 2019


states <- read_state(
  year=2019, 
  showProgress = TRUE
)


states  <- rename(states, uf = abbrev_state)

## Juntando o dataframe com o shapefile (states) com o banco de dados (comb) com as medidas criadas

comb_maps <- states  %>% 
  left_join(comb, by = c("uf")) 



map <- ggplot() +
  geom_sf(data=comb_maps, aes(fill= all_100_19), color="gray", size=.10, show.legend = TRUE) +
  labs(subtitle="", size=20) + scale_fill_distiller(palette = "Greys", 
                                                    name="N. por 100mil",direction = 1) +
  theme_minimal() + theme(legend.position="right") +
  ggtitle("",
          subtitle = "2019")
map





