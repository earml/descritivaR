rm(list=ls())
library(dplyr)

source("E:/excluir.R",continue.echo=TRUE)
# code to read the  back into memory
d2 <- data.table::fread("E:/amostraPBF2.csv",sep = "auto", header= TRUE, na.strings = "",drop = excluir) %>% data.frame()

d <- d2

d <- subset(d, linkado == 1 & nasc_antes == 0)
#d<-d[,-(1:2)]

glimpse(d)

## converto as variáveis que são caracter para inteiro ##
for(i in 1:ncol(d)){
  if(typeof(d[[i]])=="character" && (names(d[i])!="dtnasc" && names(d[i])!="dtnascmae")) d[[i]] <-  as.integer(d[[i]])
  
}

## Adicionando Proxy  ##
## 
#Esta variável indica se a criança nasceu (0) ou não (1) no mesmo município que a mãe reside ou que irá residir (a criança)
d$nasc_res <- NA 
d$nasc_res[] <- if_else(d$codmunres[] == d$codmunnasc[],0,1)

##
#Esta variável foi categorizada conforme sugestões do ?????. Primeiro
#excluimos as idades menores de 14 anos, depois criamos as seguintes
#categorias:
#1 (14-19)
#2 (20-24)
#3 (25-29)
#4 (30-34)
#5 (35-39)
#6 (40-44)
#7 (45 +)
d$idademae_cat_rally <- NA
id <- which(d$idademae<14) #Gero um vetor com os índices para as idades <14 
d$idademae[id]  <-  NA     #Atribuo 'NA' aos índices de id

#### categorizo a variável  ####

d$idademae_cat_rally[d$idademae >= 14 & d$idademae <= 19] <- 1
d$idademae_cat_rally[d$idademae >= 20 & d$idademae <= 24] <- 2
d$idademae_cat_rally[d$idademae >= 25 & d$idademae <= 29] <- 3
d$idademae_cat_rally[d$idademae >= 30 & d$idademae <= 34] <- 4
d$idademae_cat_rally[d$idademae >= 35 & d$idademae <= 39] <- 5
d$idademae_cat_rally[d$idademae >= 40 & d$idademae <= 44] <- 6
d$idademae_cat_rally[d$idademae >= 45] <- 7

table(d$idademae_cat_rally,useNA ='always')
##
#Categorizar a variável 'estcivmae': 1 (com companheiro), 2 (sem companheiro)
#OBS: esta variável já foi categorizada em 'estcivmaecat', como (0 com companheiro, 1 sem companheiro)

##
#Categorizar a data de nascimento da criança
#dia_semana, mês, semestre
d_semana <- weekdays(as.Date.character(d$dtnasc))
d_mes <- months(as.Date.character(d$dtnasc))

d$dtnasc_semana <- d_semana
d$dtnasc_mes <- d_mes

d$dtnasc_semestre[d$dtnasc_mes == "janeiro" | d$dtnasc_mes == "fevereiro" |
                    d$dtnasc_mes == "março" | d$dtnasc_mes == "abril" |
                    d$dtnasc_mes == "maio" | d$dtnasc_mes == "junho"] <- 1

d$dtnasc_semestre[d$dtnasc_mes == "julho" | d$dtnasc_mes == "agosto" |
                    d$dtnasc_mes == "setembro" | d$dtnasc_mes == "outubro" |
                    d$dtnasc_mes == "novembro" | d$dtnasc_mes == "dezembro"] <- 2

d$dtnasc_semana <- recode(d$dtnasc_semana, "segunda-feira" = "1", "terça-feira" = "2", "quarta-feira" =  "3",
                          "quinta-feira" = "4", "sexta-feira" = "5" ,"sábado" = "6", "domingo" = "7")

d$dtnasc_mes <- recode(d$dtnasc_mes, "janeiro" = "1", "fevereiro" = "2", "março" = "3", "abril" = "4",
                       "maio"  = "5", "junho" = "6", "julho"  = "7", "agosto" = "8", "setembro" = "9",
                       "outubro" = "10", "novembro" = "11", "dezembro" = "12")


#Categorizar a data de nascimento da mãe
#dia_semana, mês, semestre
d_semana_mae <- weekdays(as.Date.character(d$dtnascmae))
d_mes_mae <- months(as.Date.character(d$dtnascmae))

d$dtnascmae_semana <- d_semana_mae
d$dtnascmae_mes <- d_mes_mae

d$dtnascmae_semestre[d$dtnascmae_mes == "janeiro" | d$dtnascmae_mes == "fevereiro" |
                       d$dtnascmae_mes == "março" | d$dtnascmae_mes == "abril" |
                       d$dtnascmae_mes == "maio" | d$dtnascmae_mes == "junho"] <- 1

d$dtnascmae_semestre[d$dtnascmae_mes == "julho" | d$dtnascmae_mes == "agosto" |
                       d$dtnascmae_mes == "setembro" | d$dtnascmae_mes == "outubro" |
                       d$dtnascmae_mes == "novembro" | d$dtnascmae_mes == "dezembro"] <- 2

d$dtnascmae_semana <- recode(d$dtnascmae_semana, "segunda-feira" = "1", "terça-feira" = "2", "quarta-feira" =  "3",
                             "quinta-feira" = "4", "sexta-feira" = "5" ,"sábado" = "6", "domingo" = "7")

d$dtnascmae_mes <- recode(d$dtnascmae_mes, "janeiro" = "1", "fevereiro" = "2", "março" = "3", "abril" = "4",
                          "maio"  = "5", "junho" = "6", "julho"  = "7", "agosto" = "8", "setembro" = "9",
                          "outubro" = "10", "novembro" = "11", "dezembro" = "12")


#
##
#Categorizar a variavel 'peso' como: (0 - MBPN-Muito Baixo Peso ao Nascer, 1 - Caso Contrário 
# 
#                                     0 - EBPN-Extremo Baixo Peso ao Nascer, 1 - Caso Contrário)
d$peso_mbpn <- NA 
d$peso_mbpn[] <- if_else(d$peso[] < 1500,0,1)

d$peso_ebpn <- NA 
d$peso_ebpn[] <- if_else(d$peso[] < 1000,0,1)

##
#Categorizar a variável "semagestac" como PT - Pretermo
#( 1 - <32, 2 - 32-34, 3 - 35-36, 4 - 37-38, 5 - 39+)
d <-  d %>%
  group_by(semagestac) %>% 
  mutate(preterm = if_else(semagestac < 32, true = 1, false = 
                             if_else(semagestac >= 32 && semagestac <=34, true = 2, false = 
                                       if_else(semagestac >=35 && semagestac<=36, true = 3, false = 
                                                 if_else(semagestac >= 37 && semagestac <= 38, true = 4, false = 5)))))



##
#Categorizar a variável "tpnascassi" como 1 - médico, 2 - Não Médico 
#OBS: Não consegui categorizar esta variável, pois no dicionário, começa em 0 - 4 depois 9
#Já no banco, começa com 0 - 4 depois 88
#d$tpnascassi_cat <- NA 
#d$tpnascassi_cat[] <- if_else(d$tpnascassi[] == d$codmunnasc[],0,1)

##
#Categorizar a variável "cd_grau_instrucao_v6" como < escolaridade e > escolaridade
#< escolaridade = (0, 1, 2, 3, 4, 5)
#> escolaridade = (6, 7, 8, 9, 10, 11, 12)
d$escolaridade <- NA 
d$escolaridade[] <- ifelse(d$cd_grau_instrucao_v6[] == "NA", "NA",
                           ifelse(d$cd_grau_instrucao_v6[] %in% c(0,1,2,3,4,5),0,1))


##############################################################################################
##############################################################################################
# add birth weight z-score
#devtools::install_github("HBGDki/growthstandards")
library(growthstandards)

library(dplyr)
d$sexo <- recode(d$sexo, "1" = "Male", "2" = "Female")

d$score_z_peso <- NA
idx <- which(!is.na(d$sexo))
d$score_z_peso[idx] <- igb_wtkg2zscore(d$semagestac[idx] * 7, d$peso[idx] / 1000, sex = as.character(d$sexo[idx]))

hist(d$score_z_peso)
# looks pretty good

# add birth weight centile
d$percentil <- NA
idx <- which(!is.na(d$sexo))

d$percentil[idx] <- igb_wtkg2centile(d$semagestac[idx] * 7, d$peso[idx] / 1000, sex = as.character(d$sexo[idx]))

hist(d$percentil)

length(which(is.na(d$score_z_peso))) / nrow(d)

# we don't have brthwt_z for 5,98% of the data

## add SGA and LGA and NORMAL
d$class_preterm <- NA 
d$class_preterm[] <- ifelse(d$percentil < 10, 1,
                            ifelse(d$percentil > 90, 2, 
                                   ifelse(d$percentil >= 10 & d$percentil <= 90, 3, 'NA')))


###############################################################################################
###############################################################################################

#
## descritiva dos dados  ##
library(psych)
descritiva <- describe(d,na.rm = TRUE,skew = TRUE, IQR=TRUE,quant=c(.1,.25,.5,.75,.90),
                       fast = TRUE, type = 3)

## visualizo a descritiva ##
library(DT)
DT::datatable(descritiva)

## salvo a descritiva  ##
data.table::fwrite(descritiva, file = "E:/descritiva.csv",sep = "|",dec = ",",row.names = TRUE)


######################################################################################
######################################################################################
### TABELAS CRUZDAS  ###
###                  ###
######################################################################################
######################################################################################
source("E:/nao_usar.txt",continue.echo=TRUE)

library(descr)
library(base)

df = d[,!(names(d) %in% nao_usar)]
variaveis <- names(df)
tabelas <- list()
for(j in variaveis){ 
  tabelas[[j]] <- crosstab(df$pesocat,df[[j]],  prop.r = T, prop.t = T, plot = FALSE)
}


# save the 
data.table::fwrite(tabelas, file = "E:/tabelas.csv")


require(dplyr)
require(lazyeval)
require(tidyverse)

create_group <- function(data, var1, var2){
  data %>% select_(.dots = list(lazy(var1), lazy(var2))) %>%
    group_by_(.dots = lazy(var1))
}

df <- data.frame(x = rpois(100, 4), y = rpois(100,2), z = rep(c("M","F"), each = 50))

df %>% create_group(var1 = z, var2 = y)

create_group <- function(data, var1, var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  data %>% 
    select(!!var1, !!var2) %>% 
    group_by(!!var1) %>% 
    summarise(media = mean(!!var2))
}

create_group
