#2 perfilação e testes
library(tidyverse)
rm(list=ls())
library(sjPlot)
library(wesanderson)
library(memisc)
library(huxtable)
library(marginaleffects)
df <- read_csv("df.csv")
#P22 até P25 - são as questões de tolerância #
table(df$P22)
table(df$P23)
table(df$P24)
table(df$P25)
prop.table(table(df$P22))*100
#perfis ################
#recodificar em três categorias para gráficos iniciais-> Não Conhece/NS/NR ,0-3, 4-6,7-10
df$dirPT <- memisc::recode(as.factor(df$P22), "Não Conhece/NS/NR" <- c(11,12),
                           "0-3" <- c(0,1,2,3),
                           "4-6"<-c(4,5,6),
                           "7-10"<-c(7,8,9,10))
prop.table(table(df$dirPT))*100#pra verificar se bate com o relatório (PDF file)
df$dirPLBolsonaro <- memisc::recode(as.factor(df$P24), "Não Conhece/NS/NR" <- c(11,12),
                                    "0-3" <- c(0,1,2,3),
                                    "4-6"<-c(4,5,6),
                                    "7-10"<-c(7,8,9,10))
prop.table(table(df$dirPLBolsonaro))*100#pra verificar se bate com o relatório (PDF file)
df$dirMDB <- memisc::recode(as.factor(df$P23), "Não Conhece/NS/NR" <- c(11,12),
                            "0-3" <- c(0,1,2,3),
                            "4-6"<-c(4,5,6),
                            "7-10"<-c(7,8,9,10))
prop.table(table(df$dirMDB))*100#pra verificar se bate com o relatório (PDF file)
df$dirCentrao <- memisc::recode(as.factor(df$P25), "Não Conhece/NS/NR" <- c(11,12),
                                "0-3" <- c(0,1,2,3),
                                "4-6"<-c(4,5,6),
                                "7-10"<-c(7,8,9,10))
prop.table(table(df$dirCentrao))*100#pra verificar se bate com o relatório (PDF file)

# ideia - comparar percentuais de desaprovação do direito de concorrer

#A
df$desaprovaSohDirPt <- df$dirPT == "0-3" & df$dirMDB != "0-3" & df$dirCentrao != "0-3" &
  df$dirPLBolsonaro  != "0-3"

prop.table(table(df$desaprovaSohDirPt))*100 # 10,9% desaprova só o direito do PT
A<- 10.93


#B
df$desaprovaSohDirPLBOLSO <- df$dirPT != "0-3" & df$dirMDB != "0-3" & df$dirCentrao != "0-3" &
  df$dirPLBolsonaro  == "0-3"

prop.table(table(df$desaprovaSohDirPLBOLSO))*100 # 9.53% desaprova só o direito do PL, partid do Bolso
B <- 9.53

#C
df$desaprovaSohDirCentrao <- df$dirPT != "0-3" & df$dirMDB != "0-3" & df$dirCentrao == "0-3" &
  df$dirPLBolsonaro  != "0-3"
prop.table(table(df$desaprovaSohDirCentrao))*100 # 1.93% desaprova só o direito do Centrão
C<-1.93


#D
df$desaprovaSohDirMDB <- df$dirPT != "0-3" & df$dirMDB == "0-3" & df$dirCentrao != "0-3" &
  df$dirPLBolsonaro  != "0-3"
prop.table(table(df$desaprovaSohDirMDB))*100 # 0.66% desaprova só o direito do Centrão
D<-0.66

#E
df$desaprovaSohDirCentrao_e_MDB <- df$dirPT != "0-3" & df$dirPLBolsonaro  != "0-3"&
  df$dirMDB == "0-3" & df$dirCentrao == "0-3"

prop.table(table(df$desaprovaSohDirCentrao_e_MDB))*100 # 0.8% desaprova direit do Centrão e do MDB
E <- 0.8

#F
df$desaprova_DirCentrao_e_MDB_e_PT <- df$dirPT == "0-3" & df$dirPLBolsonaro  != "0-3"&
  df$dirMDB == "0-3" & df$dirCentrao == "0-3"

prop.table(table(df$desaprova_DirCentrao_e_MDB_e_PT))*100 # 6.13% desaprova direit do Centrão,MDB e PT
F <- 6.13


#G
df$desaprova_DirCentrao_e_MDB_e_PLBOLSO <- df$dirPT != "0-3" & df$dirPLBolsonaro  == "0-3"&
  df$dirMDB == "0-3" & df$dirCentrao == "0-3"

prop.table(table(df$desaprova_DirCentrao_e_MDB_e_PLBOLSO))*100 # 5.73% desaprova direit do Centrão,MDB e PL do Bolso
G <- 5.73

#H
df$desaprova_DirPLdoBOLSO_edo_PT <- df$dirPT == "0-3" & df$dirPLBolsonaro  == "0-3"&
  df$dirMDB != "0-3" & df$dirCentrao != "0-3"

prop.table(table(df$desaprova_DirPLdoBOLSO_edo_PT))*100 # 1% desaprova direit do PL(bolso) e do PT
H <- 1


#I
df$desaprova_os4 <- df$dirPT == "0-3" & df$dirPLBolsonaro  == "0-3"&
  df$dirMDB == "0-3" & df$dirCentrao == "0-3"

prop.table(table(df$desaprova_os4))*100 # 5.26% desaprova direit do PL(bolso) e do PT
I <- 5.26



Quantidade <- c(A,B,C,D,E,F,G,H,I)
Perfil <- c("A","B","C","D","E","F","G","H","I")
color <- c("y","y","n","n","n","y","y","n","y")
graf1.1.1 <- data.frame(Quantidade, Perfil,color)

p <- ggplot(graf1.1.1, aes(Perfil, Quantidade, fill=color)) + geom_bar(stat="identity")+
  stat_summary(aes(label=round(..y.., 2)), fun = mean,
               geom = "text", size=4, vjust=1.4, color = "black")+
  labs(y="%", title= "Porcentagem de respondentes por perfil",
       caption= "Fonte: Base de dados de survey do projeto “As bases das clivagens políticas no Brasil”.
       Os destacados em Azul são os que tem mais % e seguirão para análises procedentes")


p + theme(plot.title = element_text(size=16, face="bold"),
          plot.caption = element_text(size=10.5))+ theme(legend.position="none")


# EM SUMA #
df$desaprovaSohDirPt -> df$PA
df$desaprovaSohDirPLBOLSO -> df$PB
df$desaprova_DirCentrao_e_MDB_e_PT -> df$PF
df$desaprova_DirCentrao_e_MDB_e_PLBOLSO -> df$PG
df$desaprova_os4 -> df$PI

# variável dep confirmatória ############
#removendo NA´s das questões P50 a P56 e também da P58

table(df$P50)#
df$P50_recod <- factor(ifelse(df$P50 == 1, "Tanto Faz",
                              ifelse(df$P50 == 2, "Preferível",
                                     ifelse(df$P50 == 3, "Governo autoritário",
                                            "NS/NR"))))
table(df$P50_recod)
df$P50_recod_numeric <- as.numeric(ifelse(is.na(df$P50) | df$P50 == 4 | df$P50 == 5, NA, df$P50))
table(df$P50_recod_numeric)
df$P50_antidemoc_positivo <- ifelse(df$P50_recod_numeric == 2, 0, 1)
table(df$P50_antidemoc_positivo)

table(df$P51)#
df$P51_recod_numeric <- as.numeric(df$P51)
df$P51_recod_numeric[df$P51_recod_numeric %in% c(11, 12)] <- NA
table(df$P51_recod_numeric)
df$P51_recod <- as.character(df$P51)
df$P51_recod[df$P51_recod == "11"] <- "NS/NR"
df$P51_recod[df$P51_recod == "12"] <- "NS/NR"
df$P51_recod[!is.na(df$P51_recod_numeric)] <- as.character(as.numeric(df$P51_recod_numeric[!is.na(df$P51_recod_numeric)]))
df$P51_recod <- factor(df$P51_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "NS/NR"))
table(df$P51_recod)

table(df$P52)#
df$P52_recod_numeric <- as.numeric(df$P52)
df$P52_recod_numeric[df$P52_recod_numeric %in% c(11, 12)] <- NA
table(df$P52_recod_numeric)
df$P52_recod <- as.character(df$P52)
df$P52_recod[df$P52_recod == "11"] <- "NS/NR"
df$P52_recod[df$P52_recod == "12"] <- "NS/NR"
df$P52_recod[!is.na(df$P52_recod_numeric)] <- as.character(as.numeric(df$P52_recod_numeric[!is.na(df$P52_recod_numeric)]))
df$P52_recod <- factor(df$P52_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P52_recod)

table(df$P53)#
df$P53_recod_numeric <- as.numeric(df$P53)
df$P53_recod_numeric[df$P53_recod_numeric %in% c(11, 12)] <- NA
table(df$P53_recod_numeric)
df$P53_recod <- as.character(df$P53)
df$P53_recod[df$P53_recod == "11"] <- "NS/NR"
df$P53_recod[df$P53_recod == "12"] <- "NS/NR"
df$P53_recod[!is.na(df$P53_recod_numeric)] <- as.character(as.numeric(df$P53_recod_numeric[!is.na(df$P53_recod_numeric)]))
df$P53_recod <- factor(df$P53_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P53_recod)

table(df$P54)#
df$P54_recod_numeric <- as.numeric(df$P54)
df$P54_recod_numeric[df$P54_recod_numeric %in% c(11, 12)] <- NA
table(df$P54_recod_numeric)
df$P54_recod <- as.character(df$P54)
df$P54_recod[df$P54_recod == "11"] <- "NS/NR"
df$P54_recod[df$P54_recod == "12"] <- "NS/NR"
df$P54_recod[!is.na(df$P54_recod_numeric)] <- as.character(as.numeric(df$P54_recod_numeric[!is.na(df$P54_recod_numeric)]))
df$P54_recod <- factor(df$P54_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P54_recod)

table(df$P55)#
df$P55_recod_numeric <- as.numeric(df$P55)
df$P55_recod_numeric[df$P55_recod_numeric %in% c(11, 12)] <- NA
table(df$P55_recod_numeric)
df$P55_recod <- as.character(df$P55)
df$P55_recod[df$P55_recod == "11"] <- "NS/NR"
df$P55_recod[df$P55_recod == "12"] <- "NS/NR"
df$P55_recod[!is.na(df$P55_recod_numeric)] <- as.character(as.numeric(df$P55_recod_numeric[!is.na(df$P55_recod_numeric)]))
df$P55_recod <- factor(df$P55_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P55_recod)

table(df$P56)
df$P56_recod_numeric_apoio_positivo <- df$P56
df$P56_recod_numeric_apoio_positivo[df$P56 == 4 | df$P56 == 5] <- NA
df$P56_recod_numeric_apoio_positivo[df$P56 == 1 | df$P56 == 2] <- 1
df$P56_recod_numeric_apoio_positivo[df$P56 == 3] <- 0
table(df$P56_recod_numeric_apoio_positivo)
df$P56_recod_8jan <- factor(df$P56, levels = c(1, 2, 3, 4, 5), labels = c("Apoio", "Apoio", "Não Apoio", "NS/NR", "NS/NR"))
table(df$P56_recod_8jan)


table(df$P58)#
df$P58_recod_numeric <- as.numeric(df$P58)
df$P58_recod_numeric[df$P58_recod_numeric %in% c(11, 12)] <- NA
table(df$P58_recod_numeric)
df$P58_recod <- as.character(df$P58)
df$P58_recod[df$P58_recod == "11"] <- "NS/NR"
df$P58_recod[df$P58_recod == "12"] <- "NS/NR"
df$P58_recod[!is.na(df$P58_recod_numeric)] <- as.character(as.numeric(df$P58_recod_numeric[!is.na(df$P58_recod_numeric)]))
df$P58_recod <- factor(df$P58_recod, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","NS/NR"))
table(df$P58_recod)


#ok todas estão no formato original, até aqui, no entanto, é necessário:
#padronizar
#padronizar? Deixar todas com o mesmo sinal
#a escolha foi deixar sinal positivo para posição antidemocrática


table(df$P50_antidemoc_positivo)# sinal ok

#P51 10 é totalmente justificável fechar o congresso
table(df$P51_recod_numeric) # ok


#P52  formato original: 10 é “concordo fortemente que a democracia eleitoral é o melhor”
#essa é uma que tem inverter
table(df$P52_recod)#tem q arrumar!
table(df$P52_recod_numeric)#tem q arrumar!

df$P52_recod <- memisc::recode(as.character(df$P52_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P52_recod_numeric <- memisc::recode(as.numeric(df$P52_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P52_recod)#Conferir se deu certo?
table(df$P52_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#P53 10 é apoia fortemente participação de pessoas em manifestações permitidas por lei
#essa é uma que tem inverter
table(df$P53_recod)#tem q arrumar!
table(df$P53_recod_numeric)#tem q arrumar!

df$P53_recod <- memisc::recode(as.character(df$P53_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P53_recod_numeric <- memisc::recode(as.numeric(df$P53_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P53_recod)#Conferir se deu certo?
table(df$P53_recod_numeric)#Conferir se deu certo?
#Sim


#próx
#essa é uma que tem inverter
#P54  10 “concordo totalmente que, para poder prender criminosos, elas devem sempre respeitar as leis”
table(df$P54_recod)#tem q arrumar!
table(df$P54_recod_numeric)#tem q arrumar!

df$P54_recod <- memisc::recode(as.character(df$P54_recod ), "0" <- c("10"),
                               "1" <-c("9"),
                               "2"<-c("8"),
                               "3"<-c("7"),
                               "4"<-c("6"),
                               "5"<-c("5"),
                               "6"<-c("4"),
                               "7"<-c("3"),
                               "8"<-c("2"),
                               "9"<-c("1"),
                               "10"<-c("0"),
                               "NS/NR"<-c("NS/NR"))
df$P54_recod_numeric <- memisc::recode(as.numeric(df$P54_recod_numeric), 0 <- c(10),
                                       1 <- c(9),
                                       2<-c(8),
                                       3<-c(7),
                                       4<-c(6),
                                       5<-c(5),
                                       6<-c(4),
                                       7<-c(3),
                                       8<-c(2),
                                       9<-c(1),
                                       10<-c(0))
table(df$P54_recod)#Conferir se deu certo?
table(df$P54_recod_numeric)#Conferir se deu certo?
#Sim


#próx

#P55 10 é “concorda totalmente”, até que ponto o(a) Sr(a) concorda que a vontade da maioria deveria sempre prevalecer, mesmo que prejudique os direitos das minorias

# nao precisa inverter
table(df$P55_recod)
table(df$P55_recod_numeric)


#próxima
# apoio a 8 de janeiro
# nao precisa inverter
table(df$P56_recod_8jan)
table(df$P56_recod_numeric_apoio_positivo)

# P58 10 é se justificaria o interv milit/golpe militar
# nao precisa inverter
table(df$P58_recod)
table(df$P58_recod_numeric)



#colocar entre 0 e 1 

table(df$P50_antidemoc_positivo)#ok, não precisa
df$P51_recod_numeric <- scales::rescale(df$P51_recod_numeric, to = c(0, 1))
table(df$P51_recod_numeric)#OK
df$P52_recod_numeric <- scales::rescale(df$P52_recod_numeric, to = c(0, 1))
table(df$P52_recod_numeric)#OK
df$P53_recod_numeric <- scales::rescale(df$P53_recod_numeric, to = c(0, 1))
table(df$P53_recod_numeric)#OK
df$P54_recod_numeric <- scales::rescale(df$P54_recod_numeric, to = c(0, 1))
table(df$P54_recod_numeric)#OK
df$P55_recod_numeric <- scales::rescale(df$P55_recod_numeric, to = c(0, 1))
table(df$P55_recod_numeric)#OK
table(df$P56_recod_numeric_apoio_positivo)#ok, não precisa
df$P58_recod_numeric <- scales::rescale(df$P58_recod_numeric, to = c(0, 1))
table(df$P58_recod_numeric)#OK


#confirmatoria em si
library(semTools)
library(lavaan)
library(semTools)
library(psych)
library(mice)
library(lavaanPlot)
dfcfa <- subset(df, select=c(P51_recod_numeric,
                             P52_recod_numeric,
                             P53_recod_numeric,
                             P54_recod_numeric,
                             P55_recod_numeric,
                             P56_recod_numeric_apoio_positivo,
                             P58_recod_numeric))

summary(dfcfa)
imp <- mice(dfcfa, seed=23109)# o nome da base e a seed sempre essa 23109
dfcfa <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(dfcfa)


model <- 'f =~ P51_recod_numeric + P52_recod_numeric + P53_recod_numeric + P54_recod_numeric + P55_recod_numeric + P56_recod_numeric_apoio_positivo + P58_recod_numeric'
# Rodando a análise fatorial confirmatória
cfa_model_mice <- cfa(model, data = dfcfa)
summary(cfa_model_mice, standardized = TRUE)


lavaanPlot(model =cfa_model_mice, node_options = list(shape = "circle", fontname = 
                                                   "Garamond"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = TRUE)


semTools::fitmeasures(cfa_model_mice, c("tli", "cfi", "rmsea", "srmr", "gfi"))

scores <- lavPredict(cfa_model_mice)
as.numeric(scores) -> dfcfa$scores
#scores -> df$scores
str(dfcfa$scores)

dfcfa$scores <- scales::rescale(dfcfa$scores, to = c(0, 1))#Esse código utiliza a função rescale() do pacote scales para reescalar os valores da variável dfcfa$scores. A função rescale() ajusta os valores para um novo intervalo de valores definido pelos argumentos to, que neste caso é de 0 a 1. Isso é útil quando se deseja comparar variáveis que têm escalas diferentes ou quando se deseja normalizar os valores de uma variável para que eles sejam mais facilmente interpretáveis.
summary(dfcfa$scores)#deu boa
hist(dfcfa$score, breaks=50)




# recod final e subset ###########

dfcfa$scores -> df$PosturaAntiDemoc

table(df$P12)#escolarid ok
df$Escolaridade <- df$P12
table(df$P6)
df$Mulher <- df$P6 == 2
summary(df$P8)
df$Idade <- df$P8
table(df$P60)
df$Renda <- df$P60
table(df$P7)
df$Branca <- df$P7 == 1
df$evangelico <- df$P13 == 3
df$catolico <- df$P13 == 1
df$Nenhum <- df$P13 == 9

table(df$P57)
df$desinteresse <- df$P57
df$desinteresse[df$desinteresse  == 5] <- NA
df$desinteresse[df$desinteresse  == 6] <- NA
df$interesse <- -1*df$desinteresse
df$interesse <- scales::rescale(df$interesse, to = c(1, 4))


df <- subset(df, select=c(PosturaAntiDemoc,Nenhum,catolico,
                          interesse,evangelico,Branca,Renda,Idade,
                        Mulher,Escolaridade,PA,PB,PF,PG,PI))

#

#  modelos de regressão logística ###########

ModeloAfull <- glm(PA~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                  evangelico+catolico+Nenhum+PosturaAntiDemoc,data=df,
                family=binomial(link=logit))
ModeloA <- glm(PA~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                  evangelico+catolico+Nenhum,data=df,
                family=binomial(link=logit))
tab_model(ModeloAfull, ModeloA)
ModeloBfull <- glm(PB~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                     evangelico+catolico+Nenhum+PosturaAntiDemoc,data=df,
                   family=binomial(link=logit))
ModeloB <- glm(PB~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                 evangelico+catolico+Nenhum,data=df,
               family=binomial(link=logit))
tab_model(ModeloBfull, ModeloB)

ModeloFfull <- glm(PF~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                     evangelico+catolico+Nenhum+PosturaAntiDemoc,data=df,
                   family=binomial(link=logit))
ModeloF <- glm(PF~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                 evangelico+catolico+Nenhum,data=df,
               family=binomial(link=logit))
tab_model(ModeloFfull, ModeloF)

ModeloGfull <- glm(PG~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                     evangelico+catolico+Nenhum+PosturaAntiDemoc,data=df,
                   family=binomial(link=logit))
ModeloG <- glm(PG~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                 evangelico+catolico+Nenhum,data=df,
               family=binomial(link=logit))
tab_model(ModeloGfull, ModeloG)

ModeloIfull <- glm(PI~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                     evangelico+catolico+Nenhum+PosturaAntiDemoc,data=df,
                   family=binomial(link=logit))
ModeloI <- glm(PI~Escolaridade+Mulher+Idade+Renda+Branca+interesse+
                 evangelico+catolico+Nenhum,data=df,
               family=binomial(link=logit))
tab_model(ModeloIfull, ModeloI)



plot_models(ModeloAfull, ModeloBfull, ModeloFfull, ModeloGfull, ModeloIfull,
            vline.color = "orange", ci.lvl = 0.9)#só para conferir


a<- plot_models(ModeloAfull, ModeloBfull, ModeloFfull, ModeloGfull, ModeloIfull,
            vline.color = "orange",
            title="",
            legend.title = "Perfil",
            m.labels = c("Desaprova direito do PT",
                         "Desaprova direito do PL de Bolsonaro",
                         "Desaprova direito do PT, MDB e Centrão",
                         "Desaprova direito do MDB,Centrão e do PL de Bolsonaro",
                         "Desaprova direito de MDB, Centrão, PT e PL de Bolsonaro"),
            axis.labels=c("Indicador Postura Anti Democrática",
                          "Religião: Evangélico",
                          "Religião: Católico",
                          "Religião: Nenhum","Interesse por política",
                          "Raça Branca","Renda","Idade","Gênero Fem","Escolaridade"),
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, 
            colors=c(wes_palette("Darjeeling2")
            ))#

a + theme_bw()+theme( legend.title = element_text(color = "blue", size = 14),
                      legend.text = element_text(size = 12),axis.title.x = element_text(size = 14), 
                      axis.text.x = element_text(size=14), axis.text.y = element_text(size=16))+
  ggtitle("")+
  theme(plot.title = element_text(family="Georgia", colour="black", size=14)) #+
  #labs(caption= "cat. ref.: Outras Religiões")


library(marginaleffects)
#df$interesse_fac <- as.factor(df$interesse)
library(memisc)

df$interesse_fac <- memisc::recode(as.factor(df$interesse),
                                   "Muito interessado(a)" <- c(4),
                                   "Interessado(a)" <- c(3),
                                   "Pouco interessado(a)"<-c(2),
                                   "Nada interessado(a)"<-c(1))


table(df$interesse_fac)
table(df$interesse)
ModeloImodificado <- glm(PI~Escolaridade+Mulher+Idade+Renda+Branca+interesse_fac+
                 evangelico+catolico+Nenhum,data=df,
               family=binomial(link=logit))
b<-plot_cap(ModeloImodificado, condition = c("interesse_fac"), conf_level = .9)
b + labs(y="Probabilidades preditas de perfil I",
         x="Interesse por política")+theme_minimal()+
  theme( legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(size = 12),axis.title.x = element_text(size = 12), 
        axis.text.x = element_text(size=11), axis.text.y = element_text(size=12))
predictions(ModeloImodificado, newdata = datagrid(interesse_fac = "Muito interessado(a)"))
predictions(ModeloImodificado, newdata = datagrid(interesse_fac = "Nada interessado(a)"))

c <- plot_cap(ModeloAfull, condition = c("PosturaAntiDemoc"), conf_level = .9)
c <- c + labs(title = "                                                                A",
              y="Probabilidades preditas de perfil A",
              x="Postura AntiDemocrática")+theme_minimal() 
c 
d <-plot_cap(ModeloBfull, condition = c("PosturaAntiDemoc"), conf_level = .9) 
d <- d + labs(title = "                                                                B",
                y="Probabilidades preditas de perfil B",
                x="Postura AntiDemocrática")+theme_minimal() 
d
library(gridExtra)
grid.arrange(c,d)


predictions(ModeloAfull, newdata = datagrid(PosturaAntiDemoc = 0))
predictions(ModeloAfull, newdata = datagrid(PosturaAntiDemoc = 1))
predictions(ModeloBfull, newdata = datagrid(PosturaAntiDemoc = 0))
predictions(ModeloBfull, newdata = datagrid(PosturaAntiDemoc = 1))
#exploraçoes adicionais
plot_cap(ModeloAfull, condition = "evangelico", conf_level = .9)
plot_cap(ModeloBfull, condition = "evangelico", conf_level = .9)

predictions(mod, newdata = datagrid(am = 0, wt = seq(2, 3, .2)))
predictions(ModeloAfull, newdata = datagrid(PosturaAntiDemoc = 1,
                                            evangelico = T,
                                            catolico = F))
plot_cap(ModeloAfull, condition =c("PosturaAntiDemoc","evangelico"), conf_level = .9)

predictions(ModeloIfull, newdata = datagrid(interesse = 4,
                                            Mulher = T))
plot_cap(ModeloImodificado, condition =c("interesse_fac","Mulher"), conf_level = .9)

predictions(ModeloBfull, newdata = datagrid(PosturaAntiDemoc = seq(0,1,.25),
                                            Mulher = T))
plot_cap(ModeloBfull, condition =c("PosturaAntiDemoc","Mulher"), conf_level = .9)


#
str(df)
prop.table(table(df$Mulher))*100
str(df$Idade)
mean(df$Idade)
str(df$Renda)
prop.table(table(df$Renda))*100
17.4+25.5+36.7+13.2+5.3+1.9
prop.table(table(df$Nenhum))*100
prop.table(table(df$evangelico))*100
prop.table(table(df$catolico))*100
100-(15.06+17.46+50.33)
prop.table(table(df$Branca))*100
prop.table(table(df$interesse))
28.2+25.5+29.5+16.7
summary(df$PosturaAntiDemoc)

#
rm(list=ls())
library(tidyverse)
df <- read_csv("df.csv")
table(df$P56)
df$P56_recod_numeric_apoio_positivo <- df$P56
df$P56_recod_numeric_apoio_positivo[df$P56 == 4 | df$P56 == 5] <- NA
df$P56_recod_numeric_apoio_positivo[df$P56 == 1 | df$P56 == 2] <- 1
df$P56_recod_numeric_apoio_positivo[df$P56 == 3] <- 0
table(df$P56_recod_numeric_apoio_positivo)
#'f =~ P51_recod_numeric + P52_recod_numeric +
#' P53_recod_numeric + P54_recod_numeric + 
#' P55_recod_numeric + P56_recod_numeric_apoio_positivo +
#'  P58_recod_numeric'

df<-subset(df,select=c(P51,P52,P53,P54,P55,P56_recod_numeric_apoio_positivo,P58))
str(df)
summary(df)
df <- df %>%
  mutate_all(~ ifelse(. > 10, NA, .))

df <- df %>%
  mutate_all(as.factor)
summary(df)

prop.table(table(df$P56_recod_numeric_apoio_positivo))
df <- subset(df, select=-c(P56_recod_numeric_apoio_positivo))
# Função para calcular as porcentagens de cada categoria em relação ao total
calculate_percentages <- function(column) {
  category_counts <- table(column)
  category_percentages <- prop.table(category_counts) * 100
  category_percentages
}

# Calcular as porcentagens para cada coluna do dataframe
percent_table <- sapply(df, calculate_percentages)

# Converter os resultados em um dataframe
percent_df <- as.data.frame(percent_table)
percent_df
