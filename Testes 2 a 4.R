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


# EM SUMA
df$desaprovaSohDirPt -> df$A
df$desaprovaSohDirPLBOLSO -> df$B
df$desaprova_DirCentrao_e_MDB_e_PT -> df$F
df$desaprova_DirCentrao_e_MDB_e_PLBOLSO -> df$G
df$desaprova_os4 -> df$I


# ideia - modelos de regressão logística
# primeiro recodificar e renomear as variáveis indep

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

table(df$P57)
df$desinteresse <- df$P57
df$desinteresse[df$desinteresse  == 5] <- NA
df$desinteresse[df$desinteresse  == 6] <- NA

#descritiva
table(df$P62)
df$primeiroturno <- memisc::recode(as.factor(df$P62), "Bolsonaro" <- c(4),
                                   "Lula" <- c(6),
                                   "Outros"<-c(1,2,3,5,7,8,9,10,11),
                                   "Abstenção/Nulo/Branco"<-c(12,13,14,15))
table(df$primeiroturno)
#Abstenção/Nuolo/Branco como categoria de referência
df$primeiroturno <- relevel(df$primeiroturno, "Abstenção/Nulo/Branco")


Modelo1A <- glm(A~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse,data=df,
                family=binomial(link=logit))
Modelo2A <- glm(A~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse+primeiroturno,data=df,
                family=binomial(link=logit))

tab_model(Modelo1A, Modelo2A,  p.style = "stars")


Modelo1B <- glm(B~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse,data=df,
                family=binomial(link=logit))
Modelo2B <- glm(B~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse+primeiroturno,data=df,
                family=binomial(link=logit))

tab_model(Modelo1B, Modelo2B,p.style = "stars")


Modelo1F <- glm(F~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse,data=df,
                family=binomial(link=logit))
Modelo2F <- glm(F~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse+primeiroturno,data=df,
                family=binomial(link=logit))

tab_model(Modelo1F, Modelo2F,p.style = "stars")

Modelo1G <- glm(G~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse,data=df,
                family=binomial(link=logit))
Modelo2G <- glm(G~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse+primeiroturno,data=df,
                family=binomial(link=logit))

tab_model(Modelo1G, Modelo2G,p.style = "stars")

Modelo1I <- glm(I~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse,data=df,
                family=binomial(link=logit))
Modelo2I <- glm(I~Escolaridade+Mulher+Idade+Renda+Branca+desinteresse+primeiroturno,data=df,
                family=binomial(link=logit))

tab_model(Modelo1I, Modelo2I,p.style = "stars")

#Gráfico comparando modelos de tipo 2
plot_models(Modelo2A, Modelo2B, Modelo2F, Modelo2G, Modelo2I,
            vline.color = "orange", ci.lvl = 0.9)#só para conferir


plot_models(Modelo2A, Modelo2B, Modelo2F, Modelo2G, Modelo2I,
            vline.color = "orange",
            title="",
            legend.title = "Perfil",m.labels = c("A","B","F","G","I"),
            axis.labels=c("Outros (cat. ref. Abstenção/Nulo/Branco)",
                          "Lula (cat. ref. Abstenção/Nulo/Branco)",
                          "Bolsonaro (cat. ref. Abstenção/Nulo/Branco)",
                          "Desinteresse por Política",
                          "Raça Branca","Renda","Idade","Gênero","Escolaridade"),
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, 
            colors=c(wes_palette("Cavalcanti1")
            ))#


p2 <- plot_models(Modelo2A, Modelo2B, Modelo2F, Modelo2G, Modelo2I,
                  vline.color = "orange",
                  title="",
                  legend.title = "Perfil",m.labels = c("Desaprova direito do PT",
                                                       "Desaprova direito do PL de Bolsonaro",
                                                       "Desaprova direito do PT, MDB e Centrão",
                                                       "Desaprova direito do MDB,Centrão e do PL de Bolsonaro",
                                                       "Desaprova direito de MDB, Centrão, PT e PL de Bolsonaro"),
                  rm.terms=c("Escolaridade", "Idade","Renda","Idade","desinteresse", "BrancaTRUE"),
                  axis.labels=c("Outros (cat. ref. Abstenção/Nulo/Branco)",
                                "Lula (cat. ref. Abstenção/Nulo/Branco)",
                                "Bolsonaro (cat. ref. Abstenção/Nulo/Branco)","Gênero"),
                  std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, 
                  colors=c(wes_palette("Darjeeling2")
                  ))#
p2 + labs(title = "Determinantes da opinião sobre direito de concorrer à presidência, Brasil 2023",
          caption = "Obs: Escolaridade, Idade, Renda, Idade, Interesse por Política e Cor/Raça omitidos do gráfico. 
          Os modelos completos estão disponíveis no Apêndice.")+
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
        plot.caption = element_text(size=9, hjust=0.5))


efA <- plot_cap(Modelo2A, condition = "primeiroturno", conf_level =.9)
efA + labs(title= "Probabilidades de rejeição ao direito do PT de concorrer à presidência",
           subtitle = "Dado pela votação no primeiro turno de 2022",
           caption="Fonte: Base de dados de survey do projeto As bases das clivagens políticas no Brasil",
           y="",x="Voto primeiro turno 2022")



efB <- plot_cap(Modelo2B, condition = "primeiroturno", conf_level =.9)
efB
efB + labs(title= "Figura 4-Probabilidades de rejeição ao direito do PL de Bolsonaro de concorrer à presidência",
           subtitle = "Dado pela votação no primeiro turno de 2022",
           caption="Fonte: Base de dados de survey do projeto As bases das clivagens políticas no Brasil",
           y="",x="Voto primeiro turno 2022")


efF <- plot_cap(Modelo2F, condition = "primeiroturno", conf_level =.9)
efF
efF + labs(title= "Probabilidades de rejeição ao direito do PT,MDB e Centrão de concorrer à presidência",
           subtitle = "Dado pela votação no primeiro turno de 2022",
           caption="Fonte: Base de dados de survey do projeto As bases das clivagens políticas no Brasil",
           y="",x="Voto primeiro turno 2022")


efG <- plot_cap(Modelo2G, condition = "primeiroturno", conf_level =.9)
efG
efG + labs(title= "Probabilidades de rejeição ao direito do PL do Bolsonaro,MDB e
           Centrão de concorrer à presidência",
           subtitle = "Dado pela votação no primeiro turno de 2022",
           caption="Fonte: Base de dados de survey do projeto As bases das clivagens políticas no Brasil",
           y="",x="Voto primeiro turno 2022")


efI <- plot_cap(Modelo2I, condition = "primeiroturno", conf_level =.9)
efI
efI + labs(title= "Probabilidades de rejeição ao direito do PT, PL do Bolsonaro,MDB e
           Centrão de concorrer à presidência",
           subtitle = "Dado pela votação no primeiro turno de 2022",
           caption="Fonte: Base de dados de survey do projeto As bases das clivagens políticas no Brasil",
           y="",x="Voto primeiro turno 2022") + theme_bw()

efI + labs(y="",x="") + theme_bw()

efA <- efA +labs(title= "Perfil A",
                 x="",y="")
efB <- efB +labs(title= "Perfil B",
                 x="",y="")
efF <- efF +labs(title= "Perfil F",
                 x="",y="")
efG <- efG +labs(title= "Perfil G",
                 x="",y="")
library(gridExtra)
grid.arrange(efA,efB,efF,efG,nrow=2)

predictions(Modelo2I, newdata = datagrid(primeiroturno = "Bolsonaro"),
            conf_level = .9)#
predictions(Modelo2I, newdata = datagrid(primeiroturno = "Outros"),
            conf_level = .9)#
predictions(Modelo2I, newdata = datagrid(primeiroturno = "Lula"),
            conf_level = .9)#
predictions(Modelo2I, newdata = datagrid(primeiroturno = "Abstenção/Nulo/Branco"),
            conf_level = .9)#


efIb <- plot_cap(Modelo2I, condition =c ("primeiroturno","Mulher"), conf_level =.9)
efIb + labs(y="",x="") + theme_bw()

predictions(Modelo2I, newdata = datagrid(primeiroturno = "Bolsonaro", Mulher = FALSE),
            conf_level = .9)#
predictions(Modelo2I, newdata = datagrid(primeiroturno = "Lula", Mulher = TRUE),
            conf_level = .9)#
predictions(Modelo2I, newdata = datagrid(primeiroturno = "Abstenção/Nulo/Branco", Mulher=FALSE),
            conf_level = .9)#
