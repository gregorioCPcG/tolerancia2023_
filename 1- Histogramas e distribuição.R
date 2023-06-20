library(tidyverse)
library(sjPlot)
library(wesanderson)
library(memisc)
library(huxtable)
df <- read_csv("df.csv")
# 0 é desaprova , 10 aprova, 11 e 12 NA
table(df$P22)#PT
table(df$P23)#MDB
table(df$P24)#PL do Bolsonaro
table(df$P25)#Partidos do Centrão

# Crie um vetor com os rótulos personalizados para cada nível
labels <- c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "NC/NS", "NR")

# Transforme a variável P22 em um fator e atribua os rótulos personalizados aos níveis
df$PT <- factor(df$P22, levels = 0:12, labels = labels)
table(df$PT)
summary(df$PT)

df$MDB <- factor(df$P23, levels = 0:12, labels = labels)
table(df$MDB)


df$PL_Bolsonaro<- factor(df$P24, levels = 0:12, labels = labels)
table(df$PL_Bolsonaro)


df$Centrao<- factor(df$P25, levels = 0:12, labels = labels)
table(df$Centrao)
summary(df$PT)



df$cont <- 1
cores <- c(rep("blue", 11), "black", "gray")


PT <- ggplot(df, aes(PT,cont)) + geom_bar(stat = "identity")
PT#gráfico secão, só para PT para conferência

# Transforma os valores em porcentagem
prop.table(table(df$PT))*100 -> PT_percent
PT_percent <- as.data.frame(PT_percent)
PT_percent$cores <- cores
PT_plot <- ggplot(PT_percent, aes(Var1, Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  ylab("Porcentagem") +
  xlab("PT") +
  labs(title="PT", y="%", x="") +
  scale_fill_manual(values = c("blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue",
                               "black","grey")) +
  guides(fill = FALSE) + theme_classic() +
  theme(axis.title.y = element_text(size = 16, family = "sans"),
        plot.title = element_text(size = 21, family = "sans",
                                  face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 40))


# Exibe o gráfico
PT_plot 

prop.table(table(df$MDB))*100 -> MDB_percent
MDB_percent <- as.data.frame(MDB_percent)
MDB_percent$cores <- cores
MDB_plot <- ggplot(MDB_percent, aes(Var1, Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  ylab("Porcentagem") +
  xlab("MDB") +
  labs(title="MDB", y="%", x="") +
  scale_fill_manual(values = c("blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue",
                               "black","grey")) +
  guides(fill = FALSE) + theme_classic() +
  theme(axis.title.y = element_text(size = 16, family = "sans"),
        plot.title = element_text(size = 21, family = "sans",
                                  face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 40))


# Exibe o gráfico
MDB_plot 

prop.table(table(df$Centrao))*100 -> Centrao_percent
Centrao_percent <- as.data.frame(Centrao_percent)
Centrao_percent$cores <- cores
Centrao_plot <- ggplot(Centrao_percent, aes(Var1, Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  ylab("Porcentagem") +
  xlab("Partidos do Centrão") +
  labs(title="Partidos do Centrão", y="%", x="") +
  scale_fill_manual(values = c("blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue",
                               "black","grey")) +
  guides(fill = FALSE) + theme_classic() +
  theme(axis.title.y = element_text(size = 16, family = "sans"),
        plot.title = element_text(size = 21, family = "sans",
                                  face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 40))


# Exibe o gráfico
Centrao_plot 


prop.table(table(df$PL_Bolsonaro))*100 -> PL_Bolsonaro_percent
PL_Bolsonaro_percent <- as.data.frame(PL_Bolsonaro_percent)
PL_Bolsonaro_percent$cores <- cores
PL_Bolsonaro_plot <- ggplot(PL_Bolsonaro_percent, aes(Var1, Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  ylab("Porcentagem") +
  xlab("PL_Bolsonaro") +
  labs(title="PL do Bolsonaro", y="%", x="") +
  scale_fill_manual(values = c("blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue","blue",
                               "blue",
                               "black","grey")) +
  guides(fill = FALSE) + theme_classic() +
  theme(axis.title.y = element_text(size = 16, family = "sans"),
        plot.title = element_text(size = 21, family = "sans",
                                  face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 40))


# Exibe o gráfico
PL_Bolsonaro_plot 


library(gridExtra)
grid.arrange(PT_plot,PL_Bolsonaro_plot, MDB_plot,
             Centrao_plot, nrow=2)


#means e NA´S

df$P22recod <- ifelse(df$P22 > 10, NA, df$P22)
df$P23recod <- ifelse(df$P23 > 10, NA, df$P23)
df$P24recod <- ifelse(df$P24 > 10, NA, df$P24)
df$P25recod <- ifelse(df$P25 > 10, NA, df$P25)

summary(df$P22recod)
summary(df$P23recod)
summary(df$P24recod)
summary(df$P25recod)




mean(df$P22recod, na.rm = TRUE)
mean(df$P23recod, na.rm = TRUE)
mean(df$P24recod, na.rm = TRUE)
mean(df$P25recod, na.rm = TRUE)


sd(df$P22recod,na.rm = TRUE)
sd(df$P23recod,na.rm = TRUE)
sd(df$P24recod,na.rm = TRUE)
sd(df$P25recod,na.rm = TRUE)


temp <- subset(df,select=c(P22recod,P23recod,P24recod,P25recod) )
library(ggdist)
library(tidyquant)

# Transformar as variáveis em uma coluna "Pergunta" e "Resposta"
new_temp<- temp %>%
  pivot_longer(cols = starts_with("P"), names_to = "Pergunta", values_to = "Resposta")

# Verificar o novo data frame
print(new_temp)
new_temp %>%
  ggplot(aes(x = Pergunta, y = Resposta, fill = Pergunta)) +
  geom_boxplot(width = -0.12, outlier.colour = NA, alpha = 0.5,
               size = 1) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -0.2, .width = 0, point_colour = 1) +
  scale_fill_manual(values = c("blue", "orange", "green", "red")) +
  #geom_errorbar(ymin=me)+
  theme_minimal() +
  labs(title = "", subtitle = "", x = "", y = "", fill = "")+
  coord_flip()#
median(temp$P22recod, na.rm = T)#PT
median(temp$P23recod, na.rm=T)#MDB
median(temp$P24recod, na.rm=T)#PL
median(temp$P25recod, na.rm=T
       )

