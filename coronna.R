dados<-read.table('Documentos/Projetos GitHub/CoronaVirus/cv.csv', sep = ',', header = T)

head(dados)

dados<-dados[,c(-1,-5)]

head(dados)

summary(dados[,c(4,5,6)])

unique(dados[,3])


dados[which(dados[,3]=='Mainland China'),3]<-'China'

unique(dados[,3])

unique(dados[,1])
dados[,1]

library(lubridate)
d<- as.character(dados[,1])
d<-mdy_hms(d)
dados[,1] <- as.Date(d, "%d:%m:%Y")


library(ggplot2)
ggplot(dados, aes(Date, Confirmed )) + geom_col() +
  scale_x_date(breaks = unique(dados[,1])) +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0))



ggplot(dados, aes(Date, Deaths)) + geom_col() + geom_col(aes(Date, Recovered,color=3)) +
  scale_x_date(breaks = unique(dados[,1])) +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0))

dados %>%
  gather(tipo, qt, -Province.State, -Date,-Country) %>% ggplot(aes(Country, qt, fill=tipo)) + geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0)) 



ggplot(dados, aes(x=reorder(Country,-Confirmed),y=Confirmed)) + geom_bar(stat="identity")+
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0))



dados_china<-dados[which(dados[,3]=='China'),]

dados_china %>%
  gather(tipo, qt, -Province.State, -Date,-Country) %>% ggplot(aes(Country, qt, fill=tipo)) + geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0)) 




dados_brasil<-dados[which(dados[,3]=='Brazil'),]
dados
dados_china<-dados_china[dados_china$Confirmed>1,]
ggplot(dados_china, aes(x =reorder(Province.State, -Confirmed, desc = TRUE), Confirmed)) + geom_col() +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0))


























df <- data.frame(CID = c("A", "A", "B", "C", "C", "Z"),
                 AFASTAMENTOS = c(2,3,5,8,9, 12),
                 ATENDIMENTOS = c(21, 32, 4, 6, 7, 43),
                 stringsAsFactors = FALSE )

library(tidyr)
library(ggplot2)
df %>%
  gather(Procedimento, Qtd, - CID) %>%
  ggplot(aes(x = CID, y = Qtd, fill = Procedimento)) +
  geom_bar(stat = "identity", position = "dodge")
