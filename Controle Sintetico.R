################################ Controle Sintetico - Crise Economica do Brasil 
######## Pacotes 
library(Synth)
library(readxl)
library(dplyr)

######## Exportando os Dados 
local="/run/media/administrador/Alexandre 2/Monografia/base.xlsx"
data=read_xlsx(local)
data=as.data.frame(data)
data
data[is.na(data)]<-0
data

######## Filtrando os dados 
dados<-data%>%filter(Sigla %in% c('Chi','Chn','Bra','Col','Idn','Ind','Mal','Mex','Paq','Per','Fil','Tai','Tur','Afs') &Ano>=1970)
summary(dados$Pais)
######## Controle Sintetico 
sint1<-dataprep(foo=dados,predictors=c('Inflaçao','Consumo Total','Poupança'),predictors.op="mean",dependent='Crescimento',unit.variable='ID',time.variable='Ano',treatment.identifier=c(4),controls.identifier=c(2,3,5,7,8,10,11,12,13,14,16,17,20),unit.names.variable="Sigla",time.predictors.prior=(1970:2012),time.optimize.ssr=c(1970:2017),time.plot=c(1970:2017))
Cont_sint1<-synth(data.prep.obj = sint1,optimxmethod = "BFGS")
png("grafico1.png")
path.plot(dataprep.res = sint1,synth.res = Cont_sint1)
dev.off()
Cont_tab1<-synth.tab(dataprep.res = sint1,synth.res = Cont_sint1)
write.csv(Cont_tab1$tab.pred,"a.csv")
write.csv(Cont_tab1$tab.v,"b.csv")
write.csv(Cont_tab1$tab.w,"c.csv")
write.csv(Cont_tab1$tab.loss,"d.csv")
