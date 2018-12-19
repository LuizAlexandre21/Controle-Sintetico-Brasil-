########################Controle SIntetico Generalizado ###########################################
#######Pacotes #####################
library(gsynth)
library(readxl)
library(dplyr)
library(panelView)
require(Rcpp)
require(ggplot2)
require(GGally)
require(foreach)
require(doParallel)
require(abind)
######## Exportando os Dados 
local="/run/media/administrador/Alexandre 2/Monografia/base.xlsx"
data=read_xlsx(local)
data=as.data.frame(data)
data[is.na(data)]<-0
dados<-data%>%filter(Sigla %in% c('Chi','Chn','Bra','Col','Idn','Ind','Mal','Mex','Paq','Per','Fil','Tai','Tur','Afs') &Ano>=1970)
######## Visualizando os Resultados 
panelView(Crescimento~Tratamento,data=dados,index=c("ID","Ano"))
######## Controle Sintetico Generalizado 
sin<-gsynth(Crescimento~Tratamento+Inflaçao+`Consumo Total`+Poupança,data=dados,index=c("ID","Ano"),force="two-way",r=c(0,5),CV=TRUE,EM=TRUE,se=TRUE,nboots=1000,inference = "parametric",parallel=TRUE)
png("graficoa.png")
plot(sin)
dev.off()
png("graficob.png")
plot(sin,type="counterfactual",raw="band",xlab="Time",ylim=c(-2,18))
dev.off()
png("graficoc.png")
plot(sin,type="loadings")
dev.off()
outb1<-sin$est.beta
outat1<-sin$est.att
outav1<-sin$est.avg
outv1<-sin$wgt.implied
write.csv(outb1,"e.csv")
write.csv(outat1,"f.csv")
write.csv(outav1,"g.csv")
write.csv(outv1,"h.csv")
