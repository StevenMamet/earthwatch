setwd("~/Desktop/Workspace/MacPass")

###

rm(list = ls())
DD<-read.csv('D6.csv',skip=2)

DD$Season<-NA
DD$Season[DD$Month%in%c(12,1,2)]<-'S1'
DD$Season[DD$Month%in%c(3,4,5)]<-'S2'
DD$Season[DD$Month%in%c(6,7,8)]<-'S3'
DD$Season[DD$Month%in%c(9,10,11)]<-'S4'
DD$Season<-as.factor(DD$Season)

############
######################

jan.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==1,])
feb.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==2,])
mar.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==3,])
apr.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==4,])
may.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==5,])
jun.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==6,])
jul.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==7,])
aug.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==8,])
sep.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==9,])
oct.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==10,])
nov.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==11,])
dec.bp <- lm(P150 ~ P5_0 + P2_5 + P0_0 + PT150, data = DD[DD$Month==12,])


summary(jan.bp)
summary(feb.bp)
summary(mar.bp)
summary(apr.bp)
summary(may.bp)
summary(jun.bp)
summary(jul.bp)
summary(aug.bp)
summary(sep.bp)
summary(oct.bp)
summary(nov.bp)
summary(dec.bp)
coef(jan.bp)
coef(feb.bp)
coef(mar.bp)
coef(apr.bp)
coef(may.bp)
coef(jun.bp)
coef(jul.bp)
coef(aug.bp)
coef(sep.bp)
coef(oct.bp)
coef(nov.bp)
coef(dec.bp)

######for year 2004###################
par(mfrow=c(2,2))
plot(DD[DD$Year==2004,c('PTg150','PT5_0')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -5 vs 150 above ground')
plot(DD[DD$Year==2004,c('PTg150','PT150')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -150 vs 150 above ground')
###
plot(DD[DD$Year==2004,c('PTg150','F5_0')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Fen -5 vs 150 above ground')
plot(DD[DD$Year==2004,c('PTg150','F150')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Fen -150 vs 150 above ground')
###################


XX<-pi*seq(0,3,len=100)
plot(XX,sin(XX))
lines(XX,cos(XX))

plot(sin(XX),cos(XX-pi/3))


######for year 2004 Fen vs Palsa###################
par(mfrow=c(2,2))
plot(DD[DD$Year==2004,c('F5_0','PT5_0')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -5 vs Fen -5')
plot(DD[DD$Year==2004,c('F5_0','PT150')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -150 vs Fen -5')
###
plot(DD[DD$Year==2004,c('F150','PT5_0')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -5 vs Fen -150')
plot(DD[DD$Year==2004,c('F150','PT150')],pch=18,col=c(1,3,2,4)[DD[DD$Year==2004,c('Season')]])
title('Palsa -150 vs Fen -150')
######################



############
######################
NDD<-DD
NDD$tBP5<-NDD$BP5>0
NDD$tPTg150<-NDD$PTg150>0
NDD$tPT5_0<-NDD$PT5_0>0
NDD$tPT150<-NDD$PT150>0

NDD$ind5<-paste(NDD$tPT5_0,NDD$tBP5)
NDD$ind150<-paste(NDD$tPT150,NDD$tPTg150)

NDD$ind5[is.na(apply(cbind(NDD$tPT5_0,NDD$tBP5),1,sum))]<-NA
NDD$ind150[is.na(apply(cbind(NDD$tPT150,NDD$tPTg150),1,sum))]<-NA


NDD$ind5col<-NA
NDD$ind5col[which(NDD$ind5=='TRUE TRUE')]<-'red'
NDD$ind5col[which(NDD$ind5=='TRUE FALSE')]<-'orange'
NDD$ind5col[which(NDD$ind5=='FALSE TRUE')]<-'grey'
NDD$ind5col[which(NDD$ind5=='FALSE FALSE')]<-'black'

NDD$ind150col<-NA
NDD$ind150col[which(NDD$ind150=='TRUE TRUE')]<-'red'
NDD$ind150col[which(NDD$ind150=='TRUE FALSE')]<-'orange'
NDD$ind150col[which(NDD$ind150=='FALSE TRUE')]<-'grey'
NDD$ind150col[which(NDD$ind150=='FALSE FALSE')]<-'black'


par(mfrow=c(1,1))
plot(NDD$PT5_0,NDD$BP5,pch=18,col=NDD$ind5col,xlab='Pulsa -5',ylab='Fen -5')
abline(h=0,lwd=3)
abline(v=0,lwd=3)

par(mfrow=c(1,1))
plot(NDD$PT150,NDD$PTg150,pch=18,col=NDD$ind150col,xlab='Pulsa -150',ylab='Fen -150')
abline(h=0,lwd=3)
abline(v=0,lwd=3)


