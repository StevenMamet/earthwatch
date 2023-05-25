# Using nlme and accounting for temporal autocorrelation

thaw.ARMA <- lme(thaw ~ shrub + density + TI_0 + TI150 + FI_0 + FI150 + CST150 + WST150 + CST0 + WST0 +
                   WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                 correlation=corARMA(form=~year|site/line, p=3, q=0))

# p=1, q=0
summary(thaw.ARMA) # AIC = -42.51654
# p=0, q=1
summary(thaw.ARMA) # AIC = 14.83518
# p=0, q=2
summary(thaw.ARMA) # AIC = -5.640376
# p=0, q=3
summary(thaw.ARMA) # AIC = -44.22766
# p=1, q=3
summary(thaw.ARMA) # AIC = -88.23438
# p=2, q=3
summary(thaw.ARMA) # AIC = -87.35964
# p=3, q=3 -- failed to converge
# p=3, q=2
summary(thaw.ARMA) # AIC = -88.24794
# p=3, q=1
summary(thaw.ARMA) # AIC = -90.12602
# p=2, q=1
summary(thaw.ARMA) # AIC = -86.59231
# p=2, q=2
summary(thaw.ARMA) # AIC = -85.04451
# p=1, q=2
summary(thaw.ARMA) # AIC = -86.79874
# p=0, q=0 -- doesn't work
# p=3, q=0
summary(thaw.ARMA) # AIC = -91.7998
# p=2, q=0
summary(thaw.ARMA) # AIC = -75.32896
# p=1, q=1
summary(thaw.ARMA) # AIC = -88.18301


M0 <- gls(thaw ~ shrub + density + TI_0 + TI150 + FI_0 + FI150 + CST150 +
            WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU,
          na.action = na.omit, data = logit.mp2)
summary(M0)
E <- residuals(M0, type = "normalized")
acf(E, na.action = na.pass,
    main = "Auto-correlation plot for residuals")
E2 <- residuals(thaw.ARMA, type = "normalized")
acf(E2, na.action = na.pass,
    main = "Auto-correlation plot for residuals")

thaw.shrub <- lme(thaw ~ density + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                    WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                  correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.density <- lme(thaw ~ shrub   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                      WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                    correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.ti0 <- lme(thaw ~ shrub + density   + TI150 + FI_0 + FI150 + CST150 +
                  WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.ti150 <- lme(thaw ~ shrub + density   + TI_0 + FI_0 + FI150 + CST150 +
                    WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                  correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.fi0 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI150 + CST150 +
                  WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.fi150 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + CST150 +
                    WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                  correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.cst150 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 +
                     WST150 + CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                   correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.wst150 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                     CST0 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                   correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.cst0 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                   WST150 + WST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                 correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.wst0 <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                   WST150 + CST0 + WSP_CRU + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                 correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.wsp <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                  WST150 + CST0 + WST0 + CSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.csp <- lme(thaw ~ shrub + density   + TI_0 + TI150 + FI_0 + FI150 + CST150 +
                  WST150 + CST0 + WST0 + WSP_CRU, method = "ML", random = ~1|site/line, data = logit.mp2,
                correlation=corARMA(form=~year|site/line, p=3, q=0))

summary(thaw.final)
aov(thaw.final)

thaw.final <- lme(thaw ~ density + CST150 + WST150 + CST0 + WST0, method = "ML", 
                  random = ~1|site/line, data = logit.mp2,
                  correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.shrub <- lme(thaw ~ density + CST150 + WST150 + CST0 + WST0, method = "ML", 
                        random = ~1|site/line, data = logit.mp2,
                        correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.density <- lme(thaw ~ CST150 + WST150 + CST0 + WST0, method = "ML", 
                          random = ~1|site/line, data = logit.mp2,
                          correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.cst150 <- lme(thaw ~ density + WST150 + CST0 + WST0, method = "ML", 
                         random = ~1|site/line, data = logit.mp2,
                         correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.wst150 <- lme(thaw ~ density + CST150 + CST0 + WST0, method = "ML", 
                         random = ~1|site/line, data = logit.mp2,
                         correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.cst0 <- lme(thaw ~ density + CST150 + WST150 + WST0, method = "ML", 
                       random = ~1|site/line, data = logit.mp2,
                       correlation=corARMA(form=~year|site/line, p=3, q=0))
thaw.final.wst0 <- lme(thaw ~ density + CST150 + WST150 + CST0, method = "ML", 
                       random = ~1|site/line, data = logit.mp2,
                       correlation=corARMA(form=~year|site/line, p=3, q=0))

summary(thaw.final)
vif(thaw.final)

anova(thaw.final, thaw.final.shrub) # Not sig
anova(thaw.final, thaw.final.density) # Sig
anova(thaw.final, thaw.final.wst150) # Sig
anova(thaw.final, thaw.final.cst150) # Sig
anova(thaw.final, thaw.final.cst0) # Sig
anova(thaw.final, thaw.final.wst0) # Sig

vif(thaw.ARMA)
anova(thaw.ARMA, thaw.shrub) # Not sig
anova(thaw.ARMA, thaw.density) # Sig
anova(thaw.ARMA, thaw.ti0) # Not sig
anova(thaw.ARMA, thaw.ti150) # Not sig
anova(thaw.ARMA, thaw.fi0) # Not sig
anova(thaw.ARMA, thaw.fi150) # Not sig
anova(thaw.ARMA, thaw.cst150) # Sig
anova(thaw.ARMA, thaw.wst150) # Sig
anova(thaw.ARMA, thaw.cst0) # Sig
anova(thaw.ARMA, thaw.wst0) # Sig
anova(thaw.ARMA, thaw.wsp) # Not sig
anova(thaw.ARMA, thaw.csp) # Not sig

plot(thaw.final,resid(.,type="p")~fitted(.)|line)
qqnorm(thaw.final,~resid(.)|line)



######for year 2004###################

par(mfrow=c(2,2))
plot(BP[,c('PT150','P150')],pch=18,col=c(1,3)[BP[,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1))

plot(BP[BP$YEAR==1993,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1993,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==1996,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1996,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==1997,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1997,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==1998,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1998,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==1999,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1999,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2000,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2000,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2003,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2003,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2004,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2004,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2005,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2005,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2006,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2006,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2007,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2007,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1), xaxt = "n", yaxt = "n")
par(new=T)
plot(BP[BP$YEAR==2014,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2014,c('Season')]],xlab = "",ylab="", xlim = c(-50,20), ylim = c(-5,1))


plot(BP[BP$YEAR==1992,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1992,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1993,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1993,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1994,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1994,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1995,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1995,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1996,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1996,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1997,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1997,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1998,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1998,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==1999,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==1999,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2000,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2000,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2001,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2001,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2002,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2002,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2003,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2003,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2004,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2004,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2005,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2005,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2006,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2006,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2007,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2007,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2008,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2008,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2009,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2009,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2010,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2010,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2011,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2011,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2012,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2012,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2013,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2013,c('Season')]],xlab = "",ylab="")
plot(BP[BP$YEAR==2014,c('PT150','P150')],pch=18,col=c(1,3)[BP[BP$YEAR==2014,c('Season')]],xlab = "",ylab="")


###################


XX<-pi*seq(0,3,len=100)
plot(XX,sin(XX))
lines(XX,cos(XX))

plot(sin(XX),cos(XX-pi/3))




############
######################
NDD<-DD
NDD$tP5_0<-NDD$P5_0>0
NDD$tPT150<-NDD$PT150>0
NDD$tP150<-NDD$P150>0

NDD$ind5<-paste(NDD$tPT5_0,NDD$tBP5)
NDD$ind150<-paste(NDD$tP150,NDD$tPT150)

NDD$ind5[is.na(apply(cbind(NDD$tPT5_0,NDD$tBP5),1,sum))]<-NA
NDD$ind150[is.na(apply(cbind(NDD$tP150,NDD$tPT150),1,sum))]<-NA


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
plot(NDD$P150,NDD$PT150,pch=18,col=NDD$ind150col,xlab='Palsa -150',ylab='Fen -150')
abline(h=0,lwd=3)
abline(v=0,lwd=3)

# Random forest analyses for data reduction

set.seed(500)

fit.mp <- randomForest(thaw ~ elev + height + shrub + organic + TI_0 + TI5_0 + TI150 + FI_0 +
                         FI5_0 + FI150 + CST150 + WST150 + CST0 + WST0 + CST_150 + WST_150 + CSP_CRU + WSP_CRU, 
                       data=logit.mp, importance=TRUE, ntree=200000)
varImpPlot(fit.mp)#, type =2)

glm.full <- glm(thaw ~ ., family = gaussian(link = "identity"), data = logit.mp2)
glm.full <- glm(thaw ~ shrub + TI_0
                + TI5_0 + TI150 + FI_0 + FI5_0 + FI150 + 
                  CST150 + WST150 + CST0 + WST0 + CST_150 + WST_150 + CSP_CRU + WSP_CRU, 
                family = gaussian(link = "identity"), data = logit.mp2)
summary(glm.full)
vif(glm.full)
aov.full <- aov(glm.full)
summary(aov.full)


glm.full <- glm(thaw ~ TI_0 + TI5_0 + TI150 + FI_0 + FI5_0 + FI150 +
                  CST150 + WST150 + CST0 + WST0 + CST_150 + WST_150 + CSP_CRU + WSP_CRU,
                family = gaussian(link = "identity"), data = logit.mp)
summary(glm.full)
vif(glm.full)


cor.test(logit.mp$organic, logit.mp$shrub)
cor.test(logit.mp$shrub, logit.mp$elev)

step.full <- step(glm.full, direction = "both")
summary(step.full)
coef(step.full)
vif(step.full)
aov.step <- aov(step.full)
summary(aov.step)

qqnorm(residuals(step.full))
qqline(residuals(step.full),probs=c(0.25,0.75))
hist(residuals(step.full))
plot(fitted(step.full),residuals(step.full))


## Check the change in AIC when each variable is removed

m2 <- update(step.full, ~.-shrub)
summary(m2)
m3 <- update(step.full, ~.-TI5_0)
summary(m3)
m4 <- update(step.full, ~.-WST150)
summary(m4)
m5 <- update(step.full, ~.-CST0)
summary(m5)
m6 <- update(step.full, ~.-WST0)
summary(m6)
m7 <- update(step.full, ~.-WST_150)
summary(m7)
#m8 <- update(step.full, ~.-WST0)
#summary(m8)
#m9 <- update(step.full, ~.-WST_150)
#summary(m9)
#m10 <- update(step.full, ~.-WSP_CRU)
#summary(m10)



# The data are normal - used a shapiro test to check this
shapiro.test(aov.full)

glm.bp <- glm(thaw ~ TI5_0 + WST150 + CST0 + WST0 + WST_150, 
              family = gaussian(link = "identity"), data = logit.mp2[logit.mp2$site=='BP',c(1:28)])
summary(glm.bp)
vif(glm.bp)
step.bp  <- step(glm.bp, direction = "both")
summary(step.bp)
vif(step.bp)
cor.test(logit.mp2[logit.mp2$site=='BP',4],fitted(step.bp))
aov.bp <- aov(step.bp)
summary(aov.bp)
qqnorm(residuals(step.bp))
qqline(residuals(step.bp),probs=c(0.25,0.75))
hist(residuals(step.bp))
plot(fitted(step.bp),residuals(step.bp))

glm.hf <- glm(thaw ~ TI5_0 + WST150 + CST0 + WST0 + WST_150, 
              family = gaussian(link = "identity"), data = logit.mp2[logit.mp2$site=='HF',c(1:28)])
summary(glm.hf)
vif(glm.hf)
step.hf  <- step(glm.hf, direction = "both")
summary(step.hf)
vif(step.hf)
cor.test(logit.mp2[logit.mp2$site=='HF',4],fitted(step.hf))
aov.hf <- aov(step.hf)
summary(aov.hf)
qqnorm(residuals(step.hf))
qqline(residuals(step.hf),probs=c(0.25,0.75))
hist(residuals(step.hf))
plot(fitted(step.hf),residuals(step.hf))

glm.d2 <- glm(thaw ~ TI5_0 + WST150 + CST0 + WST0 + WST_150, 
              family = gaussian(link = "identity"), data = logit.mp2[logit.mp2$site=='D2',c(1:28)])
summary(glm.d2)
vif(glm.d2)
step.d2  <- step(glm.d2, direction = "both")
summary(step.d2)
vif(step.d2)
cor.test(logit.mp2[logit.mp2$site=='D2',4],fitted(step.d2))
aov.d2 <- aov(step.d2)
summary(aov.d2)
qqnorm(residuals(step.d2))
qqline(residuals(step.d2),probs=c(0.25,0.75))
hist(residuals(step.d2))
plot(fitted(step.d2),residuals(step.d2))

glm.d6 <- glm(log(thaw) ~ TI5_0 + WST150 + CST0 + WST0 + WST_150, 
              family = gaussian(link = "identity"), data = logit.mp2[logit.mp2$site=='D6',c(1:28)])
summary(glm.d6)
vif(glm.d6)
cor.test(logit.mp2[logit.mp2$site=='D6',4],fitted(step.d6))
step.d6  <- step(glm.d6, direction = "both")
summary(step.d6)
vif(step.d6)
aov.d6 <- aov(step.d6)
summary(aov.d6)
qqnorm(residuals(step.d6))
qqline(residuals(step.d6),probs=c(0.25,0.75))
hist(residuals(step.d6))
plot(fitted(step.d6),residuals(step.d6))

glm.gf <- glm(thaw ~ TI5_0 + WST150 + CST0 + WST0 + WST_150, 
              family = gaussian(link = "identity"), data = logit.mp2[logit.mp2$site=='GF',c(1:28)])
summary(glm.gf)
vif(glm.gf)
step.gf  <- step(glm.gf, direction = "both")
summary(step.gf)
vif(step.gf)
cor.test(logit.mp[logit.mp$site=='GF',21],fitted(step.gf))
aov.gf <- aov(step.gf)
summary(aov.gf)
qqnorm(residuals(step.gf))
qqline(residuals(step.gf),probs=c(0.25,0.75))
hist(residuals(step.gf))
plot(fitted(step.gf),residuals(step.gf))


#####

## Export at 5 x 8
par(mfrow = c(5, 1))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 10 pts
par(mar = c(1, 2, 1.5, 1), oma = c(2.5,1,0,1))

plot(logit.mp[logit.mp$site=='HF',1],-1*exp(fitted(step.hf)), xlim=c(1990,2015), ylim = c(-220,-180),type="l",xlab = "",ylab="",col = 3)
par(new = T)
plot(logit.mp[logit.mp$site=='HF',1], -1*exp(logit.mp2[logit.mp$site=='HF',10]), xlim=c(1990,2015), ylim = c(-220,-180), type="l",xlab = "",ylab="")
legend("topleft", "(a) HF (1260 m.a.s.l.)", bty = "n", inset = c(-0.02,-0.35))

plot(logit.mp[logit.mp$site=='BP',8],-1*fitted(step.bp),xlim=c(1990,2015),ylim = c(-70,-30),type="l",xlab = "",ylab="",col = 3)
par(new = T)
plot(logit.mp[logit.mp$site=='BP',8], -1*logit.mp[logit.mp$site=='BP',21], xlim=c(1990,2015),ylim = c(-70,-30),type="l",xlab = "",ylab="")
legend("topleft", "(b) BP (1272 m.a.s.l.)", bty = "n", inset = c(-0.02,-0.35))

plot(logit.mp[logit.mp$site=='D2',8],-1*fitted(step.d2),xlim=c(1990,2015),ylim = c(-65,-30),type="l",xlab = "",ylab="",col = 3)
par(new = T)
plot(logit.mp[logit.mp$site=='D2',8], -1*logit.mp[logit.mp$site=='D2',21], xlim=c(1990,2015),ylim = c(-65,-30),type="l",xlab = "",ylab="")
legend("topleft", "(c) D2 (1477 m.a.s.l.)", bty = "n", inset = c(-0.02,-0.35))

plot(logit.mp[logit.mp$site=='D6',8],-1*fitted(step.d6),xlim=c(1990,2015),ylim = c(-100,-60),type="l",xlab = "",ylab="",col = 3)
par(new = T)
plot(logit.mp[logit.mp$site=='D6',8], -1*logit.mp[logit.mp$site=='D6',21], xlim=c(1990,2015),ylim = c(-100,-60),type="l",xlab = "",ylab="")
legend("topleft", "(d) D6 (1473 m.a.s.l.)", bty = "n", inset = c(-0.02,-0.35))

plot(logit.mp[logit.mp$site=='GF',8],-1*fitted(step.gf),xlim=c(1990,2015),ylim = c(-120,-20),type="l",xlab = "",ylab="",col = 3)
par(new = T)
plot(logit.mp[logit.mp$site=='GF',8], -1*logit.mp[logit.mp$site=='GF',21], xlim=c(1990,2015),ylim = c(-120,-20),type="l",xlab = "",ylab="")
legend("topleft", "(e) GF (1621 m.a.s.l.)", bty = "n", inset = c(-0.02,-0.35))

mtext("Thaw depth (cm)", side=2, outer=TRUE, cex = 1, adj=0.5)
mtext("Year", side=1, outer=TRUE, cex=1, adj=0.5, line = 1)

## Check the change in AIC when each variable is removed

bp2 <- update(step.bp, ~.-TI_0)
summary(bp2)
anova(step.bp, bp2)
bp3 <- update(step.bp, ~.-FI_0)
summary(bp3)
anova(step.bp, bp3)
m4 <- update(glmer.full, ~.-DV)
summary(m4)
anova(glmer.full, m4)
m5 <- update(glmer.full, ~.-cmi.f)
summary(m5)
anova(glmer.full, m5)
m6 <- update(glmer.full, ~.-TV)
summary(m6)
anova(glmer.full, m6)

# Check the pseudo r squared for each model
r.squaredGLMM(step.full)
r.squaredGLMM(glmer.full)


ojp.logit$mt<-NA
ojp.logit[names(predict(step.full,type = c("response"))),]$mt<-predict(step.full,type = c("response"))

summary(step.full)
m2 <- update(step.full, ~.-AgeFA)
summary(m2)
anova(glm.full, m2)
m3 <- update(step.full, ~.-pre.gr)
summary(m3)
anova(step.full, m3)
m4 <- update(glm.full, ~.-DV)
summary(m4)
anova(glm.full, m4)
m5 <- update(glm.full, ~.-cmi.f)
summary(m5)
anova(glm.full, m5)
m6 <- update(glm.full, ~.-TV)
summary(m6)
anova(glm.full, m6)
vif(glm.full)



#example

library(lme4)
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)

gm0 <- glm(cbind(incidence, size - incidence) ~ period +  herd,
           data = cbpp, family = binomial)

phtest_glmer(glmer.full,glm.full)

##  Hausman Test

## data:  cbpp
## chisq = 10.2747, df = 4, p-value = 0.03605
## alternative hypothesis: one model is inconsistent
## so the results here means that we need random effects (p<0.05). 


phtest(glm.full, glmer.full)

###############################################################################
## Stepwise regression of thaw depth versus environmental/climatic variables ##
###############################################################################

bp.rda <- read.csv(file = "bp.rda.csv", header = TRUE)
hf.rda <- read.csv(file = "hf.rda.csv", header = TRUE)
gf.rda <- read.csv(file = "gf.rda.csv", header = TRUE)
d2.rda <- read.csv(file = "d2.rda.csv", header = TRUE)
d6.rda <- read.csv(file = "d6.rda.csv", header = TRUE)
bp.env <- read.csv(file = "bp.env.csv", header = TRUE)
hf.env <- read.csv(file = "hf.env.csv", header = TRUE)
gf.env <- read.csv(file = "gf.env.csv", header = TRUE)
d2.env <- read.csv(file = "d2.env.csv", header = TRUE)
d6.env <- read.csv(file = "d6.env.csv", header = TRUE)

row.names(bp.rda) <- bp.rda$year
row.names(hf.rda) <- hf.rda$year
row.names(gf.rda) <- gf.rda$year
row.names(d2.rda) <- d2.rda$year
row.names(d6.rda) <- d6.rda$year
row.names(bp.env) <- bp.env$year
row.names(hf.env) <- hf.env$year
row.names(gf.env) <- gf.env$year
row.names(d2.env) <- d2.env$year
row.names(d6.env) <- d6.env$year
bp.rda$year <- NULL
hf.rda$year <- NULL
gf.rda$year <- NULL
d2.rda$year <- NULL
d6.rda$year <- NULL
bp.env$year <- NULL
hf.env$year <- NULL
gf.env$year <- NULL
d2.env$year <- NULL
d6.env$year <- NULL

# Scatter plots for all pairs of environmental variables
# ******************************************************

# Load additionnal functions
# (files must be in the working directory)
source("~/Desktop/Workspace/borcard/panelutils.R")

# Bivariate plots with histograms on the diagonal and smooth fitted curves
quartz(title="Bivariate descriptor plots")
op <- par(mfrow=c(1,1), pty="s")
pairs(logit.mp2, panel=panel.smooth, diag.panel=panel.hist,
      main="Bivariate Plots with Histograms and Smooth Curves")
par(op)

# log base 10 transformation
bp.rda2 <- decostand(bp.rda, "normalize")
hf.rda2 <- decostand(hf.rda, "normalize")
gf.rda2 <- decostand(gf.rda, "normalize")
d2.rda2 <- decostand(d2.rda, "normalize")
d6.rda2 <- decostand(d6.rda, "normalize")
bp.rda2 <- decostand(bp.rda, "hellinger")
hf.rda2 <- decostand(hf.rda, "hellinger")
gf.rda2 <- decostand(gf.rda, "hellinger")
d2.rda2 <- decostand(d2.rda, "hellinger")
d6.rda2 <- decostand(d6.rda, "hellinger")
bp.rda2 <- decostand(bp.rda, "log", logbase = 10)
hf.rda2 <- decostand(hf.rda, "log", logbase = 10)
gf.rda2 <- decostand(gf.rda, "log", logbase = 10)
d2.rda2 <- decostand(d2.rda, "log", logbase = 10)
d6.rda2 <- decostand(d6.rda, "log", logbase = 10)

# Center and scale = standardize variables (z-scores)

bp.env2 <- decostand(bp.env, "standardize")
apply(bp.env2, 2, mean)  # means = 0
apply(bp.env2, 2, sd)		# standard deviations = 1
hf.env2 <- decostand(hf.env, "standardize")
apply(hf.env2, 2, mean)  # means = 0
apply(hf.env2, 2, sd)  	# standard deviations = 1
gf.env2 <- decostand(gf.env, "standardize")
apply(gf.env2, 2, mean)  # means = 0
apply(gf.env2, 2, sd)  	# standard deviations = 1
d2.env2 <- decostand(d2.env, "standardize")
apply(d2.env2, 2, mean)  # means = 0
apply(d2.env2, 2, sd)  	# standard deviations = 1
d6.env2 <- decostand(d6.env, "standardize")
apply(d6.env2, 2, mean)  # means = 0
apply(d6.env2, 2, sd)  	# standard deviations = 1


par(mfrow=c(2,2))
hist(hf.rda$thaw.a, col="bisque", right=FALSE)
hist(hf.rda2$thaw.a, col="light green", right=F, main="Histogram of ln(env$thaw.a)")
boxplot(hf.rda$thaw.a, col="bisque", main="Boxplot of env$thaw.a", ylab="env$thaw.a")
boxplot(hf.rda2$thaw.a, col="light green", main="Boxplot of ln(env$thaw.a)",
        ylab="log(env$thaw.a)")

shapiro.test(bp.rda2$thaw.a) # Normal
shapiro.test(bp.rda2$thaw.b)
shapiro.test(bp.rda2$thaw.c) # Normal
shapiro.test(bp.rda2$thaw.d)
shapiro.test(bp.rda2$thaw.e)

shapiro.test(hf.rda2$thaw.a) # Normal
shapiro.test(hf.rda2$thaw.b) # Normal

shapiro.test(gf.rda2$thaw.a) # Normal
shapiro.test(gf.rda2$thaw.b) # Normal

shapiro.test(d2.rda2$thaw.a) # Normal
shapiro.test(d2.rda2$thaw.b) # Normal
shapiro.test(d2.rda2$thaw.c) # Normal
shapiro.test(d2.rda2$thaw.d) # Normal

shapiro.test(d6.rda2$thaw.a) # Normal
shapiro.test(d6.rda2$thaw.b) # Normal
shapiro.test(d6.rda2$thaw.c)

plot(bp.rda2$thaw.a, type = "l")
par(new=T)
plot(bp.rda2$thaw.b, type = "l", col = 2)
par(new=T)
plot(bp.rda2$thaw.c, type = "l", col = 3)
par(new=T)
plot(bp.rda2$thaw.d, type = "l", col = 4)
par(new=T)
plot(bp.rda2$thaw.e, type = "l", col = 5)

#######################################
## PCA based on stepwise regressions ##
#######################################

names(bp.rda)
gf.env2 <- gf.env2[-3]
hf.env2 <- hf.env2[-3]
d2.env2 <- d2.env2[-3]
bp.env3 <- subset(bp.env2, select = c("TI5_0","TI150","FI_0","FI5_0","CST150","CST0","WST0","WST_150","WSP_CRU"))
hf.env3 <- subset(hf.env2, select = c("TI5_0","TI150","FI_0","FI5_0","CST150","CST0","WST0","WST_150","WSP_CRU"))
gf.env3 <- subset(gf.env2, select = c("TI5_0","TI150","FI_0","FI5_0","CST150","CST0","WST0","WST_150","WSP_CRU"))
d2.env3 <- subset(d2.env2, select = c("TI5_0","TI150","FI_0","FI5_0","CST150","CST0","WST0","WST_150","WSP_CRU"))
d6.env3 <- subset(d6.env2, select = c("TI5_0","TI150","FI_0","FI5_0","CST150","CST0","WST0","WST_150","WSP_CRU"))

# First, a global model with all sites combined
# *********************************************

all.rda <- read.csv(file = "all.rda.csv", header = TRUE)
all.env <- read.csv(file = "all.env.csv", header = TRUE)

row.names(all.rda) <- all.rda$year
row.names(all.env) <- all.env$year

all.rda$year <- NULL
all.env$year <- NULL

all.rda2 <- na.omit(all.rda)
all.env2 <- all.env[-c(1,2,3,4,5),]

set.seed(500)

all.mod0 <- rda(all.rda2 ~ 1, all.env2)  # Model with intercept only
all.mod1 <- rda(all.rda2 ~ ., all.env2)  # Model with all explanatory variables
vif(all.mod1)
all.step <- ordiR2step(all.mod0, scope = formula(all.mod1), direction = "both", R2scope = TRUE, 
                       Pin = 0.05, perm.max = 999)
summary(all.step)
vif(all.step)
all.step$anova
RsquareAdj(all.step)$r.squared
RsquareAdj(all.step)$adj.r.squared

all.pca<-pca(all.rda2,cor=TRUE, dim = min(nrow(all.rda2),ncol(all.rda2)))
summary(all.pca)
all.load<-loadings.pca(all.pca, dim = min(nrow(all.rda),ncol(all.rda)))
#vpch<-c(21,22,24)
plot(x=all.pca$scores[,1], y=all.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC1 (57.4%)", side=1, line=2, cex=1, outer=FALSE)
mtext("PC2 (19.4%)", side=2, line=2, cex=1, outer=FALSE)
all.vec<-envfit(all.pca$scores, all.env2[-c(1,2,3,4,5,6,7,9,11,13)], na.rm=T)
all.vec
plot(all.vec, add=T, col="black", cex=0.9)




bp.mod0 <- rda(bp.rda2 ~ 1, bp.env2)  # Model with intercept only
bp.mod1 <- rda(bp.rda2 ~ ., bp.env2)  # Model with all explanatory variables
vif(bp.mod1)
bp.step <- ordiR2step(bp.mod0, scope = formula(bp.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(bp.step)
vif(bp.step)
bp.step$anova
RsquareAdj(bp.step)$r.squared
RsquareAdj(bp.step)$adj.r.squared

gf.mod0 <- rda(gf.rda ~ 1, gf.env)  # Model with intercept only
gf.mod1 <- rda(gf.rda ~ ., gf.env)  # Model with all explanatory variables
vif(gf.mod1)
gf.step <- ordiR2step(gf.mod0, scope = formula(gf.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(gf.step)
vif(gf.step)
gf.step$anova
RsquareAdj(gf.step)$r.squared
RsquareAdj(gf.step)$adj.r.squared

hf.mod0 <- rda(hf.rda2 ~ 1, hf.env2)  # Model with intercept only
hf.mod1 <- rda(hf.rda2 ~ ., hf.env2)  # Model with all explanatory variables
vif(hf.mod1)
hf.step <- ordiR2step(hf.mod0, scope = formula(hf.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(hf.step)
vif(hf.step)
hf.step$anova
RsquareAdj(hf.step)$r.squared
RsquareAdj(hf.step)$adj.r.squared

d2.mod0 <- rda(d2.rda2 ~ 1, d2.env2)  # Model with intercept only
d2.mod1 <- rda(d2.rda2 ~ ., d2.env2)  # Model with all explanatory variables
vif(d2.mod1)
d2.step <- ordiR2step(d2.mod0, scope = formula(d2.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(d2.step)
vif(d2.step)
d2.step$anova
RsquareAdj(d2.step)$r.squared
RsquareAdj(d2.step)$adj.r.squared

d6.mod0 <- rda(d6.rda2 ~ 1, d6.env2)  # Model with intercept only
d6.mod1 <- rda(d6.rda2 ~ ., d6.env2)  # Model with all explanatory variables
vif(d6.mod1)
d6.step <- ordiR2step(d6.mod0, scope = formula(d6.mod1), direction = "both", R2scope = TRUE, 
                      Pin = 0.05, perm.max = 999)
summary(d6.step)
vif(d6.step)
d6.step$anova
RsquareAdj(d6.step)$r.squared
RsquareAdj(d6.step)$adj.r.squared


bp.pca<-pca(bp.rda,cor=TRUE, dim = min(nrow(bp.rda),ncol(bp.rda)))
summary(bp.pca)
bp.load<-loadings.pca(bp.pca, dim = min(nrow(bp.rda),ncol(bp.rda)))
#vpch<-c(21,22,24)
plot(x=bp.pca$scores[,1], y=bp.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (21.5%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (54.2%)", side=1, line=2, cex=1, outer=FALSE)
bp.vec<-envfit(bp.pca$scores, bp.env2, na.rm=T)
bp.vec
plot(bp.vec, add=T, col="black", cex=0.9)


hf.pca<-pca(hf.rda,cor=TRUE, dim = min(nrow(hf.rda),ncol(hf.rda)))
summary(hf.pca)
hf.load<-loadings.pca(hf.pca, dim = min(nrow(hf.rda),ncol(hf.rda)))
#vpch<-c(21,22,24)
plot(x=hf.pca$scores[,1], y=hf.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (45.2%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (54.8%)", side=1, line=2, cex=1, outer=FALSE)
hf.vec<-envfit(hf.pca$scores, hf.env2, na.rm=T)
hf.vec
plot(hf.vec, add=T, col="black", cex=0.9)


d2.pca<-pca(d2.rda,cor=TRUE, dim = min(nrow(d2.rda),ncol(d2.rda)))
summary(d2.pca)
d2.load<-loadings.pca(d2.pca, dim = min(nrow(d2.rda),ncol(d2.rda)))
#vpch<-c(21,22,24)
plot(x=d2.pca$scores[,1], y=d2.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (16.6%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (74.5%)", side=1, line=2, cex=1, outer=FALSE)
d2.vec<-envfit(d2.pca$scores, d2.env2, na.rm=T)
d2.vec
plot(d2.vec, add=T, col="black", cex=0.9)

gf.pca<-pca(gf.rda[1:2],cor=TRUE, dim = min(nrow(gf.rda[1:2]),ncol(gf.rda[1:2])))
summary(gf.pca)
gf.load<-loadings.pca(gf.pca, dim = min(nrow(gf.rda),ncol(gf.rda)))
#vpch<-c(21,22,24)
plot(x=gf.pca$scores[,1], y=gf.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (44.6%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (55.4%)", side=1, line=2, cex=1, outer=FALSE)
gf.vec<-envfit(gf.pca$scores, gf.env2, na.rm=T)
gf.vec
plot(gf.vec, add=T, col="black", cex=0.9)

#### Output at 7 x 6 inches
par(mfrow = c(2, 2))
par(ps = 10, cex = 1, cex.axis = 1) # Sets the font size to 18 pts
par(mar = c(2, 1, 1.75, 3), oma = c(1,2,1,1))

hf.pca<-pca(hf.rda,cor=TRUE, dim = min(nrow(hf.rda),ncol(hf.rda)))
summary(hf.pca)
hf.load<-loadings.pca(hf.pca, dim = min(nrow(hf.rda),ncol(hf.rda)))
#vpch<-c(21,22,24)
plot(x=hf.pca$scores[,1], y=hf.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (45.2%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (54.8%)", side=1, line=2, cex=1, outer=FALSE)
hf.vec<-envfit(hf.pca$scores, hf.env2, na.rm=T)
hf.vec
plot(hf.vec, add=T, col="black", cex=0.9)
legend("topleft", "(a) HF", bty = "n", inset = c(-0.05,-0.05))


bp.pca<-pca(bp.rda,cor=TRUE, dim = min(nrow(bp.rda),ncol(bp.rda)))
summary(bp.pca)
bp.load<-loadings.pca(bp.pca, dim = min(nrow(bp.rda),ncol(bp.rda)))
#vpch<-c(21,22,24)
plot(x=bp.pca$scores[,1], y=bp.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (21.5%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (54.2%)", side=1, line=2, cex=1, outer=FALSE)
bp.vec<-envfit(bp.pca$scores, bp.env2, na.rm=T)
bp.vec
plot(bp.vec, add=T, col="black", cex=0.9)
legend("topleft", "(b) BP", bty = "n", inset = c(-0.05,-0.05))

d2.pca<-pca(d2.rda,cor=TRUE, dim = min(nrow(d2.rda),ncol(d2.rda)))
summary(d2.pca)
d2.load<-loadings.pca(d2.pca, dim = min(nrow(d2.rda),ncol(d2.rda)))
#vpch<-c(21,22,24)
plot(x=d2.pca$scores[,1], y=d2.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (16.6%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (74.5%)", side=1, line=2, cex=1, outer=FALSE)
d2.vec<-envfit(d2.pca$scores, d2.env2, na.rm=T)
d2.vec
plot(d2.vec, add=T, col="black", cex=0.9)
legend("topleft", "(c) D2", bty = "n", inset = c(-0.05,-0.05))

gf.pca<-pca(gf.rda[1:2],cor=TRUE, dim = min(nrow(gf.rda[1:2]),ncol(gf.rda[1:2])))
summary(gf.pca)
gf.load<-loadings.pca(gf.pca, dim = min(nrow(gf.rda),ncol(gf.rda)))
#vpch<-c(21,22,24)
plot(x=gf.pca$scores[,1], y=gf.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5),  
     cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (44.6%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (55.4%)", side=1, line=2, cex=1, outer=FALSE)
gf.vec<-envfit(gf.pca$scores, gf.env2, na.rm=T)
gf.vec
plot(gf.vec, add=T, col="black", cex=0.9)
legend("topleft", "(d) GF", bty = "n", inset = c(-0.05,-0.05))




sf.pca<-pca(sf2[1:4],cor=TRUE, dim = min(nrow(sf2[1:4]),ncol(sf2[1:4])))
summary(sf.pca)
sf.load<-loadings.pca(sf.pca, dim = min(nrow(sf2[1:4]),ncol(sf2[1:4])))
vpch<-c(17,21)
plot(x=sf.pca$scores[,1], y=sf.pca$scores[,2], xlab="", ylab="", 
     cex.lab=1.2, cex.main=1.5, xlim=c(-5,5), ylim=c(-5,5), pch=vpch[sf2$group], 
     bg=ifelse(sf2$group == 3, "gray", "white"), cex = 0.75)
abline(h=0,lty=3, col = "gray48")
abline(v=0,lty=3, col = "gray48")
mtext("PC2 (31.1%)", side=2, line=2, cex=1, outer=FALSE)
mtext("PC1 (60.7%)", side=1, line=2, cex=1, outer=FALSE)
sf.vec<-envfit(sf.pca$scores, sf4, choices = c(1,2), na.rm=T)
sf.vec
plot(sf.vec, add=T, choices = c(1,2), col="black", cex=0.9)


