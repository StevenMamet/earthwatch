library(dplyr)
library(psych)    # Pairs plot
library(splines)

rm(list=ls())

# Churchill met station data
churchill <- read.csv(file = "~/Desktop/Workspace/Earthwatch/ch_microclimate.csv", stringsAsFactors = F,  header = TRUE)
churchill[4529,103] <- -2.872
churchill[972,67] <- NA
churchill$pfr150 <- as.numeric(churchill$pfr150)
churchill$pfr150[churchill$pfr150 == min(churchill$pfr150, na.rm = T)] <- NA
churchill$pfr150[churchill$pfr150 == min(churchill$pfr150, na.rm = T)] <- NA
churchill$pfr150[churchill$pfr150 == min(churchill$pfr150, na.rm = T)] <- NA

plot(churchill$tun150 ~ rownames(churchill), type = 'l')
lines(rownames(churchill), churchill$EC.air,  type = 'l', col = "blue")
sum(is.na(churchill$EC.air))
levels(as.factor(churchill$Month))
str(churchill)

churchill <- churchill[c(242:7206),]

###################################################################
## ********************
### Step 1: Fill the missing air temperatures

## TUN 150 cm based on EC

## January
# Construct linear model based on non-NA pairs
tun150.1 <- churchill %>% filter(!is.na(tun150) & Month == 1)
tun150.1.fit <- lm(tun150 ~ EC.air, data = tun150.1)
summary(tun150.1.fit) # R2 = 0.961
churchill <- churchill %>% 
  mutate(pred = predict(tun150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 1), pred, tun150))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
tun150.2 <- churchill %>% filter(!is.na(tun150) & Month == 2)
tun150.2.fit <- lm(tun150 ~ EC.air, data = tun150.2)
summary(tun150.2.fit) # R2 = 0.9516
churchill <- churchill %>% 
  mutate(pred = predict(tun150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 2), pred, tun150))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
tun150.3 <- churchill %>% filter(!is.na(tun150) & Month == 3)
tun150.3.fit <- lm(tun150 ~ EC.air, data = tun150.3)
summary(tun150.3.fit) # R2 = 0.929
churchill <- churchill %>% 
  mutate(pred = predict(tun150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 3), pred, tun150))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
tun150.4 <- churchill %>% filter(!is.na(tun150) & Month == 4)
tun150.4.fit <- lm(tun150 ~ EC.air, data = tun150.4)
summary(tun150.4.fit) # R2 = 0.9487
churchill <- churchill %>% 
  mutate(pred = predict(tun150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 4), pred, tun150))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
tun150.5 <- churchill %>% filter(!is.na(tun150) & Month == 5)
tun150.5.fit <- lm(tun150 ~ EC.air, data = tun150.5)
summary(tun150.5.fit) # R2 = 0.949  
churchill <- churchill %>% 
  mutate(pred = predict(tun150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 5), pred, tun150))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
tun150.6 <- churchill %>% filter(!is.na(tun150) & Month == 6)
tun150.6.fit <- lm(tun150 ~ EC.air, data = tun150.6)
summary(tun150.6.fit) # R2 = 0.909   
churchill <- churchill %>% 
  mutate(pred = predict(tun150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 6), pred, tun150))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
tun150.7 <- churchill %>% filter(!is.na(tun150) & Month == 7)
tun150.7.fit <- lm(tun150 ~ EC.air, data = tun150.7)
summary(tun150.7.fit) # R2 = 0.8588
churchill <- churchill %>% 
  mutate(pred = predict(tun150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 7), pred, tun150))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
tun150.8 <- churchill %>% filter(!is.na(tun150) & Month == 8)
tun150.8.fit <- lm(tun150 ~ EC.air, data = tun150.8)
summary(tun150.8.fit) # R2 = 0.9002 
churchill <- churchill %>% 
  mutate(pred = predict(tun150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 8), pred, tun150))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
tun150.9 <- churchill %>% filter(!is.na(tun150) & Month == 9)
tun150.9.fit <- lm(tun150 ~ EC.air, data = tun150.9)
summary(tun150.9.fit) # R2 = 0.9505  
churchill <- churchill %>% 
  mutate(pred = predict(tun150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 9), pred, tun150))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
tun150.10 <- churchill %>% filter(!is.na(tun150) & Month == 10)
tun150.10.fit <- lm(tun150 ~ EC.air, data = tun150.10)
summary(tun150.10.fit) # R2 = 0.9478
churchill <- churchill %>% 
  mutate(pred = predict(tun150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 10), pred, tun150))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
tun150.11 <- churchill %>% filter(!is.na(tun150) & Month == 11)
tun150.11.fit <- lm(tun150 ~ EC.air, data = tun150.11)
summary(tun150.11.fit) # R2 = 0.9829
churchill <- churchill %>% 
  mutate(pred = predict(tun150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 11), pred, tun150))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
tun150.12 <- churchill %>% filter(!is.na(tun150) & Month == 12)
tun150.12.fit <- lm(tun150 ~ EC.air, data = tun150.12)
summary(tun150.12.fit) # R2 = 0.9673
churchill <- churchill %>% 
  mutate(pred = predict(tun150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun150 = ifelse((is.na(tun150) & Month == 12), pred, tun150))
churchill %>% as.data.frame()

plot(churchill$tun150, type = "l", col = "blue")
plot(EC.air ~ rownames(churchill), data = churchill, type = "l", col = "blue")

## FEN 150 cm based on EC

## January
# Construct linear model based on non-NA pairs
fen150.1 <- churchill %>% filter(!is.na(fen150) & Month == 1)
fen150.1.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.1)
summary(fen150.1.fit) # R2 = 0.906   
churchill <- churchill %>% 
  mutate(pred = predict(fen150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 1, pred, fen150))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
fen150.2 <- churchill %>% filter(!is.na(fen150) & Month == 2)
fen150.2.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.2)
summary(fen150.2.fit) # R2 = 0.9388 
churchill <- churchill %>% 
  mutate(pred = predict(fen150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 2, pred, fen150))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
fen150.3 <- churchill %>% filter(!is.na(fen150) & Month == 3)
fen150.3.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.3)
summary(fen150.3.fit) # R2 = 0.9891   
churchill <- churchill %>% 
  mutate(pred = predict(fen150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 3, pred, fen150))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
fen150.4 <- churchill %>% filter(!is.na(fen150) & Month == 4)
fen150.4.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.4)
summary(fen150.4.fit) # R2 = 0.9898    
churchill <- churchill %>% 
  mutate(pred = predict(fen150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 4, pred, fen150))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
fen150.5 <- churchill %>% filter(!is.na(fen150) & Month == 5)
fen150.5.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.5)
summary(fen150.5.fit) # R2 = 0.9545    
churchill <- churchill %>% 
  mutate(pred = predict(fen150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 5, pred, fen150))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
fen150.6 <- churchill %>% filter(!is.na(fen150) & Month == 6)
fen150.6.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.6)
summary(fen150.6.fit) # R2 = 0.9747     
churchill <- churchill %>% 
  mutate(pred = predict(fen150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 6, pred, fen150))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
fen150.7 <- churchill %>% filter(!is.na(fen150) & Month == 7)
fen150.7.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.7)
summary(fen150.7.fit) # R2 = 0.9613     
churchill <- churchill %>% 
  mutate(pred = predict(fen150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 7, pred, fen150))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
fen150.8 <- churchill %>% filter(!is.na(fen150) & Month == 8)
fen150.8.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.8)
summary(fen150.8.fit) # R2 = 0.9606       
churchill <- churchill %>% 
  mutate(pred = predict(fen150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 8, pred, fen150))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
fen150.9 <- churchill %>% filter(!is.na(fen150) & Month == 9)
fen150.9.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.9)
summary(fen150.9.fit) # R2 = 0.979      
churchill <- churchill %>% 
  mutate(pred = predict(fen150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 9, pred, fen150))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
fen150.10 <- churchill %>% filter(!is.na(fen150) & Month == 10)
fen150.10.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.10)
summary(fen150.10.fit) # R2 = 0.9061         
churchill <- churchill %>% 
  mutate(pred = predict(fen150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 10, pred, fen150))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
fen150.11 <- churchill %>% filter(!is.na(fen150) & Month == 11)
fen150.11.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.11)
summary(fen150.11.fit) # R2 = 0.9911       
churchill <- churchill %>% 
  mutate(pred = predict(fen150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 11, pred, fen150))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
fen150.12 <- churchill %>% filter(!is.na(fen150) & Month == 12)
fen150.12.fit <- lm(fen150 ~ tun150 + EC.air, data = fen150.12)
summary(fen150.12.fit) # R2 = 0.9708          
churchill <- churchill %>% 
  mutate(pred = predict(fen150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen150 = ifelse(is.na(fen150) & Month == 12, pred, fen150))
churchill %>% as.data.frame()

plot(fen150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")



## WSU 150 cm

## January
# Construct linear model based on non-NA pairs
wsu150.1 <- churchill %>% filter(!is.na(wsu150) & Month == 1)
wsu150.1.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.1)
summary(wsu150.1.fit) # R2 = 0.9618   
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 1, pred, wsu150))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
wsu150.2 <- churchill %>% filter(!is.na(wsu150) & Month == 2)
wsu150.2.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.2)
summary(wsu150.2.fit) # R2 = 0.978  
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 2, pred, wsu150))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
wsu150.3 <- churchill %>% filter(!is.na(wsu150) & Month == 3)
wsu150.3.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.3)
summary(wsu150.3.fit) # R2 = 0.9902  
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 3, pred, wsu150))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
wsu150.4 <- churchill %>% filter(!is.na(wsu150) & Month == 4)
wsu150.4.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.4)
summary(wsu150.4.fit) # R2 = 0.9886  
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 4, pred, wsu150))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
wsu150.5 <- churchill %>% filter(!is.na(wsu150) & Month == 5)
wsu150.5.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.5)
summary(wsu150.5.fit) # R2 = 0.9696    
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 5, pred, wsu150))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
wsu150.6 <- churchill %>% filter(!is.na(wsu150) & Month == 6)
wsu150.6.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.6)
summary(wsu150.6.fit) # R2 = 0.9764     
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 6, pred, wsu150))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
wsu150.7 <- churchill %>% filter(!is.na(wsu150) & Month == 7)
wsu150.7.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.7)
summary(wsu150.7.fit) # R2 = 0.9632       
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 7, pred, wsu150))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
wsu150.8 <- churchill %>% filter(!is.na(wsu150) & Month == 8)
wsu150.8.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.8)
summary(wsu150.8.fit) # R2 = 0.9687       
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 8, pred, wsu150))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
wsu150.9 <- churchill %>% filter(!is.na(wsu150) & Month == 9)
wsu150.9.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.9)
summary(wsu150.9.fit) # R2 = 0.9784        
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 9, pred, wsu150))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
wsu150.10 <- churchill %>% filter(!is.na(wsu150) & Month == 10)
wsu150.10.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.10)
summary(wsu150.10.fit) # R2 = 0.9773        
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 10, pred, wsu150))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
wsu150.11 <- churchill %>% filter(!is.na(wsu150) & Month == 11)
wsu150.11.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.11)
summary(wsu150.11.fit) # R2 = 0.9915        
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 11, pred, wsu150))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
wsu150.12 <- churchill %>% filter(!is.na(wsu150) & Month == 12)
wsu150.12.fit <- lm(wsu150 ~ tun150 + fen150, data = wsu150.12)
summary(wsu150.12.fit) # R2 = 0.9894          
churchill <- churchill %>% 
  mutate(pred = predict(wsu150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu150 = ifelse(is.na(wsu150) & Month == 12, pred, wsu150))
churchill %>% as.data.frame()

plot(wsu150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")


## PPA 150 cm


## January
# Construct linear model based on non-NA pairs
ppa150.all <- churchill %>% filter(!is.na(ppa150))
ppa150.all.fit <- lm(ppa150 ~ tun150 + fen150 + EC.air, data = ppa150.all)
summary(ppa150.all.fit) # R2 = 0.9945
churchill <- churchill %>% 
  mutate(pred = predict(ppa150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa150 = ifelse(is.na(ppa150), pred, ppa150))
churchill %>% as.data.frame()


plot(tun150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")
lines(churchill$EC.air, col = "green")
lines(churchill$ppa150, col = "red")

plot(ppa150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")

## AIR 150 cm

# Construct linear model based on non-NA pairs
air150all <- churchill %>% filter(!is.na(airp150))
air150all.fit <- lm(airp150 ~ wsu150 + fen150, data = air150all)
summary(air150all.fit) # R2 = 0.9981   
churchill <- churchill %>% 
  mutate(pred = predict(air150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(airp150 = ifelse(is.na(airp150), pred, airp150))
churchill %>% as.data.frame()

## BSW 150 cm
# Construct linear model based on non-NA pbsws
bsw150all <- churchill %>% filter(!is.na(bsw150))
bsw150all.fit <- lm(bsw150 ~ wsu150 + airp150, data = bsw150all)
summary(bsw150all.fit) # R2 = 0.9981   
churchill <- churchill %>% 
  mutate(pred = predict(bsw150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bsw150 = ifelse(is.na(bsw150), pred, bsw150))
churchill %>% as.data.frame()

## TIS 150 cm
# Construct linear model based on non-NA 
tis150all <- churchill %>% filter(!is.na(tis150))
tis150all.fit <- lm(tis150 ~ wsu150 + airp150 + bsw150 + ppa150, data = tis150all)
summary(tis150all.fit) # R2 = 0.9619   
churchill <- churchill %>% 
  mutate(pred = predict(tis150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tis150 = ifelse(is.na(tis150), pred, tis150))
churchill %>% as.data.frame()

## BFR 150 cm
# Construct linear model based on non-NA 
bfr150all <- churchill %>% filter(!is.na(bfr150))
bfr150all.fit <- lm(bfr150 ~ wsu150 + ppa150, data = bfr150all)
summary(bfr150all.fit) # R2 = 0.9945   
churchill <- churchill %>% 
  mutate(pred = predict(bfr150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bfr150 = ifelse(is.na(bfr150), pred, bfr150))
churchill %>% as.data.frame()

## PFR 150 cm
# Construct linear model based on non-NA 
pfr150all <- churchill %>% filter(!is.na(pfr150))
pfr150all.fit <- lm(pfr150 ~ wsu150 + bfr150 + bsw150 + ppa150, data = pfr150all)
summary(pfr150all.fit) # R2 = 0.9945   
churchill <- churchill %>% 
  mutate(pred = predict(pfr150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(pfr150 = ifelse(is.na(pfr150), pred, pfr150))
churchill %>% as.data.frame()

## PPD 150 cm
# Construct linear model based on non-NA 
ppd150all <- churchill %>% filter(!is.na(ppd150))
ppd150all.fit <- lm(ppd150 ~ wsu150 + bfr150 + bsw150 + ppa150, data = ppd150all)
summary(ppd150all.fit) # R2 = 0.9945   
churchill <- churchill %>% 
  mutate(pred = predict(ppd150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppd150 = ifelse(is.na(ppd150), pred, ppd150))
churchill %>% as.data.frame()

## BWP 150 cm
# Construct linear model based on non-NA 
bwp150all <- churchill %>% filter(!is.na(bwp150))
bwp150all.fit <- lm(bwp150 ~ wsu150 + ppd150, data = bwp150all)
summary(bwp150all.fit) # R2 = 0.9761   
churchill <- churchill %>% 
  mutate(pred = predict(bwp150all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwp150 = ifelse(is.na(bwp150), pred, bwp150))
churchill %>% as.data.frame()

plot(airp150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")
lines(churchill$bfr150, col = "green")
lines(churchill$bsw150, col = "red")
lines(churchill$bwp150, col = "orange")
lines(churchill$fen150, col = "violet")
lines(churchill$pfr150, col = "firebrick") # check this one
lines(churchill$ppa150, col = "darkgreen")
lines(churchill$ppd150, col = "gold1")
lines(churchill$tis150, col = "deeppink1")
lines(churchill$tun150, col = "lightgoldenrod1")
lines(churchill$wsu150, col = "mediumpurple2")

plot(pfr150 ~ rownames(churchill), data = churchill, type = "l", col = "blue")
lines(churchill$bfr150, col = "green")















## TUN 0 cm based on EC and tun 150

## January
# Construct linear model based on non-NA pairs
tun0.1 <- churchill %>% filter(!is.na(tun0) & Month == 1)
tun0.1.fit <- lm(tun0 ~ EC.air, data = tun0.1)
summary(tun0.1.fit) # R2 = 0.4645
churchill <- churchill %>% 
  mutate(pred = predict(tun0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 1, pred, tun0))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
tun0.2 <- churchill %>% filter(!is.na(tun0) & Month == 2)
tun0.2.fit <- lm(tun0 ~ tun150, data = tun0.2)
summary(tun0.2.fit) # R2 = 0.5395
churchill <- churchill %>% 
  mutate(pred = predict(tun0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 2, pred, tun0))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
tun0.3 <- churchill %>% filter(!is.na(tun0) & Month == 3)
tun0.3.fit <- lm(tun0 ~ tun150, data = tun0.3)
summary(tun0.3.fit) # R2 = 0.749  
churchill <- churchill %>% 
  mutate(pred = predict(tun0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 3, pred, tun0))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
tun0.4 <- churchill %>% filter(!is.na(tun0) & Month == 4)
tun0.4.fit <- lm(tun0 ~ tun150, data = tun0.4)
summary(tun0.4.fit) # R2 = 0.7133
churchill <- churchill %>% 
  mutate(pred = predict(tun0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 4, pred, tun0))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
tun0.5 <- churchill %>% filter(!is.na(tun0) & Month == 5)
tun0.5.fit <- lm(tun0 ~ tun150, data = tun0.5)
summary(tun0.5.fit) # R2 = 0.7254 
churchill <- churchill %>% 
  mutate(pred = predict(tun0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 5, pred, tun0))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
tun0.6 <- churchill %>% filter(!is.na(tun0) & Month == 6)
tun0.6.fit <- lm(tun0 ~ tun150, data = tun0.6)
summary(tun0.6.fit) # R2 = 0.7792    
churchill <- churchill %>% 
  mutate(pred = predict(tun0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 6, pred, tun0))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
tun0.7 <- churchill %>% filter(!is.na(tun0) & Month == 7)
tun0.7.fit <- lm(tun0 ~ tun150, data = tun0.7)
summary(tun0.7.fit) # R2 = 0.8109
churchill <- churchill %>% 
  mutate(pred = predict(tun0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 7, pred, tun0))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
tun0.8 <- churchill %>% filter(!is.na(tun0) & Month == 8)
tun0.8.fit <- lm(tun0 ~ tun150, data = tun0.8)
summary(tun0.8.fit) # R2 = 0.7989   
churchill <- churchill %>% 
  mutate(pred = predict(tun0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 8, pred, tun0))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
tun0.9 <- churchill %>% filter(!is.na(tun0) & Month == 9)
tun0.9.fit <- lm(tun0 ~ tun150 + EC.air, data = tun0.9)
summary(tun0.9.fit) # R2 = 0.8597    
churchill <- churchill %>% 
  mutate(pred = predict(tun0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 9, pred, tun0))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
tun0.10 <- churchill %>% filter(!is.na(tun0) & Month == 10)
tun0.10.fit <- lm(tun0 ~ tun150, data = tun0.10)
summary(tun0.10.fit) # R2 = 0.7228   
churchill <- churchill %>% 
  mutate(pred = predict(tun0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 10, pred, tun0))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
tun0.11 <- churchill %>% filter(!is.na(tun0) & Month == 11)
tun0.11.fit <- lm(tun0 ~ tun150, data = tun0.11)
summary(tun0.11.fit) # R2 = 0.8349     
churchill <- churchill %>% 
  mutate(pred = predict(tun0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 11, pred, tun0))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
tun0.12 <- churchill %>% filter(!is.na(tun0) & Month == 12)
tun0.12.fit <- lm(tun0 ~ tun150, data = tun0.12)
summary(tun0.12.fit) # R2 = 0.6637     
churchill <- churchill %>% 
  mutate(pred = predict(tun0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(tun0 = ifelse(is.na(tun0) & Month == 12, pred, tun0))
churchill %>% as.data.frame()

plot(tun0 ~ rownames(churchill), data = churchill, type = "l", col = "blue")





## PPA 0 cm based on EC and ppa 150

## January
# Construct linear model based on non-NA pairs
ppa0.1 <- churchill %>% filter(!is.na(ppa0) & Month == 1)
ppa0.1.fit <- lm(ppa0 ~ ppa150, data = ppa0.1)
summary(ppa0.1.fit) # R2 = 0.7184
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 1, pred, ppa0))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
ppa0.2 <- churchill %>% filter(!is.na(ppa0) & Month == 2)
ppa0.2.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.2)
summary(ppa0.2.fit) # R2 = 0.8175
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 2, pred, ppa0))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
ppa0.3 <- churchill %>% filter(!is.na(ppa0) & Month == 3)
ppa0.3.fit <- lm(ppa0 ~ ppa150, data = ppa0.3)
summary(ppa0.3.fit) # R2 = 0.9071
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 3, pred, ppa0))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
ppa0.4 <- churchill %>% filter(!is.na(ppa0) & Month == 4)
ppa0.4.fit <- lm(ppa0 ~ ppa150, data = ppa0.4)
summary(ppa0.4.fit) # R2 = 0.9031
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 4, pred, ppa0))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
ppa0.5 <- churchill %>% filter(!is.na(ppa0) & Month == 5)
ppa0.5.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.5)
summary(ppa0.5.fit) # R2 = 0.9281
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 5, pred, ppa0))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
ppa0.6 <- churchill %>% filter(!is.na(ppa0) & Month == 6)
ppa0.6.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.6)
summary(ppa0.6.fit) # R2 = 0.9339   
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 6, pred, ppa0))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
ppa0.7 <- churchill %>% filter(!is.na(ppa0) & Month == 7)
ppa0.7.fit <- lm(ppa0 ~ ppa150, data = ppa0.7)
summary(ppa0.7.fit) # R2 = 0.9261
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 7, pred, ppa0))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
ppa0.8 <- churchill %>% filter(!is.na(ppa0) & Month == 8)
ppa0.8.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.8)
summary(ppa0.8.fit) # R2 = 0.9358 
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 8, pred, ppa0))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
ppa0.9 <- churchill %>% filter(!is.na(ppa0) & Month == 9)
ppa0.9.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.9)
summary(ppa0.9.fit) # R2 = 0.9684
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 9, pred, ppa0))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
ppa0.10 <- churchill %>% filter(!is.na(ppa0) & Month == 10)
ppa0.10.fit <- lm(ppa0 ~ ppa150 + EC.air, data = ppa0.10)
summary(ppa0.10.fit) # R2 = 0.9276
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 10, pred, ppa0))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
ppa0.11 <- churchill %>% filter(!is.na(ppa0) & Month == 11)
ppa0.11.fit <- lm(ppa0 ~ ppa150, data = ppa0.11)
summary(ppa0.11.fit) # R2 = 0.8635  
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 11, pred, ppa0))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
ppa0.12 <- churchill %>% filter(!is.na(ppa0) & Month == 12)
ppa0.12.fit <- lm(ppa0 ~ ppa150, data = ppa0.12)
summary(ppa0.12.fit) # R2 = 0.832
churchill <- churchill %>% 
  mutate(pred = predict(ppa0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(ppa0 = ifelse(is.na(ppa0) & Month == 12, pred, ppa0))
churchill %>% as.data.frame()

plot(ppa0 ~ rownames(churchill), data = churchill, type = "l", col = "blue")



## FEN 0 cm based on EC and fen 150

## January
# Construct linear model based on non-NA pairs
fen0.1 <- churchill %>% filter(!is.na(fen0) & Month == 1)
fen0.1.fit <- lm(fen0 ~ tun150, data = fen0.1)
summary(fen0.1.fit) # R2 = 0.124
churchill <- churchill %>% 
  mutate(pred = predict(fen0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 1, pred, fen0))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
fen0.2 <- churchill %>% filter(!is.na(fen0) & Month == 2)
fen0.2.fit <- lm(fen0 ~ tun150, data = fen0.2)
summary(fen0.2.fit) # R2 = 0.1082
churchill <- churchill %>% 
  mutate(pred = predict(fen0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 2, pred, fen0))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
fen0.3 <- churchill %>% filter(!is.na(fen0) & Month == 3)
fen0.3.fit <- lm(fen0 ~ EC.air, data = fen0.3)
summary(fen0.3.fit) # R2 = 0.1777
churchill <- churchill %>% 
  mutate(pred = predict(fen0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 3, pred, fen0))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
fen0.4 <- churchill %>% filter(!is.na(fen0) & Month == 4)
fen0.4.fit <- lm(fen0 ~ tun150, data = fen0.4)
summary(fen0.4.fit) # R2 = 0.4229
churchill <- churchill %>% 
  mutate(pred = predict(fen0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 4, pred, fen0))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
fen0.5 <- churchill %>% filter(!is.na(fen0) & Month == 5)
fen0.5.fit <- lm(fen0 ~ fen150 + tun150, data = fen0.5)
summary(fen0.5.fit) # R2 = 0.5156
churchill <- churchill %>% 
  mutate(pred = predict(fen0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 5, pred, fen0))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
fen0.6 <- churchill %>% filter(!is.na(fen0) & Month == 6)
fen0.6.fit <- lm(fen0 ~ fen150 + tun150, data = fen0.6)
summary(fen0.6.fit) # R2 = 0.7529 
churchill <- churchill %>% 
  mutate(pred = predict(fen0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 6, pred, fen0))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
fen0.7 <- churchill %>% filter(!is.na(fen0) & Month == 7)
fen0.7.fit <- lm(fen0 ~ fen150 + tun150, data = fen0.7)
summary(fen0.7.fit) # R2 = 0.7594
churchill <- churchill %>% 
  mutate(pred = predict(fen0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 7, pred, fen0))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
fen0.8 <- churchill %>% filter(!is.na(fen0) & Month == 8)
fen0.8.fit <- lm(fen0 ~ fen150 + tun150, data = fen0.8)
summary(fen0.8.fit) # R2 = 0.7379
churchill <- churchill %>% 
  mutate(pred = predict(fen0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 8, pred, fen0))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
fen0.9 <- churchill %>% filter(!is.na(fen0) & Month == 9)
fen0.9.fit <- lm(fen0 ~ fen150 + EC.air, data = fen0.9)
summary(fen0.9.fit) # R2 = 0.8122
churchill <- churchill %>% 
  mutate(pred = predict(fen0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 9, pred, fen0))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
fen0.10 <- churchill %>% filter(!is.na(fen0) & Month == 10)
fen0.10.fit <- lm(fen0 ~ fen150, data = fen0.10)
summary(fen0.10.fit) # R2 = 0.5976
churchill <- churchill %>% 
  mutate(pred = predict(fen0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 10, pred, fen0))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
fen0.11 <- churchill %>% filter(!is.na(fen0) & Month == 11)
fen0.11.fit <- lm(fen0 ~ fen150 + tun150, data = fen0.11)
summary(fen0.11.fit) # R2 = 0.4699
churchill <- churchill %>% 
  mutate(pred = predict(fen0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 11, pred, fen0))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
fen0.12 <- churchill %>% filter(!is.na(fen0) & Month == 12)
fen0.12.fit <- lm(fen0 ~ tun0, data = fen0.12)
summary(fen0.12.fit) # R2 = 0.1872
churchill <- churchill %>% 
  mutate(pred = predict(fen0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(fen0 = ifelse(is.na(fen0) & Month == 12, pred, fen0))
churchill %>% as.data.frame()

plot(fen0 ~ rownames(churchill), data = churchill, type = "l", col = "blue")


## WSU 0 cm based on EC and wsu 150

## January
# Construct linear model based on non-NA pairs
wsu0.1 <- churchill %>% filter(!is.na(wsu0) & Month == 1)
wsu0.1.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.1)
summary(wsu0.1.fit) # R2 = 0.205
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 1, pred, wsu0))
churchill %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
wsu0.2 <- churchill %>% filter(!is.na(wsu0) & Month == 2)
wsu0.2.fit <- lm(wsu0 ~ wsu150, data = wsu0.2)
summary(wsu0.2.fit) # R2 = 0.1191
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 2, pred, wsu0))
churchill %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
wsu0.3 <- churchill %>% filter(!is.na(wsu0) & Month == 3)
wsu0.3.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.3)
summary(wsu0.3.fit) # R2 = 0.3277
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 3, pred, wsu0))
churchill %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
wsu0.4 <- churchill %>% filter(!is.na(wsu0) & Month == 4)
wsu0.4.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.4)
summary(wsu0.4.fit) # R2 = 0.5326
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 4, pred, wsu0))
churchill %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
wsu0.5 <- churchill %>% filter(!is.na(wsu0) & Month == 5)
wsu0.5.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.5)
summary(wsu0.5.fit) # R2 = 0.5353
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 5, pred, wsu0))
churchill %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
wsu0.6 <- churchill %>% filter(!is.na(wsu0) & Month == 6)
wsu0.6.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.6)
summary(wsu0.6.fit) # R2 = 0.6675
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 6, pred, wsu0))
churchill %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
wsu0.7 <- churchill %>% filter(!is.na(wsu0) & Month == 7)
wsu0.7.fit <- lm(wsu0 ~ wsu150, data = wsu0.7)
summary(wsu0.7.fit) # R2 = 0.6829
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 7, pred, wsu0))
churchill %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
wsu0.8 <- churchill %>% filter(!is.na(wsu0) & Month == 8)
wsu0.8.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.8)
summary(wsu0.8.fit) # R2 = 0.7773
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 8, pred, wsu0))
churchill %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
wsu0.9 <- churchill %>% filter(!is.na(wsu0) & Month == 9)
wsu0.9.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.9)
summary(wsu0.9.fit) # R2 = 0.871
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 9, pred, wsu0))
churchill %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
wsu0.10 <- churchill %>% filter(!is.na(wsu0) & Month == 10)
wsu0.10.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.10)
summary(wsu0.10.fit) # R2 = 0.8204
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 10, pred, wsu0))
churchill %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
wsu0.11 <- churchill %>% filter(!is.na(wsu0) & Month == 11)
wsu0.11.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.11)
summary(wsu0.11.fit) # R2 = 0.7738
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 11, pred, wsu0))
churchill %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
wsu0.12 <- churchill %>% filter(!is.na(wsu0) & Month == 12)
wsu0.12.fit <- lm(wsu0 ~ wsu150 + fen0, data = wsu0.12)
summary(wsu0.12.fit) # R2 = 0.5189
churchill <- churchill %>% 
  mutate(pred = predict(wsu0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(wsu0 = ifelse(is.na(wsu0) & Month == 12, pred, wsu0))
churchill %>% as.data.frame()

plot(wsu0 ~ rownames(churchill), data = churchill, type = "l", col = "blue")



matplot(rownames(churchill), churchill[,c(35,36,43,44,82,83,102,103)], type = "l")
matplot(rownames(churchill), churchill[,c(35,36)], type = "l", col = c("red","blue"))
matplot(rownames(churchill), churchill[,c(43,44)], type = "l", col = c("red","blue"))
matplot(rownames(churchill), churchill[,c(82,83)], type = "l", col = c("red","blue"))
matplot(rownames(churchill), churchill[,c(102,103)], type = "l", col = c("red","blue"))

air.t <- cbind.data.frame(churchill[,c(1:5)],
                          churchill$wsu150,
                          churchill$fen150,
                          churchill$ppa150,
                          churchill$tun150,
                          churchill$wsu0,
                          churchill$fen0,
                          churchill$ppa0,
                          churchill$tun0)



air.t.12.19 <- subset(air.t, Year %in% c(2012:2019))
names(air.t.12.19) <- c("date","year","month","day","jd",
                        "wsu150","fen150","ppa150","tun150",
                        "wsu0","fen0","ppa0","tun0")

matplot(rownames(air.t.12.19), air.t.12.19[,c(6:13)], type = "l")
plot(air.t.12.19$tun0, type = "l")

names(air.t.12.19)

DataForJon <- air.t.12.19 %>%
  group_by(year, month) %>%
  summarise_at(vars(wsu150:tun0), mean)

plot(DataForJon$wsu0~rownames(DataForJon), type = "l", ylim = c(-25,15))
lines(DataForJon$fen0, col = "red")
lines(DataForJon$ppa0, col = "blue")
lines(DataForJon$tun0, col = "green")

plot(DataForJon$wsu150~rownames(DataForJon), type = "l", ylim = c(-30,15))
lines(DataForJon$fen150, col = "red")
lines(DataForJon$ppa150, col = "blue")
lines(DataForJon$tun150, col = "green")


write.csv(DataForJon, "~/Desktop/DataForJon.csv")


