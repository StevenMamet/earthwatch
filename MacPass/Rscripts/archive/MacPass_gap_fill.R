library(dplyr)
library(psych)    # Pairs plot
library(splines)
library(lubridate)

rm(list=ls())

# Mac Pass met station data
# macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/microclimate.mm2.csv", header = TRUE)
macpass <- read.csv(file = "~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Workspace/Earthwatch/microclimate.mm2.csv", header = TRUE)

# Converts to date format (lubridate)
macpass$Date <- ymd(macpass$Date)

plot(gf.150 ~ Date, macpass, type = "l")
plot(gf.0 ~ Date, macpass, type = "l")
plot(gf.neg150 ~ Date, macpass, type = "l")

plot(d2.150 ~ Date, macpass, type = "l")
plot(d2.0 ~ Date, macpass, type = "l")
plot(d2.neg150 ~ Date, macpass, type = "l")

plot(d6.150 ~ Date, macpass, type = "l")
plot(d6.0 ~ Date, macpass, type = "l")
plot(d6.neg150 ~ Date, macpass, type = "l")

plot(hf.150 ~ Date, macpass, type = "l")
plot(hf.0 ~ Date, macpass, type = "l")
plot(hf.neg150 ~ Date, macpass, type = "l")

plot(bp.150 ~ Date, macpass, type = "l")
plot(bp.0 ~ Date, macpass, type = "l")
plot(bp.neg150 ~ Date, macpass, type = "l")

###################################################################
## ********************
### Step 1: Fill the missing air temperatures

## Hare Foot

## January
# Construct linear model based on non-NA pairs
hf150.1 <- macpass %>% filter(!is.na(hf.150) & Month == 1)
hf150.1.fit <- lm(hf.150 ~ mean.150, data = hf150.1)
summary(hf150.1.fit) # R2 = 0.8655
macpass <- macpass %>% 
  mutate(pred = predict(hf150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 1, pred, hf.150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
hf150.2 <- macpass %>% filter(!is.na(hf.150) & Month == 2)
hf150.2.fit <- lm(hf.150 ~ mean.150, data = hf150.2)
summary(hf150.2.fit) # R2 = 0.8218
macpass <- macpass %>% 
  mutate(pred = predict(hf150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 2, pred, hf.150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
hf150.3 <- macpass %>% filter(!is.na(hf.150) & Month == 3)
hf150.3.fit <- lm(hf.150 ~ mean.150, data = hf150.3)
summary(hf150.3.fit) # R2 = 0.8822
macpass <- macpass %>% 
  mutate(pred = predict(hf150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 3, pred, hf.150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
hf150.4 <- macpass %>% filter(!is.na(hf.150) & Month == 4)
hf150.4.fit <- lm(hf.150 ~ mean.150, data = hf150.4)
summary(hf150.4.fit) # R2 = 0.9502
macpass <- macpass %>% 
  mutate(pred = predict(hf150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 4, pred, hf.150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
hf150.5 <- macpass %>% filter(!is.na(hf.150) & Month == 5)
hf150.5.fit <- lm(hf.150 ~ mean.150, data = hf150.5)
summary(hf150.5.fit) # R2 = 0.9577
macpass <- macpass %>% 
  mutate(pred = predict(hf150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 5, pred, hf.150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
hf150.6 <- macpass %>% filter(!is.na(hf.150) & Month == 6)
hf150.6.fit <- lm(hf.150 ~ mean.150, data = hf150.6)
summary(hf150.6.fit) # R2 = 0.947
macpass <- macpass %>% 
  mutate(pred = predict(hf150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 6, pred, hf.150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
hf150.7 <- macpass %>% filter(!is.na(hf.150) & Month == 7)
hf150.7.fit <- lm(hf.150 ~ mean.150, data = hf150.7)
summary(hf150.7.fit) # R2 = 0.9445
macpass <- macpass %>% 
  mutate(pred = predict(hf150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 7, pred, hf.150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
hf150.8 <- macpass %>% filter(!is.na(hf.150) & Month == 8)
hf150.8.fit <- lm(hf.150 ~ mean.150, data = hf150.8)
summary(hf150.8.fit) # R2 = 0.9504
macpass <- macpass %>% 
  mutate(pred = predict(hf150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 8, pred, hf.150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
hf150.9 <- macpass %>% filter(!is.na(hf.150) & Month == 9)
hf150.9.fit <- lm(hf.150 ~ mean.150, data = hf150.9)
summary(hf150.9.fit) # R2 = 0.958
macpass <- macpass %>% 
  mutate(pred = predict(hf150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 9, pred, hf.150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
hf150.10 <- macpass %>% filter(!is.na(hf.150) & Month == 10)
hf150.10.fit <- lm(hf.150 ~ mean.150, data = hf150.10)
summary(hf150.10.fit) # R2 = 0.9435
macpass <- macpass %>% 
  mutate(pred = predict(hf150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 10, pred, hf.150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
hf150.11 <- macpass %>% filter(!is.na(hf.150) & Month == 11)
hf150.11.fit <- lm(hf.150 ~ mean.150, data = hf150.11)
summary(hf150.11.fit) # R2 = 0.9459
macpass <- macpass %>% 
  mutate(pred = predict(hf150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 11, pred, hf.150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
hf150.12 <- macpass %>% filter(!is.na(hf.150) & Month == 12)
hf150.12.fit <- lm(hf.150 ~ mean.150, data = hf150.12)
summary(hf150.12.fit) # R2 = 0.9404
macpass <- macpass %>% 
  mutate(pred = predict(hf150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.150 = ifelse(is.na(hf.150) & Month == 12, pred, hf.150))
macpass %>% as.data.frame()



## ********************
## Beaver Pond

## January
# Construct linear model based on non-NA pairs
bp150.1 <- macpass %>% filter(!is.na(bp.150) & Month == 1)
bp150.1.fit <- lm(bp.150 ~ mean.150, data = bp150.1)
summary(bp150.1.fit) # R2 = 0.9671
macpass <- macpass %>% 
  mutate(pred = predict(bp150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bp150.2 <- macpass %>% filter(!is.na(bp.150) & Month == 2)
bp150.2.fit <- lm(bp.150 ~ mean.150, data = bp150.2)
summary(bp150.2.fit) # R2 = 0.9693
macpass <- macpass %>% 
  mutate(pred = predict(bp150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bp150.3 <- macpass %>% filter(!is.na(bp.150) & Month == 3)
bp150.3.fit <- lm(bp.150 ~ mean.150, data = bp150.3)
summary(bp150.3.fit) # R2 = 0.9725
macpass <- macpass %>% 
  mutate(pred = predict(bp150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bp150.4 <- macpass %>% filter(!is.na(bp.150) & Month == 4)
bp150.4.fit <- lm(bp.150 ~ mean.150, data = bp150.4)
summary(bp150.4.fit) # R2 = 0.9726
macpass <- macpass %>% 
  mutate(pred = predict(bp150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bp150.5 <- macpass %>% filter(!is.na(bp.150) & Month == 5)
bp150.5.fit <- lm(bp.150 ~ mean.150, data = bp150.5)
summary(bp150.5.fit) # R2 = 0.9726
macpass <- macpass %>% 
  mutate(pred = predict(bp150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bp150.6 <- macpass %>% filter(!is.na(bp.150) & Month == 6)
bp150.6.fit <- lm(bp.150 ~ mean.150, data = bp150.6)
summary(bp150.6.fit) # R2 = 0.9405
macpass <- macpass %>% 
  mutate(pred = predict(bp150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bp150.7 <- macpass %>% filter(!is.na(bp.150) & Month == 7)
bp150.7.fit <- lm(bp.150 ~ mean.150, data = bp150.7)
summary(bp150.7.fit) # R2 = 0.9025
macpass <- macpass %>% 
  mutate(pred = predict(bp150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bp150.8 <- macpass %>% filter(!is.na(bp.150) & Month == 8)
bp150.8.fit <- lm(bp.150 ~ mean.150, data = bp150.8)
summary(bp150.8.fit) # R2 = 0.9366
macpass <- macpass %>% 
  mutate(pred = predict(bp150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bp150.9 <- macpass %>% filter(!is.na(bp.150) & Month == 9)
bp150.9.fit <- lm(bp.150 ~ mean.150, data = bp150.9)
summary(bp150.9.fit) # R2 = 0.9571
macpass <- macpass %>% 
  mutate(pred = predict(bp150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bp150.10 <- macpass %>% filter(!is.na(bp.150) & Month == 10)
bp150.10.fit <- lm(bp.150 ~ mean.150, data = bp150.10)
summary(bp150.10.fit) # R2 = 0.9696
macpass <- macpass %>% 
  mutate(pred = predict(bp150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bp150.11 <- macpass %>% filter(!is.na(bp.150) & Month == 11)
bp150.11.fit <- lm(bp.150 ~ mean.150, data = bp150.11)
summary(bp150.11.fit) # R2 = 0.9718
macpass <- macpass %>% 
  mutate(pred = predict(bp150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bp150.12 <- macpass %>% filter(!is.na(bp.150) & Month == 12)
bp150.12.fit <- lm(bp.150 ~ mean.150, data = bp150.12)
summary(bp150.12.fit) # R2 = 0.9672
macpass <- macpass %>% 
  mutate(pred = predict(bp150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.150 = ifelse(is.na(bp.150), pred, bp.150))
macpass %>% as.data.frame()


## ********************
## Dale2

## January
# Construct linear model based on non-NA pairs
d2150.1 <- macpass %>% filter(!is.na(d2.150) & Month == 1)
d2150.1.fit <- lm(d2.150 ~ mean.150, data = d2150.1)
summary(d2150.1.fit) # R2 = 0.8168
macpass <- macpass %>% 
  mutate(pred = predict(d2150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d2150.2 <- macpass %>% filter(!is.na(d2.150) & Month == 2)
d2150.2.fit <- lm(d2.150 ~ mean.150, data = d2150.2)
summary(d2150.2.fit) # R2 = 0.8728
macpass <- macpass %>% 
  mutate(pred = predict(d2150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d2150.3 <- macpass %>% filter(!is.na(d2.150) & Month == 3)
d2150.3.fit <- lm(d2.150 ~ mean.150, data = d2150.3)
summary(d2150.3.fit) # R2 = 0.876
macpass <- macpass %>% 
  mutate(pred = predict(d2150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d2150.4 <- macpass %>% filter(!is.na(d2.150) & Month == 4)
d2150.4.fit <- lm(d2.150 ~ mean.150, data = d2150.4)
summary(d2150.4.fit) # R2 = 0.8698
macpass <- macpass %>% 
  mutate(pred = predict(d2150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d2150.5 <- macpass %>% filter(!is.na(d2.150) & Month == 5)
d2150.5.fit <- lm(d2.150 ~ mean.150, data = d2150.5)
summary(d2150.5.fit) # R2 = 0.9122
macpass <- macpass %>% 
  mutate(pred = predict(d2150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d2150.6 <- macpass %>% filter(!is.na(d2.150) & Month == 6)
d2150.6.fit <- lm(d2.150 ~ mean.150, data = d2150.6)
summary(d2150.6.fit) # R2 = 0.851
macpass <- macpass %>% 
  mutate(pred = predict(d2150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d2150.7 <- macpass %>% filter(!is.na(d2.150) & Month == 7)
d2150.7.fit <- lm(d2.150 ~ mean.150, data = d2150.7)
summary(d2150.7.fit) # R2 = 0.8333
macpass <- macpass %>% 
  mutate(pred = predict(d2150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d2150.8 <- macpass %>% filter(!is.na(d2.150) & Month == 8)
d2150.8.fit <- lm(d2.150 ~ mean.150, data = d2150.8)
summary(d2150.8.fit) # R2 = 0.8749
macpass <- macpass %>% 
  mutate(pred = predict(d2150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d2150.9 <- macpass %>% filter(!is.na(d2.150) & Month == 9)
d2150.9.fit <- lm(d2.150 ~ mean.150, data = d2150.9)
summary(d2150.9.fit) # R2 = 0.8272
macpass <- macpass %>% 
  mutate(pred = predict(d2150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d2150.10 <- macpass %>% filter(!is.na(d2.150) & Month == 10)
d2150.10.fit <- lm(d2.150 ~ mean.150, data = d2150.10)
summary(d2150.10.fit) # R2 = 0.8338
macpass <- macpass %>% 
  mutate(pred = predict(d2150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d2150.11 <- macpass %>% filter(!is.na(d2.150) & Month == 11)
d2150.11.fit <- lm(d2.150 ~ mean.150, data = d2150.11)
summary(d2150.11.fit) # R2 = 0.8517
macpass <- macpass %>% 
  mutate(pred = predict(d2150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d2150.12 <- macpass %>% filter(!is.na(d2.150) & Month == 12)
d2150.12.fit <- lm(d2.150 ~ mean.150, data = d2150.12)
summary(d2150.12.fit) # R2 = 0.8311
macpass <- macpass %>% 
  mutate(pred = predict(d2150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.150 = ifelse(is.na(d2.150), pred, d2.150))
macpass %>% as.data.frame()



## ********************
## Dale6

## January
# Construct linear model based on non-NA pairs
d6150.1 <- macpass %>% filter(!is.na(d6.150) & Month == 1)
d6150.1.fit <- lm(d6.150 ~ mean.150, data = d6150.1)
summary(d6150.1.fit) # R2 = 0.9532
macpass <- macpass %>% 
  mutate(pred = predict(d6150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d6150.2 <- macpass %>% filter(!is.na(d6.150) & Month == 2)
d6150.2.fit <- lm(d6.150 ~ mean.150, data = d6150.2)
summary(d6150.2.fit) # R2 = 0.9473
macpass <- macpass %>% 
  mutate(pred = predict(d6150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d6150.3 <- macpass %>% filter(!is.na(d6.150) & Month == 3)
d6150.3.fit <- lm(d6.150 ~ mean.150, data = d6150.3)
summary(d6150.3.fit) # R2 = 0.9552
macpass <- macpass %>% 
  mutate(pred = predict(d6150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d6150.4 <- macpass %>% filter(!is.na(d6.150) & Month == 4)
d6150.4.fit <- lm(d6.150 ~ mean.150, data = d6150.4)
summary(d6150.4.fit) # R2 = 0.9465
macpass <- macpass %>% 
  mutate(pred = predict(d6150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d6150.5 <- macpass %>% filter(!is.na(d6.150) & Month == 5)
d6150.5.fit <- lm(d6.150 ~ mean.150, data = d6150.5)
summary(d6150.5.fit) # R2 = 0.9356
macpass <- macpass %>% 
  mutate(pred = predict(d6150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d6150.6 <- macpass %>% filter(!is.na(d6.150) & Month == 6)
d6150.6.fit <- lm(d6.150 ~ mean.150, data = d6150.6)
summary(d6150.6.fit) # R2 = 0.8366
macpass <- macpass %>% 
  mutate(pred = predict(d6150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d6150.7 <- macpass %>% filter(!is.na(d6.150) & Month == 7)
d6150.7.fit <- lm(d6.150 ~ mean.150, data = d6150.7)
summary(d6150.7.fit) # R2 = 0.8125
macpass <- macpass %>% 
  mutate(pred = predict(d6150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d6150.8 <- macpass %>% filter(!is.na(d6.150) & Month == 8)
d6150.8.fit <- lm(d6.150 ~ mean.150, data = d6150.8)
summary(d6150.8.fit) # R2 = 0.8753
macpass <- macpass %>% 
  mutate(pred = predict(d6150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d6150.9 <- macpass %>% filter(!is.na(d6.150) & Month == 9)
d6150.9.fit <- lm(d6.150 ~ mean.150, data = d6150.9)
summary(d6150.9.fit) # R2 = 0.9303
macpass <- macpass %>% 
  mutate(pred = predict(d6150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d6150.10 <- macpass %>% filter(!is.na(d6.150) & Month == 10)
d6150.10.fit <- lm(d6.150 ~ mean.150, data = d6150.10)
summary(d6150.10.fit) # R2 = 0.9398
macpass <- macpass %>% 
  mutate(pred = predict(d6150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d6150.11 <- macpass %>% filter(!is.na(d6.150) & Month == 11)
d6150.11.fit <- lm(d6.150 ~ mean.150, data = d6150.11)
summary(d6150.11.fit) # R2 = 0.9591
macpass <- macpass %>% 
  mutate(pred = predict(d6150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d6150.12 <- macpass %>% filter(!is.na(d6.150) & Month == 12)
d6150.12.fit <- lm(d6.150 ~ mean.150, data = d6150.12)
summary(d6150.12.fit) # R2 = 0.9455
macpass <- macpass %>% 
  mutate(pred = predict(d6150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.150 = ifelse(is.na(d6.150), pred, d6.150))
macpass %>% as.data.frame()




## ********************
## Goose Flats

## January
# Construct linear model based on non-NA pairs
gf150.1 <- macpass %>% filter(!is.na(gf.150) & Month == 1)
gf150.1.fit <- lm(gf.150 ~ mean.150, data = gf150.1)
summary(gf150.1.fit) # R2 = 0.8623
macpass <- macpass %>% 
  mutate(pred = predict(gf150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
gf150.2 <- macpass %>% filter(!is.na(gf.150) & Month == 2)
gf150.2.fit <- lm(gf.150 ~ mean.150, data = gf150.2)
summary(gf150.2.fit) # R2 = 0.8971
macpass <- macpass %>% 
  mutate(pred = predict(gf150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
gf150.3 <- macpass %>% filter(!is.na(gf.150) & Month == 3)
gf150.3.fit <- lm(gf.150 ~ mean.150, data = gf150.3)
summary(gf150.3.fit) # R2 = 0.904
macpass <- macpass %>% 
  mutate(pred = predict(gf150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
gf150.4 <- macpass %>% filter(!is.na(gf.150) & Month == 4)
gf150.4.fit <- lm(gf.150 ~ mean.150, data = gf150.4)
summary(gf150.4.fit) # R2 = 0.9233
macpass <- macpass %>% 
  mutate(pred = predict(gf150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
gf150.5 <- macpass %>% filter(!is.na(gf.150) & Month == 5)
gf150.5.fit <- lm(gf.150 ~ mean.150, data = gf150.5)
summary(gf150.5.fit) # R2 = 0.901
macpass <- macpass %>% 
  mutate(pred = predict(gf150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
gf150.6 <- macpass %>% filter(!is.na(gf.150) & Month == 6)
gf150.6.fit <- lm(gf.150 ~ mean.150, data = gf150.6)
summary(gf150.6.fit) # R2 = 0.7191
macpass <- macpass %>% 
  mutate(pred = predict(gf150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
gf150.7 <- macpass %>% filter(!is.na(gf.150) & Month == 7)
gf150.7.fit <- lm(gf.150 ~ mean.150, data = gf150.7)
summary(gf150.7.fit) # R2 = 0.5568
macpass <- macpass %>% 
  mutate(pred = predict(gf150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
gf150.8 <- macpass %>% filter(!is.na(gf.150) & Month == 8)
gf150.8.fit <- lm(gf.150 ~ mean.150, data = gf150.8)
summary(gf150.8.fit) # R2 = 0.7573
macpass <- macpass %>% 
  mutate(pred = predict(gf150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
gf150.9 <- macpass %>% filter(!is.na(gf.150) & Month == 9)
gf150.9.fit <- lm(gf.150 ~ mean.150, data = gf150.9)
summary(gf150.9.fit) # R2 = 0.8859
macpass <- macpass %>% 
  mutate(pred = predict(gf150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
gf150.10 <- macpass %>% filter(!is.na(gf.150) & Month == 10)
gf150.10.fit <- lm(gf.150 ~ mean.150, data = gf150.10)
summary(gf150.10.fit) # R2 = 0.8913
macpass <- macpass %>% 
  mutate(pred = predict(gf150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
gf150.11 <- macpass %>% filter(!is.na(gf.150) & Month == 11)
gf150.11.fit <- lm(gf.150 ~ mean.150, data = gf150.11)
summary(gf150.11.fit) # R2 = 0.8584
macpass <- macpass %>% 
  mutate(pred = predict(gf150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
gf150.12 <- macpass %>% filter(!is.na(gf.150) & Month == 12)
gf150.12.fit <- lm(gf.150 ~ mean.150, data = gf150.12)
summary(gf150.12.fit) # R2 = 0.8917
macpass <- macpass %>% 
  mutate(pred = predict(gf150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.150 = ifelse(is.na(gf.150), pred, gf.150))
macpass %>% as.data.frame()



###################################################################
## ********************
### Step 2: Fill the missing HF temperatures

## Hare Foot (0 cm)

## January
# Construct linear model based on non-NA pairs
hf0.1 <- macpass %>% filter(!is.na(hf.0) & Month == 1)
ccf(hf0.1$hf.150, hf0.1$hf.0)
hf0.1.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.1)
summary(hf0.1.fit) # R2 = 0.9634
macpass <- macpass %>% 
  mutate(pred = predict(hf0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
hf0.2 <- macpass %>% filter(!is.na(hf.0) & Month == 2)
ccf(hf0.2$hf.150, hf0.2$hf.0)
hf0.2.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.2)
summary(hf0.2.fit) # R2 = 0.9646
macpass <- macpass %>% 
  mutate(pred = predict(hf0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
hf0.3 <- macpass %>% filter(!is.na(hf.0) & Month == 3)
ccf(hf0.3$hf.150, hf0.3$hf.0)
hf0.3.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.3)
summary(hf0.3.fit) # R2 = 0.9626
macpass <- macpass %>% 
  mutate(pred = predict(hf0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
hf0.4 <- macpass %>% filter(!is.na(hf.0) & Month == 4)
ccf(hf0.4$hf.150, hf0.4$hf.0)
hf0.4.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.4)
summary(hf0.4.fit) # R2 = 0.9841
macpass <- macpass %>% 
  mutate(pred = predict(hf0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
hf0.5 <- macpass %>% filter(!is.na(hf.0) & Month == 5)
ccf(hf0.5$hf.150, hf0.5$hf.0)
hf0.5.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.5)
summary(hf0.5.fit) # R2 = 0.8584
macpass <- macpass %>% 
  mutate(pred = predict(hf0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
hf0.6 <- macpass %>% filter(!is.na(hf.0) & Month == 6)
ccf(hf0.6$hf.150, hf0.6$hf.0)
hf0.6.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.6)
summary(hf0.6.fit) # R2 = 0.7928
macpass <- macpass %>% 
  mutate(pred = predict(hf0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
hf0.7 <- macpass %>% filter(!is.na(hf.0) & Month == 7)
ccf(hf0.7$hf.150, hf0.7$hf.0)
hf0.7.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.7)
summary(hf0.7.fit) # R2 = 0.8433
macpass <- macpass %>% 
  mutate(pred = predict(hf0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
hf0.8 <- macpass %>% filter(!is.na(hf.0) & Month == 8)
ccf(hf0.8$hf.150, hf0.8$hf.0)
hf0.8.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.8)
summary(hf0.8.fit) # R2 = 0.9501
macpass <- macpass %>% 
  mutate(pred = predict(hf0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
hf0.9 <- macpass %>% filter(!is.na(hf.0) & Month == 9)
ccf(hf0.9$hf.150, hf0.9$hf.0)
hf0.9.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.9)
summary(hf0.9.fit) # R2 = 0.9562
macpass <- macpass %>% 
  mutate(pred = predict(hf0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
hf0.10 <- macpass %>% filter(!is.na(hf.0) & Month == 10)
ccf(hf0.10$hf.150, hf0.10$hf.0)
hf0.10.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.10)
summary(hf0.10.fit) # R2 = 0.8972
macpass <- macpass %>% 
  mutate(pred = predict(hf0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
hf0.11 <- macpass %>% filter(!is.na(hf.0) & Month == 11)
ccf(hf0.11$hf.150, hf0.11$hf.0)
hf0.11.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.11)
summary(hf0.11.fit) # R2 = 0.9165
macpass <- macpass %>% 
  mutate(pred = predict(hf0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
hf0.12 <- macpass %>% filter(!is.na(hf.0) & Month == 12)
ccf(hf0.12$hf.150, hf0.12$hf.0)
hf0.12.fit <- lm(hf.0 ~ hf.150 + hf.neg.5, data = hf0.12)
summary(hf0.12.fit) # R2 = 0.9582
macpass <- macpass %>% 
  mutate(pred = predict(hf0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.integer(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$hf.0, col = "blue")
lines(macpass$Date, macpass$hf.neg2p5, col = "green")


## Hare Foot (-2.5cm)

## January
# Construct linear model based on non-NA pairs
hfneg2p5.1 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 1)
ccf(hfneg2p5.1$hf.150, hfneg2p5.1$hf.neg2p5)
hfneg2p5.1.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.1)
summary(hfneg2p5.1.fit) # R2 = 0.9874
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
hfneg2p5.2 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 2)
ccf(hfneg2p5.2$hf.150, hfneg2p5.2$hf.neg2p5)
hfneg2p5.2.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.2)
summary(hfneg2p5.2.fit) # R2 = 0.9871
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
hfneg2p5.3 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 3)
ccf(hfneg2p5.3$hf.150, hfneg2p5.3$hf.neg2p5)
hfneg2p5.3.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.3)
summary(hfneg2p5.3.fit) # R2 = 0.9887
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
hfneg2p5.4 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 4)
ccf(hfneg2p5.4$hf.150, hfneg2p5.4$hf.neg2p5)
hfneg2p5.4.fit <- lm(hf.neg2p5 ~ hf.neg.5, data = hfneg2p5.4)
summary(hfneg2p5.4.fit) # R2 = 0.9942
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
hfneg2p5.5 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 5)
ccf(hfneg2p5.5$hf.150, hfneg2p5.5$hf.neg2p5)
hfneg2p5.5.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.5)
summary(hfneg2p5.5.fit) # R2 = 0.9279
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
hfneg2p5.6 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 6)
ccf(hfneg2p5.6$hf.150, hfneg2p5.6$hf.neg2p5)
hfneg2p5.6.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.6)
summary(hfneg2p5.6.fit) # R2 = 0.9079
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
hfneg2p5.7 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 7)
ccf(hfneg2p5.7$hf.150, hfneg2p5.7$hf.neg2p5)
hfneg2p5.7.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.7)
summary(hfneg2p5.7.fit) # R2 = 0.9284
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
hfneg2p5.8 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 8)
ccf(hfneg2p5.8$hf.150, hfneg2p5.8$hf.neg2p5)
hfneg2p5.8.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.8)
summary(hfneg2p5.8.fit) # R2 = 0.9774
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
hfneg2p5.9 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 9)
ccf(hfneg2p5.9$hf.150, hfneg2p5.9$hf.neg2p5)
hfneg2p5.9.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.9)
summary(hfneg2p5.9.fit) # R2 = 0.9791
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
hfneg2p5.10 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 10)
ccf(hfneg2p5.10$hf.150, hfneg2p5.10$hf.neg2p5)
hfneg2p5.10.fit <- lm(hf.neg2p5 ~ hf.0 + hf.neg.5, data = hfneg2p5.10)
summary(hfneg2p5.10.fit) # R2 = 0.9362
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
hfneg2p5.11 <- macpass %>% filter(!is.na(hf.neg2p5) & Month == 11)
ccf(hfneg2p5.11$hf.150, hfneg2p5.11$hf.neg2p5)
hfneg2p5.11.fit <- lm(hf.neg2p5 ~ hf.150 + hf.0 + hf.neg.5, data = hfneg2p5.11)
summary(hfneg2p5.11.fit) # R2 = 0.8162
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg2p5 = ifelse(is.na(hf.neg2p5), pred, hf.neg2p5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
hfneg2p5.12 <- macpass %>% filter(!is.na(hf.0) & Month == 12)
ccf(hfneg2p5.12$hf.neg2p5, hfneg2p5.12$hf.0)
hfneg2p5.12.fit <- lm(hf.neg2p5 ~ hf.neg.5, data = hfneg2p5.12)
summary(hfneg2p5.12.fit) # R2 = 0.8917
macpass <- macpass %>% 
  mutate(pred = predict(hfneg2p5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$hf.neg2p5, col = "blue")


## Hare Foot (-5cm)

## January
# Construct linear model based on non-NA pairs
hfneg5.1 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 1)
ccf(hfneg5.1$hf.150, hfneg5.1$hf.neg.5)
hfneg5.1.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.1)
summary(hfneg5.1.fit) # R2 = 0.9878
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
hfneg5.2 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 2)
ccf(hfneg5.2$hf.150, hfneg5.2$hf.neg.5)
hfneg5.2.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.2)
summary(hfneg5.2.fit) # R2 = 0.9875
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
hfneg5.3 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 3)
ccf(hfneg5.3$hf.150, hfneg5.3$hf.neg.5)
hfneg5.3.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.3)
summary(hfneg5.3.fit) # R2 = 0.9892
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
hfneg5.4 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 4)
ccf(hfneg5.4$hf.150, hfneg5.4$hf.neg.5)
hfneg5.4.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.4)
summary(hfneg5.4.fit) # R2 = 0.9956
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
hfneg5.5 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 5)
ccf(hfneg5.5$hf.150, hfneg5.5$hf.neg.5)
hfneg5.5.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.5)
summary(hfneg5.5.fit) # R2 = 0.9203
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
hfneg5.6 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 6)
ccf(hfneg5.6$hf.150, hfneg5.6$hf.neg.5)
hfneg5.6.fit <- lm(hf.neg.5 ~ hf.neg2p5, data = hfneg5.6)
summary(hfneg5.6.fit) # R2 = 0.8685
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
hfneg5.7 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 7)
ccf(hfneg5.7$hf.150, hfneg5.7$hf.neg.5)
hfneg5.7.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.7)
summary(hfneg5.7.fit) # R2 = 0.905
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
hfneg5.8 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 8)
ccf(hfneg5.8$hf.150, hfneg5.8$hf.neg.5)
hfneg5.8.fit <- lm(hf.neg.5 ~ hf.neg2p5, data = hfneg5.8)
summary(hfneg5.8.fit) # R2 = 0.9684
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
hfneg5.9 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 9)
ccf(hfneg5.9$hf.150, hfneg5.9$hf.neg.5)
hfneg5.9.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.9)
summary(hfneg5.9.fit) # R2 = 0.9762
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
hfneg5.10 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 10)
ccf(hfneg5.10$hf.150, hfneg5.10$hf.neg.5)
hfneg5.10.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.10)
summary(hfneg5.10.fit) # R2 = 0.9406
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
hfneg5.11 <- macpass %>% filter(!is.na(hf.neg.5) & Month == 11)
ccf(hfneg5.11$hf.150, hfneg5.11$hf.neg.5)
hfneg5.11.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.11)
summary(hfneg5.11.fit) # R2 = 0.9224
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg.5 = ifelse(is.na(hf.neg.5), pred, hf.neg.5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
hfneg5.12 <- macpass %>% filter(!is.na(hf.0) & Month == 12)
ccf(hfneg5.12$hf.neg.5, hfneg5.12$hf.0)
hfneg5.12.fit <- lm(hf.neg.5 ~ hf.0 + hf.neg2p5, data = hfneg5.12)
summary(hfneg5.12.fit) # R2 = 0.9641
macpass <- macpass %>% 
  mutate(pred = predict(hfneg5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.0 = ifelse(is.na(hf.0), pred, hf.0))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$hf.neg.5, col = "blue")
plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 1))
lines(macpass$Date, macpass$hf.neg150, col = "blue")

## Hare Foot (-150cm)

## All
# Construct linear model based on non-NA pairs
hfneg150.all <- macpass %>% filter(!is.na(hf.neg150) & Month == 1)
hfneg150.all.fit <- lm(hf.neg150 ~ hf.0 + hf.neg2p5, data = hfneg150.all)
summary(hfneg150.all.fit) # R2 = 0.2049
macpass <- macpass %>% 
  mutate(pred = predict(hfneg150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(hf.neg150 = ifelse(is.na(hf.neg150), pred, hf.neg150))
macpass %>% as.data.frame()


macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 1] <- macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 1] -2
macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 0.9] <- macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 0.9] -1.75
macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 0] <- macpass$hf.neg150[!is.na(macpass$hf.neg150) & macpass$hf.neg150 > 0] *-1

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-8, 2))
lines(macpass$Date, macpass$hf.neg150, col = "blue")
abline(h = 0)


hf.0.spline <- spline(as.numeric(macpass$Date), macpass$hf.0, method = "periodic", n = 365.25)
hf.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$hf.neg2p5, method = "periodic", n = 365.25)
hf.neg.5.spline <- spline(as.numeric(macpass$Date), macpass$hf.neg.5, method = "periodic", n = 365.25)
hf.neg150.spline <- spline(as.numeric(macpass$Date), macpass$hf.neg150, method = "periodic", n = 365.25)
hf.0.spline.int <- approx(hf.neg150.spline$x, hf.0.spline$y, n=10627)
hf.neg2p5.spline.int <- approx(hf.neg150.spline$x, hf.neg2p5.spline$y, n=10627)
hf.neg.5.spline.int <- approx(hf.neg150.spline$x, hf.neg.5.spline$y, n=10627)
hf.neg150.spline.int <- approx(hf.neg150.spline$x, hf.neg150.spline$y, n=10627)
macpass$hf.0.s <- hf.0.spline.int$y
macpass$hf.neg2p5.s <- hf.neg2p5.spline.int$y
macpass$hf.neg.5.s <- hf.neg.5.spline.int$y
macpass$hf.neg150.s <- hf.neg150.spline.int$y


plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-40, 20))
lines(macpass$Date, macpass$hf.0.s, col = "darkred")
lines(macpass$Date, macpass$hf.neg2p5.s, col = "red")
lines(macpass$Date, macpass$hf.neg.5.s, col = "orange")
lines(macpass$Date, macpass$hf.neg150.s, col = "blue")


## Beaver Pond (0 cm)

## January
# Construct linear model based on non-NA pairs
bp0.1 <- macpass %>% filter(!is.na(bp.0) & Month == 1)
bp0.1.fit <- lm(bp.0 ~ hf.0, data = bp0.1)
summary(bp0.1.fit) # R2 = 0.8035
macpass <- macpass %>% 
  mutate(pred = predict(bp0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bp0.2 <- macpass %>% filter(!is.na(bp.0) & Month == 2)
bp0.2.fit <- lm(bp.0 ~ hf.0, data = bp0.2)
summary(bp0.2.fit) # R2 = 0.7983
macpass <- macpass %>% 
  mutate(pred = predict(bp0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bp0.3 <- macpass %>% filter(!is.na(bp.0) & Month == 3)
bp0.3.fit <- lm(bp.0 ~ hf.0, data = bp0.3)
summary(bp0.3.fit) # R2 = 0.8923
macpass <- macpass %>% 
  mutate(pred = predict(bp0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bp0.4 <- macpass %>% filter(!is.na(bp.0) & Month == 4)
bp0.4.fit <- lm(bp.0 ~ hf.0, data = bp0.4)
summary(bp0.4.fit) # R2 = 0.8987
macpass <- macpass %>% 
  mutate(pred = predict(bp0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bp0.5 <- macpass %>% filter(!is.na(bp.0) & Month == 5)
bp0.5.fit <- lm(bp.0 ~ hf.0, data = bp0.5)
summary(bp0.5.fit) # R2 = 0.8911
macpass <- macpass %>% 
  mutate(pred = predict(bp0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bp0.6 <- macpass %>% filter(!is.na(bp.0) & Month == 6)
bp0.6.fit <- lm(bp.0 ~ hf.0, data = bp0.6)
summary(bp0.6.fit) # R2 = 0.8369
macpass <- macpass %>% 
  mutate(pred = predict(bp0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bp0.7 <- macpass %>% filter(!is.na(bp.0) & Month == 7)
bp0.7.fit <- lm(bp.0 ~ hf.0, data = bp0.7)
summary(bp0.7.fit) # R2 = 0.806
macpass <- macpass %>% 
  mutate(pred = predict(bp0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bp0.8 <- macpass %>% filter(!is.na(bp.0) & Month == 8)
bp0.8.fit <- lm(bp.0 ~ hf.0, data = bp0.8)
summary(bp0.8.fit) # R2 = 0.8866
macpass <- macpass %>% 
  mutate(pred = predict(bp0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bp0.9 <- macpass %>% filter(!is.na(bp.0) & Month == 9)
bp0.9.fit <- lm(bp.0 ~ hf.0, data = bp0.9)
summary(bp0.9.fit) # R2 = 0.9479
macpass <- macpass %>% 
  mutate(pred = predict(bp0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bp0.10 <- macpass %>% filter(!is.na(bp.0) & Month == 10)
bp0.10.fit <- lm(bp.0 ~ hf.0, data = bp0.10)
summary(bp0.10.fit) # R2 = 0.8724
macpass <- macpass %>% 
  mutate(pred = predict(bp0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bp0.11 <- macpass %>% filter(!is.na(bp.0) & Month == 11)
bp0.11.fit <- lm(bp.0 ~ hf.0, data = bp0.11)
summary(bp0.11.fit) # R2 = 0.8476
macpass <- macpass %>% 
  mutate(pred = predict(bp0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bp0.12 <- macpass %>% filter(!is.na(bp.0) & Month == 12)
bp0.12.fit <- lm(bp.0 ~ hf.0, data = bp0.12)
summary(bp0.12.fit) # R2 = 0.8202
macpass <- macpass %>% 
  mutate(pred = predict(bp0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.0 = ifelse(is.na(bp.0), pred, bp.0))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$bp.0, col = "blue")


## Beaver Pond (-2.5 cm)

## January
# Construct linear model based on non-NA pairs
bpneg2p5.1 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 1)
bpneg2p5.1.fit <- lm(bp.neg2p5 ~ bp.neg0 + hf.neg2p5, data = bpneg2p5.1)
summary(bpneg2p5.1.fit) # R2 = 0.8506
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bpneg2p5.2 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 2)
bpneg2p5.2.fit <- lm(bp.neg2p5 ~ bp.0, data = bpneg2p5.2)
summary(bpneg2p5.2.fit) # R2 = 0.894
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bpneg2p5.3 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 3)
bpneg2p5.3.fit <- lm(bp.neg2p5 ~ bp.0, data = bpneg2p5.3)
summary(bpneg2p5.3.fit) # R2 = 0.9518
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bpneg2p5.4 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 4)
bpneg2p5.4.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5 + hf.neg.5, data = bpneg2p5.4)
summary(bpneg2p5.4.fit) # R2 = 0.9122
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bpneg2p5.5 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 5)
bpneg2p5.5.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg.5, data = bpneg2p5.5)
summary(bpneg2p5.5.fit) # R2 = 0.8618
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bpneg2p5.6 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 6)
bpneg2p5.6.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5 + hf.neg.5, data = bpneg2p5.6)
summary(bpneg2p5.6.fit) # R2 = 0.842
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bpneg2p5.7 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 7)
bpneg2p5.7.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg.5, data = bpneg2p5.7)
summary(bpneg2p5.7.fit) # R2 = 0.8688
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bpneg2p5.8 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 8)
bpneg2p5.8.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5 + hf.neg.5, data = bpneg2p5.8)
summary(bpneg2p5.8.fit) # R2 = 0.9554
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bpneg2p5.9 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 9)
bpneg2p5.9.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5, data = bpneg2p5.9)
summary(bpneg2p5.9.fit) # R2 = 0.9474
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bpneg2p5.10 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 10)
bpneg2p5.10.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5, data = bpneg2p5.10)
summary(bpneg2p5.10.fit) # R2 = 0.8328
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bpneg2p5.11 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 11)
bpneg2p5.11.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5 + hf.neg.5, data = bpneg2p5.11)
summary(bpneg2p5.11.fit) # R2 = 0.8192
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bpneg2p5.12 <- macpass %>% filter(!is.na(bp.neg2p5) & Month == 12)
bpneg2p5.12.fit <- lm(bp.neg2p5 ~ bp.0 + hf.neg2p5, data = bpneg2p5.12)
summary(bpneg2p5.12.fit) # R2 = 0.789
macpass <- macpass %>% 
  mutate(pred = predict(bpneg2p5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg2p5 = ifelse(is.na(bp.neg2p5), pred, bp.neg2p5))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$bp.neg2p5, col = "blue")


## Beaver Pond (-5 cm)

## January
# Construct linear model based on non-NA pairs
bpneg5.1 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 1)
bpneg5.1.fit <- lm(bp.neg.5 ~ bp.neg2p5, data = bpneg5.1)
summary(bpneg5.1.fit) # R2 = 0.9535
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bpneg5.2 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 2)
bpneg5.2.fit <- lm(bp.neg.5 ~ bp.neg2p5 + hf.neg.5, data = bpneg5.2)
summary(bpneg5.2.fit) # R2 = 0.9668
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bpneg5.3 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 3)
bpneg5.3.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.3)
summary(bpneg5.3.fit) # R2 = 0.9796
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bpneg5.4 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 4)
bpneg5.4.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.4)
summary(bpneg5.4.fit) # R2 = 0.9817
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bpneg5.5 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 5)
bpneg5.5.fit <- lm(bp.neg.5 ~ bp.neg2p5 + hf.neg.5, data = bpneg5.5)
summary(bpneg5.5.fit) # R2 = 0.9147
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bpneg5.6 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 6)
bpneg5.6.fit <- lm(bp.neg.5 ~ bp.neg2p5 + hf.neg.5, data = bpneg5.6)
summary(bpneg5.6.fit) # R2 = 0.7772
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bpneg5.7 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 7)
bpneg5.7.fit <- lm(bp.neg.5 ~ bp.neg2p5 + hf.neg.5, data = bpneg5.7)
summary(bpneg5.7.fit) # R2 = 0.7999
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bpneg5.8 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 8)
bpneg5.8.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.8)
summary(bpneg5.8.fit) # R2 = 0.9143
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bpneg5.9 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 9)
bpneg5.9.fit <- lm(bp.neg.5 ~ bp.neg2p5 + hf.neg.5, data = bpneg5.9)
summary(bpneg5.9.fit) # R2 = 0.9154
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bpneg5.10 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 10)
bpneg5.10.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.10)
summary(bpneg5.10.fit) # R2 = 0.7919
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bpneg5.11 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 11)
bpneg5.11.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.11)
summary(bpneg5.11.fit) # R2 = 0.9398
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bpneg5.12 <- macpass %>% filter(!is.na(bp.neg.5) & Month == 12)
bpneg5.12.fit <- lm(bp.neg.5 ~ bp.0 + bp.neg2p5 + hf.neg.5, data = bpneg5.12)
summary(bpneg5.12.fit) # R2 = 0.9592
macpass <- macpass %>% 
  mutate(pred = predict(bpneg5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg.5 = ifelse(is.na(bp.neg.5), pred, bp.neg.5))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$bp.neg.5, col = "blue")


## Beaver Pond (-150 cm)

## All
# Construct linear model based on non-NA pairs
bpneg150.all <- macpass %>% filter(!is.na(bp.neg150))
bpneg150.all.fit <- lm(bp.neg150 ~ hf.neg150, data = bpneg150.all)
summary(bpneg150.all.fit) # R2 = 0.04946
macpass <- macpass %>% 
  mutate(pred = predict(bpneg150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg150 = ifelse(is.na(bp.neg150), pred, bp.neg150))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 3))
lines(macpass$Date, macpass$bp.neg150, col = "blue")



## Beaver Pond (-215 cm)

## All
# Construct linear model based on non-NA pairs
bpneg215.all <- macpass %>% filter(!is.na(bp.neg215))
bpneg215.all.fit <- lm(bp.neg215 ~ hf.neg150 + bp.neg150, data = bpneg215.all)
summary(bpneg215.all.fit) # R2 = 0.6432
macpass <- macpass %>% 
  mutate(pred = predict(bpneg215.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg215 = ifelse(is.na(bp.neg215), pred, bp.neg215))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 3))
lines(macpass$Date, macpass$bp.neg215, col = "blue")
lines(macpass$Date, macpass$bp.neg150, col = "red")

## Beaver Pond (-385 cm)

## All
# Construct linear model based on non-NA pairs
bpneg385.all <- macpass %>% filter(!is.na(bp.neg385))
bpneg385.all.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150 + bp.neg215, data = bpneg385.all)
summary(bpneg385.all.fit) # R2 = 0.602
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## January
# Construct linear model based on non-NA pairs
bpneg385.1 <- macpass %>% filter(!is.na(bp.neg385) & Month == 1)
bpneg385.1.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150, data = bpneg385.1)
summary(bpneg385.1.fit) # R2 = 0.602
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bpneg385.2 <- macpass %>% filter(!is.na(bp.neg385) & Month == 2)
bpneg385.2.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150 + bp.neg215 + gf.neg150, data = bpneg385.2)
summary(bpneg385.2.fit) # R2 = 0.3649
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bpneg385.3 <- macpass %>% filter(!is.na(bp.neg385) & Month == 3)
bpneg385.3.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150 + bp.neg215 + gf.neg150, data = bpneg385.3)
summary(bpneg385.3.fit) # R2 = 0.112
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bpneg385.4 <- macpass %>% filter(!is.na(bp.neg385) & Month == 4)
bpneg385.4.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150 + bp.neg215 + gf.neg150, data = bpneg385.4)
summary(bpneg385.4.fit) # R2 = 0.2593
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bpneg385.5 <- macpass %>% filter(!is.na(bp.neg385) & Month == 5)
bpneg385.5.fit <- lm(bp.neg385 ~ bp.neg150 + bp.neg215 + gf.neg150, data = bpneg385.5)
summary(bpneg385.5.fit) # R2 = 0.635
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bpneg385.6 <- macpass %>% filter(!is.na(bp.neg385) & Month == 6)
bpneg385.6.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg215 + gf.neg150, data = bpneg385.6)
summary(bpneg385.6.fit) # R2 = 0.7398
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bpneg385.7 <- macpass %>% filter(!is.na(bp.neg385) & Month == 7)
bpneg385.7.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg215, data = bpneg385.7)
summary(bpneg385.7.fit) # R2 = 0.6485
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bpneg385.8 <- macpass %>% filter(!is.na(bp.neg385) & Month == 8)
bpneg385.8.fit <- lm(bp.neg385 ~ bp.neg150, data = bpneg385.8)
summary(bpneg385.8.fit) # R2 = 0.7101
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bpneg385.9 <- macpass %>% filter(!is.na(bp.neg385) & Month == 9)
bpneg385.9.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150, data = bpneg385.9)
summary(bpneg385.9.fit) # R2 = 0.7629
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bpneg385.10 <- macpass %>% filter(!is.na(bp.neg385) & Month == 10)
bpneg385.10.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg150 + bp.neg215 + gf.neg150, data = bpneg385.10)
summary(bpneg385.10.fit) # R2 = 0.7195
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bpneg385.11 <- macpass %>% filter(!is.na(bp.neg385) & Month == 11)
bpneg385.11.fit <- lm(bp.neg385 ~ bp.neg150 + gf.neg150, data = bpneg385.11)
summary(bpneg385.11.fit) # R2 = 0.789
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bpneg385.12 <- macpass %>% filter(!is.na(bp.neg385) & Month == 12)
bpneg385.12.fit <- lm(bp.neg385 ~ hf.neg150 + bp.neg215 + gf.neg150, data = bpneg385.12)
summary(bpneg385.12.fit) # R2 = 0.8436
macpass <- macpass %>% 
  mutate(pred = predict(bpneg385.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bp.neg385 = ifelse(is.na(bp.neg385), pred, bp.neg385))
macpass %>% as.data.frame()

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 3))
lines(macpass$Date, macpass$bp.neg385, col = "blue")


bp.0.spline <- spline(as.numeric(macpass$Date), macpass$bp.0, method = "periodic", n = 365.25)
bp.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$bp.neg2p5, method = "periodic", n = 365.25)
bp.neg.5.spline <- spline(as.numeric(macpass$Date), macpass$bp.neg.5, method = "periodic", n = 365.25)
bp.neg150.spline <- spline(as.numeric(macpass$Date), macpass$bp.neg150, method = "periodic", n = 365.25)
bp.neg215.spline <- spline(as.numeric(macpass$Date), macpass$bp.neg215, method = "periodic", n = 365.25)
bp.neg385.spline <- spline(as.numeric(macpass$Date), macpass$bp.neg385, method = "periodic", n = 365.25)
bp.0.spline.int <- approx(bp.neg150.spline$x, bp.0.spline$y, n=10627)
bp.neg2p5.spline.int <- approx(bp.neg150.spline$x, bp.neg2p5.spline$y, n=10627)
bp.neg.5.spline.int <- approx(bp.neg150.spline$x, bp.neg.5.spline$y, n=10627)
bp.neg150.spline.int <- approx(bp.neg150.spline$x, bp.neg150.spline$y, n=10627)
bp.neg215.spline.int <- approx(bp.neg150.spline$x, bp.neg215.spline$y, n=10627)
bp.neg385.spline.int <- approx(bp.neg150.spline$x, bp.neg385.spline$y, n=10627)
macpass$bp.0.s <- bp.0.spline.int$y
macpass$bp.neg2p5.s <- bp.neg2p5.spline.int$y
macpass$bp.neg.5.s <- bp.neg.5.spline.int$y
macpass$bp.neg150.s <- bp.neg150.spline.int$y
macpass$bp.neg215.s <- bp.neg215.spline.int$y
macpass$bp.neg385.s <- bp.neg385.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-22, 15))
lines(macpass$Date, macpass$bp.0.s, col = "darkred")
lines(macpass$Date, macpass$bp.neg2p5.s, col = "red")
lines(macpass$Date, macpass$bp.neg.5.s, col = "orange")
lines(macpass$Date, macpass$bp.neg150.s, col = "blue")
lines(macpass$Date, macpass$bp.neg215.s, col = "darkblue")
lines(macpass$Date, macpass$bp.neg385.s, col = "violet")
plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-5, 5))
lines(macpass$Date, macpass$bp.neg150.s, col = "blue")
lines(macpass$Date, macpass$bp.neg215.s, col = "darkblue")
lines(macpass$Date, macpass$bp.neg385.s, col = "violet")






## Dale2 (0 cm)

## All
# Construct linear model based on non-NA pairs
d20.all <- macpass %>% filter(!is.na(d2.0))
d20.all.fit <- lm(d2.0 ~ d2.150 + hf.0 + hf.neg2p5 + hf.neg.5, data = d20.all)
summary(d20.all.fit) # R2 = 0.5136
macpass <- macpass %>% 
  mutate(pred = predict(d20.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.0 = ifelse(is.na(d2.0), pred, d2.0))
macpass %>% as.data.frame()

d2.0.spline <- spline(as.numeric(macpass$Date), macpass$d2.0, method = "periodic", n = 365.25)
d2.0.spline.int <- approx(d2.0.spline$x, d2.0.spline$y, n=10627)
macpass$d2.0.s <- d2.0.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$d2.0, col = "blue")
lines(macpass$Date, macpass$d2.0.s, col = "green")


## Dale2 (-2.5cm)

## January
# Construct linear model based on non-NA pairs
d2neg2p5.1 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 1)
d2neg2p5.1.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.1)
summary(d2neg2p5.1.fit) # R2 = 0.5803
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d2neg2p5.2 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 2)
d2neg2p5.2.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.2)
summary(d2neg2p5.2.fit) # R2 = 0.7705
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d2neg2p5.3 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 3)
d2neg2p5.3.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.3)
summary(d2neg2p5.3.fit) # R2 = 0.7783
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d2neg2p5.4 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 4)
d2neg2p5.4.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.4)
summary(d2neg2p5.4.fit) # R2 = 0.8007
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d2neg2p5.5 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 5)
d2neg2p5.5.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.5)
summary(d2neg2p5.5.fit) # R2 = 0.7004
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d2neg2p5.6 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 6)
d2neg2p5.6.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.6)
summary(d2neg2p5.6.fit) # R2 = 0.4495
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d2neg2p5.7 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 7)
d2neg2p5.7.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.7)
summary(d2neg2p5.7.fit) # R2 = 0.4182
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d2neg2p5.8 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 8)
d2neg2p5.8.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.8)
summary(d2neg2p5.8.fit) # R2 = 0.3599
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d2neg2p5.9 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 9)
d2neg2p5.9.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.9)
summary(d2neg2p5.9.fit) # R2 = 0.473
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d2neg2p5.10 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 10)
d2neg2p5.10.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.10)
summary(d2neg2p5.10.fit) # R2 = 0.5989
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d2neg2p5.11 <- macpass %>% filter(!is.na(d2.neg2p5) & Month == 11)
d2neg2p5.11.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.11)
summary(d2neg2p5.11.fit) # R2 = 0.5974
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg2p5 = ifelse(is.na(d2.neg2p5), pred, d2.neg2p5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d2neg2p5.12 <- macpass %>% filter(!is.na(d2.0) & Month == 12)
d2neg2p5.12.fit <- lm(d2.neg2p5 ~ d2.0.s + hf.0, data = d2neg2p5.12)
summary(d2neg2p5.12.fit) # R2 = 0.7088
macpass <- macpass %>% 
  mutate(pred = predict(d2neg2p5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.0 = ifelse(is.na(d2.0), pred, d2.0))
macpass %>% as.data.frame()

d2.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$d2.neg2p5, method = "periodic", n = 365.25)
d2.neg2p5.spline.int <- approx(d2.neg2p5.spline$x, d2.neg2p5.spline$y, n=10627)
macpass$d2.neg2p5.s <- d2.neg2p5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$d2.neg2p5, col = "blue")
lines(macpass$Date, macpass$d2.neg2p5.s, col = "red")

## Dale2 (-5cm)

## January
# Construct linear model based on non-NA pairs
d2neg5.1 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 1)
d2neg5.1.fit <- lm(d2.neg.5 ~ d2.0.s + d2.neg2p5, data = d2neg5.1)
summary(d2neg5.1.fit) # R2 = 0.8989
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d2neg5.2 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 2)
d2neg5.2.fit <- lm(d2.neg.5 ~ d2.0.s + d2.neg2p5, data = d2neg5.2)
summary(d2neg5.2.fit) # R2 = 0.9797
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d2neg5.3 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 3)
d2neg5.3.fit <- lm(d2.neg.5 ~ d2.0 + d2.neg2p5.s + d2.neg2p5, data = d2neg5.3)
summary(d2neg5.3.fit) # R2 = 0.9936
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d2neg5.4 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 4)
d2neg5.4.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5, data = d2neg5.4)
summary(d2neg5.4.fit) # R2 = 0.9804
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d2neg5.5 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 5)
d2neg5.5.fit <- lm(d2.neg.5 ~ d2.neg2p5.s + d2.neg2p5, data = d2neg5.5)
summary(d2neg5.5.fit) # R2 = 0.921
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d2neg5.6 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 6)
d2neg5.6.fit <- lm(d2.neg.5 ~ d2.0 + d2.neg2p5, data = d2neg5.6)
summary(d2neg5.6.fit) # R2 = 0.8207
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d2neg5.7 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 7)
d2neg5.7.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5, data = d2neg5.7)
summary(d2neg5.7.fit) # R2 = 0.8342
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d2neg5.8 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 8)
d2neg5.8.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5, data = d2neg5.8)
summary(d2neg5.8.fit) # R2 = 0.9578
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d2neg5.9 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 9)
d2neg5.9.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5.s + d2.neg2p5, data = d2neg5.9)
summary(d2neg5.9.fit) # R2 = 0.9351
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d2neg5.10 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 10)
d2neg5.10.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5.s + d2.neg2p5, data = d2neg5.10)
summary(d2neg5.10.fit) # R2 = 0.8395
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d2neg5.11 <- macpass %>% filter(!is.na(d2.neg.5) & Month == 11)
d2neg5.11.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5.s + d2.neg2p5, data = d2neg5.11)
summary(d2neg5.11.fit) # R2 = 0.9531
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg.5 = ifelse(is.na(d2.neg.5), pred, d2.neg.5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d2neg5.12 <- macpass %>% filter(!is.na(d2.0) & Month == 12)
d2neg5.12.fit <- lm(d2.neg.5 ~ d2.0 + d2.0.s + d2.neg2p5, data = d2neg5.12)
summary(d2neg5.12.fit) # R2 = 0.9717
macpass <- macpass %>% 
  mutate(pred = predict(d2neg5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.0 = ifelse(is.na(d2.0), pred, d2.0))
macpass %>% as.data.frame()

d2.neg.5.spline <- spline(as.numeric(macpass$Date), macpass$d2.neg.5, method = "periodic", n = 365.25)
d2.neg.5.spline.int <- approx(d2.neg.5.spline$x, d2.neg.5.spline$y, n=10627)
macpass$d2.neg.5.s <- d2.neg.5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
lines(macpass$Date, macpass$d2.neg.5, col = "blue")
lines(macpass$Date, macpass$d2.neg.5.s, col = "red")



pairs.panels(d2neg150.1[c("d2.neg150","d2.0","d2.0.s","d2.neg2p5","d2.neg2p5.s","d2.neg.5","d2.neg.5.s",
                       "hf.neg150","hf.neg150.s","bp.neg150.s")], lm = TRUE, stars = TRUE) # n.s.

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 5))
lines(macpass$Date, macpass$d2.neg150, col = "red")
lines(macpass$Date, macpass$d6.neg150, col = "blue")
lines(macpass$Date, macpass$hf.neg150.s, col = "orange")

## Dale2 (-150cm)

## All
# Construct linear model based on non-NA pairs
d2neg150.all <- macpass %>% filter(!is.na(d2.neg150))
d2neg150.all.fit <- lm(d2.neg150 ~ hf.neg150 + bp.neg150 + bp.neg215, data = d2neg150.all)
summary(d2neg150.all.fit) # R2 = 0.0811
macpass <- macpass %>% 
  mutate(pred = predict(d2neg150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d2.neg150 = ifelse(is.na(d2.neg150), pred, d2.neg150))
macpass %>% as.data.frame()

d2.0.spline <- spline(as.numeric(macpass$Date), macpass$d2.0, method = "periodic", n = 365.25)
d2.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$d2.neg2p5, method = "periodic", n = 365.25)
d2.neg.5.spline <- spline(as.numeric(macpass$Date), macpass$d2.neg.5, method = "periodic", n = 365.25)
d2.neg150.spline <- spline(as.numeric(macpass$Date), macpass$d2.neg150, method = "periodic", n = 365.25)
d2.0.spline.int <- approx(d2.neg150.spline$x, d2.0.spline$y, n=10627)
d2.neg2p5.spline.int <- approx(d2.neg150.spline$x, d2.neg2p5.spline$y, n=10627)
d2.neg.5.spline.int <- approx(d2.neg150.spline$x, d2.neg.5.spline$y, n=10627)
d2.neg150.spline.int <- approx(d2.neg150.spline$x, d2.neg150.spline$y, n=10627)
macpass$d2.0.s <- d2.0.spline.int$y
macpass$d2.neg2p5.s <- d2.neg2p5.spline.int$y
macpass$d2.neg.5.s <- d2.neg.5.spline.int$y
macpass$d2.neg150.s <- d2.neg150.spline.int$y


plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-40, 20))
lines(macpass$Date, macpass$d2.0.s, col = "darkred")
lines(macpass$Date, macpass$d2.neg2p5.s, col = "red")
lines(macpass$Date, macpass$d2.neg.5.s, col = "orange")
lines(macpass$Date, macpass$d2.neg150.s, col = "blue")
plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 10))
lines(macpass$Date, macpass$d2.neg150, col = "blue")
lines(macpass$Date, macpass$d2.neg150.s, col = "red")

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-40, 20))
lines(macpass$Date, macpass$gf.0, col = "blue")

## Goose Flats
## (0 cm)

## January
# Construct linear model based on non-NA pairs
gf0.1 <- macpass %>% filter(!is.na(gf.0) & Month == 1)
gf0.1.fit <- lm(gf.0 ~ gf.150 + hf.0 + d2.0 + d2.0.s, data = gf0.1)
summary(gf0.1.fit) # R2 = 0.4787
macpass <- macpass %>% 
  mutate(pred = predict(gf0.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
gf0.2 <- macpass %>% filter(!is.na(gf.0) & Month == 2)
gf0.2.fit <- lm(gf.0 ~ gf.150 + hf.0 + hf.0.s + d2.0 + d2.0.s, data = gf0.2)
summary(gf0.2.fit) # R2 = 0.5687
macpass <- macpass %>% 
  mutate(pred = predict(gf0.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
gf0.3 <- macpass %>% filter(!is.na(gf.0) & Month == 3)
gf0.3.fit <- lm(gf.0 ~ gf.150 + hf.0 + hf.0.s + d2.0 + d2.0.s, data = gf0.3)
summary(gf0.3.fit) # R2 = 0.6677
macpass <- macpass %>% 
  mutate(pred = predict(gf0.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
gf0.4 <- macpass %>% filter(!is.na(gf.0) & Month == 4)
gf0.4.fit <- lm(gf.0 ~ gf.150 + hf.0 + d2.0 + d2.0.s, data = gf0.4)
summary(gf0.4.fit) # R2 = 0.6997
macpass <- macpass %>% 
  mutate(pred = predict(gf0.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
gf0.5 <- macpass %>% filter(!is.na(gf.0) & Month == 5)
gf0.5.fit <- lm(gf.0 ~ gf.150 + hf.0 + hf.0.s + d2.0 + d2.0.s, data = gf0.5)
summary(gf0.5.fit) # R2 = 0.6963
macpass <- macpass %>% 
  mutate(pred = predict(gf0.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
gf0.6 <- macpass %>% filter(!is.na(gf.0) & Month == 6)
gf0.6.fit <- lm(gf.0 ~ gf.150 + d2.0 + d2.0.s, data = gf0.6)
summary(gf0.6.fit) # R2 = 0.7269
macpass <- macpass %>% 
  mutate(pred = predict(gf0.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
gf0.7 <- macpass %>% filter(!is.na(gf.0) & Month == 7)
gf0.7.fit <- lm(gf.0 ~ gf.150 + hf.0 + d2.0 + d2.0.s, data = gf0.7)
summary(gf0.7.fit) # R2 = 0.6553
macpass <- macpass %>% 
  mutate(pred = predict(gf0.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
gf0.8 <- macpass %>% filter(!is.na(gf.0) & Month == 8)
gf0.8.fit <- lm(gf.0 ~ gf.150 + d2.0 + d2.0.s, data = gf0.8)
summary(gf0.8.fit) # R2 = 0.8358
macpass <- macpass %>% 
  mutate(pred = predict(gf0.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
gf0.9 <- macpass %>% filter(!is.na(gf.0) & Month == 9)
gf0.9.fit <- lm(gf.0 ~ gf.150 + hf.0 + hf.0.s + d2.0.s, data = gf0.9)
summary(gf0.9.fit) # R2 = 0.7564
macpass <- macpass %>% 
  mutate(pred = predict(gf0.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
gf0.10 <- macpass %>% filter(!is.na(gf.0) & Month == 10)
gf0.10.fit <- lm(gf.0 ~ gf.150 + hf.0 + d2.0, data = gf0.10)
summary(gf0.10.fit) # R2 = 0.451
macpass <- macpass %>% 
  mutate(pred = predict(gf0.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
gf0.11 <- macpass %>% filter(!is.na(gf.0) & Month == 11)
gf0.11.fit <- lm(gf.0 ~ gf.150 + hf.0 + hf.0.s + d2.0, data = gf0.11)
summary(gf0.11.fit) # R2 = 0.3962
macpass <- macpass %>% 
  mutate(pred = predict(gf0.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
gf0.12 <- macpass %>% filter(!is.na(gf.0) & Month == 12)
gf0.12.fit <- lm(gf.0 ~ gf.150 + hf.0 + d2.0.s, data = gf0.12)
summary(gf0.12.fit) # R2 = 0.5173
macpass <- macpass %>% 
  mutate(pred = predict(gf0.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.0 = ifelse(is.na(gf.0), pred, gf.0))
macpass %>% as.data.frame()

gf.0.spline <- spline(as.numeric(macpass$Date), macpass$gf.0, method = "periodic", n = 365.25)
gf.0.spline.int <- approx(gf.0.spline$x, gf.0.spline$y, n=10627)
macpass$gf.0.s <- gf.0.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
# plot(macpass$gf.0 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$gf.0, col = "blue")
lines(macpass$Date, macpass$gf.0.s, col = "red")





gfneg2p5.full <- macpass %>% filter(!is.na(gf.neg2p5))
gfneg2p5.full.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.full)
summary(gfneg2p5.full.fit)

## January
# Construct linear model based on non-NA pairs
gfneg2p5.1 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 1)
gfneg2p5.1.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.1)
summary(gfneg2p5.1.fit) # R2 = 0.8726
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
gfneg2p5.2 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 2)
gfneg2p5.2.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.2)
summary(gfneg2p5.2.fit) # R2 = 0.8471
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
gfneg2p5.3 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 3)
gfneg2p5.3.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.3)
summary(gfneg2p5.3.fit) # R2 = 0.8147
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
gfneg2p5.4 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 4)
gfneg2p5.4.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.4)
summary(gfneg2p5.4.fit) # R2 = 0.8702
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
gfneg2p5.5 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 5)
gfneg2p5.5.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.5)
summary(gfneg2p5.5.fit) # R2 = 0.8012
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
gfneg2p5.6 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 6)
gfneg2p5.6.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.6)
summary(gfneg2p5.6.fit) # R2 = 0.6634
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
gfneg2p5.7 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 7)
gfneg2p5.7.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.7)
summary(gfneg2p5.7.fit) # R2 = 0.5844
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
gfneg2p5.8 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 8)
gfneg2p5.8.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.8)
summary(gfneg2p5.8.fit) # R2 = 0.7493
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
gfneg2p5.9 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 9)
gfneg2p5.9.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.9)
summary(gfneg2p5.9.fit) # R2 = 0.7577 
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
gfneg2p5.10 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 10)
gfneg2p5.10.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.10)
summary(gfneg2p5.10.fit) # R2 = 0.6667
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
gfneg2p5.11 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 11)
gfneg2p5.11.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.11)
summary(gfneg2p5.11.fit) # R2 = 0.7948
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
gfneg2p5.12 <- macpass %>% filter(!is.na(gf.neg2p5) & Month == 12)
gfneg2p5.12.fit <- lm(gf.neg2p5 ~ gf.150 + gf.0 + gf.0.s, data = gfneg2p5.12)
summary(gfneg2p5.12.fit) # R2 = 0.8463
macpass <- macpass %>% 
  mutate(pred = predict(gfneg2p5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg2p5 = ifelse(is.na(gf.neg2p5), pred, gf.neg2p5))
macpass %>% as.data.frame()

gf.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$gf.neg2p5, method = "periodic", n = 365.25)
gf.neg2p5.spline.int <- approx(gf.neg2p5.spline$x, gf.neg2p5.spline$y, n=10627)
macpass$gf.neg2p5.s <- gf.neg2p5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
# plot(macpass$gf.neg2p5 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$gf.neg2p5, col = "blue")
lines(macpass$Date, macpass$gf.neg2p5.s, col = "red")






gfneg5.full <- macpass %>% filter(!is.na(gf.neg5))
gfneg5.full.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.full)
summary(gfneg5.full.fit)

## January
# Construct linear model based on non-NA pairs
gfneg5.1 <- macpass %>% filter(!is.na(gf.neg5) & Month == 1)
gfneg5.1.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.1)
summary(gfneg5.1.fit) # R2 = 0.8625
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
gfneg5.2 <- macpass %>% filter(!is.na(gf.neg5) & Month == 2)
gfneg5.2.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.2)
summary(gfneg5.2.fit) # R2 = 0.7108
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
gfneg5.3 <- macpass %>% filter(!is.na(gf.neg5) & Month == 3)
gfneg5.3.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.3)
summary(gfneg5.3.fit) # R2 = 0.6361
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
gfneg5.4 <- macpass %>% filter(!is.na(gf.neg5) & Month == 4)
gfneg5.4.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.4)
summary(gfneg5.4.fit) # R2 = 0.718
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
gfneg5.5 <- macpass %>% filter(!is.na(gf.neg5) & Month == 5)
gfneg5.5.fit <- lm(gf.neg5 ~ gf.0 + gf.neg2p5 + d2.neg.5 , data = gfneg5.5)
summary(gfneg5.5.fit) # R2 = 0.6899
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
gfneg5.6 <- macpass %>% filter(!is.na(gf.neg5) & Month == 6)
gfneg5.6.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + d2.neg.5, data = gfneg5.6)
summary(gfneg5.6.fit) # R2 = 0.5665
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
gfneg5.7 <- macpass %>% filter(!is.na(gf.neg5) & Month == 7)
gfneg5.7.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5 + d2.neg.5 + hf.neg.5 + bp.neg.5, data = gfneg5.7)
summary(gfneg5.7.fit) # R2 = 0.6137
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
gfneg5.8 <- macpass %>% filter(!is.na(gf.neg5) & Month == 8)
gfneg5.8.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5 + d2.neg.5, data = gfneg5.8)
summary(gfneg5.8.fit) # R2 = 0.8194
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
gfneg5.9 <- macpass %>% filter(!is.na(gf.neg5) & Month == 9)
gfneg5.9.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.9)
summary(gfneg5.9.fit) # R2 = 0.8765 
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
gfneg5.10 <- macpass %>% filter(!is.na(gf.neg5) & Month == 10)
gfneg5.10.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5, data = gfneg5.10)
summary(gfneg5.10.fit) # R2 = 0.7822
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
gfneg5.11 <- macpass %>% filter(!is.na(gf.neg5) & Month == 11)
gfneg5.11.fit <- lm(gf.neg5 ~ gf.150 + gf.0 + gf.0.s + gf.neg2p5 + d2.neg.5, data = gfneg5.11)
summary(gfneg5.11.fit) # R2 = 0.7693
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
gfneg5.12 <- macpass %>% filter(!is.na(gf.neg5) & Month == 12)
gfneg5.12.fit <- lm(gf.neg5 ~ gf.0 + gf.0.s + d2.neg.5, data = gfneg5.12)
summary(gfneg5.12.fit) # R2 = 0.7594
macpass <- macpass %>% 
  mutate(pred = predict(gfneg5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg5 = ifelse(is.na(gf.neg5), pred, gf.neg5))
macpass %>% as.data.frame()

gf.neg5.spline <- spline(as.numeric(macpass$Date), macpass$gf.neg5, method = "periodic", n = 365.25)
gf.neg5.spline.int <- approx(gf.neg5.spline$x, gf.neg5.spline$y, n=10627)
macpass$gf.neg5.s <- gf.neg5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-50, 30))
# plot(macpass$gf.neg5 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$gf.neg5, col = "blue")
lines(macpass$Date, macpass$gf.neg5.s, col = "red")






plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-6, 2))
# plot(macpass$gf.0 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$gf.neg150, col = "blue")
lines(macpass$Date, macpass$bp.neg150.s, col = "red")

## Goose Flats (-150 cm)

## All
# Construct linear model based on non-NA pairs
gfneg150.all <- macpass %>% filter(!is.na(gf.neg150))
gfneg150.all.fit <- lm(gf.neg150 ~ hf.neg150.s + d2.neg150.s, data = gfneg150.all)
summary(gfneg150.all.fit) # R2 = 0.6911
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## January
# Construct linear model based on non-NA pairs
gfneg150.1 <- macpass %>% filter(!is.na(gf.neg150) & Month == 1)
gfneg150.1.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150.s, data = gfneg150.1)
summary(gfneg150.1.fit) # R2 = 0.6172
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
gfneg150.2 <- macpass %>% filter(!is.na(gf.neg150) & Month == 2)
gfneg150.2.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150.s, data = gfneg150.2)
summary(gfneg150.2.fit) # R2 = 0.5313
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
gfneg150.3 <- macpass %>% filter(!is.na(gf.neg150) & Month == 3)
gfneg150.3.fit <- lm(gf.neg150 ~ hf.neg150.s + d2.neg150 + d2.neg150.s, data = gfneg150.3)
summary(gfneg150.3.fit) # R2 = 0.4912
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
gfneg150.4 <- macpass %>% filter(!is.na(gf.neg150) & Month == 4)
gfneg150.4.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150 + d2.neg150.s, data = gfneg150.4)
summary(gfneg150.4.fit) # R2 = 0.5847
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
gfneg150.5 <- macpass %>% filter(!is.na(gf.neg150) & Month == 5)
gfneg150.5.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150 + d2.neg150.s, data = gfneg150.5)
summary(gfneg150.5.fit) # R2 = 0.5567
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
gfneg150.6 <- macpass %>% filter(!is.na(gf.neg150) & Month == 6)
gfneg150.6.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s, data = gfneg150.6)
summary(gfneg150.6.fit) # R2 = 0.4895
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
gfneg150.7 <- macpass %>% filter(!is.na(gf.neg150) & Month == 7)
gfneg150.7.fit <- lm(gf.neg150 ~ hf.neg150.s + d2.neg150, data = gfneg150.7)
summary(gfneg150.7.fit) # R2 = 0.09181
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
gfneg150.8 <- macpass %>% filter(!is.na(gf.neg150) & Month == 8)
gfneg150.8.fit <- lm(gf.neg150 ~ hf.neg150.s + d2.neg150.s, data = gfneg150.8)
summary(gfneg150.8.fit) # R2 = 0.367
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
gfneg150.9 <- macpass %>% filter(!is.na(gf.neg150) & Month == 9)
gfneg150.9.fit <- lm(gf.neg150 ~ hf.neg150 + d2.neg150.s, data = gfneg150.9)
summary(gfneg150.9.fit) # R2 = 0.4388
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
gfneg150.10 <- macpass %>% filter(!is.na(gf.neg150) & Month == 10)
gfneg150.10.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150.s, data = gfneg150.10)
summary(gfneg150.10.fit) # R2 = 0.3499
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
gfneg150.11 <- macpass %>% filter(!is.na(gf.neg150) & Month == 11)
gfneg150.11.fit <- lm(gf.neg150 ~ hf.neg150 + d2.neg150.s, data = gfneg150.11)
summary(gfneg150.11.fit) # R2 = 0.215
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
gfneg150.12 <- macpass %>% filter(!is.na(gf.neg150) & Month == 12)
gfneg150.12.fit <- lm(gf.neg150 ~ hf.neg150 + hf.neg150.s + d2.neg150 + d2.neg150.s, data = gfneg150.12)
summary(gfneg150.12.fit) # R2 = 0.4246
macpass <- macpass %>% 
  mutate(pred = predict(gfneg150.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(gf.neg150 = ifelse(is.na(gf.neg150), pred, gf.neg150))
macpass %>% as.data.frame()

gf.neg150.spline <- spline(as.numeric(macpass$Date), macpass$gf.neg150, method = "periodic", n = 365.25)
gf.neg150.spline.int <- approx(gf.neg150.spline$x, gf.neg150.spline$y, n=10627)
macpass$gf.neg150.s <- gf.neg150.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-6, 2))
# plot(macpass$gf.neg150 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$gf.neg150, col = "blue")
lines(macpass$Date, macpass$gf.neg150.s, col = "red")




## Dale6 (0 cm)

## January
# Construct linear model based on non-NA pairs
d60.1 <- macpass %>% filter(!is.na(d6.0) & Month == 1)
d60.1.fit <- lm(d6.0 ~ d2.0 + d2.150 + hf.150, data = d60.1)
summary(d60.1.fit) # R2 = 0.4192
macpass <- macpass %>% 
  mutate(pred = predict(d60.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d60.2 <- macpass %>% filter(!is.na(d6.0) & Month == 2)
d60.2.fit <- lm(d6.0 ~ d2.0 + d6.150 + d2.150 + bp.150 + hf.150 + gf.150, data = d60.2)
summary(d60.2.fit) # R2 = 0.396
macpass <- macpass %>% 
  mutate(pred = predict(d60.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d60.3 <- macpass %>% filter(!is.na(d6.0) & Month == 3)
d60.3.fit <- lm(d6.0 ~ gf.0 + d2.0 + d2.150 + bp.150 + hf.150, data = d60.3)
summary(d60.3.fit) # R2 = 0.3911
macpass <- macpass %>% 
  mutate(pred = predict(d60.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d60.4 <- macpass %>% filter(!is.na(d6.0) & Month == 4)
d60.4.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d6.150 + bp.150 + hf.150, data = d60.4)
summary(d60.4.fit) # R2 = 0.6156
macpass <- macpass %>% 
  mutate(pred = predict(d60.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d60.5 <- macpass %>% filter(!is.na(d6.0) & Month == 5)
d60.5.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d6.150 + hf.150, data = d60.5)
summary(d60.5.fit) # R2 = 0.5639
macpass <- macpass %>% 
  mutate(pred = predict(d60.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d60.6 <- macpass %>% filter(!is.na(d6.0) & Month == 6)
d60.6.fit <- lm(d6.0 ~ hf.0 + gf.0 + d6.150 + d2.150 + hf.150 + gf.150, data = d60.6)
summary(d60.6.fit) # R2 = 0.2906
macpass <- macpass %>% 
  mutate(pred = predict(d60.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d60.7 <- macpass %>% filter(!is.na(d6.0) & Month == 7)
d60.7.fit <- lm(d6.0 ~ hf.0 + gf.0 + d6.150 + d2.150 + hf.150, data = d60.7)
summary(d60.7.fit) # R2 = 0.2823
macpass <- macpass %>% 
  mutate(pred = predict(d60.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d60.8 <- macpass %>% filter(!is.na(d6.0) & Month == 8)
d60.8.fit <- lm(d6.0 ~ hf.0 + gf.0 + d6.150 + d2.150 + hf.150 + gf.150, data = d60.8)
summary(d60.8.fit) # R2 = 0.2651
macpass <- macpass %>% 
  mutate(pred = predict(d60.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d60.9 <- macpass %>% filter(!is.na(d6.0) & Month == 9)
d60.9.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d6.150 + d2.150 + bp.150 + hf.150 + gf.150, data = d60.9)
summary(d60.9.fit) # R2 = 0.511
macpass <- macpass %>% 
  mutate(pred = predict(d60.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d60.10 <- macpass %>% filter(!is.na(d6.0) & Month == 10)
d60.10.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d2.150 + hf.150, data = d60.10)
summary(d60.10.fit) # R2 = 0.4907
macpass <- macpass %>% 
  mutate(pred = predict(d60.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d60.11 <- macpass %>% filter(!is.na(d6.0) & Month == 11)
d60.11.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d2.150 + hf.150 + gf.150, data = d60.11)
summary(d60.11.fit) # R2 = 0.4896
macpass <- macpass %>% 
  mutate(pred = predict(d60.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d60.12 <- macpass %>% filter(!is.na(d6.0) & Month == 12)
d60.12.fit <- lm(d6.0 ~ hf.0 + gf.0 + d2.0 + d2.150 + bp.150, data = d60.12)
summary(d60.12.fit) # R2 = 0.4517
macpass <- macpass %>% 
  mutate(pred = predict(d60.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.0 = ifelse(is.na(d6.0), pred, d6.0))
macpass %>% as.data.frame()

d6.0.spline <- spline(as.numeric(macpass$Date), macpass$d6.0, method = "periodic", n = 365.25)
d6.0.spline.int <- approx(d6.0.spline$x, d6.0.spline$y, n=10627)
macpass$d6.0.s <- d6.0.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-20, 20))
# plot(macpass$d6.0 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.0, col = "blue")
lines(macpass$Date, macpass$d6.0.s, col = "red")


## Dale6 (-2.5 cm)

## January
# Construct linear model based on non-NA pairs
d6neg2p5.1 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 1)
d6neg2p5.1.fit <- lm(d6.neg2p5 ~ hf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + bp.150, data = d6neg2p5.1)
summary(d6neg2p5.1.fit) # R2 = 0.853
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d6neg2p5.2 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 2)
d6neg2p5.2.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + hf.150 + gf.150, data = d6neg2p5.2)
summary(d6neg2p5.2.fit) # R2 = 0.8744
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d6neg2p5.3 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 3)
d6neg2p5.3.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + bp.150 + hf.150, data = d6neg2p5.3)
summary(d6neg2p5.3.fit) # R2 = 0.6666
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d6neg2p5.4 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 4)
d6neg2p5.4.fit <- lm(d6.neg2p5 ~ hf.0.s + d6.0 + hf.neg2p5.s + d6.150 + d2.150, data = d6neg2p5.4)
summary(d6neg2p5.4.fit) # R2 = 0.8691
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d6neg2p5.5 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 5)
d6neg2p5.5.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + gf.150, data = d6neg2p5.5)
summary(d6neg2p5.5.fit) # R2 = 0.7861
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d6neg2p5.6 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 6)
d6neg2p5.6.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + gf.150, data = d6neg2p5.6)
summary(d6neg2p5.6.fit) # R2 = 0.7388
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d6neg2p5.7 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 7)
d6neg2p5.7.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + bp.150, data = d6neg2p5.7)
summary(d6neg2p5.7.fit) # R2 = 0.5512
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d6neg2p5.8 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 8)
d6neg2p5.8.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d6.0 + d2.neg2p5.s + bp.150 + gf.150, data = d6neg2p5.8)
summary(d6neg2p5.8.fit) # R2 = 0.5973
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d6neg2p5.9 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 9)
d6neg2p5.9.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + bp.150 + hf.150 + gf.150, data = d6neg2p5.9)
summary(d6neg2p5.9.fit) # R2 = 0.7597
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d6neg2p5.10 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 10)
d6neg2p5.10.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + hf.150 + gf.150, data = d6neg2p5.10)
summary(d6neg2p5.10.fit) # R2 = 0.5368
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d6neg2p5.11 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 11)
d6neg2p5.11.fit <- lm(d6.neg2p5 ~ hf.0.s + gf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + hf.150, data = d6neg2p5.11)
summary(d6neg2p5.11.fit) # R2 = 0.4907
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d6neg2p5.12 <- macpass %>% filter(!is.na(d6.neg2p5) & Month == 12)
d6neg2p5.12.fit <- lm(d6.neg2p5 ~ hf.0.s + d2.0.s + d6.0 + hf.neg2p5.s + d2.neg2p5.s + d6.150 + d2.150 + bp.150 + hf.150 + gf.150, data = d6neg2p5.12)
summary(d6neg2p5.12.fit) # R2 = 0.6927
macpass <- macpass %>% 
  mutate(pred = predict(d6neg2p5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg2p5 = ifelse(is.na(d6.neg2p5), pred, d6.neg2p5))
macpass %>% as.data.frame()

d6.neg2p5.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg2p5, method = "periodic", n = 365.25)
d6.neg2p5.spline.int <- approx(d6.neg2p5.spline$x, d6.neg2p5.spline$y, n=10627)
macpass$d6.neg2p5.s <- d6.neg2p5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-20, 20))
# plot(macpass$d6.neg2p5 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg2p5, col = "blue")
lines(macpass$Date, macpass$d6.neg2p5.s, col = "red")



## Dale6 (-5 cm)

## January
# Construct linear model based on non-NA pairs
d6neg.5.1 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 1)
d6neg.5.1.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.1)
summary(d6neg.5.1.fit) # R2 = 0.904
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d6neg.5.2 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 2)
d6neg.5.2.fit <- lm(d6.neg.5 ~ hf.neg.5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.2)
summary(d6neg.5.2.fit) # R2 = 0.9446
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d6neg.5.3 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 3)
d6neg.5.3.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d2.0.s + d6.neg2p5, data = d6neg.5.3)
summary(d6neg.5.3.fit) # R2 = 0.8739
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d6neg.5.4 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 4)
d6neg.5.4.fit <- lm(d6.neg.5 ~ d2.neg.5.s + d6.neg2p5, data = d6neg.5.4)
summary(d6neg.5.4.fit) # R2 = 0.9678
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d6neg.5.5 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 5)
d6neg.5.5.fit <- lm(d6.neg.5 ~ d2.0.s + d6.neg2p5, data = d6neg.5.5)
summary(d6neg.5.5.fit) # R2 = 0.9239
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d6neg.5.6 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 6)
d6neg.5.6.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d6.0.s + d6.neg2p5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.6)
summary(d6neg.5.6.fit) # R2 = 0.8694
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d6neg.5.7 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 7)
d6neg.5.7.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d6.0.s + d6.neg2p5.s + d2.0.s + d6.neg2p5, data = d6neg.5.7)
summary(d6neg.5.7.fit) # R2 = 0.7857
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d6neg.5.8 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 8)
d6neg.5.8.fit <- lm(d6.neg.5 ~ d6.150 + d6.0.s + d6.neg2p5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.8)
summary(d6neg.5.8.fit) # R2 = 0.8377
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d6neg.5.9 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 9)
d6neg.5.9.fit <- lm(d6.neg.5 ~ hf.neg.5.s + d6.neg2p5.s + d2.0.s + d6.neg2p5, data = d6neg.5.9)
summary(d6neg.5.9.fit) # R2 = 0.9182
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d6neg.5.10 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 10)
d6neg.5.10.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d6.0.s + d6.neg2p5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.10)
summary(d6neg.5.10.fit) # R2 = 0.965
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d6neg.5.11 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 11)
d6neg.5.11.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d6.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.11)
summary(d6neg.5.11.fit) # R2 = 0.9827
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d6neg.5.12 <- macpass %>% filter(!is.na(d6.neg.5) & Month == 12)
d6neg.5.12.fit <- lm(d6.neg.5 ~ d6.150 + hf.neg.5.s + d6.0.s + d6.neg2p5.s + d2.0.s + d2.neg.5.s + d6.neg2p5, data = d6neg.5.12)
summary(d6neg.5.12.fit) # R2 = 0.9682
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.5.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.5 = ifelse(is.na(d6.neg.5), pred, d6.neg.5))
macpass %>% as.data.frame()

d6.neg.5.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg.5, method = "periodic", n = 365.25)
d6.neg.5.spline.int <- approx(d6.neg.5.spline$x, d6.neg.5.spline$y, n=10627)
macpass$d6.neg.5.s <- d6.neg.5.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-20, 20))
# plot(macpass$d6.neg.5 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg.5, col = "blue")
lines(macpass$Date, macpass$d6.neg.5.s, col = "red")




## Dale6 (-25 cm)

## January
# Construct linear model based on non-NA pairs
d6neg25.1 <- macpass %>% filter(!is.na(d6.neg25) & Month == 1)
d6neg25.1.fit <- lm(d6.neg25 ~ d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.1)
summary(d6neg25.1.fit) # R2 = 0.3713
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d6neg25.2 <- macpass %>% filter(!is.na(d6.neg25) & Month == 2)
d6neg25.2.fit <- lm(d6.neg25 ~ d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.2)
summary(d6neg25.2.fit) # R2 = 0.4465
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d6neg25.3 <- macpass %>% filter(!is.na(d6.neg25) & Month == 3)
d6neg25.3.fit <- lm(d6.neg25 ~ d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.3)
summary(d6neg25.3.fit) # R2 = 0.4789
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d6neg25.4 <- macpass %>% filter(!is.na(d6.neg25) & Month == 4)
d6neg25.4.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.4)
summary(d6neg25.4.fit) # R2 = 0.726
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d6neg25.5 <- macpass %>% filter(!is.na(d6.neg25) & Month == 5)
d6neg25.5.fit <- lm(d6.neg25 ~ d6.neg.5, data = d6neg25.5)
summary(d6neg25.5.fit) # R2 = 0.4598
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d6neg25.6 <- macpass %>% filter(!is.na(d6.neg25) & Month == 6)
d6neg25.6.fit <- lm(d6.neg25 ~ d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.6)
summary(d6neg25.6.fit) # R2 = 0.2161
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d6neg25.7 <- macpass %>% filter(!is.na(d6.neg25) & Month == 7)
d6neg25.7.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + d2.neg.5.s, data = d6neg25.7)
summary(d6neg25.7.fit) # R2 = 0.7857
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d6neg25.8 <- macpass %>% filter(!is.na(d6.neg25) & Month == 8)
d6neg25.8.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + d2.neg.5.s, data = d6neg25.8)
summary(d6neg25.8.fit) # R2 = 0.3309
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d6neg25.9 <- macpass %>% filter(!is.na(d6.neg25) & Month == 9)
d6neg25.9.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.9)
summary(d6neg25.9.fit) # R2 = 0.4436
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d6neg25.10 <- macpass %>% filter(!is.na(d6.neg25) & Month == 10)
d6neg25.10.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.10)
summary(d6neg25.10.fit) # R2 = 0.4297
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d6neg25.11 <- macpass %>% filter(!is.na(d6.neg25) & Month == 11)
d6neg25.11.fit <- lm(d6.neg25 ~ d6.neg2p5 + d6.neg.5 + hf.neg.5.s + d2.neg.5.s, data = d6neg25.11)
summary(d6neg25.11.fit) # R2 = 0.7798
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d6neg25.12 <- macpass %>% filter(!is.na(d6.neg25) & Month == 12)
d6neg25.12.fit <- lm(d6.neg25 ~ d6.neg.5 + d2.neg.5.s, data = d6neg25.12)
summary(d6neg25.12.fit) # R2 = 0.692
macpass <- macpass %>% 
  mutate(pred = predict(d6neg25.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg25 = ifelse(is.na(d6.neg25), pred, d6.neg25))
macpass %>% as.data.frame()

d6.neg25.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg25, method = "periodic", n = 365.25)
d6.neg25.spline.int <- approx(d6.neg25.spline$x, d6.neg25.spline$y, n=10627)
macpass$d6.neg25.s <- d6.neg25.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-20, 20))
# plot(macpass$d6.neg25 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg25, col = "blue")
lines(macpass$Date, macpass$d6.neg25.s, col = "red")


## Dale6 (-50 cm)

## January
# Construct linear model based on non-NA pairs
d6neg.50.1 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 1)
d6neg.50.1.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.1)
summary(d6neg.50.1.fit) # R2 = 0.6932
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
d6neg.50.2 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 2)
d6neg.50.2.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.2)
summary(d6neg.50.2.fit) # R2 = 0.8978
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.2.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
d6neg.50.3 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 3)
d6neg.50.3.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.3)
summary(d6neg.50.3.fit) # R2 = 0.8503
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.3.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
d6neg.50.4 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 4)
d6neg.50.4.fit <- lm(d6.neg.50 ~ d6.neg25 + hf.neg150 + hf.neg150.s, data = d6neg.50.4)
summary(d6neg.50.4.fit) # R2 = 0.8646
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.4.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
d6neg.50.5 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 5)
d6neg.50.5.fit <- lm(d6.neg.50 ~ d6.neg25 + hf.neg150 + hf.neg150.s, data = d6neg.50.5)
summary(d6neg.50.5.fit) # R2 = 0.8202
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.5.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
d6neg.50.6 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 6)
d6neg.50.6.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s, data = d6neg.50.6)
summary(d6neg.50.6.fit) # R2 = 0.6821
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.6.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
d6neg.50.7 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 7)
d6neg.50.7.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.7)
summary(d6neg.50.7.fit) # R2 = 0.4195
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.7.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
d6neg.50.8 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 8)
d6neg.50.8.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150.s, data = d6neg.50.8)
summary(d6neg.50.8.fit) # R2 = 0.643
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.8.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
d6neg.50.9 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 9)
d6neg.50.9.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150, data = d6neg.50.9)
summary(d6neg.50.9.fit) # R2 = 0.8315
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.9.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
d6neg.50.10 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 10)
d6neg.50.10.fit <- lm(d6.neg.50 ~ d6.neg25 + hf.neg150 + hf.neg150.s, data = d6neg.50.10)
summary(d6neg.50.10.fit) # R2 = 0.74
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.10.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
d6neg.50.11 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 11)
d6neg.50.11.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.11)
summary(d6neg.50.11.fit) # R2 = 0.8071
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.11.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
d6neg.50.12 <- macpass %>% filter(!is.na(d6.neg.50) & Month == 12)
d6neg.50.12.fit <- lm(d6.neg.50 ~ d6.neg25 + d6.neg25.s + hf.neg150 + hf.neg150.s, data = d6neg.50.12)
summary(d6neg.50.12.fit) # R2 = 0.8423
macpass <- macpass %>% 
  mutate(pred = predict(d6neg.50.12.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg.50 = ifelse(is.na(d6.neg.50), pred, d6.neg.50))
macpass %>% as.data.frame()

d6.neg.50.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg.50, method = "periodic", n = 365.50)
d6.neg.50.spline.int <- approx(d6.neg.50.spline$x, d6.neg.50.spline$y, n=10627)
macpass$d6.neg.50.s <- d6.neg.50.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-20, 20))
# plot(macpass$d6.neg.50 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg.50, col = "blue")
lines(macpass$Date, macpass$d6.neg.50.s, col = "red")




## Dale6 (-150 cm)

## Trying the entire thing at once
# Construct linear model based on non-NA pairs
d6neg150.all <- macpass %>% filter(!is.na(d6.neg150))
d6neg150.all.fit <- lm(d6.neg150 ~ hf.neg150 + d2.neg150, data = d6neg150.all)
summary(d6neg150.all.fit) # R2 = 0.6538
macpass <- macpass %>% 
  mutate(pred = predict(d6neg150.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg150 = ifelse(is.na(d6.neg150), pred, d6.neg150))
macpass %>% as.data.frame()

d6.neg150.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg150, method = "periodic", n = 365.25)
d6.neg150.spline.int <- approx(d6.neg150.spline$x, d6.neg150.spline$y, n=10627)
macpass$d6.neg150.s <- d6.neg150.spline.int$y

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-3, 1))
# plot(macpass$d6.neg150 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg150, col = "blue")
lines(macpass$Date, macpass$d6.neg150.s, col = "red")

plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-10, 1))
lines(macpass$Date, macpass$d6.neg150.s, col = "green")
lines(macpass$Date, macpass$d2.neg150.s, col = "darkblue")
lines(macpass$Date, macpass$hf.neg150.s, col = "blue")
lines(macpass$Date, macpass$gf.neg150.s, col = "red")
lines(macpass$Date, macpass$bp.neg150.s, col = "darkgoldenrod1")


# Dale6 (-200 cm)

## Trying the entire thing at once
# Construct linear model based on non-NA pairs
d6neg200.all <- macpass %>% filter(!is.na(d6.neg200))
d6neg200.all.fit <- lm(d6.neg200 ~ hf.neg150 + d2.neg150, data = d6neg200.all)
summary(d6neg200.all.fit) # R2 = 0.4474
macpass <- macpass %>% 
  mutate(pred = predict(d6neg200.all.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(d6.neg200 = ifelse(is.na(d6.neg200), pred, d6.neg200))
macpass %>% as.data.frame()

d6.neg200.spline <- spline(as.numeric(macpass$Date), macpass$d6.neg200, method = "periodic", n = 365.25)
d6.neg200.spline.int <- approx(d6.neg200.spline$x, d6.neg200.spline$y, n=10627)
macpass$d6.neg200.s <- d6.neg200.spline.int$y


plot(1, type="n", xlab="", ylab="", xlim=c(0, max(as.numeric(macpass$Date))), ylim=c(-3, 1))
# plot(macpass$d6.neg200 ~ macpass$Date, type = "l", col = "white")
lines(macpass$Date, macpass$d6.neg200, col = "blue")
lines(macpass$Date, macpass$d6.neg200.s, col = "red")

hf.150.spline <- spline(as.numeric(macpass$Date), macpass$hf.150, method = "periodic", n = 365.25)
bp.150.spline <- spline(as.numeric(macpass$Date), macpass$bp.150, method = "periodic", n = 365.25)
d6.150.spline <- spline(as.numeric(macpass$Date), macpass$d6.150, method = "periodic", n = 365.25)
d2.150.spline <- spline(as.numeric(macpass$Date), macpass$d2.150, method = "periodic", n = 365.25)
gf.150.spline <- spline(as.numeric(macpass$Date), macpass$gf.150, method = "periodic", n = 365.25)

hf.150.spline.int <- approx(hf.150.spline$x, hf.150.spline$y, n=10627)
bp.150.spline.int <- approx(bp.150.spline$x, bp.150.spline$y, n=10627)
d6.150.spline.int <- approx(d6.150.spline$x, d6.150.spline$y, n=10627)
d2.150.spline.int <- approx(d2.150.spline$x, d2.150.spline$y, n=10627)
gf.150.spline.int <- approx(gf.150.spline$x, gf.150.spline$y, n=10627)

macpass$hf.150.s <- hf.150.spline.int$y
macpass$bp.150.s <- bp.150.spline.int$y
macpass$d6.150.s <- d6.150.spline.int$y
macpass$d2.150.s <- d2.150.spline.int$y
macpass$gf.150.s <- gf.150.spline.int$y



macpass2 <- cbind.data.frame(macpass[c("Month","Date","YEAR","DAY",
                                       "hf.150","hf.0","hf.neg2p5","hf.neg.5","hf.neg150",
                                       "bp.150","bp.0","bp.neg2p5","bp.neg.5","bp.neg150","bp.neg215","bp.neg385",
                                       "d2.150","d2.0","d2.neg2p5","d2.neg.5","d2.neg150",
                                       "d6.150","d6.0","d6.neg2p5","d6.neg.5","d6.neg25","d6.neg.50","d6.neg150","d6.neg200",
                                       "gf.150","gf.0","gf.neg2p5","gf.neg5","gf.neg150",
                                       "hf.150.s","hf.0.s","hf.neg2p5.s","hf.neg.5.s","hf.neg150.s",
                                       "bp.150.s","bp.0.s","bp.neg2p5.s","bp.neg.5.s","bp.neg150.s","bp.neg215.s","bp.neg385.s",
                                       "d6.150.s","d6.0.s","d6.neg2p5.s","d6.neg.5.s","d6.neg25.s","d6.neg.50.s","d6.neg150.s","d6.neg200.s",
                                       "d2.150.s","d2.0.s","d2.neg2p5.s","d2.neg.5.s","d2.neg150.s",
                                       "gf.150.s","gf.0.s","gf.neg2p5.s","gf.neg5.s","gf.neg150.s")])
names(macpass2)[33] <- "gf.neg.5"

write.csv(macpass2, "~/Desktop/Workspace/Earthwatch/mm_microclimate3.csv")

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

### Filling GTREE temps
macpass <- read.csv(file = "~/Desktop/Workspace/Earthwatch/macpass_gtree_temps.csv", header = TRUE)
macpass2 <- macpass

##********************************
### bwc_sca_cage
## January
# Construct linear model based on non-NA pairs
bwc_sca_cage.1 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 1)
bwc_sca_cage.1.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_sca_cage.2 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 2)
bwc_sca_cage.2.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_sca_cage.3 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 3)
bwc_sca_cage.3.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_sca_cage.4 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 4)
bwc_sca_cage.4.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_sca_cage.5 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 5)
bwc_sca_cage.5.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_sca_cage.6 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 6)
bwc_sca_cage.6.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_sca_cage.7 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 7)
bwc_sca_cage.7.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_sca_cage.8 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 8)
bwc_sca_cage.8.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_sca_cage.9 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 9)
bwc_sca_cage.9.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_sca_cage.10 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 10)
bwc_sca_cage.10.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_sca_cage.11 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 11)
bwc_sca_cage.11.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_sca_cage.12 <- macpass %>% filter(!is.na(bwc_sca_cage) & Month == 12)
bwc_sca_cage.12.fit <- lm(bwc_sca_cage ~ gf.soil, data = bwc_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_sca_cage = ifelse(is.na(bwc_sca_cage), pred, bwc_sca_cage))
macpass %>% as.data.frame()



##********************************
### bwc_sca_no
## January
# Construct linear model based on non-NA pairs
bwc_sca_no.1 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 1)
bwc_sca_no.1.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_sca_no.2 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 2)
bwc_sca_no.2.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_sca_no.3 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 3)
bwc_sca_no.3.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_sca_no.4 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 4)
bwc_sca_no.4.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_sca_no.5 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 5)
bwc_sca_no.5.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_sca_no.6 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 6)
bwc_sca_no.6.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_sca_no.7 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 7)
bwc_sca_no.7.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_sca_no.8 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 8)
bwc_sca_no.8.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_sca_no.9 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 9)
bwc_sca_no.9.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_sca_no.10 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 10)
bwc_sca_no.10.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_sca_no.11 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 11)
bwc_sca_no.11.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_sca_no.12 <- macpass %>% filter(!is.na(bwc_sca_no) & Month == 12)
bwc_sca_no.12.fit <- lm(bwc_sca_no ~ gf.soil, data = bwc_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_sca_no = ifelse(is.na(bwc_sca_no), pred, bwc_sca_no))
macpass %>% as.data.frame()





##********************************
### bwc_veg_cage
## January
# Construct linear model based on non-NA pairs
bwc_veg_cage.1 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 1)
bwc_veg_cage.1.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_veg_cage.2 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 2)
bwc_veg_cage.2.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_veg_cage.3 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 3)
bwc_veg_cage.3.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_veg_cage.4 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 4)
bwc_veg_cage.4.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_veg_cage.5 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 5)
bwc_veg_cage.5.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_veg_cage.6 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 6)
bwc_veg_cage.6.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_veg_cage.7 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 7)
bwc_veg_cage.7.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_veg_cage.8 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 8)
bwc_veg_cage.8.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_veg_cage.9 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 9)
bwc_veg_cage.9.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_veg_cage.10 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 10)
bwc_veg_cage.10.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_veg_cage.11 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 11)
bwc_veg_cage.11.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_veg_cage.12 <- macpass %>% filter(!is.na(bwc_veg_cage) & Month == 12)
bwc_veg_cage.12.fit <- lm(bwc_veg_cage ~ gf.soil, data = bwc_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_veg_cage = ifelse(is.na(bwc_veg_cage), pred, bwc_veg_cage))
macpass %>% as.data.frame()


##********************************
### bwc_veg_no
## January
# Construct linear model based on non-NA pairs
bwc_veg_no.1 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 1)
bwc_veg_no.1.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bwc_veg_no.2 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 2)
bwc_veg_no.2.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bwc_veg_no.3 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 3)
bwc_veg_no.3.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bwc_veg_no.4 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 4)
bwc_veg_no.4.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bwc_veg_no.5 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 5)
bwc_veg_no.5.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bwc_veg_no.6 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 6)
bwc_veg_no.6.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bwc_veg_no.7 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 7)
bwc_veg_no.7.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bwc_veg_no.8 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 8)
bwc_veg_no.8.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bwc_veg_no.9 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 9)
bwc_veg_no.9.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bwc_veg_no.10 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 10)
bwc_veg_no.10.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bwc_veg_no.11 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 11)
bwc_veg_no.11.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bwc_veg_no.12 <- macpass %>% filter(!is.na(bwc_veg_no) & Month == 12)
bwc_veg_no.12.fit <- lm(bwc_veg_no ~ gf.soil, data = bwc_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bwc_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bwc_veg_no = ifelse(is.na(bwc_veg_no), pred, bwc_veg_no))
macpass %>% as.data.frame()

############################################################
############################################################
############################################################
############################################################

##********************************
### bws_sca_cage
## January
# Construct linear model based on non-NA pairs
bws_sca_cage.1 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 1)
bws_sca_cage.1.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_sca_cage.2 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 2)
bws_sca_cage.2.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_sca_cage.3 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 3)
bws_sca_cage.3.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_sca_cage.4 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 4)
bws_sca_cage.4.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_sca_cage.5 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 5)
bws_sca_cage.5.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_sca_cage.6 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 6)
bws_sca_cage.6.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_sca_cage.7 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 7)
bws_sca_cage.7.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_sca_cage.8 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 8)
bws_sca_cage.8.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_sca_cage.9 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 9)
bws_sca_cage.9.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_sca_cage.10 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 10)
bws_sca_cage.10.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_sca_cage.11 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 11)
bws_sca_cage.11.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_sca_cage.12 <- macpass %>% filter(!is.na(bws_sca_cage) & Month == 12)
bws_sca_cage.12.fit <- lm(bws_sca_cage ~ gf.soil, data = bws_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_sca_cage = ifelse(is.na(bws_sca_cage), pred, bws_sca_cage))
macpass %>% as.data.frame()



##********************************
### bws_sca_no
## January
# Construct linear model based on non-NA pairs
bws_sca_no.1 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 1)
bws_sca_no.1.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_sca_no.2 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 2)
bws_sca_no.2.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_sca_no.3 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 3)
bws_sca_no.3.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_sca_no.4 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 4)
bws_sca_no.4.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_sca_no.5 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 5)
bws_sca_no.5.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_sca_no.6 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 6)
bws_sca_no.6.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_sca_no.7 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 7)
bws_sca_no.7.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_sca_no.8 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 8)
bws_sca_no.8.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_sca_no.9 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 9)
bws_sca_no.9.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_sca_no.10 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 10)
bws_sca_no.10.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_sca_no.11 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 11)
bws_sca_no.11.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_sca_no.12 <- macpass %>% filter(!is.na(bws_sca_no) & Month == 12)
bws_sca_no.12.fit <- lm(bws_sca_no ~ gf.soil, data = bws_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_sca_no = ifelse(is.na(bws_sca_no), pred, bws_sca_no))
macpass %>% as.data.frame()





##********************************
### bws_veg_cage
## January
# Construct linear model based on non-NA pairs
bws_veg_cage.1 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 1)
bws_veg_cage.1.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_veg_cage.2 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 2)
bws_veg_cage.2.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_veg_cage.3 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 3)
bws_veg_cage.3.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_veg_cage.4 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 4)
bws_veg_cage.4.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_veg_cage.5 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 5)
bws_veg_cage.5.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_veg_cage.6 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 6)
bws_veg_cage.6.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_veg_cage.7 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 7)
bws_veg_cage.7.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_veg_cage.8 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 8)
bws_veg_cage.8.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_veg_cage.9 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 9)
bws_veg_cage.9.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_veg_cage.10 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 10)
bws_veg_cage.10.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_veg_cage.11 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 11)
bws_veg_cage.11.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_veg_cage.12 <- macpass %>% filter(!is.na(bws_veg_cage) & Month == 12)
bws_veg_cage.12.fit <- lm(bws_veg_cage ~ gf.soil, data = bws_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_veg_cage = ifelse(is.na(bws_veg_cage), pred, bws_veg_cage))
macpass %>% as.data.frame()


##********************************
### bws_veg_no
## January
# Construct linear model based on non-NA pairs
bws_veg_no.1 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 1)
bws_veg_no.1.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
bws_veg_no.2 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 2)
bws_veg_no.2.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
bws_veg_no.3 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 3)
bws_veg_no.3.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
bws_veg_no.4 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 4)
bws_veg_no.4.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
bws_veg_no.5 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 5)
bws_veg_no.5.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
bws_veg_no.6 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 6)
bws_veg_no.6.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
bws_veg_no.7 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 7)
bws_veg_no.7.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
bws_veg_no.8 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 8)
bws_veg_no.8.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
bws_veg_no.9 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 9)
bws_veg_no.9.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
bws_veg_no.10 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 10)
bws_veg_no.10.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
bws_veg_no.11 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 11)
bws_veg_no.11.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
bws_veg_no.12 <- macpass %>% filter(!is.na(bws_veg_no) & Month == 12)
bws_veg_no.12.fit <- lm(bws_veg_no ~ gf.soil, data = bws_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(bws_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(bws_veg_no = ifelse(is.na(bws_veg_no), pred, bws_veg_no))
macpass %>% as.data.frame()


############################################################
############################################################
############################################################
############################################################

##********************************
### dln_sca_cage
## January
# Construct linear model based on non-NA pairs
dln_sca_cage.1 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 1)
dln_sca_cage.1.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_sca_cage.2 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 2)
dln_sca_cage.2.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_sca_cage.3 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 3)
dln_sca_cage.3.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_sca_cage.4 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 4)
dln_sca_cage.4.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_sca_cage.5 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 5)
dln_sca_cage.5.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_sca_cage.6 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 6)
dln_sca_cage.6.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_sca_cage.7 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 7)
dln_sca_cage.7.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_sca_cage.8 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 8)
dln_sca_cage.8.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_sca_cage.9 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 9)
dln_sca_cage.9.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_sca_cage.10 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 10)
dln_sca_cage.10.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_sca_cage.11 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 11)
dln_sca_cage.11.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_sca_cage.12 <- macpass %>% filter(!is.na(dln_sca_cage) & Month == 12)
dln_sca_cage.12.fit <- lm(dln_sca_cage ~ gf.soil, data = dln_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_sca_cage = ifelse(is.na(dln_sca_cage), pred, dln_sca_cage))
macpass %>% as.data.frame()



##********************************
### dln_sca_no
## January
# Construct linear model based on non-NA pairs
dln_sca_no.1 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 1)
dln_sca_no.1.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_sca_no.2 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 2)
dln_sca_no.2.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_sca_no.3 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 3)
dln_sca_no.3.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_sca_no.4 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 4)
dln_sca_no.4.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_sca_no.5 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 5)
dln_sca_no.5.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_sca_no.6 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 6)
dln_sca_no.6.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_sca_no.7 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 7)
dln_sca_no.7.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_sca_no.8 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 8)
dln_sca_no.8.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_sca_no.9 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 9)
dln_sca_no.9.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_sca_no.10 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 10)
dln_sca_no.10.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_sca_no.11 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 11)
dln_sca_no.11.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_sca_no.12 <- macpass %>% filter(!is.na(dln_sca_no) & Month == 12)
dln_sca_no.12.fit <- lm(dln_sca_no ~ gf.soil, data = dln_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_sca_no = ifelse(is.na(dln_sca_no), pred, dln_sca_no))
macpass %>% as.data.frame()





##********************************
### dln_veg_cage
## January
# Construct linear model based on non-NA pairs
dln_veg_cage.1 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 1)
dln_veg_cage.1.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_veg_cage.2 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 2)
dln_veg_cage.2.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_veg_cage.3 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 3)
dln_veg_cage.3.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_veg_cage.4 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 4)
dln_veg_cage.4.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_veg_cage.5 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 5)
dln_veg_cage.5.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_veg_cage.6 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 6)
dln_veg_cage.6.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_veg_cage.7 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 7)
dln_veg_cage.7.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_veg_cage.8 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 8)
dln_veg_cage.8.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_veg_cage.9 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 9)
dln_veg_cage.9.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_veg_cage.10 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 10)
dln_veg_cage.10.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_veg_cage.11 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 11)
dln_veg_cage.11.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_veg_cage.12 <- macpass %>% filter(!is.na(dln_veg_cage) & Month == 12)
dln_veg_cage.12.fit <- lm(dln_veg_cage ~ gf.soil, data = dln_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_veg_cage = ifelse(is.na(dln_veg_cage), pred, dln_veg_cage))
macpass %>% as.data.frame()


##********************************
### dln_veg_no
## January
# Construct linear model based on non-NA pairs
dln_veg_no.1 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 1)
dln_veg_no.1.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dln_veg_no.2 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 2)
dln_veg_no.2.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dln_veg_no.3 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 3)
dln_veg_no.3.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dln_veg_no.4 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 4)
dln_veg_no.4.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dln_veg_no.5 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 5)
dln_veg_no.5.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dln_veg_no.6 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 6)
dln_veg_no.6.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dln_veg_no.7 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 7)
dln_veg_no.7.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dln_veg_no.8 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 8)
dln_veg_no.8.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dln_veg_no.9 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 9)
dln_veg_no.9.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dln_veg_no.10 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 10)
dln_veg_no.10.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dln_veg_no.11 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 11)
dln_veg_no.11.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dln_veg_no.12 <- macpass %>% filter(!is.na(dln_veg_no) & Month == 12)
dln_veg_no.12.fit <- lm(dln_veg_no ~ gf.soil, data = dln_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dln_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dln_veg_no = ifelse(is.na(dln_veg_no), pred, dln_veg_no))
macpass %>% as.data.frame()



############################################################
############################################################
############################################################
############################################################

##********************************
### dls_sca_cage
## January
# Construct linear model based on non-NA pairs
dls_sca_cage.1 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 1)
dls_sca_cage.1.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_sca_cage.2 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 2)
dls_sca_cage.2.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_sca_cage.3 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 3)
dls_sca_cage.3.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_sca_cage.4 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 4)
dls_sca_cage.4.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_sca_cage.5 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 5)
dls_sca_cage.5.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_sca_cage.6 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 6)
dls_sca_cage.6.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_sca_cage.7 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 7)
dls_sca_cage.7.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_sca_cage.8 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 8)
dls_sca_cage.8.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_sca_cage.9 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 9)
dls_sca_cage.9.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_sca_cage.10 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 10)
dls_sca_cage.10.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_sca_cage.11 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 11)
dls_sca_cage.11.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_sca_cage.12 <- macpass %>% filter(!is.na(dls_sca_cage) & Month == 12)
dls_sca_cage.12.fit <- lm(dls_sca_cage ~ gf.soil, data = dls_sca_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_sca_cage = ifelse(is.na(dls_sca_cage), pred, dls_sca_cage))
macpass %>% as.data.frame()



##********************************
### dls_sca_no
## January
# Construct linear model based on non-NA pairs
dls_sca_no.1 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 1)
dls_sca_no.1.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_sca_no.2 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 2)
dls_sca_no.2.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_sca_no.3 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 3)
dls_sca_no.3.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_sca_no.4 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 4)
dls_sca_no.4.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_sca_no.5 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 5)
dls_sca_no.5.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_sca_no.6 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 6)
dls_sca_no.6.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_sca_no.7 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 7)
dls_sca_no.7.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_sca_no.8 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 8)
dls_sca_no.8.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_sca_no.9 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 9)
dls_sca_no.9.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_sca_no.10 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 10)
dls_sca_no.10.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_sca_no.11 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 11)
dls_sca_no.11.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_sca_no.12 <- macpass %>% filter(!is.na(dls_sca_no) & Month == 12)
dls_sca_no.12.fit <- lm(dls_sca_no ~ gf.soil, data = dls_sca_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_sca_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_sca_no = ifelse(is.na(dls_sca_no), pred, dls_sca_no))
macpass %>% as.data.frame()





##********************************
### dls_veg_cage
## January
# Construct linear model based on non-NA pairs
dls_veg_cage.1 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 1)
dls_veg_cage.1.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_veg_cage.2 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 2)
dls_veg_cage.2.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_veg_cage.3 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 3)
dls_veg_cage.3.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_veg_cage.4 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 4)
dls_veg_cage.4.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_veg_cage.5 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 5)
dls_veg_cage.5.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_veg_cage.6 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 6)
dls_veg_cage.6.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_veg_cage.7 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 7)
dls_veg_cage.7.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_veg_cage.8 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 8)
dls_veg_cage.8.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_veg_cage.9 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 9)
dls_veg_cage.9.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_veg_cage.10 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 10)
dls_veg_cage.10.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_veg_cage.11 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 11)
dls_veg_cage.11.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_veg_cage.12 <- macpass %>% filter(!is.na(dls_veg_cage) & Month == 12)
dls_veg_cage.12.fit <- lm(dls_veg_cage ~ gf.soil, data = dls_veg_cage.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_cage.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_veg_cage = ifelse(is.na(dls_veg_cage), pred, dls_veg_cage))
macpass %>% as.data.frame()


##********************************
### dls_veg_no
## January
# Construct linear model based on non-NA pairs
dls_veg_no.1 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 1)
dls_veg_no.1.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.1)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.1.fit, .)) %>%
  # Replace NA with pred in var1
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## February
# Construct linear model based on non-NA pairs
dls_veg_no.2 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 2)
dls_veg_no.2.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.2)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.2.fit, .)) %>%
  # Replace NA with pred in var2
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## March
# Construct linear model based on non-NA pairs
dls_veg_no.3 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 3)
dls_veg_no.3.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.3)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.3.fit, .)) %>%
  # Replace NA with pred in var3
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## April
# Construct linear model based on non-NA pairs
dls_veg_no.4 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 4)
dls_veg_no.4.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.4)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.4.fit, .)) %>%
  # Replace NA with pred in var4
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## May
# Construct linear model based on non-NA pairs
dls_veg_no.5 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 5)
dls_veg_no.5.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.5)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.5.fit, .)) %>%
  # Replace NA with pred in var5
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## June
# Construct linear model based on non-NA pairs
dls_veg_no.6 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 6)
dls_veg_no.6.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.6)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.6.fit, .)) %>%
  # Replace NA with pred in var6
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## July
# Construct linear model based on non-NA pairs
dls_veg_no.7 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 7)
dls_veg_no.7.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.7)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.7.fit, .)) %>%
  # Replace NA with pred in var7
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## August
# Construct linear model based on non-NA pairs
dls_veg_no.8 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 8)
dls_veg_no.8.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.8)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.8.fit, .)) %>%
  # Replace NA with pred in var8
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## September
# Construct linear model based on non-NA pairs
dls_veg_no.9 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 9)
dls_veg_no.9.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.9)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.9.fit, .)) %>%
  # Replace NA with pred in var9
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## October
# Construct linear model based on non-NA pairs
dls_veg_no.10 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 10)
dls_veg_no.10.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.10)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.10.fit, .)) %>%
  # Replace NA with pred in var10
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## November
# Construct linear model based on non-NA pairs
dls_veg_no.11 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 11)
dls_veg_no.11.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.11)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.11.fit, .)) %>%
  # Replace NA with pred in var11
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

## December
# Construct linear model based on non-NA pairs
dls_veg_no.12 <- macpass %>% filter(!is.na(dls_veg_no) & Month == 12)
dls_veg_no.12.fit <- lm(dls_veg_no ~ gf.soil, data = dls_veg_no.12)
macpass <- macpass %>% 
  mutate(pred = predict(dls_veg_no.12.fit, .)) %>%
  # Replace NA with pred in var12
  mutate(dls_veg_no = ifelse(is.na(dls_veg_no), pred, dls_veg_no))
macpass %>% as.data.frame()

plot(macpass$bwc_sca_cage, type = "l", col = "blue")
lines(macpass2$bwc_sca_cage)

write.csv(macpass2, "~/Desktop/Workspace/MacPass/MM_temps_2019.csv")
