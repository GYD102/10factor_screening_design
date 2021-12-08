setwd("D:/stonybrook_coursework/ams582")

## ---- packages ----
require(FrF2)
require(FrF2.catlg128)
require(DoE.wrapper)
require(DoE.base)
require(dplyr)
require(ggplot2)
require(ggpubr)
require(MASS)
require(RcmdrMisc)

## ---- rcmdr ----

require(RcmdrPlugin.DoE)

## ----  design_object_setup1 ----

set.seed(2052898)
design <- FrF2(nfactors = 10, resolution = 5, ncenter = 3)
set.seed(8362889)
design <- ccd.augment(design)

set.seed(2052898)
design_2 <- FrF2(nfactors = 10, resolution = 5, ncenter = 3)
set.seed(8362889)

face_center <- function(x){
  if(x == 0)
    return(0)
  return(x/abs(x))
}

for(i in 1:nrow(design)){
  for(j in 2:ncol(design)){
    design[i,j] <- face_center(design[i,j])
  }
}

for(i in 1:nrow(design_2)){
  for(j in 2:ncol(design_2)){
    design[i,j] <- face_center(design[i,j])
  }
}

# design_to_csv <- design[,2:11]
# rownames(design_to_csv) <- NULL
# write.csv(design_to_csv, "Group_11.csv", row.names = FALSE)

resp <- read.csv("total11.csv")

design <- add.response(design = design, response = resp$y)
design_2 <- add.response(design = design_2, response = resp$y[1:131])

## ----  design_object_setup2 ----

design_centers <- design[c("C1.129","C1.130","C1.131"), 2:12]
design_centers <- rbind(design_centers, design[132:155, 2:12])

cs <- c("C1.129","C1.130","C1.131")

all_0 <- design_centers$A == 0 & design_centers$B == 0 &
  design_centers$C == 0 & design_centers$D == 0 &
  design_centers$E == 0 & design_centers$F == 0 &
  design_centers$G == 0 & design_centers$H == 0 &
  design_centers$J == 0 & design_centers$K == 0

As <- xor(design_centers$A != 0, all_0)
Bs <- xor(design_centers$B != 0, all_0)
Cs <- xor(design_centers$C != 0, all_0)
Ds <- xor(design_centers$D != 0, all_0)
Es <- xor(design_centers$E != 0, all_0)
Fs <- xor(design_centers$F != 0, all_0)
Gs <- xor(design_centers$G != 0, all_0)
Hs <- xor(design_centers$H != 0, all_0)
Js <- xor(design_centers$J != 0, all_0)
Ks <- xor(design_centers$K != 0, all_0)

## ---- centers_plot ----

Ap <- ggplot(data = design_centers[As,], aes(x = as.factor(A), y = resp.y)) + geom_boxplot()
Bp <- ggplot(data = design_centers[Bs,], aes(x = as.factor(B), y = resp.y)) + geom_boxplot()
Cp <- ggplot(data = design_centers[Cs,], aes(x = as.factor(C), y = resp.y)) + geom_boxplot()
Dp <- ggplot(data = design_centers[Ds,], aes(x = as.factor(D), y = resp.y)) + geom_boxplot()
Ep <- ggplot(data = design_centers[Es,], aes(x = as.factor(E), y = resp.y)) + geom_boxplot()
Fp <- ggplot(data = design_centers[Fs,], aes(x = as.factor(F), y = resp.y)) + geom_boxplot()
Gp <- ggplot(data = design_centers[Gs,], aes(x = as.factor(G), y = resp.y)) + geom_boxplot()
Hp <- ggplot(data = design_centers[Hs,], aes(x = as.factor(H), y = resp.y)) + geom_boxplot()
Jp <- ggplot(data = design_centers[Js,], aes(x = as.factor(J), y = resp.y)) + geom_boxplot()
Kp <- ggplot(data = design_centers[Ks,], aes(x = as.factor(K), y = resp.y)) + geom_boxplot()

figure <- ggarrange(Ap, Bp, Cp, Dp, Ep, Fp, Gp, Hp, Jp, Kp, 
                    labels = Letters[1:10],
                    ncol = 4, nrow = 3)

figure

## ---- centers_tests ----

fstats <- summary(lm(resp.y ~ D, data = design_centers))$fstatistic
1 - pf(fstats[1], fstats[2], fstats[3])

summary(lm(log(resp.y) ~ D, data = design_centers, na.action = na.omit))

fstats <- summary(lm(resp.y ~ E, data = design_centers))$fstatistic
1 - pf(fstats[1], fstats[2], fstats[3])

summary(lm(log(resp.y) ~ E, data = design_centers, na.action = na.omit))

fstats <- summary(lm(resp.y ~ F, data = design_centers))$fstatistic
1 - pf(fstats[1], fstats[2], fstats[3])

summary(lm(log(resp.y) ~ F, data = design_centers, na.action = na.omit))

## ---- rcmdr1 ----
MEPlot(design_2, abbrev=4, select=c(1,2,3,4,5,6,7,8,9,10), 
       response="resp.y.1.131.")

## ---- rcmdr1.2 ----
IAPlot(design_2, abbrev=4, show.alias=FALSE, select=c(1,3,4,6,10))

## ---- rcmdr1.3 ----
IAPlot(design_2, abbrev=4, show.alias=FALSE, select=c(2,5,6,8,9))

## ---- rcmdr2 ----
LinearModel.1 <- lm(resp.y ~ (A + B + C + D + E + F + G + H + J + K)^2 + 
                      I(A^2) + I(B^2) + I(C^2) + I(D^2) + I(E^2) + I(F^2) + I(G^2) + I(H^2) + 
                      I(J^2) + I(K^2), data=design)
summary(aov(LinearModel.1))

## ---- rcmdr3 ----
IAPlot(design_2, abbrev=4, show.alias=FALSE, select=c(3,4,5,6,7))

## ---- rcmdr4 ----
stepwise(LinearModel.1, direction='backward/forward', criterion='BIC', trace = 0)

## ---- rcmdr5 ----
LinearModel.2 <- lm(resp.y ~ D * G, data=design)
LinearModel.3 <- lm(resp.y ~ D*G + C:D, data=design)
LinearModel.4 <- lm(resp.y ~ D*G + C:D + E + F, data=design)

anova(LinearModel.2, LinearModel.3)
anova(LinearModel.3, LinearModel.4)

## ---- rcmdr6 ----

#oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(LinearModel.4)
#par(oldpar)

## ---- rcmdr7 ----

DanielPlot(design_2, code=TRUE, autolab=TRUE, alpha=0.05, half=TRUE, 
           response="resp.y.1.131.")

## ---- rcmdr8 ----

LinearModel.5 <- lm(resp.y ~ D*G + C:D + E + F:(1 + C + G), data=design)
summary(LinearModel.5)

anova(LinearModel.3, LinearModel.5)

## ---- rcmdr9 ----
#BJK alias CBDG -> CDG (assuming B isn't present)
LinearModel.6 <- lm(resp.y ~ D*G + C:D + E + F:(1 + C + G) + C:D:G, 
                    data=design)
anova(LinearModel.5, LinearModel.6)

## ---- rcmdr10 ----
# Importance of axial runs (not center runs)
LinearModel.7 <- lm(resp.y.1.131. ~ (A + B + C + D + E + F + G + H + J + 
                                       K)^2, data=design_2)
summary(aov(LinearModel.7))

## ---- rcmdr11 ----
# Ludicrous model, chasing R^2
LinearModel.8 <- lm(resp.y ~ G + C:D + D:G + E:H + B:J + G:B:J + C:G:E:H + 
                      C:D:G:E:H + C:D:G:E:H:B:J, data=design)
summary(LinearModel.8)
anova(LinearModel.5, LinearModel.8)

## ---- rcmdr12 ----
plot(LinearModel.8)

## ---- appendix1 ----
print(design)
attributes(design)
summary(design)

