library(ggplot2)
library(lme4)
library(AICcmodavg)

MPLS.LS <- read.csv("/Users/allisonking/Downloads/MPLS Long.csv")

#Center time variable at first obs (grade 5)
MPLS.LS$grade5 <- MPLS.LS$grade - 5

#Collapse ethnicity to white and non-white
MPLS.LS$eth2 <- factor(ifelse(MPLS.LS$eth=='Whi', yes ='W', no ='NW'))

#Fit 6 competing models
model.1 <- lmer(read ~ grade5 + risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.2 <- lmer(read ~ grade5 + eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.3 <- lmer(read ~ grade5 + risk2 + eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.4 <- lmer(read ~ grade5 * risk2 + (grade5 | subid), MPLS.LS, REML = FALSE)
model.5 <- lmer(read ~ grade5 * eth2  + (grade5 | subid), MPLS.LS, REML = FALSE)
model.6 <- lmer(read ~ grade5 * risk2 + grade5 * eth2 + (grade5 | subid), MPLS.LS, REML = FALSE)

#Create table of model fit statistics
mynames <- paste("M", as.character(1:6), sep = "")
mymodels <- list(model.1, model.2, model.3, model.4, model.5, model.6)
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames,
                               sort = FALSE)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[,-1], 2))

#Define components for later use in ggplot
myx <- scale_x_continuous(breaks = 5:8)
theme_set(theme_bw())

#Create bar graphs of weight of evidence
g1 <- ggplot(myaicc, aes(x = Modnames, y = AICcWt)) + ylab("Weight")
g2 <- g1 + geom_bar(stat = "identity", fill = "grey80", color = "black") + xlab("Model")
g3 <- g2 + scale_y_continuous(limits = c(0,1))
print(g3)

#Create bar graphs of weight of evidence ratio
g4 <- ggplot(myaicc, aes(x = Modnames, y = Eratio)) + ylab("Ratio")
g5 <- g4 + geom_bar(stat = "identity", fill = "grey80", color = "black") + xlab("Model")
g6 <- g5 + geom_hline(aes(yintercept = 1), linetype = 2)
print(g6)

#Include cumulative weight after sorting from worst model to best
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames)[ ,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[ ,-1], 2))

#Extract fixed effects from model 1
mytab <- as.data.frame(coef(summary(model.1)))

#Calculate approximate CI
mytab$LCI <- mytab[,'Estimate'] - 2*mytab[,'Std. Error']
mytab$UCI <- mytab[,'Estimate'] + 2*mytab[,'Std. Error']
round(mytab[ ,-3], 2)

#Create data frame for graphing raw data with fitted values
#Specifically, observe means (points) and fitted curves (lines) for risk groups
plotdata <- model.1@frame
plotdata$pred <- model.matrix(model.1) %*% fixef(model.1)
plotdata$grade <- plotdata$grade + 5

#Plot data
g1 <- ggplot(plotdata, aes(x = grade, y = read, linetype = risk2))
g2 <- g1 + stat_summary(fun = "mean", geom = "point", cex = 2)
g3 <- g2 + stat_summary(aes(y = pred), fun = "mean", geom = "line")
g4 <- g3 + myx + theme(legend.background = element_blank(), 
                       legend.box.background = element_rect(colour = "black"), legend.position="bottom")
print(g4)

#Plot fitted values faceted by subject
MPLS.LS.nomiss <- subset(MPLS.LS, !is.na(read))
MPLS.LS.nomiss$fitted <- fitted(model.1)

g1 <- ggplot(data = MPLS.LS.nomiss, aes(x = grade, y = read , group = subid))
g3 <- g1 + geom_point() + facet_wrap(~ subid) + 
  stat_summary(aes(y = fitted), fun = "mean", geom = "line")
print(g3)
