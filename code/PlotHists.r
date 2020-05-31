#Plot some histograms of the distance data, pretty much the detection functions

library(ggplot2)
library(dplyr)
library(here)
library(Distance)

#Distance data from 2_PmEncData.rmd
dists <- sw
#can I overlay min max and best data to see what's going on?
#Look at differences between including/excluding 1641 given the larger errors

#Separate the peaks
distBestA <- filter(dists, type == 'best' & peak == 'A')
distMinA <- filter(dists, type == 'min' & peak == 'A')
distMaxA <- filter(dists, type == 'max' & peak == 'A')

distBestB <- filter(dists, type == 'best' & peak == 'B')
distMinB <- filter(dists, type == 'min' & peak == 'B')
distMaxB <- filter(dists, type == 'max' & peak == 'B')


#ALL PEAKS, with 1641
distBest0 <- filter(dists, type == 'best')
distMin0 <- filter(dists, type == 'min')
distMax0 <- filter(dists, type == 'max')

#ALL PEAKS, without 1641
distBest1 <- filter(dists, type == 'best', survey != 1641)
distMin1 <- filter(dists, type == 'min', survey != 1641)
distMax1 <- filter(dists, type == 'max', survey != 1641)


ggplot(dists, aes(x=pdist, fill=type)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 100) +
  scale_fill_manual(values = c("grey20", "grey60", "grey80"))  + 
  theme_bw()
  




#detection function from Distance

#Peak A only, With 1641
distBestA_dtfun <- ds(distBestA$pdist)
distMinA_dtfun <- ds(distMinA$pdist)
distMaxA_dtfun <- ds(distMaxA$pdist)
par(mfrow=c(1,3))
plot(distBestA_dtfun, main = "Best Distances, peak A")
plot(distMinA_dtfun, main = "Min Distances, peak A")
plot(distMaxA_dtfun, main = "Max Distances, peak A")


#Peak B only, With 1641
distBestB_dtfun <- ds(distBestB$pdist)
distMinB_dtfun <- ds(distMinB$pdist)
distMaxB_dtfun <- ds(distMaxB$pdist)
par(mfrow=c(1,3))
plot(distBestB_dtfun, main = "Best Distances, peak B")
plot(distMinB_dtfun, main = "Min Distances, peak B")
plot(distMaxB_dtfun, main = "Max Distances, peak B")


#All Peaks, With 1641
distBest0_dtfun <- ds(distBest0$pdist)
distMin0_dtfun <- ds(distMin0$pdist)
distMax0_dtfun <- ds(distMax0$pdist)
par(mfrow=c(1,3))
plot(distBest0_dtfun, main = "Best Distances (w/ 1641)")
plot(distMin0_dtfun, main = "Min Distances (w/ 1641)")
plot(distMax0_dtfun, main = "Max Distances (w/ 1641)")


#All peaks, Without 1641
distBest1_dtfun <- ds(distBest1$pdist)
distMin1_dtfun <- ds(distMin1$pdist)
distMax1_dtfun <- ds(distMax1$pdist)
par(mfrow=c(1,3))
plot(distBest1_dtfun, main = "Best Distances (w/o 1641)")
plot(distMin1_dtfun, main = "Min Distances (w/o 1641)")
plot(distMax1_dtfun, main = "Max Distances (w/o 1641)")

par(mfrow=c(1,1))
plot(distBest1_dtfun, main = "Best Distances (w/o 1641)")
plot(distBest0_dtfun, main = "Best Distances (w/ 1641)")


########## Error Distribution #############

# Load data from matlab Step 5. This is from Len's suggestion of modeling the error in loc estimates.

SwDists <- read.csv(here('data', 'SpermieDistances_distributionALL.csv'), header = FALSE)
# SwDists <-SwDists[-c(56,58),]
# SwDists <-SwDists[-c(56,58),]

colnames(SwDists) <- c('survey', 'acid', 'mean', 'std', 'cv')

mdl <- lm(std ~ mean, data=SwDists)
pval = round(summary(mdl)$coefficients[2,4],5)
r2 = summary(mdl)$adj.r.squared
#plot linear model with 95% confidence intervals
plotOrder <- order(SwDists$mean)
cint <- as.data.frame(predict(mdl, interval = "confidence"))

plot(SwDists$mean, SwDists$std, xlab = 'Mean (m)', ylab='Standard Deviation (m)', pch = 19) + abline(lm(std ~ mean, data=SwDists), col = 'blue', lwd=2) 
lines(SwDists$mean[plotOrder], cint$lwr[plotOrder], col='blue', lwd=1, lty=2) # plot lower interval
lines(SwDists$mean[plotOrder], cint$upr[plotOrder], col='blue', lwd=1, lty=2) 
SwRP <- vector('expression', 2)
SwRP[1] <- substitute(expression(italic(R)^2 == MYVALUE),
                      list(MYVALUE = format(r2, digits =3)))[2]
SwRP[2] <- substitute(expression(italic(p) == MYOTHERVALUE),
                       list(MYOTHERVALUE = format(pval, digits =5)))[2]
legend('topleft', legend = SwRP, bty = 'n')
