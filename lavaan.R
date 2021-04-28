library(lavaan)

cor(fullDataset[-c(1355, 1412, 5400, 4775,
                   5404, 5858, 5864, 6741, 6924),
                c("EE1", "EE2", "EE3", "EE4", "EE5", "EE6", "EE7")])

cor(fullDataset[-c(1355, 1412, 5400, 4775,
                   5404, 5858, 5864, 6741, 6924),
                c("FR1", "FR2", "FR3", "FR4", "FR5", "FR6")])

cor(fullDataset[-c(1355, 1412, 5400, 4775,
                   5404, 5858, 5864, 6741, 6924),
                c("OP1", "OP2", "OP3", "OP4")])


bestModel <- ' Experience =~ EE1 + EE2 + EE3 + EE4
Resources =~ FR1 + FR2 + FR3 + FR4
Performance =~ OP1 + OP2 + OP3 + OP4 '
fitBest <- lavaan::lavaan(bestModel, scale(fullDataset),
              auto.var=TRUE,
              auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
x1 <- summary(fitBest, fit.measures=TRUE )
fitMeasures(fitBest)

n <- nrow(fullDataset)
fit <- lavaan::cfa(bestModel, scale(fullDataset))
Tm <- lavaan::fitMeasures(fit)[['chisq']]
DFm <- lavaan::fitMeasures(fit)['df']
sqrt((Tm - DFm)/(n * DFm))


lambda_c <- 0.05^2 * n * DFm
pchisq(Tm, DFm, lambda_c, lower.tail = FALSE)
lavaan::fitMeasures(fit)['rmsea.pvalue']

lambda_c <- 0.08^2 * n * DFm
pchisq(Tm, DFm, lambda_c, lower.tail = TRUE)

Tb <- lavaan::fitMeasures(fit)['baseline.chisq']
Tb
DFb <- lavaan::fitMeasures(fit)['baseline.df']
DFb

#semTools::findRMSEAsamplesize(rmsea0=.06, rmseaA=.1, df=51, power=.999)
