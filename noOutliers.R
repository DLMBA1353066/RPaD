for(col in colnames(fullDataset))
{
  col="FR4"
  print(fullDataset %>%
          pull(col) %>%
          EnvStats::rosnerTest(k = 10))

}

noOutliers <- fullDataset %>%
                dplyr::filter(#EE1 < 36 &
                              #EE2 < 16 &
                              #EE3 < 27 &
                              #EE4 < 20 & #31 peggiora 8 peggiora
                              #FR4 < 44 &
                              OP1 < 116 &
                              OP2 < 334 &
                              OP3 < 80 &
                              OP4 < 160
                              )

measurements <- seminr::constructs(
  seminr::composite("Experience", seminr::multi_items("EE", 1:4)),
  seminr::composite("Resources", seminr::multi_items("FR", 1:4)),
  seminr::composite("Performance", seminr::multi_items("OP", 1:4))
)

#cfa_modelDWLS <- seminr::estimate_cfa(data = scale(noOutliers),
#                                  seminr::as.reflective(measurements),
#                                  estimator="DWLS") #
cfa_modelMLR_NoOut <- seminr::estimate_cfa(data = scale(noOutliers[,1:12]),
                                     seminr::as.reflective(measurements),
                                     estimator="MLR")
#summary(cfa_modelDWLS)
summary(cfa_modelMLR_NoOut)

pls_model_NoOut <- seminr::estimate_pls(data = noOutliers,
                                  measurement_model = measurements,
                                  structural_model = structure)
summary(pls_model_NoOut)

boot_estimates_NoOut <- seminr::bootstrap_model(pls_model_NoOut,
                                          nboot = 1000,
                                          cores = parallel::detectCores(),
                                          seed = 42)

seminr::confidence_interval(boot_estimates_NoOut,
                            from = "Experience",
                            through = "Resources",
                            to = "Performance")

summary(boot_estimates_NoOut)$bootstrapped_p
summary(boot_estimates_NoOut)$bootstrapped_l


x1 <- as.data.frame(noOutliers)

tentClus <- kmeans(x1[,1:12], centers=15, iter.max = 50)

noOutlies <- noOutliers[tentClus$cluster!=2,]
noOutliers_bk <- noOutliers

x1 <- lm(OP1 ~ EE1 + EE2 + EE3 + EE4, fullDataset)
x2 <- lm(OP2 ~ EE1 + EE2 + EE3 + EE4, fullDataset)
x3 <- lm(OP3 ~ EE1 + EE2 + EE3 + EE4, fullDataset)
x4 <- lm(OP4 ~ EE1 + EE2 + EE3 + EE4, fullDataset)


base::plot(x1[,"EE1"], x1[,"OP1"], type="p")
base::plot(x1[,"EE1"], x1[,"OP2"], type="p")
base::plot(x1[,"EE1"], x1[,"OP3"], type="p")
base::plot(x1[,"EE1"], x1[,"OP4"], type="p")

base::plot(x1[,"EE2"], x1[,"OP1"], type="p")
base::plot(x1[,"EE2"], x1[,"OP2"], type="p")
base::plot(x1[,"EE2"], x1[,"OP3"], type="p")
base::plot(x1[,"EE2"], x1[,"OP4"], type="p")

base::plot(x1[,"EE3"], x1[,"OP1"], type="p")
base::plot(x1[,"EE3"], x1[,"OP2"], type="p")
base::plot(x1[,"EE3"], x1[,"OP3"], type="p")
base::plot(x1[,"EE3"], x1[,"OP4"], type="p")

base::plot(x1[,"EE4"], x1[,"OP1"], type="p")
base::plot(x1[,"EE4"], x1[,"OP2"], type="p")
base::plot(x1[,"EE4"], x1[,"OP3"], type="p")
base::plot(x1[,"EE4"], x1[,"OP4"], type="p")
