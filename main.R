library(dplyr)

#TODO 100K son tanti, 50K per il futuro
#TODO

IndustriesList <-c("Industry_Healthcare",
"Industry_FoodAndBeverages",
"Industry_Energy",
"Industry_Marketing",
"Industry_ConsumerGS",
"Industry_ProfessionalGS",
"Industry_Travel",
"Industry_Security",
"Industry_Software",
"Industry_Transportation",
"Industry_Internet",
"Industry_ComputerHS",
"Industry_Financial",
"Industry_Industrial",
"Industry_Others")

people <- readFounders()

companies <- composeCompanies()

fullDataset <- matchCompaniesFounders(companies, people) %>%
  addIndustries() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(NOE = mapNOE(Number_of_Employees),
                RevenueRange = mapRevenue(Estimated_Revenue)
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(#Entrepreneurial Experience
                #"Organisation_Name",
                "Number_of_Portfolio_Companies",
                "Number_of_Founded_Organizations",
                #"Number_of_Partner_Investments",
                "Number_of_Exits",
                "Number_of_Investments",
                #"Number_of_Exits_IPO",
                #"Number_of_Lead_Investments",
                #"Number_of_Founders",
                #Fund Raised
                #"Total_Equity_Funding_Amount_Currency_in_USD",
                "Total_Funding_Amount_Currency_in_USD",
                "Funding_Top10",
                "Funding_Top5",
                "Number_of_Investors",
                #"Number_of_Lead_Investors",
                #"Number_of_Funding_Rounds",
                #Operational Performance
                "Trademarks_Registered",
                "Patents_Granted",
                "Total_Products_Active",
                "Active_Tech_Count",
                #"RevenueRange",
                dplyr::starts_with("Industry"),
                "NOE",
                "Geographic_Region",
                "Company_Age"
                ) %>%
  magrittr::set_colnames(c(paste0("EE", seq(1,4,1)),
                           paste0("FR", seq(1,4,1)),
                           paste0("OP", seq(1,4,1)),
                           paste0("IN", seq(1,15,1)),
                           "NOE",
                           "GEO",
                           "AGE"
  )) %>%
  dplyr::filter(#EE1 < 36 &
                #EE2 < 16 &
                #EE3 < 27 &
                #EE4 < 43 &
                #FR4 < 44 &
                OP1 < 116 &
                OP2 < 334 &
                OP3 < 80 &
                OP4 < 160
  )

# Step1: OP1 < 116 - OP2 < 334 - OP4 < 160
# Step2: OP3 < 80
# Step3:

#### CFA ####

measurements <- seminr::constructs(
  seminr::composite("Experience", seminr::multi_items("EE", 1:4)),
  seminr::composite("Resources", seminr::multi_items("FR", 1:4)),
  seminr::composite("Performance", seminr::multi_items("OP", 1:4))
)

covaries <- seminr::associations(
  seminr::item_errors("EE1", "EE2"),
  seminr::item_errors("EE1", "EE3"),
  seminr::item_errors("EE1", "EE4"),
  seminr::item_errors("EE2", "EE3"),
  #seminr::item_errors("EE2", "EE4"),
  seminr::item_errors("EE3", "EE4"),
  seminr::item_errors("FR1", "FR2"),
  seminr::item_errors("FR1", "FR3"),
  seminr::item_errors("FR1", "FR4"),
  #seminr::item_errors("FR2", "FR3"),
  #seminr::item_errors("FR2", "FR4"),
  seminr::item_errors("FR3", "FR4"),
  seminr::item_errors("OP1", "OP2"),
  seminr::item_errors("OP1", "OP3"),
  seminr::item_errors("OP1", "OP4"),
  seminr::item_errors("OP2", "OP3")
  #seminr::item_errors("OP2", "OP4")
  #seminr::item_errors("OP3", "OP4"),
  )

cfa_modelMLR <- seminr::estimate_cfa(data = scale(fullDataset[,c(1:12)]),
                                  seminr::as.reflective(measurements),
                                  estimator="MLR",
                                  item_associations = covaries
                                 )
summary(cfa_modelMLR)

#HS.model <- ' Experience  =~ EE1 + EE2 + EE3 + EE4
#              Resources =~ FR1 + FR2 + FR3 + FR4
#              Performance   =~ OP1 + OP2 + OP3 + OP4 '

#semTools::htmt(HS.model, as.data.frame(scale(fullDataset[,1:12])))

#Fit metrics:
#  npar      fmin      pnfi      logl       aic       bic    ntotal      bic2       rmr      srmr      crmr
#3.30e+01  3.50e-01  7.59e-01 -1.29e+05  2.57e+05  2.57e+05  7.36e+03  2.57e+05  6.12e-02  6.12e-02  6.55e-02
#gfi      agfi      pgfi       mfi      ecvi
#9.03e-01  8.66e-01  6.55e-01  7.09e-01  7.09e-01

#sqrt((2883.877-51)/(51*7361))
#sqrt((4.69e+03-74)/(74*7361))


#### MODEL ####

measurements <- seminr::constructs(
  seminr::composite("Experience", seminr::multi_items("EE", 1:4)),
  seminr::composite("Resources", seminr::multi_items("FR", 1:4)),
  seminr::composite("Performance", seminr::multi_items("OP", 1:4)),
  seminr::composite(IndustriesList[1], seminr::single_item("IN1")),
  seminr::composite(IndustriesList[2], seminr::single_item("IN2")),
  seminr::composite(IndustriesList[3], seminr::single_item("IN3")),
  seminr::composite(IndustriesList[4], seminr::single_item("IN4")),
  seminr::composite(IndustriesList[5], seminr::single_item("IN5")),
  seminr::composite(IndustriesList[6], seminr::single_item("IN6")),
  seminr::composite(IndustriesList[7], seminr::single_item("IN7")),
  seminr::composite(IndustriesList[8], seminr::single_item("IN8")),
  seminr::composite(IndustriesList[9], seminr::single_item("IN9")),
  seminr::composite(IndustriesList[10], seminr::single_item("IN10")),
  seminr::composite(IndustriesList[11], seminr::single_item("IN11")),
  seminr::composite(IndustriesList[12], seminr::single_item("IN12")),
  seminr::composite(IndustriesList[13], seminr::single_item("IN13")),
  seminr::composite(IndustriesList[14], seminr::single_item("IN14")),
  seminr::composite(IndustriesList[15], seminr::single_item("IN15")),
  seminr::composite("AGE", seminr::single_item("AGE")),
  seminr::composite("GEO", seminr::single_item("GEO")),
  seminr::composite("NOE", seminr::single_item("NOE")),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[1]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[2]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[3]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[4]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[5]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[6]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[7]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[8]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[9]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[10]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[11]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[12]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[13]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[14]),
  seminr::interaction_term(iv = "Experience", moderator = IndustriesList[15])
)

structure <- seminr::relationships(
  seminr::paths(from = c("Experience",
                 paste("Experience", IndustriesList, sep="*")),
                to = "Resources"
                ),
  seminr::paths(from = "Experience", to = "Performance"),
  seminr::paths(from = c("Resources",
                 "AGE",
                 "GEO",
                 "NOE"
                ),
                 to = "Performance")
)

pls_model <- seminr::estimate_pls(data = fullDataset,
                                    measurement_model = measurements,
                                    structural_model = structure)
summary(pls_model)

#model_summary <- summary(pls_model)
#model_summary$descriptives$statistics$items
#model_summary$descriptives$correlations$items
#model_summary$descriptives$statistics$constructs
#model_summary$descriptives$correlations$constructs[c(1,17, 18, 19, 20, 21), c(1,17, 18, 19, 20, 21)]

#loads <- as.data.frame(pls_model$outer_loadings)

boot_estimates <- seminr::bootstrap_model(pls_model,
                                          nboot = 50000,
                                          cores = parallel::detectCores(),
                                          seed = 42)

seminr::confidence_interval(boot_estimates,
                            from = "Experience",
                            through = "Resources",
                            to = "Performance")

summary(boot_estimates)$bootstrapped_p
summary(boot_estimates)$bootstrapped_l

save.image("~/Desktop/Admin/MBA/Research Portfolio and Dissertation/R/data/RData/Model_50000_Sum.RData")

boot_res <- summary(boot_estimates)$bootstrapped_paths
boot_res <- cbind(boot_res, rep(0, nrow(boot_res)))
colnames(boot_res)[7] <- "p-value"
n <- nrow(fullDataset)- ncol(fullDataset) - 1
for(i in 1:nrow(boot_res))
{
  boot_res[i,7] <- round(2*(1-pt(q=abs(boot_res[i,4]), df=n)), 4)
}
round(boot_res[c(1:2, 18:21), ], 3)

round(boot_res[3:17, ], 3)

boot_res_load <- summary(boot_estimates)$bootstrapped_l
boot_res_load <- cbind(boot_res_load, rep(0, nrow(boot_res_load)))
colnames(boot_res_load)[7] <- "p-value"
n <- nrow(fullDataset)- ncol(fullDataset) - 1
for(i in 1:nrow(boot_res_load))
{
  boot_res_load[i,7] <- 2*(1-pt(q=abs(boot_res_load[i,4]), df=n))
}
round(boot_res_load[1:12, ], 3)

#cor(fullDataset[,c(1,2,3,4,5,6,7,8)])
#Model1 <- lm(FR1 ~ EE1 + EE2 + EE3 + EE4+ EE6 + EE7 + EE8, data=fullDataset)
#car::vif(Model1)
#alias(Model1)
#E5 is linear combination

#Model2 <- lm(OP1 ~ FR1 + FR2 + FR3 + FR4+ FR5 + FR6, data=fullDataset)
#car::vif(Model2)

longDataset<- fullDataset %>%
  #mutate(Group = ifelse(FR2==0, 0, ifelse(FR3==1, 2, 1))) %>%
  dplyr::select(FR2, FR3, OP1, OP2, OP3, OP4) %>%
  tidyr::pivot_longer(-c(FR2, FR3), names_to = "variables", values_to = "value")

longDataset<- fullDataset %>%
  mutate(Group = ifelse(FR2==0, 0, ifelse(FR3==1, 2, 1))) %>%
  dplyr::select(Group, OP1, OP2, OP3, OP4) %>%
  tidyr::pivot_longer(-Group, names_to = "variables", values_to = "value")

statTestTop <- longDataset  %>%
  #dplyr::filter(FR3==0) %>%
  dplyr::group_by(variables) %>%
  rstatix::t_test(value ~ Group) %>%
  rstatix::adjust_pvalue(method = "bonferroni") %>%
  rstatix::add_significance()

statTestTop10 <- longDataset  %>%
  #dplyr::filter(FR3==0) %>%
  dplyr::group_by(variables) %>%
  rstatix::t_test(value ~ FR2) %>%
  rstatix::adjust_pvalue(method = "bonferroni") %>%
  rstatix::add_significance()

statTestTop5 <- longDataset  %>%
  #dplyr::filter(FR2==0) %>%
  dplyr::group_by(variables) %>%
  rstatix::t_test(value ~ FR3) %>%
  rstatix::adjust_pvalue(method = "bonferroni") %>%
  rstatix::add_significance()
