composeCompanies <- function(dataPath="/../data/companies/")
{
  #' Compose the database for Companies
  #'
  #' @param dataPath: path of raw data
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr mutate distinct select pull filter group_by ungroup arrange rename sym lag
  #' @importFrom tidyr na_replace
  #' @importFrom stringr str_replace str_remove_all str_remove
  #'
  #' @return The Companies dataset
  #' @export
  #'

  filesToRead <- list.files(path = paste0(getwd(), dataPath), pattern="*.csv")

  companies <- Reduce(rbind, lapply(filesToRead,
                                       FUN=function(x) {
                                         read.csv(file.path(paste0(getwd(), dataPath), x))
                                       })) %>%
    magrittr::set_colnames(c("Organisation_Name",
                           "URL",
                           "Headquarters_Location",
                           "Industries",
                           "Industry_Groups",
                           "Headquarters_Regions",
                           "Founded_Date",
                           "Founded_Date_Precision",
                           "Number_of_Founders",
                           "Founders",
                           "Number_of_Employees",
                           "Number_of_Funding_Rounds",
                           "Funding_Status",
                           "Total_Equity_Funding_Amount",
                           "Total_Equity_Funding_Amount_Currency",
                           "Total_Equity_Funding_Amount_Currency_in_USD",
                           "Total_Funding_Amount",
                           "Total_Funding_Amount_Currency",
                           "Total_Funding_Amount_Currency_in_USD",
                           "Number_of_Lead_Investors",
                           "Number_of_Investors",
                           "Active_Tech_Count",
                           "Total_Products_Active",
                           "Patents_Granted",
                           "Trademarks_Registered",
                           "Estimated_Revenue"
                           )) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(Total_Funding_Amount_Currency_in_USD)) %>%
    dplyr::mutate(Founders = stringr::str_remove_all(Founders, ", PhD"),
                  Founders = stringr::str_remove_all(Founders, ", Ph.D."),
                  Founders = stringr::str_remove_all(Founders, ", MD, MPH, MTM"),
                  Active_Tech_Count = tidyr::replace_na(Active_Tech_Count, 0),
                  Total_Products_Active = tidyr::replace_na(Total_Products_Active, 0),
                  Patents_Granted = stringr::str_replace(Patents_Granted, ",", ""),
                  Patents_Granted = tidyr::replace_na(Patents_Granted, 0),
                  Patents_Granted = ifelse(Patents_Granted=="", 0, Patents_Granted),
                  Patents_Granted = as.numeric(Patents_Granted),
                  Number_of_Employees = ifelse(Number_of_Employees=="", "1-10", Number_of_Employees),
                  Trademarks_Registered = tidyr::replace_na(Trademarks_Registered, 0),
                  Number_of_Lead_Investors = tidyr::replace_na(Number_of_Lead_Investors, 0),
                  Number_of_Investors = tidyr::replace_na(Number_of_Investors, 0),
                  #Funding_First5 = ifelse(Total_Funding_Amount_Currency_in_USD <= getPercentile(., "Total_Funding_Amount_Currency_in_USD", 0.00),
                  #                         1, 0),
                  #Funding_First10 = ifelse(Total_Funding_Amount_Currency_in_USD <= getPercentile(., "Total_Funding_Amount_Currency_in_USD", 0.10),
                  #                 1, 0),
                  Funding_Top10 = ifelse(Total_Funding_Amount_Currency_in_USD >= getPercentile(., "Total_Funding_Amount_Currency_in_USD", 0.90),
                                  1, 0),
                  Funding_Top5 = ifelse(Total_Funding_Amount_Currency_in_USD >= getPercentile(., "Total_Funding_Amount_Currency_in_USD", 0.95),
                                          1, 0),
                  Total_Equity_Funding_Amount_Currency_in_USD = ifelse(is.na(Total_Equity_Funding_Amount_Currency_in_USD),
                                                                       median(log(Total_Equity_Funding_Amount_Currency_in_USD), na.rm=TRUE),
                                                                       log(Total_Equity_Funding_Amount_Currency_in_USD)),
                  Total_Funding_Amount_Currency_in_USD = ifelse(is.na(Total_Funding_Amount_Currency_in_USD),
                                                         median(log(Total_Funding_Amount_Currency_in_USD), na.rm=TRUE),
                                                         log(Total_Funding_Amount_Currency_in_USD))
                  ) %>%
    rowwise() %>%
    dplyr::mutate(Geographic_Region = locateHeadquarter(Headquarters_Location),
                  Company_Age = as.numeric(format(Sys.time(), "%Y")) -
                    as.numeric(stringr::str_sub(Founded_Date, start = 1, end = 4))) %>%
    dplyr::select(-URL,
                  -Funding_Status,
                  -Founded_Date_Precision,
                  -Headquarters_Regions,
                  -Headquarters_Location,
                  -Total_Equity_Funding_Amount,
                  -Total_Equity_Funding_Amount_Currency,
                  -Total_Funding_Amount,
                  -Total_Funding_Amount_Currency,
                  -Founded_Date
                  )
}
