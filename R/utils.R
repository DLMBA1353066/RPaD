locateHeadquarter <- function(column)
{
  if(stringr::str_detect(column, stringr::regex("United States|Canada")))
    return (1) #("United_States")
  else if(stringr::str_detect(column, stringr::regex("Gibraltar|Malta|Serbia|Moldova|Bosnia|Turkey|Belarus|Luxembourg|Ukraine|Slovenia|Croatia|Hungary|Czech Republic|Slovakia|Iceland|Estonia|Lithuania|Greece|Latvia|Portugal|Cyprus|Bulgaria|Romania|Denmark|Finland|Spain|Austria|Poland|Switzerland|Belgium|The Netherlands|Norway|Russian Federation|Italy|Germany|Ireland|United Kingdom|France|Sweden")))
          return (2) #("Europe")
       else return (3) #if(stringr::str_detect(column, stringr::regex("Mongolia|Vietnam|Bangladesh|Pakistan|Malaysia|Philippines|Thailand|Indonesia|Taiwan|Hong Kong|Japan|China|South Korea|Australia|Singapore|India|New Zealand")))
       #       return (3) #("Asia_Pacific")
       #     else if(stringr::str_detect(column, stringr::regex("El Salvador|Cayman Islands|Puerto Rico|Costa Rica|Venezuela|Panama|Ecuador|Peru|Argentina|Mexico|Chile|Brazil|Colombia")))
       #            return (4) #("Central_South_America")
       #          else if(stringr::str_detect(column, stringr::regex("Tunisia|Tanzania|Egypt|Uganda|Zimbabwe|Morocco|Ghana|Nigeria|South Africa|Ethiopia|Kenya")))
       #                 return (5) #("Africa")
       #               else if(stringr::str_detect(column, stringr::regex("Bahrain|United Arab Emirates|Saudi Arabia")))
       #                       return (6) #("Gulf_Cooperation_Council_(GCC)")
       #                    else if(stringr::str_detect(column, stringr::regex("Kazakhstan|Armenia|Jordan|Israel|Lebanon|Palestinian")))
       #                         return (7) #("Western_Asia")
       #                         else return (8) #("Other")

}

groupSector <- function(column, regex)
{
  ifelse(stringr::str_detect(column, regex), 1, 0)
}

addIndustries <- function(.data)
{
  .data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Industry_Healthcare = groupSector(Industry_Groups, stringr::regex("Health Care")),
                  Industry_FoodAndBeverages = groupSector(Industry_Groups, stringr::regex("Food and Beverage")),
                  Industry_Energy = groupSector(Industry_Groups, stringr::regex("Natural Resources|Energy")),
                  Industry_Marketing = groupSector(Industry_Groups, stringr::regex("Sales and Marketing|Content and Publishing|Advertising")),
                  Industry_ConsumerGS = groupSector(Industry_Groups, stringr::regex("Clothing and Apparel|Consumer Electronics|Consumer Goods")),
                  Industry_ProfessionalGS = groupSector(Industry_Groups, stringr::regex("Professional Services|Administrative Services")),
                  Industry_Travel = groupSector(Industry_Groups, stringr::regex("Travel and Tourism")),
                  Industry_Security = groupSector(Industry_Groups, stringr::regex("Government and Military|Privacy and Security")),
                  Industry_Software = groupSector(Industry_Groups, stringr::regex("Software|Apps")),
                  Industry_Transportation = groupSector(Industry_Groups, stringr::regex("Transportation")),
                  Industry_Internet = groupSector(Industry_Groups, stringr::regex("Internet Services|Navigation and Mapping|Mobile|Platforms")),
                  Industry_ComputerHS = groupSector(Industry_Groups, stringr::regex("Hardware|Information Technology")),
                  Industry_Financial = groupSector(Industry_Groups, stringr::regex("Lending and Investments|Payments|Financial Services")),
                  Industry_Industrial = groupSector(Industry_Groups, stringr::regex("Manufacturing|Science and Engineering|Biotechnology")),
                  #Industry_Analytics = groupSector(Industry_Groups, stringr::regex("Data and Analytics")),
                  Industry_Others = groupSector(Industry_Groups, stringr::regex("Other|Real Estate|Agriculture and Farming|Music and Audio|Gaming|Sports|Education|Video|Events|Media and Entertainment|Design|Commerce and Shopping|Sustainability|Community and Lifestyle|Messaging and Telecommunications")),
                  #Sum = sum(c_across(Industry_Healthcare:Industry_Others))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Industries,
           -Industry_Groups)
}

mapNOE <- function(column)
{
  if(column=="1-10") return (1)
  else if(column=="11-50") return(2)
       else return (3) #if(column=="51-100") return(3)
            #else if(column=="101-250") return(4)
            #     else if(column=="251-500") return(5)
            #          else if(column=="501-1000") return(6)
            #               else if(column=="1001-5000") return(7)
            #                    else if(column=="5001-10000") return(8)
            #                         else if(column=="10001+") return(9)
}

mapRevenue <- function(column)
{
  if(column=="Less than $1M") return (1)
  else if(column=="$1M to $10M") return(2)
       else return (3)
}

getPercentile <- function(dataset, col, perc)
{
  as.numeric(quantile(dataset %>% dplyr::pull(col), perc, na.rm = T))
}

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}
