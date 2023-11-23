library(dplyr)
library(tidyverse)
library(janitor)
library(magrittr)
library(lubridate)
library(readr)
library(stringr)
require("data.table")
require("bit64")
library(broom)
library(nlme)
library(jtools)
library(plyr)
library(DBI)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(corrplot)
library(ggplot2)

# RStudioView <- View
# View <- function(x) {
#   name  <- deparse(substitute(x))
#   if ("data.frame" %in% class(x)) { 
#     RStudioView(x[1:300000,], name)
#   } else { 
#     RStudioView(x) 
#   }
# }
# 
# main_data <- data.frame()
# data_file_name <- "pu2018.csv"
# weight_file_name <- "rw2018.csv"
# numbering_pattern <- "\\d+"
# year_values <- numeric()
# 
# 
# 
# for (i in 2018:2022)
# {    
#     ds <- c(data_file_name)
#     pu <- fread(ds, sep = "|", select = c(
#     
#     #Common case identification variables
#     'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
#     
#     #Area of Residency Identifier
#     'TEHC_ST', 'RREGION_INTV', 'TST_INTV', 'TMETRO_INTV', 
#     
#     #The base weight
#     'WPFINWGT',
#     
#     #Common demographics variables, including age at time of interview (TAGE)
#     #	and monthly age during the reference period (TAGE_EHC)
#     'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC',
#     
#     ##Asset
#     ###Individual
#     ####Income_earned
#     'TINC_BANK', 'TINC_BOND', 'TINC_STMF', 'TINC_RENT', 'TINC_OTH', 'TINC_AST',
#     ####Total_Value
#     'TVAL_BANK', 'TVAL_BOND', 'TVAL_STMF', 'TVAL_RENT', 'TDEBT_RENT', 'TEQ_RENT', 'TVAL_RE', 'TDEBT_RE', 'TEQ_RE', 'TVAL_OTH', 
#     'TVAL_RET', 'TDEBT_BUS', 'TEQ_BUS', 'TEQ_VEH', 'TNETWORTH', 'TVAL_ESAV', 'TVAL_VEH', 'TDEBT_VEH', 'TVAL_HOME', 
#     'TDEBT_HOME', 'TEQ_HOME', 'TDEBT_USEC','TDEBT_SEC', 'TDEBT_AST', 'TVAL_AST',
#     
#     ####Asset Ownership
#     ####Investing
#     ####Savings
#     ####Debt
#     
#     
#     ###Household
#     ###Income_earned
#     'THINC_BANK', 'THINC_BOND','THINC_STMF', 'THINC_RENT','THINC_OTH','THINC_AST',
#     ###
#     'THVAL_BOND','THVAL_STMF', 'THEQ_BUS', 'THDEBT_BUS', 'THVAL_BUS', 'THVAL_ESAV','THVAL_VEH',  'THDEBT_VEH', 'THVAL_HOME', 
#     'THVAL_RENT','THDEBT_RENT','THEQ_RENT','THVAL_RE','THDEBT_RE', 'THEQ_RE', 'THVAL_OTH', 'THDEBT_HOME', 
#     'THEQ_HOME', 'THDEBT_CC', 'THDEBT_ED', 'THDEBT_OT',  
#     'THDEBT_USEC', 'THDEBT_SEC', 'THDEBT_AST', 'THVAL_AST', 'THNETWORTH', 
#     'THVAL_RMU', 'THEQ_VEH','THVAL_BANK', 'THVAL_RET',
#     
#     #Annuities
#     'EOWN_ANN', 'EOWN_TR', 'TANNVAL', 'EOWN_TREQ', 'TTRVAL',
#     
#     #Big dummies
#     'EOWN_IRAKEO', 'EOWN_THR401', 'EOWN_PENSION', 'EOWN_GOVS', 'EOWN_CHK', 'EOWN_SAV', 'EOWN_MM', 'EOWN_CD', 'EOWN_MF', 'EOWN_ST', 
#     'EOWN_MCBD', 'EOWN_LIFE', 'EOWN_RP', 'EOWN_RE', 'EOWN_ANNTR', 'EOWN_BSI', 'EOWN_OINV',
#     
#     #Type of Stock and mutual funds
#     'EJSMFINCTYPE', 'EJSSTINCTYPE', 'EJOMFINCTYPE', 'EJOSTINCTYPE', 'EOMFINCTYPE', 'EOSTINCTYPE',
#     
#     #Stock and mutual funds ownership status
#     'EJSOWNMF', 'EJSOWNST', 'EJOOWNMF', 'EJOOWNST', 'EOOWNMF', 'EOOWNST', 
#     
#     #Mutual funds data
#     'TJSMFVAL', 'TJOMFVAL', 'TOMFVAL', 
#     #Stock data
#     'TJSSTVAL', 'TJOSTVAL', 'TOSTVAL', 
#     
#     #Joint student loan
#     'EJSEDDEBT', 'TJSEDDEBTVAL',
#     
#     #Joint Credit Card debt
#     'EJSCCDEBT', 'TJSCCDEBTVAL',
#     #Self credit card
#     'EOCCDEBT', 'TOCCDEBTVAL',
#     
#     #Joint bank account(Dummy)
#     ##For one spouces
#     'EJSOWNGOVS',  #Gov securities 
#     'EJSOWNCHK',   #Checking account
#     'EJSOWNSAV',   #Saving account
#     'EJSOWNMM',    #Money market deposit account
#     'EJSOWNCD',    #CD
#     'EJSOWNMCBD',  #Municipal and corporate bonds
#     ##Without spouces
#     'EJOOWNGOVS', 'EJOOWNCHK', 'EJOOWNSAV', 'EJOOWNMM', 'EJOOWNCD', 'EJOOWNMCBD',
#     ##Individual
#     'EOOWNGOVS', 'EOOWNCHK', 'EOOWNSAV', 'EOOWNMM', 'EOOWNCD', 'EOOWNMCBD', 
#     
#     #Education saving account
#     #Household
#     'EOWN_ESAV', 'TESAV1VAL', 'TESAV2VAL', 'TESAV3VAL',
#     #Self
#     'EOEDDEBT', 'TOEDDEBTVAL',
#     
#     #Retirement account
#     'TSCNTAMT_401', 'TECNTAMT_401', 'TSCNTAMT_PEN', 'TECNTAMT', 'TSCNTAMT', 'TCNTAMT', 
#     
#     #Recreational Vehicle
#     'TMCYCVAL', 'TBOATVAL', 'TRVVAL', 'TORECVAL', 
#     
#     #Property Loan
#     'TPRLOANAMT', 'TPRLOAN1YRS', 'TPRLOAN2YRS', 
#     
#     #Property
#     'TPRVAL', 'EPRDEBT', 'TPRLOAN_NUM', 'TRENTMORT',
#     
#     #Vehicle
#     'TVEH1_YEAR', 'TVEH1VAL', 'TVEH2_YEAR', 'TVEH2VAL', 'TVEH3_YEAR', 'TVEH3VAL', 'TVEH_NUM',
#     
#     #Vehicle debt
#     'EVEH1DEBT', 'TVEH1DEBTVAL', 'EVEH2DEBT', 'TVEH2DEBTVAL', 'EVEH3DEBT', 'TVEH3DEBTVAL',
#     
#     #Business owned
#     ##Owned as job
#     'EBSJ1PEROWN', 'TBSJ1VAL', 'TBSJ1DEBTVAL',
#     'EBSJ2PEROWN', 'TBSJ2VAL', 'TBSJ2DEBTVAL',
#     'EBSJ3PEROWN', 'TBSJ3VAL', 'TBSJ3DEBTVAL',
#     'EBSJ4PEROWN', 'TBSJ4VAL', 'TBSJ4DEBTVAL',
#     ##Owned as investment
#     'TBUS_INV_NUM',
#     'EBSI1PEROWN', 'TBSI1VAL', 'TBSI1DEBTVAL',
#     'EBSI2PEROWN', 'TBSI2VAL', 'TBSI2DEBTVAL',
#     'EBSI3PEROWN', 'TBSI3VAL', 'TBSI3DEBTVAL',
#     
#     #Other Debt_self
#     'EOOTDEBT', 'TOOTDEBTVAL',
#     
#     #(Special case) mobile home
#     'TMHVAL', 'EMHDEBT', 'TMHLOAN_NUM', 'TMHLOANAMT', 'EMHLOAN1SITE', 'EMHLOAN2SITE', 
#     
#     #Life insurance
#     'TLIFE_FVAL', 'ELIFE_TYPE', 'TLIFE_CVAL', 
#     
#     #Joint rental property
#     'EJSOWNRP', 'TJSRPVAL', 'EJOOWNRP','TJORPVAL', 'EOOWNRP', 'TORPVAL',
#     
#     #Joint real estate(Dummy)
#     'EJSOWNRE', 'EJOOWNRE', 'EOOWNRE', 
#     
#     #Joint other debt
#     'EJSOTDEBT', 'TJSOTDEBTVAL',
#     
#     #Debt values
#     'TDEBT_CC','TDEBT_ED' ,'EDEBT_MED', 'TMED_AMT', 'TDEBT_OT', 'TUTILS',
#     
#     #Industry
#     'TJB1_IND', 'TJB2_IND','TJB3_IND', 'TJB4_IND', 'TJB5_IND', 'TJB6_IND',
#     
#     #Labor Union Status
#     "EJB1_UNION", "EJB2_UNION", "EJB3_UNION", "EJB4_UNION", "EJB5_UNION", "EJB6_UNION", 
#     
#     #Business
#     "TVAL_BUS",
#     
#     #RMU
#     "TVAL_RMU"))
#     
#     
# 
# 
#     names(pu) <- toupper(names(pu))
#     head(pu, 20)
#     
#     
#     
#     dw <- c(weight_file_name)
#     rw <- fread(dw, sep = "|")
#     
#     names(rw) <- toupper(names(rw))
#     
#     head(rw, 20)
#     
#     data <- inner_join(pu, rw, by = c("SSUID","PNUM","MONTHCODE"))
#     
#     num_match <- str_extract(data_file_name, numbering_pattern)
#     
#     number <- as.numeric(num_match)
#     
#     number <- number - 1
#     
#     data$year <- number
# 
#     main_data <- rbind(main_data, data, fill=TRUE)
#     
#     
#     
#     if (as.numeric(num_match) != -1) {
#       num <- as.numeric(num_match)
#       num <- num + 1
#       data_file_name <- gsub(numbering_pattern, num, data_file_name)
#       weight_file_name <- gsub(numbering_pattern, num, weight_file_name)
#     }
# }
# #WARNING: ERROR GENERATED! REMOVE THAT COLUMN!!
# main_data <- subset(main_data, !(year == 1))


mydb <- dbConnect(RSQLite::SQLite(), "research-db.sqlite")
# dbWriteTable(mydb, "sipp_main", main_data)
# dbListTables(mydb)
# dbDisconnect(mydb)


main_data <- data.frame(dbGetQuery(mydb, "SELECT * FROM sipp_main"))

dbDisconnect(mydb)
# 
# view(head(rw, 100))
# 
# data_first500 <- head(data, 500)


main_data[["TVAL_BANK"]]

#Calculate the percentage of the income distribution to different segment
##EJBx_UNION(1<=x<=7)
##Remove people residing in foreign country
main_data <- main_data[main_data$TEHC_ST != "61", ]


length(main_data[["TVAL_AST"]])
sum((main_data[["TDEBT_AST"]]), na.rm = TRUE)

##RMU processing
main_data[["TVAL_RMU"]]
main_data[["TVAL_RMU"]][is.na(main_data[["TVAL_RMU"]])] <- 0
main_data[["RMU_prop_to_asset"]] <- main_data[["TVAL_RMU"]]/main_data[["TVAL_AST"]]
main_data[["RMU_prop_to_asset"]]
main_data[["RMU_prop_to_asset"]][is.nan(main_data[["RMU_prop_to_asset"]]) | main_data[["RMU_prop_to_asset"]]=="Inf"] = 0
main_data[["RMU_prop_to_asset"]]

main_data[["overall_leveraged_ratio"]] <- main_data[["TDEBT_AST"]]/main_data[["TVAL_AST"]]
main_data[["overall_leveraged_ratio"]][is.nan(main_data[["overall_leveraged_ratio"]]) | main_data[["overall_leveraged_ratio"]]=="Inf"] = 0
main_data[["overall_leveraged_ratio"]]


Ast_list <- c("TVAL_BANK",
               "TVAL_STMF", "TVAL_BOND", "TVAL_RENT", "TVAL_RE",
               "TVAL_OTH", "TVAL_RET", "TVAL_BUS", "TVAL_HOME",
               "TVAL_VEH", "TVAL_ESAV")

main_data[["TVAL_BANK"]]

for (j in Ast_list)
{
  main_data[[j]][is.na(main_data[[j]])] <- 0
  print(length(main_data[[j]]))
  main_data[[paste0(j, "_proportion")]] <- main_data[[j]]/main_data[["TVAL_AST"]]
  main_data[[paste0(j, "_proportion")]][is.nan(main_data[[paste0(j, "_proportion")]]) | 
                                          main_data[[paste0(j, "_proportion")]]=="Inf" | 
                                          is.na(main_data[[paste0(j, "_proportion")]]) |
                                          main_data[[paste0(j, "_proportion")]]=="-Inf"] = 0
}
main_data[["TVAL_BANK"]]


##Log
for (j in Ast_list)
{
  main_data[[paste0(j, "_holder")]] = main_data[[j]]
  main_data[[paste0(j, "_holder")]][main_data[[paste0(j, "_holder")]] == 0] <- 0
  print(length(main_data[[j]]))
  main_data[[paste0(j, "_log")]] <- log(main_data[[paste0(j, "_holder")]])
  main_data[[paste0(j, "_log")]][is.na(main_data[[paste0(j, "_log")]]) |
                                   is.nan(main_data[[paste0(j, "_log")]]) | 
                                   main_data[[paste0(j, "_log")]]== "-Inf" |
                                   main_data[[paste0(j, "_log")]]=="Inf"] <- 0
}

main_data[["TVAL_BANK"]]
main_data[["TVAL_BANK_log"]]


Debt_list <- c("TDEBT_HOME", "TDEBT_VEH", "TDEBT_RENT", "TDEBT_RE", 
               "TDEBT_BUS", "TDEBT_CC", "TDEBT_ED", "TDEBT_OT", "TMED_AMT", "overall_leveraged_ratio")

for (i in Debt_list)
{
  print(i)
  main_data[[i]][is.na(main_data[[i]])] <- 0
  print(length(main_data[[i]]))
  main_data[[paste0(i, "_proportion")]] <- main_data[[i]]/main_data[["TDEBT_AST"]]
  main_data[[paste0(j, "_proportion")]][is.nan(main_data[[paste0(j, "_proportion")]]) | 
                                          main_data[[paste0(j, "_proportion")]]=="Inf" | 
                                          is.na(main_data[[paste0(j, "_proportion")]]) |
                                          main_data[[paste0(j, "_proportion")]]=="-Inf"]
}

##Log
for (j in Debt_list)
{
  main_data[[paste0(j, "_holder")]] = main_data[[j]]
  main_data[[paste0(j, "_holder")]][main_data[[paste0(j, "_holder")]] == 0] <- 0
  print(length(main_data[[j]]))
  main_data[[paste0(j, "_log")]] <- log(main_data[[paste0(j, "_holder")]])
  main_data[[paste0(j, "_log")]][is.na(main_data[[paste0(j, "_log")]])| 
                                   is.nan(main_data[[paste0(j, "_log")]]) | 
                                   main_data[[paste0(j, "_log")]]== "-Inf" |
                                  main_data[[paste0(j, "_log")]]=="Inf"] <- 0
}



main_data[["TVAL_BANK"]]

##SPILT INTO 5!!!!!!
##Process the dummy: change 2 to 0

my_string = "EJB1_UNION"
num_pattern = "\\d+"
# Loop through the string and increment the number by 1
for (i in 1:5) {
  print(my_string)
  # Find the first occurrence of a number in the string
  num_match <- str_extract(my_string, num_pattern)
  #Process the NA and 2
  main_data[[my_string]] <- ifelse(main_data[[my_string]] == 2, 0, main_data[[my_string]])
  ##Create sub-dataframe
  name <- paste0(my_string, "_data")
  df <- main_data %>% drop_na(my_string)
  assign(name, df)
  

  # If a number was found, increment it by 1 and replace it in the string
  if (as.numeric(num_match) != -1) {
    num <- as.numeric(num_match)
    num <- num + 1
    my_string <- gsub(num_pattern, num, my_string)
  }
}

`EJB1_UNION_data` <- `EJB1_UNION_data` %>% relocate("EJB1_UNION")
view(head(`EJB1_UNION_data`, 1000))

length(`EJB2_UNION_data`[["TVAL_AST"]])
length(main_data[["TVAL_AST"]])
sum(!is.na(main_data[["EJB1_UNION"]]))



for (i in seq(Ast_list)) {
  # Append the string "-fruit" to the current string
  Ast_list[i] <- paste0(Ast_list[i], "_proportion")
  
  # Print the new string
}

Ast_list_with_prop <- Ast_list
Ast_list_with_prop


Ast_list <- c("TVAL_BANK",
              "TVAL_STMF", "TVAL_BOND", "TVAL_RENT", "TVAL_RE",
              "TVAL_OTH", "TVAL_RET", "TVAL_BUS", "TVAL_HOME",
              "TVAL_VEH", "TVAL_ESAV")
for (i in seq(Ast_list)) {
  # Append the string "-fruit" to the current string
  Ast_list[i] <- paste0(Ast_list[i], "_log")
  
  # Print the new string
}
Ast_list_with_log <- Ast_list
Ast_list_with_log
Ast_list <- c("TVAL_BANK",
              "TVAL_STMF", "TVAL_BOND", "TVAL_RENT", "TVAL_RE",
              "TVAL_OTH", "TVAL_RET", "TVAL_BUS", "TVAL_HOME",
              "TVAL_VEH", "TVAL_ESAV")

Debt_list <- c("TDEBT_HOME", "TDEBT_VEH", "TDEBT_RENT", "TDEBT_RE", 
               "TDEBT_BUS", "TDEBT_CC", "TDEBT_ED", "TDEBT_OT", "TMED_AMT")
for (i in seq(Debt_list)) {
  # Append the string "-fruit" to the current string
  Debt_list[i] <- paste0(Debt_list[i], "_log")
  
  # Print the new string
}
Debt_list_with_log <- Debt_list
Debt_list_with_log
Debt_list <- c("TDEBT_HOME", "TDEBT_VEH", "TDEBT_RENT", "TDEBT_RE", 
               "TDEBT_BUS", "TDEBT_CC", "TDEBT_ED", "TDEBT_OT", "TMED_AMT")



for (i in seq(Debt_list)) {
  # Append the string "-fruit" to the current string
  Debt_list[i] <- paste0(Debt_list[i], "_proportion")
  
  # Print the new string
}
Debt_list_with_prop <- Debt_list
Debt_list_with_prop
Debt_list <- c("TDEBT_HOME", "TDEBT_VEH", "TDEBT_RENT", "TDEBT_RE", 
               "TDEBT_BUS", "TDEBT_CC", "TDEBT_ED", "TDEBT_OT", "TMED_AMT")
Debt_list_with_prop <- append(Debt_list_with_prop, "overall_leveraged_ratio")


Reg_results <- data.frame()
Reg_details <- data.frame()



for (var in Ast_list) {
  #Run the OLS regression

  y <- unlist(EJB1_UNION_data[, var, with=FALSE])#add with argument!
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(y~EJB1_UNION, data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(y~EJB1_UNION, data = .)))

  #Generate tickers
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var

  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
#Export output
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_regression_details.csv", row.names = FALSE)

Reg_results <- data.frame()
Reg_details <- data.frame()

for (var in Ast_list_with_prop) {
  #Run the OLS regression
  
  y <- unlist(EJB1_UNION_data[[var]])#add with argument!
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  #Generate tickers
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
#Export the output
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_prop_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_prop_regression_details.csv", row.names = FALSE)


#Log transformation:



Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Ast_list_with_log) {
  #Run the OLS regression
  
  y <- unlist(EJB1_UNION_data[[var]])#add with argument!
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  #Generate tickers
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
#Export the output
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_log_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_log_regression_details.csv", row.names = FALSE)



##Calculating for the debt part
Reg_results <- data.frame()
Reg_details <- data.frame()

for (var in Debt_list) {
  #Run the OLS regression
  
  y <- unlist(EJB1_UNION_data[[var]])
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  #Generate tickers
  regression_results$Debt_var <- var
  regression_details$Debt_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Debt_var)
Reg_details <- Reg_details %>% relocate(Debt_var)
write.csv(Reg_results, file = "Debt_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_regression_details.csv", row.names = FALSE)


Reg_results <- data.frame()
Reg_details <- data.frame()

for (var in Debt_list_with_prop) {
  #Run the OLS regression
  y <- unlist(EJB1_UNION_data[[var]])
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  #Generate tickers
  regression_results$Debt_var <- var
  regression_details$Debt_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Debt_var)
Reg_details <- Reg_details %>% relocate(Debt_var)
write.csv(Reg_results, file = "Debt_prop_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_prop_regression_details.csv", row.names = FALSE)


Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Debt_list_with_log) {
  #Run the OLS regression
  y <- unlist(EJB1_UNION_data[[var]])
  regression_results <- EJB1_UNION_data %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  #Generate tickers
  regression_results$Debt_var <- var
  regression_details$Debt_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Debt_var)
Reg_details <- Reg_details %>% relocate(Debt_var)
write.csv(Reg_results, file = "Debt_log_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_log_regression_details.csv", row.names = FALSE)


#Utility usage to the total income

EJB1_UNION_data[["RMU_prop_to_asset"]]
model_rmu <- lm(RMU_prop_to_asset~EJB1_UNION, data = EJB1_UNION_data)
summary(model_rmu)

#Leveraged ratio creation and calculation
##Defined by total_household_debt/disposable_personal_income
model_lev_ratio_general <- lm(overall_leveraged_ratio~EJB1_UNION, EJB1_UNION_data)
summary(model_lev_ratio_general)

view(EJB1_UNION_data[["overall_leveraged_ratio"]])

view(head(pu[["TDEBT_AST"]], 100000))




########CONTROLLING VARIABLES
###Gender
EJB1_UNION_data[["ESEX"]]
EJB1_UNION_data %>% group_by(ESEX) %>% summarize(mean = mean(TVAL_BANK))

dict_sex <- setNames(c(1, 2), c("Male", "Female"))
for (var in Debt_list_with_prop) {
  regression_results <- EJB1_UNION_data %>%
    group_by(ESEX) %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    group_by(ESEX) %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  ##Matching the value with the dictionary and print it out
  # Function to map values using the dictionary
  map_values <- function(value) {
    return(dict_sex[as.character(value)])
  }
  
  # Apply the function to create a new column based on the existing column
  regression_results$state <- sapply(regression_results$ESEX, map_values)
  regression_details$state <- sapply(regression_details$ESEX, map_values)
  
  
  # Export the results to CSV
  write.csv(regression_results, file = paste0(var, " regression_summary(by sex).csv"), row.names = FALSE)
  write.csv(regression_details, file = paste0(var, " regression_details(by sex).csv"), row.names = FALSE)
}
 




#####Test
#fitted_models <- function(df){lm(TVAL_BANK~EJB1_UNION, data = EJB1_UNION_data)}
#regression_results <- by(EJB1_UNION_data, EJB1_UNION_data$TEHC_ST, fitted_models)
#for (a in regression_results)
#{
#  print(a)
#}


###State
dict_state <- setNames(c("AL", "AK", "AZ"
, "AR"
, "CA"
, "CO"
, "CT"
, "DE"
, "DC"
, "FL"
, "GA"
, "HI"
, "ID"
, "IL"
, "IN"
, "IA"
, "KS"
, "KY"
, "LA"
, "ME"
, "MD"
, "MA"
, "MI"
, "MN"
, "MS"
, "MO"
, "MT"
, "NE"
, "NV"
, "NH"
, "NJ"
, "NM"
, "NY"
, "NC"
, "ND"
, "OH"
, "OK"
, "OR"
, "PA"
, "RI"
, "SC"
, "SD"
, "TN"
, "TX"
, "UT"
, "VT"
, "VA"
, "WA"
, "WV"
, "WI"
, "WY"
, "PR"), 
                       c("1", "2", "4", "5", "6", "8", "9", "10", "11", "12", "13", "15", "16", "17", "18", "19", 
                         "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", 
                         "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                         "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "60"))
seq(1, 56, by=1)

dict_state_df <- as.data.frame(dict_state)
dict_state_df$TEHC_ST = rownames(dict_state_df)
dict_state_df

housing_by_state <- read.csv("State_housing_data.csv", header = TRUE)
housing_by_state <- pivot_longer(
  housing_by_state,
  cols = -TEHC_ST,  # Exclude StateName from reshaping
  names_to = "Date",  # New column name for dates
  values_to = "HousingPrice" # New column name for housing prices
)
housing_by_state$Date <- sub("^X", "", housing_by_state$Date)
housing_by_state <- housing_by_state %>%
  mutate(Date = as.Date(paste(Date, "01"), format = "%Y%m%d"))
housing_by_state$year <- format(housing_by_state$Date, "%Y")
housing_by_state$MONTHCODE <- format(housing_by_state$Date, "%m")
housing_by_state[["year"]] <- as.numeric(housing_by_state[["year"]])
housing_by_state[["MONTHCODE"]] <- as.numeric(housing_by_state[["MONTHCODE"]])
housing_by_state <- merge(dict_state_df, housing_by_state, by = "TEHC_ST")

view(housing_by_state)

EJB1_UNION_data <- EJB1_UNION_data[EJB1_UNION_data$TEHC_ST != "61", ]



for (var in Debt_list_with_prop) {
  y <- unlist(EJB1_UNION_data[[var]])

  # Run regression by groups using ddply() from the 'plyr' package
  regression_results <- EJB1_UNION_data %>%
    group_by(TEHC_ST) %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    group_by(TEHC_ST) %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))

  ##Matching the value with the dictionary and print it out
  # Function to map values using the dictionary
  map_values <- function(value) {
    return(dict_state[as.character(value)])
  }
  
  # Apply the function to create a new column based on the existing column
  regression_results$state <- sapply(regression_results$TEHC_ST, map_values)
  regression_details$state <- sapply(regression_details$TEHC_ST, map_values)
  
  
  # Export the results to CSV
  write.csv(regression_results, file = paste0(var, " regression_summary(by state).csv"), row.names = FALSE)
  write.csv(regression_details, file = paste0(var, " regression_details(by state).csv"), row.names = FALSE)
}  


for (var in Ast_list_with_prop) {
  y <- unlist(EJB1_UNION_data[[var]])
  # Run regression by groups using ddply() from the 'plyr' package
  regression_results <- EJB1_UNION_data %>%
    group_by(TEHC_ST) %>%
    do(tidy(lm(paste(var, "~ EJB1_UNION"), data = .)))
  regression_details <- EJB1_UNION_data %>%
    group_by(TEHC_ST) %>%
    do(glance(lm(paste(var, "~ EJB1_UNION"), data = .)))
  
  ##Matching the value with the dictionary and print it out
  # Function to map values using the dictionary
  map_values <- function(value) {
    return(dict_state[as.character(value)])
  }
  
  # Apply the function to create a new column based on the existing column
  regression_results$state <- sapply(regression_results$TEHC_ST, map_values)
  regression_details$state <- sapply(regression_details$TEHC_ST, map_values)
  
  
  # Export the results to CSV
  write.csv(regression_results, file = paste0(var, " regression_summary(by state).csv"), row.names = FALSE)
  write.csv(regression_details, file = paste0(var, " regression_details(by state).csv"), row.names = FALSE)
}  


#####Combination of other variables
##Fed Funds rate


##CPI(Not core!!!)


##Consumer Confidence Index


##EITC Refund


###NY Fed Survey for Consumer Expectation
##Inflation uncertainty
inflation_uncertainty <- read.csv("inflation_uncertainty.csv")
view(inflation_uncertainty)
colnames(inflation_uncertainty)
merged_main_data <- merge(main_data, inflation_uncertainty, by = c("year", "MONTHCODE"))

Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Debt_list_with_log)
{
  y <- unlist(merged_main_data[[var]])
  regression_results <- merged_main_data %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation"), data = .)))
  regression_details <- merged_main_data %>%
    do(glance(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Debt_log_complex2_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_log_complex2_regression_details.csv", row.names = FALSE)


##Additional variables:

job_separation <- read.csv("Job_separation.csv")
sydney_fin_uncertain <- read.csv("SydneyFinUncertainty.csv")
view(sydney_fin_uncertain)
colnames(sydney_fin_uncertain)
merged_main_data1 <- merge(merged_main_data, job_separation, by = c("year", "MONTHCODE"))
merged_main_data1 <- merge(merged_main_data1, sydney_fin_uncertain, by = c("year", "MONTHCODE"))

Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Debt_list_with_log)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Debt_log_complex4_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_log_complex4_regression_details.csv", row.names = FALSE)



###Combined with Consumer Confidence Index


household_finance_expectation <- read.csv("Household_finance.csv")
merged_main_data1 <- merge(merged_main_data1, household_finance_expectation, by = c("year", "MONTHCODE"))
view(household_finance_expectation)
Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Ast_list_with_log)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION+EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_log_complex5_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_log_complex5_regression_details.csv", row.names = FALSE)


###Add more data
  yearAgo_credit_avail <- read.csv("Year_ago_credit_avail.csv")
  yearAhead_credit_avail <- read.csv("Year_ahead_credit_avail.csv")
  Umich_consumer_confidence <- read.csv("Umich_consumer_confidence.csv")
  Commodity_expectation <- read.csv("Commodity_expectation.csv")
  Unemploy_expectation <- read.csv("Unemployment_expectation.csv")
  CPI_rate <- read.csv("Median_CPI_Change_Rate.csv")
  
  
  CPI_rate <- subset(CPI_rate, select = -DATE)
  
  merged_main_data1 <- merge(merged_main_data1, yearAgo_credit_avail, by = c("year", "MONTHCODE"))
  merged_main_data1 <- merge(merged_main_data1, yearAhead_credit_avail, by = c("year", "MONTHCODE"))
  merged_main_data1 <- merge(merged_main_data1, Umich_consumer_confidence, by = c("year", "MONTHCODE"))
  merged_main_data1 <- merge(merged_main_data1, Commodity_expectation, by = c("year", "MONTHCODE"))
  merged_main_data1 <- merge(merged_main_data1, Unemploy_expectation, by = c("year", "MONTHCODE"))
  merged_main_data1 <- merge(merged_main_data1, CPI_rate, by = c("year", "MONTHCODE"))


Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Ast_list_with_log)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Rent+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_log_complex6_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_log_complex6_regression_details.csv", row.names = FALSE)

Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Ast_list_with_prop)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Rent+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Ast_prop_complex6_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Ast_prop_complex6_regression_details.csv", row.names = FALSE)

Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Debt_list_with_log)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Rent+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Debt_log_complex6_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_log_complex6_regression_details.csv", row.names = FALSE)

Reg_results <- data.frame()
Reg_details <- data.frame()
for (var in Debt_list_with_prop)
{
  y <- unlist(merged_main_data1[[var]])
  regression_results <- merged_main_data1 %>% 
    do(tidy(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Rent+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  regression_details <- merged_main_data1 %>%
    do(glance(lm(paste(var,"~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate"), data = .)))
  
  regression_results$Ast_var <- var
  regression_details$Ast_var <- var
  
  #Rbind!
  Reg_results <- rbind(Reg_results, regression_results)
  Reg_details <- rbind(Reg_details, regression_details)
}
Reg_results <- Reg_results %>% relocate(Ast_var)
Reg_details <- Reg_details %>% relocate(Ast_var)
write.csv(Reg_results, file = "Debt_prop_complex6_regression_summary.csv", row.names = FALSE)
write.csv(Reg_details, file = "Debt_prop_complex6_regression_details.csv", row.names = FALSE)


model_lev_ratio_general <- lm(overall_leveraged_ratio~EJB1_UNION*Median_oneYear_ahead_uncertainty_inflation+EJB1_UNION*Median_threeYear_ahead_uncertainty_inflation+EJB1_UNION*Mean_prob_losingJob+EJB1_UNION*h_1+EJB1_UNION*h_3+EJB1_UNION*h_12+EJB1_UNION*YearAgoPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*YearAheadPessimistic_HF+EJB1_UNION*Harder_side_ago+EJB1_UNION*Easier_side_ago+EJB1_UNION*Harder_side_ahead+EJB1_UNION*Harder_side_ahead+EJB1_UNION*UMICH_Consumer_Index+EJB1_UNION*Gas+EJB1_UNION*Food+EJB1_UNION*Medical+EJB1_UNION*College+EJB1_UNION*Rent+EJB1_UNION*Gold+EJB1_UNION*Mean_prob_higher_unemploy_us_oneYear+EJB1_UNION*CPI_change_rate, merged_main_data1)
summary(model_lev_ratio_general)

tab_model(Reg_results)


### Selecting specific variables for correlaltion
selected_vars <- merged_main_data1[, c("EJB1_UNION", "Median_oneYear_ahead_uncertainty_inflation", "Median_threeYear_ahead_uncertainty_inflation", "Mean_prob_losingJob", "h_1", "h_3", "h_12", "YearAgoPessimistic_HF", "YearAheadPessimistic_HF", "YearAheadPessimistic_HF", "Harder_side_ago", "Easier_side_ago", "Harder_side_ahead", "Harder_side_ahead", "UMICH_Consumer_Index", "Gas", "Food", "Medical", "College", "Rent", "Gold", "Rent", "Mean_prob_higher_unemploy_us_oneYear", "CPI_change_rate")]

# Calculating the correlation matrix
correlation_matrix <- cor(selected_vars, use='complete.obs')

# Printing the correlation matrix
print(correlation_matrix)

#Visualize the correlation chart
corrplot(correlation_matrix, tl.cex = 0.7, number.cex = 0.5)



###Regression plot
# Create the regression model
model_KEEE <- lm("TVAL_BANK_log ~ EJB1_UNION", EJB1_UNION_data)

# Plot the regression line
ggplot(EJB1_UNION_data, aes(x = "EJB1_UNION", y = "TVAL_BANK_log")) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line without confidence intervals
  labs(title = "Simple Linear Regression", x = "X-axis", y = "Y-axis")  # Add title and axis labels


