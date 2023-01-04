# data preprocessing
library(tidyverse)
library(lubridate)
# data exploration
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
# for meta-ml
library(tidymodels)

#library(ranger) # ranger random forest model. Import not explicitly necessary, will be loaded by parsnips

# let's set some global options
options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # select a lightweight ggplot theme for cleaner plotting
set.seed(2023) # in the AC, you'll be required to set a fixed random seed to make your work reproducible


# Start reading data
classification <- read_csv( 'classification.csv' )

customers <- read_csv(
  'customers.csv',
  col_types = cols(
    Item_Position = col_double() 
  )
)#change from character like "005230" to numeric number "5230"

sales_orders <- read_csv( 'sales_orders.csv')

sales_orders_header <- read_csv('sales_orders_header.csv' ) %>% 
  mutate(
    Release_Date = as_date(Release_Date) %>% as.character(),
    Creation_Date = as_date(Creation_Date) %>% as.character()
  )
#which(is.na(sales_orders_header$Release_Date)) #look if as_data is not run, exact time00:00:00 is added.
# 150048 header$Sales_Order, compared to 150064 sales_order$Sales_Order


business_units <- read_csv('business_units.csv') %>% 
  select( -YHKOKRS ) 
# YHKOKRS, all elements are 5180, useless

service_map <- read_csv('service_map.csv') 
#MATKL_service 29 distict values, compared to sales_orders$Material_Class with 329 distict values


#Start joining tibble data
cla_cus <- left_join(classification, customers, by = c('Customer_ID' = 'Customer_ID'))

#join sales order
cla_cus_sal_inner <- inner_join(cla_cus, sales_orders, by = c('Sales_Order' = 'Sales_Order','Item_Position' = 'Item_Position'))

sales_orders_rest <- anti_join(sales_orders, cla_cus, by = c('Sales_Order' = 'Sales_Order','Item_Position' = 'Item_Position')  )
sales_orders_rest <- sales_orders_rest %>% mutate(Item_Position = 0 )
cla_cus_sal_rest <- inner_join(cla_cus, sales_orders_rest, by = c('Sales_Order' = 'Sales_Order','Item_Position' = 'Item_Position'))

cla_cus_sal <- bind_rows(cla_cus_sal_inner, cla_cus_sal_rest)
#cla_cus_sal %>% dfSummary %>% view  #cla_cus_sal$Test_set_id has 512 distinct values, which is same as classification.csv

#join sales order header
cla_cus_sal_head <- left_join(cla_cus_sal, sales_orders_header, by = c('Sales_Order' = 'Sales_Order'))

#join business unit
cla_cus_sal_head_busi <- left_join(cla_cus_sal_head, business_units, by = c('Cost_Center' = 'Cost_Center'))
all_merged <- cla_cus_sal_head_busi

#join service maps
all_merged = all_merged %>%
  mutate(
    MATKL = case_when(
      Material_Class %in% service_map$MATKL_service ~ 1,
      TRUE ~ 0 # TRUE is equivalent to an “else” statement.
    )
  )
all_merged %>% dfSummary %>% view


print("[INFO] Number of NA values in each column:")
for (i in 1:ncol(all_merged)) { # for-loop over columns
  print(paste0(names(all_merged)[i], ": ", sum(is.na(all_merged[, i])), "/", nrow(all_merged)))
}
print("[INFO] Glimpse:")
glimpse(all_merged)

#join and merge is finished above
#####################################################################################
df <- all_merged
###Handling binary features

#  "Type": Replace string values "SOP,STP" with binary values
df = df %>%
  mutate(
    Type_SOTP = case_when(
      Type %in% c("SOP") ~ 1,
      Type %in% c("STP") ~ 0,
    )
  )
df = select (df,-c(Type))

#  "Sales_Organization": Replace string values "A,B " with binary values
df = df %>%
  mutate(
    Organization = case_when(
      Sales_Organization %in% c("A") ~ 1,
      Sales_Organization %in% c("B") ~ 0,
    )
  )
df = select (df,-c(Sales_Organization))

#  "Creator": "0x2500D9>69.7%"
df = df %>%
  mutate(
    Creator_Binary = case_when(
      Creator == "0x2500D9E578134DB667D820EA3100F880DAF13E408E6761CEE69DBFC3EAF833B4" ~ 1,
      TRUE ~ 0
    )
  )

###Handling date
#  "Creation_Date" and "Release_Date"
# Compare them with each other first
df = df %>%
  mutate(crea_relea = case_when(
    Creation_Date==Release_Date ~ 1,
    TRUE ~ 0
  ))
#df$crea_relea %>% dfSummary %>% view
df <- df %>% select(-crea_relea)
# Creation_Date 
#year
df = df %>%
  mutate(Creation_Year = case_when(
    !is.na(Creation_Date) ~ Creation_Date %>%
      substr(nchar(Creation_Date) - 9, nchar(Creation_Date)-6 ) %>%
      as.numeric()
  ))
df = df %>% mutate(Creation_Year = (Creation_Year-2020)* 365 )
#month
df = df %>%
  mutate(Creation_Month = case_when(
    !is.na(Creation_Date) ~ Creation_Date %>%
      substr(nchar(Creation_Date) - 4, nchar(Creation_Date)-3 ) %>%
      as.numeric()
  ))
df = df %>% mutate(Creation_Month = ( Creation_Month-1)* 30 )
#day
df = df %>%
  mutate(Creation_Day = case_when(
    !is.na(Creation_Date) ~ Creation_Date %>%
      substr(nchar(Creation_Date) - 1, nchar(Creation_Date) ) %>%
      as.numeric()
  ))
df = df %>% mutate(Creation_Date = Creation_Date )
#accumulate date
df <- df %>% mutate(Creation_Date_New = Creation_Year+Creation_Month+Creation_Day  )
df <- df %>% select(-Creation_Year, -Creation_Month, -Creation_Day)

#Release_Date
#year
df = df %>%
  mutate(Release_Year = case_when(
    !is.na(Release_Date) ~ Release_Date %>%
      substr(nchar(Release_Date) - 9, nchar(Release_Date)-6 ) %>%
      as.numeric()
  ))
df = df %>% mutate(Release_Year = (Release_Year-2020)* 365 )
#month
df = df %>%
  mutate(Release_Month = case_when(
    !is.na(Release_Date) ~ Release_Date %>%
      substr(nchar(Release_Date) - 4, nchar(Release_Date)-3 ) %>%
      as.numeric()
  ))
df = df %>% mutate(Release_Month = ( Release_Month-1)* 30 )
#day
df = df %>%
  mutate(Release_Day = case_when(
    !is.na(Release_Date) ~ Release_Date %>%
      substr(nchar(Release_Date) - 1, nchar(Release_Date) ) %>%
      as.numeric()
  ))
df = df %>% mutate(Release_Date = Release_Date )
#accumulate date
df <- df %>% mutate(Release_Date_New = Release_Year+Release_Month+Release_Day  )
df <- df %>% select(-Release_Year, -Release_Month, -Release_Day)

#Difference between release and creation data
df <- df %>% mutate(Diff_Date = Release_Date_New-Creation_Date_New  )

#df %>% dfSummary %>% view


##############################################################################
### Missing data handling 
#Material_Class  2
df[which(is.na(df$	Material_Class)),] 
#which(is.na(df$	Material_Class))
#df[866465:866474,]
#df[866531:866539,]
#not in Test_set, delete
df <- df[-which(is.na(df$	Material_Class)),]


#sales order header: 43/1409894
missing1 <- df[which(is.na(df$Creation_Date_New)),] 
missing1 %>% dfSummary %>% view
#Diff_Date: 3683/1409894
missing2 <- df[which(is.na(df$Diff_Date)),] 
missing2 %>% dfSummary %>% view
#include test data, so they cannot be deleted

ddf <- df

ddf[which(is.na(ddf$Document_Type)),]$Document_Type <- "Order"  #"Order" accounts for 99%, so...
ddf[which(is.na(ddf$Delivery)),]$Delivery <- "Not relevant"   #"Not relevant" accounts for 93.3%, so...
ddf[which(is.na(ddf$Net_Value.y)),]$Net_Value.y <- 615456    #  615455.6 is average of Net_Value.y, so...
ddf[which(is.na(ddf$Organization)),]$Organization <- 0  #Organization: 0 accounts for 67.7% and 1 is 32.3%, so..
ddf[which(is.na(ddf$Creation_Date_New)),]$Creation_Date_New <- 533 #533.3 is average of Creation_Date_New, so...
ddf[which(is.na(ddf$Release_Date_New)),]$Release_Date_New <- 542#542 is average of Release_Date_New, so...
ddf[which(is.na(ddf$Diff_Date)),]$Diff_Date <- 8     #8.2 is average of Diff_Date, so...

#ddf %>% dfSummary %>% view

#Cost_Center could be further used
#Document_Type, Order accounts for 99%, could be further used.
#Both Net_Values are not handled. Maybe log function


dfdf <- ddf %>% select(-c(Sales_Order, Material_Code, Creation_Date ,Release_Date, Creator, Cost_Center))
#dfdf %>% dfSummary %>% view



#Split Data into Labeled and Unlabeled (Test) Sets
labeled_data = dfdf[is.na(dfdf$Test_set_id),]
test_data = dfdf[!is.na(dfdf$Test_set_id),]

labeled_data %>% nrow()
test_data %>% nrow()


write_csv(x = dfdf, file = "data cleaning results/train_test_mixed.csv")
write_csv(x = labeled_data, file = "data cleaning results/labeled_data.csv")
write_csv(x = test_data, file = "data cleaning results/test_data.csv")
