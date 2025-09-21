## get current working directory
getwd()

## read data from sample txn file by choosing it or directly name can be given
## juspay_data <- read.csv(file.choose(), header=T)
juspay_txns_data <- read.csv("/Users/anandjha/Downloads/MerchantSuccessLearnathon /sampleTransactions.csv",header = T)

## viewing data in console
juspay_txns_data

library(dplyr)
library(lubridate)
str(juspay_txns_data) ## total 29,631 obs with 6 columns headers
class(juspay_txns_data$txn_initiated) ## class of the columns is factors

## filtering conditions for pending status
juspay_status_pending <- juspay_txns_data %>% filter(status == 'PENDING')
str(juspay_status_pending)

#grouping by txns initiated
juspay_txns_period <- juspay_status_pending %>% group_by(txn_initiated)
juspay_txns_period

## distinct orders from each merchants
juspay_txns_merch <- juspay_status_pending %>% group_by(merchant_id) %>% summarise(n_distinct(order_id))
juspay_txns_merch


# standard date format output %Y-%m-%d and converting from factors to date
juspay_status_pending$txn_initiated <- as.Date(juspay_status_pending$txn_initiated,format = "%m-%d-%Y")
juspay_status_pending$txn_last_modified <- as.Date(juspay_status_pending$txn_last_modified,format = "%m-%d-%Y")

juspay_status_pending
str(juspay_status_pending)

## calculate the time difference
time_diff <- difftime(juspay_status_pending$txn_last_modified,juspay_status_pending$txn_initiated,,units = "hours")
time_diff

juspay_status_pending$txn_int <- juspay_status_pending$txn_last_modified - juspay_status_pending$txn_initiated
juspay_status_pending

### time intervals of high severity
juspay_txns_intrvls <- juspay_status_pending %>% 
  group_by(txn_int) %>%
  arrange(desc(txn_int)) 

### time intervals of low severity
juspay_txns_intrvls_low <- juspay_status_pending %>% 
  group_by(txn_int) %>%
  arrange(txn_int) 

juspay_txns_intrvls_low
  
  
##mutate( txns_time = difftime(juspay_status_pending$txn_last_modified,
                              ## juspay_status_pending$txn_initiated,,units = "hours"))
  
                  


