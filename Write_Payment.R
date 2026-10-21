library(dplyr)
library(readr)
library(ggrepel)
library(tidyverse)

#######import data
unfiltered <- read_csv("Q.unfiltered100k_1.csv")
unfiltered <- unfiltered
#na.omit()


unfiltered <- unfiltered %>%
  filter (pat_age_nbr >=0) %>%
  mutate(age_cat = case_when(
    pat_age_nbr < 18 ~ "Pediatrics",
    pat_age_nbr >= 65 ~ "Elders",
    TRUE ~ "Adults"
  ))

payment <- unfiltered %>%
  select(usc_lvl3_desc,
         unifm_prod_nm,
         pri_spcl_desc,
         pat_age_nbr,
         rx_dosage_amt,
         copay_amt,
         cust_prc,
         pat_pay_amt,
         model_typ_cd,
         model_typ_nm,
         age_cat)%>%
  filter(!is.na(unifm_prod_nm))



## !!make insurance model categories

payment <- payment %>%
  mutate(insurance_type = case_when(
    str_detect(model_typ_nm, "MEDICARE") ~ "Medicare",
    str_detect(model_typ_nm, "MEDICAID") ~ "Medicaid",
    str_detect(model_typ_cd, "HIX") ~ "Health Insurance Exchange",
    str_detect(model_typ_cd, "PPO|EPO|POS|HMO|IPA|CDHP") ~ "Commercial Plans",
    str_detect(model_typ_nm, "EMPLOYER") ~ "Employer-Sponsored",
    str_detect(model_typ_nm, "FEDERAL|STATE") ~ "Government Employee Programs",
    str_detect(model_typ_cd, "CASH|DISC|VOUCHER") ~ "Self-pay",
    str_detect(model_typ_nm, "WORKER") ~ "Workers' Comp",
    str_detect(model_typ_nm, "PBM|PHARMACY BENEFIT") ~ "Pharmacy Benefit Administration",
    TRUE ~ "Other/Unspecified"
  ))


## mark payment outliers per antibiotic by age

is_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  
  return(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr)
}

payment <- payment %>%
  group_by(unifm_prod_nm, age_cat) %>%
  mutate(cust_prc_outlier = ifelse(is_outlier(cust_prc), cust_prc, as.numeric(NA)))%>%
  ungroup()

#remove all entries having NA in payment information
payment<-payment[complete.cases(payment[, c("cust_prc", "pat_pay_amt", "copay_amt")]), ]


write_csv(payment, "Payment_edited.csv")

