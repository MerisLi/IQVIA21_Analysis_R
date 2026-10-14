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

copayment <- unfiltered %>%
  select(usc_lvl3_desc,
         unifm_prod_nm,
         pri_spcl_desc,
         pat_age_nbr,
         copay_amt,
         cust_prc,
         pat_pay_amt,
         model_typ_cd,
         model_typ_nm,
         age_cat)%>%
  filter(copay_amt > 0)



## !!make insurance model categories

copayment <- copayment %>%
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


# check the assignment of categories 
copayment%>%
  select(model_typ_cd, model_typ_nm, insurance_type) %>%
  distinct() %>%
  arrange(insurance_type)


## mark copayment outliers

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
copayment <- copayment %>%
  group_by(usc_lvl3_desc, age_cat) %>%
  mutate(outlier = ifelse(is_outlier(copay_amt), copay_amt, as.numeric(NA)))%>%
  ungroup()


write_csv(copayment, "Copay_edited.csv")

