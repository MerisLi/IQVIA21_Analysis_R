library(readxl)
library(readr)
library(dplyr)


RX <- read_csv("RX_updated.csv")

RX_DDD <- read_xlsx("RXlist_ATC-DDD.xlsx")

RX_DDD<- RX_DDD %>%
  pivot_longer(
    cols = 3:last_col(),
    names_to = ".value",
    names_pattern = "^(DDD|U|Adm).*"
  ) %>%
  mutate(across(c(DDD, U, Adm), ~ na_if(as.character(.x), "")),
         DDD_mg = as.numeric(DDD)*1000) %>%
  arrange(usc_lvl3_desc, unifm_prod_nm)

RX_DDD_clean <- RX_DDD %>%
  filter(!is.na(DDD),
         Adm=="O")

RX_DDD_comb<- RX%>%
  select(1:12) %>%
  left_join(select(RX_DDD_clean, unifm_prod_nm, DDD_mg),
              by = "unifm_prod_nm")

RX_DDD_comb<- RX_DDD_comb %>%
  mutate(DDDpre = round(rx_dosage_amt*days_supply_cnt/DDD_mg, 2))


RX_DDD_comb<- RX_DDD_comb %>%
  mutate(above_DDD = case_when(
    DDDpre > dspnsd_qty ~ "T",
    TRUE ~ "F"
  ))

summary_DDD <-RX_DDD_comb %>%
  group_by(unifm_prod_nm,above_DDD) %>%
  summarise(count = n())

summary_DDD<- summary_DDD %>%
  pivot_wider(names_from = above_DDD,
              values_from = count)

summary_DDD<- summary_DDD %>%
  mutate(T=replace_na(T,0),
         F=replace_na(F,0))


summary_DDD<- summary_DDD|>
  mutate(sum=T+F,
        perc_T=T/sum)

##############
library(ggplot2)

RX_DDD_comb%>%
  filter(unifm_prod_nm=="AZITHROMYCIN")%>%
  ggplot(aes(x=above_DDD, fill=age_cat))+ #or fill by pri_spcl_desc
    geom_bar(position = "stack")+
    theme(
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 5)
  )


RX_DDD_comb%>%
  filter(unifm_prod_nm=="LEVOFLOXACIN")%>%
  ggplot(aes(x=above_DDD, fill=age_cat))+ #or fill by pri_spcl_desc
  geom_bar(position = "stack")+
  theme(
    legend.key.size = unit(0.3, "cm"),
    legend.text = element_text(size = 5)
  )