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




RX <- unfiltered %>%
  select(usc_lvl3_desc,
         unifm_prod_nm,
         pat_age_nbr,
         rx_dosage_amt,
         dosage_form_nm,
         strnt_desc,
         pri_spcl_desc,
         provider_st_cd,
         age_cat) %>%
  filter(rx_dosage_amt > 0)


########



#make rx_dosage_amt comparable, assisted with AI

#step1:Classify 49 strength descriptions into one of 8 patterns
library(stringr)
library(readr)

RX <- RX %>%
  mutate(
    
    strength_type = case_when(
      
      str_detect(strnt_desc, "^\\d+ MG$") ~ "single_mg",
      
      str_detect(strnt_desc, "^\\d+ GM$") ~ "single_g",
      
      str_detect(strnt_desc, "^\\d+-\\d+\\.?\\d* MG$") ~ "combo_mg",
      
      str_detect(strnt_desc, "^\\d+ MG/\\d+ ML$") ~ "mg_per_volume",
      
      str_detect(strnt_desc,
                 "^\\d+-\\d+\\.?\\d* MG/\\d+ ML$") ~ "combo_per_volume",
      
      str_detect(strnt_desc, "^\\d+ MG/ML$") ~ "mg_per_ml",
      
      str_detect(strnt_desc, "^\\d+ MG/24HR$") ~ "mg_per_24hr",
      
      str_detect(strnt_desc,
                 "^\\d+-\\d+\\.?\\d* MG/12HR$") ~ "combo_12hr",
      
      TRUE ~ "other"
      
    )
  )

#step2: extract the (first) ingredient
## ant1_mg = strength of the first active ingredient


RX <- RX %>%
  mutate(
    ant1_mg = case_when(
      
      strength_type == "single_mg" ~
        as.numeric(str_extract(strnt_desc, "^\\d+\\.?\\d*")),
      
      strength_type == "single_g" ~
        as.numeric(str_extract(strnt_desc, "^\\d+\\.?\\d*")) * 1000,
      
      strength_type %in% c("combo_mg",
                           "combo_per_volume",
                           "combo_12hr") ~
        as.numeric(str_extract(strnt_desc, "^\\d+\\.?\\d*")),
      
      strength_type %in% c("mg_per_volume",
                           "mg_per_ml",
                           "mg_per_24hr") ~
        as.numeric(str_extract(strnt_desc, "^\\d+\\.?\\d*")),
      
      TRUE ~ NA_real_
    )
  )

#step3: extract the second ingredient as there are combination antibiotics

## ant2_mg = strength of the second active ingredient

RX <- RX %>%
  mutate(
    ant2_mg = case_when(
      
      strength_type %in% c("combo_mg",
                           "combo_per_volume",
                           "combo_12hr") ~
        
        as.numeric(
          str_extract(
            strnt_desc,
            "(?<=-)\\d+\\.?\\d*"
          )
        ),
      
      TRUE ~ NA_real_
    )
  )

#step4: extract volumes for suspensions/liquid

## volume_ml = denominator after "/"

RX <- RX %>%
  mutate(
    volume_ml = case_when(
      
      strength_type %in% c("mg_per_volume",
                           "combo_per_volume") ~
        
        as.numeric(
          str_extract(
            strnt_desc,
            "(?<=/)\\d+"
          )
        ),
      
      strength_type == "mg_per_ml" ~ 1,
      
      TRUE ~ NA_real_
    )
  )


#then calculate the concentrations

## mg per mL


RX <- RX %>%
  mutate(
    
    ant1_mg_ml = case_when(
      
      strength_type == "mg_per_volume" ~
        ant1_mg / volume_ml,
      
      strength_type == "combo_per_volume" ~
        ant1_mg / volume_ml,
      
      strength_type == "mg_per_ml" ~
        ant1_mg,
      
      TRUE ~ NA_real_
    ),
    
    ant2_mg_ml = case_when(
      
      strength_type == "combo_per_volume" ~
        ant2_mg / volume_ml,
      
      TRUE ~ NA_real_
    )
    
  )

#step5: extract release hours if it's the delayed release type
## release_hours

RX <- RX %>%
  mutate(
    release_hours = case_when(
      
      str_detect(strnt_desc, "24HR") ~ 24,
      
      str_detect(strnt_desc, "12HR") ~ 12,
      
      TRUE ~ NA_real_
    )
  )

#step6: calculate the estimated quantity dispensed, which makes the RX amount comparable within and between antibiotics

## Estimate the number of tablets/capsules or mL dispensed


RX <- RX %>%
  mutate(
    
    estimated_quantity = case_when(
      
      strength_type %in%
        c("single_mg",
          "single_g",
          "combo_mg") ~
        
        rx_dosage_amt / ant1_mg,
      
      strength_type == "mg_per_volume" ~
        
        rx_dosage_amt / ant1_mg_ml,
      
      strength_type == "combo_per_volume" ~
        
        rx_dosage_amt / ant1_mg_ml,
      
      strength_type == "mg_per_ml" ~
        
        rx_dosage_amt / ant1_mg_ml,
      
      TRUE ~ NA_real_
      
    )
    
  )


#highest strength for each antibiotic
RX <- RX %>%
  group_by(unifm_prod_nm) %>%
  mutate(
    highest_strnt =
      ant1_mg == max(ant1_mg, na.rm = TRUE)
  ) %>%
  ungroup()


#unusually large quantity (top %5)
RX <- RX %>%
  group_by(unifm_prod_nm) %>%
  mutate(
    large_quantity =
      estimated_quantity >
      quantile(
        estimated_quantity,
        0.95,
        na.rm = TRUE
      )
  ) %>%
  ungroup()

write_csv(RX, "RX_edited.csv")


