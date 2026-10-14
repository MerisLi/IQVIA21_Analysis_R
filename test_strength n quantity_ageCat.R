library(dplyr)
library(readr)
library(ggrepel)
library(ggplot2)
library(tidyverse)
library(openxlsx)


RX <- read_csv("RX_edited.csv")


RX_filtered <- RX%>%
  group_by(unifm_prod_nm, pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50) 



# what factors are associated with stronger formulation?

RX_strength <- RX_filtered %>%
  group_by(unifm_prod_nm) %>%
  filter(n_distinct(highest_strnt) == 2) %>%
  ungroup()

RX_strength$pri_spcl_desc <- factor(RX_strength$pri_spcl_desc)
RX_strength$provider_st_cd <- factor(RX_strength$provider_st_cd)
RX_strength$dosage_form_nm <- factor(RX_strength$dosage_form_nm)
RX_strength$strnt_desc <- factor(RX_strength$strnt_desc)
RX_strength$highest_strnt <- factor(RX_strength$highest_strnt)
RX_strength$unifm_prod_nm <- factor(RX_strength$unifm_prod_nm)
RX_strength$age_cat <- factor(RX_strength$age_cat)


RX_strength$unifm_prod_nm <- relevel(RX_strength$unifm_prod_nm, ref = "AMOXICILLIN")
RX_strength$provider_st_cd <- relevel(RX_strength$provider_st_cd, ref = "NY")
RX_strength$dosage_form_nm <- relevel(RX_strength$dosage_form_nm, ref = "TABLET")
RX_strength$pri_spcl_desc <- relevel(RX_strength$pri_spcl_desc, ref = "NURSE PRACTITIONER")
RX_strength$age_cat <- relevel(RX_strength$age_cat, ref = "Adults")



model_strength<- glm(
  highest_strnt ~
    unifm_prod_nm +
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = binomial,
  data = RX_strength
)

summary(model_strength)
as.data.frame(exp(coef(model_strength)))


## What if with fewer predictors

model_strengthState<- glm(
  highest_strnt ~
    provider_st_cd,
  family = binomial,
  data = RX_strength
)

summary(model_strengthState)
as.data.frame(exp(coef(model_strengthState)))


model_strengthStAge<- glm(
  highest_strnt ~
    provider_st_cd +
    age_cat,
  family = binomial,
  data = RX_strength
)

summary(model_strengthStAge)
as.data.frame(exp(coef(model_strengthStAge)))




# what factors are associated with large prescription quantity?

##function for getting the correct AIC for gamma
correct_gamma_aic <- function(model) {
  # Extract the true MLE dispersion parameter
  disp <- MASS::gamma.dispersion(model)
  mu <- model$fitted.values
  y <- model$y
  p <- model$rank 
  log_lik <- sum(dgamma(y, shape = 1/disp, scale = mu * disp, log = TRUE))
  
  # Compute AIC: -2 * logLik + 2 * (p + 1)
  # (We add 1 to p because the dispersion parameter itself is an estimated parameter)
  true_aic <- -2 * log_lik + 2 * (p + 1)
  
  return(true_aic)}


RX_filtered <- RX%>%
  group_by(unifm_prod_nm, pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50)

RX_filtered$pri_spcl_desc <- factor(RX_filtered$pri_spcl_desc)
RX_filtered$provider_st_cd <- factor(RX_filtered$provider_st_cd)
RX_filtered$dosage_form_nm <- factor(RX_filtered$dosage_form_nm)
RX_filtered$age_cat <- factor(RX_filtered$age_cat)


###Amoxicillin
amox <- RX_filtered %>%
  filter(unifm_prod_nm == "AMOXICILLIN")

amox$pri_spcl_desc <- relevel(amox$pri_spcl_desc, ref = "NURSE PRACTITIONER")
amox$provider_st_cd <- relevel(amox$provider_st_cd, ref = "NY")
amox$dosage_form_nm <- relevel(amox$dosage_form_nm, ref = "CAPSULE")
amox$age_cat <- relevel(amox$age_cat, ref = "Adults")


model_amox <- glm(
  estimated_quantity ~
    age_cat+
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = amox
)

summary(model_amox)
as.data.frame(exp(coef(model_amox)))

#for assess test quality
correct_gamma_aic(model_amox)
summary(model_amox)$deviance
summary(model_amox)$null.deviance
summary(model_amox)$df.residual
summary(model_amox)$df.null



#########


###Cephalexin

cepha <- RX_filtered %>%
  filter(unifm_prod_nm == "CEPHALEXIN")

cepha$pri_spcl_desc <- relevel(cepha$pri_spcl_desc, ref = "NURSE PRACTITIONER")
cepha$provider_st_cd <- relevel(cepha$provider_st_cd, ref = "NY")
cepha$dosage_form_nm <- relevel(cepha$dosage_form_nm, ref = "CAPSULE")
cepha$age_cat <- relevel(cepha$age_cat, ref = "Adults")


model_cepha <- glm(
  estimated_quantity ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = cepha
)

summary(model_cepha)
as.data.frame(exp(coef(model_cepha)))


#for assess test quality
correct_gamma_aic(model_cepha)
summary(model_cepha)$deviance
summary(model_cepha)$null.deviance
summary(model_cepha)$df.residual
summary(model_cepha)$df.null



#########


###Azithromycin

azith <- RX_filtered %>%
  filter(unifm_prod_nm == "AZITHROMYCIN")

azith$pri_spcl_desc <- relevel(azith$pri_spcl_desc, ref = "NURSE PRACTITIONER")
azith$provider_st_cd <- relevel(azith$provider_st_cd, ref = "NY")
azith$dosage_form_nm <- relevel(azith$dosage_form_nm, ref = "TABLET")
azith$age_cat <- relevel(azith$age_cat, ref = "Adults")

model_azith <- glm(
  estimated_quantity ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = azith
)

summary(model_azith)
as.data.frame(exp(coef(model_azith)))


#for assess test quality
correct_gamma_aic(model_azith)
summary(model_azith)$deviance
summary(model_azith)$null.deviance
summary(model_azith)$df.residual
summary(model_azith)$df.null



#########


###Sulfamethoxazole & Trimethoprim

sulfTri <- RX_filtered %>%
  filter(unifm_prod_nm == "SULFAMETHOXAZOLE-TRIMETHOPRIM")

sulfTri$pri_spcl_desc <- relevel(sulfTri$pri_spcl_desc, ref = "NURSE PRACTITIONER")
sulfTri$provider_st_cd <- relevel(sulfTri$provider_st_cd, ref = "NY")
sulfTri$dosage_form_nm <- relevel(sulfTri$dosage_form_nm, ref = "TABLET")
sulfTri$age_cat <- relevel(sulfTri$age_cat, ref = "Adults")

model_sulfTri <- glm(
  estimated_quantity ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = sulfTri
)

summary(model_sulfTri)
as.data.frame(exp(coef(model_sulfTri)))


#for assess test quality
correct_gamma_aic(model_sulfTri)
summary(model_sulfTri)$deviance
summary(model_sulfTri)$null.deviance
summary(model_sulfTri)$df.residual
summary(model_sulfTri)$df.null