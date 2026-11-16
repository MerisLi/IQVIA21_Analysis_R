#function for getting the correct AIC for gamma
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
RX_filtered$dosage_form_nm <- factor(RX_filtered$dosage_form_nm)


#Amoxicillin
amox <- RX_filtered %>%
  filter(unifm_prod_nm == "AMOXICILLIN")

amox$pri_spcl_desc <- relevel(amox$pri_spcl_desc, ref = "NURSE PRACTITIONER")
amox$provider_st_cd <- relevel(amox$provider_st_cd, ref = "NY")
amox$age_cat <- relevel(amox$age_cat, ref = "Adults")
amox$dosage_form_nm <- relevel(amox$dosage_form_nm, ref = "CAPSULE")



model_amox <- glm(
  dspnsd_qty ~
    age_cat+
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = amox
)

summary(model_amox)
data.frame(exp(coef(model_amox)))


#for assess test quality
correct_gamma_aic(model_amox)
summary(model_amox)$deviance
summary(model_amox)$null.deviance

table(amox$unifm_prod_nm)
table(amox$age_cat)
table(amox$pri_spcl_desc)
table(amox$provider_st_cd)



####################

#Cephalexin

cepha <- RX_filtered %>%
  filter(unifm_prod_nm == "CEPHALEXIN")

cepha$pri_spcl_desc <- relevel(cepha$pri_spcl_desc, ref = "NURSE PRACTITIONER")
cepha$provider_st_cd <- relevel(cepha$provider_st_cd, ref = "NY")
cepha$age_cat <- relevel(cepha$age_cat, ref = "Adults")
cepha$dosage_form_nm <- relevel(cepha$dosage_form_nm, ref = "CAPSULE")



model_cepha <- glm(
  dspnsd_qty ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = cepha
)

summary(model_cepha)
data.frame(exp(coef(model_cepha)))


#for assess test quality
correct_gamma_aic(model_cepha)
summary(model_cepha)$deviance
summary(model_cepha)$null.deviance
summary(model_cepha)$df.residual
summary(model_cepha)$df.null

table(cepha$unifm_prod_nm)
table(cepha$age_cat)
table(cepha$pri_spcl_desc)
table(cepha$provider_st_cd)



####################

#Ciprofloxacin HCl only have one dosage form

###################

#Azithromycin

azith <- RX_filtered %>%
  filter(unifm_prod_nm == "AZITHROMYCIN")

azith$pri_spcl_desc <- relevel(azith$pri_spcl_desc, ref = "NURSE PRACTITIONER")
azith$provider_st_cd <- relevel(azith$provider_st_cd, ref = "NY")
azith$age_cat <- relevel(azith$age_cat, ref = "Adults")
azith$dosage_form_nm <- relevel(azith$dosage_form_nm, ref = "TABLET")


model_azith <- glm(
  dspnsd_qty ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = azith
)

summary(model_azith)
data.frame(exp(coef(model_azith)))


#for assess test quality
correct_gamma_aic(model_azith)
summary(model_azith)$deviance
summary(model_azith)$null.deviance
summary(model_azith)$df.residual
summary(model_azith)$df.null

table(azith$unifm_prod_nm)
table(azith$age_cat)
table(azith$pri_spcl_desc)
table(azith$provider_st_cd)



####################

#Sulfamethoxazole & Trimethoprim

sulfTri <- RX_filtered %>%
  filter(unifm_prod_nm == "SULFAMETHOXAZOLE-TRIMETHOPRIM")

sulfTri$pri_spcl_desc <- relevel(sulfTri$pri_spcl_desc, ref = "NURSE PRACTITIONER")
sulfTri$provider_st_cd <- relevel(sulfTri$provider_st_cd, ref = "NY")
sulfTri$age_cat <- relevel(sulfTri$age_cat, ref = "Adults")
sulfTri$dosage_form_nm <- relevel(sulfTri$dosage_form_nm, ref = "TABLET")



model_sulfTri <- glm(
  dspnsd_qty ~
    age_cat +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = sulfTri
)

summary(model_sulfTri)
data.frame(exp(coef(model_sulfTri)))


#for assess test quality
correct_gamma_aic(model_sulfTri)
summary(model_sulfTri)$deviance
summary(model_sulfTri)$null.deviance
summary(model_sulfTri)$df.residual
summary(model_sulfTri)$df.null

table(sulfTri$unifm_prod_nm)
table(sulfTri$age_cat)
table(sulfTri$pri_spcl_desc)
table(sulfTri$provider_st_cd)


#########################
df_stateCount <- data.frame(
  amox = as.data.frame(table(amox$provider_st_cd)),
  azith = as.data.frame(table(azith$provider_st_cd)),
  cepha = as.data.frame(table(cepha$provider_st_cd)),
  cipro = as.data.frame(table(cipro$provider_st_cd)),
  sulfTri = as.data.frame(table(sulfTri$provider_st_cd))
)
print(df_stateCount)
