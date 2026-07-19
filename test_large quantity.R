#Amoxicillin
RX_filtered <- RX%>%
  group_by(unifm_prod_nm, pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50)

RX_filtered$pri_spcl_desc <- factor(RX_filtered$pri_spcl_desc)
RX_filtered$provider_st_cd <- factor(RX_filtered$provider_st_cd)
RX_filtered$dosage_form_nm <- factor(RX_filtered$dosage_form_nm)


#unique(RX_filtered$unifm_prod_nm)

amox <- RX_filtered %>%
  filter(unifm_prod_nm == "AMOXICILLIN")

amox$pri_spcl_desc <- relevel(amox$pri_spcl_desc, ref = "NURSE PRACTITIONER")
amox$provider_st_cd <- relevel(amox$provider_st_cd, ref = "NY")
amox$dosage_form_nm <- relevel(amox$dosage_form_nm, ref = "CAPSULE")


model_amox <- glm(
  estimated_quantity ~
    pat_age_nbr +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = amox
)

summary(model_amox)
exp(coef(model_amox))



#for selecting ref
RX %>%
  filter(unifm_prod_nm == "AMOXICILLIN") %>%
  group_by(pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50) %>%
  ggplot(aes(x=unifm_prod_nm, y=rx_dosage_amt, color=dosage_form_nm))+
  geom_boxplot(width=0.5)+
  #facet_wrap(~ pri_spcl_desc)+
  theme(axis.text=element_text(size=7),
        strip.text = element_text(size = 7))+
  labs(title = "Distribution of RX amount across prescribing conditions",
       subtitle = "AMOXICILLIN")

RX_filtered%>%
  filter(unifm_prod_nm == "AMOXICILLIN") %>%
  group_by(dosage_form_nm) %>%
  summarize(count=n())



#for assess test quality
summary(model_amox)$aic
summary(model_amox)$deviance
summary(model_amox)$null.deviance

table(amox$unifm_prod_nm)
table(amox$pri_spcl_desc)
table(amox$provider_st_cd)
table(amox$dosage_form_nm)


summary(model_amox)$coefficients %>%
  as.data.frame() %>%
  filter(`Pr(>|z|)` < 0.05) 


####################

#Cephalexin

cepha <- RX_filtered %>%
  filter(unifm_prod_nm == "CEPHALEXIN")

cepha$pri_spcl_desc <- relevel(cepha$pri_spcl_desc, ref = "NURSE PRACTITIONER")
cepha$provider_st_cd <- relevel(cepha$provider_st_cd, ref = "NY")
cepha$dosage_form_nm <- relevel(cepha$dosage_form_nm, ref = "CAPSULE")


model_cepha <- glm(
  estimated_quantity ~
    pat_age_nbr +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = cepha
)

summary(model_cepha)
list(exp(coef(model_cepha)))


#for assess test quality
summary(model_cepha)$aic
summary(model_cepha)$deviance
summary(model_cepha)$null.deviance
summary(model_cepha)$df.residual
summary(model_cepha)$df.null


table(cepha$pri_spcl_desc)
table(cepha$provider_st_cd)
table(cepha$dosage_form_nm)


####################

#Ciprofloxacin HCl

RX %>%
  filter(unifm_prod_nm == "CIPROFLOXACIN HCL") %>%
  group_by(pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50) %>%
  ggplot(aes(x=unifm_prod_nm, y=rx_dosage_amt, color=dosage_form_nm))+
  geom_boxplot(width=0.5)+
  facet_wrap(~ pri_spcl_desc)+
  theme(axis.text=element_text(size=7),
        strip.text = element_text(size = 7))+
  labs(title = "Distribution of RX amount across prescribing conditions",
       subtitle = "CIPROFLOXACIN HCL")



####################

#Azithromycin

azith <- RX_filtered %>%
  filter(unifm_prod_nm == "AZITHROMYCIN")

azith$pri_spcl_desc <- relevel(azith$pri_spcl_desc, ref = "NURSE PRACTITIONER")
azith$provider_st_cd <- relevel(azith$provider_st_cd, ref = "NY")
azith$dosage_form_nm <- relevel(azith$dosage_form_nm, ref = "TABLET")

RX %>%
  filter(unifm_prod_nm == "AZITHROMYCIN") %>%
  group_by(pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50) %>%
  ggplot(aes(x=unifm_prod_nm, y=rx_dosage_amt, color=dosage_form_nm))+
  geom_boxplot(width=0.5)+
  facet_wrap(~ pri_spcl_desc)+
  theme(axis.text=element_text(size=7),
        strip.text = element_text(size = 7))+
  labs(title = "Distribution of RX amount across prescribing conditions",
       subtitle = "AZITHROMYCIN")

model_azith <- glm(
  estimated_quantity ~
    pat_age_nbr +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = azith
)

summary(model_azith)
list(exp(coef(model_azith)))



#for assess test quality
summary(model_azith)$aic
summary(model_azith)$deviance
summary(model_azith)$null.deviance
summary(model_azith)$df.residual
summary(model_azith)$df.null


table(azith$pri_spcl_desc)
table(azith$provider_st_cd)
table(azith$dosage_form_nm)



####################

#Sulfamethoxazole & Trimethoprim

sulfTri <- RX_filtered %>%
  filter(unifm_prod_nm == "SULFAMETHOXAZOLE-TRIMETHOPRIM")

sulfTri$pri_spcl_desc <- relevel(sulfTri$pri_spcl_desc, ref = "NURSE PRACTITIONER")
sulfTri$provider_st_cd <- relevel(sulfTri$provider_st_cd, ref = "NY")
sulfTri$dosage_form_nm <- relevel(sulfTri$dosage_form_nm, ref = "TABLET")

RX %>%
  filter(unifm_prod_nm == "SULFAMETHOXAZOLE-TRIMETHOPRIM") %>%
  group_by(pri_spcl_desc) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  filter(num >50) %>%
  ggplot(aes(x=unifm_prod_nm, y=rx_dosage_amt, color=dosage_form_nm))+
  geom_boxplot(width=0.5)+
  #facet_wrap(~ pri_spcl_desc)+
  theme(axis.text=element_text(size=7),
        strip.text = element_text(size = 7))+
  labs(title = "Distribution of RX amount across prescribing conditions",
       subtitle = "SULFAMETHOXAZOLE-TRIMETHOPRIM")

model_sulfTri <- glm(
  estimated_quantity ~
    pat_age_nbr +
    pri_spcl_desc +
    provider_st_cd +
    dosage_form_nm,
  family = Gamma(link = "log"),
  data = sulfTri
)

summary(model_sulfTri)
list(exp(coef(model_sulfTri)))



#for assess test quality
summary(model_sulfTri)$aic
summary(model_sulfTri)$deviance
summary(model_sulfTri)$null.deviance
summary(model_sulfTri)$df.residual
summary(model_sulfTri)$df.null


table(sulfTri$pri_spcl_desc)
table(sulfTri$provider_st_cd)
table(sulfTri$dosage_form_nm)

