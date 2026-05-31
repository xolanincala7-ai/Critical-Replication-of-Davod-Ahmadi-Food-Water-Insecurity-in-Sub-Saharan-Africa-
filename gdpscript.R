ord_reg_data <- Afr_roun6_with_GDP[, c("ISO3", "URBRUR", "Q101", "Q1", "Q97", "Q95", "X2018", "Q8A", "Q8B", "Q8E", "asset_ownership")]

# Rename the columns
colnames(ord_reg_data) <- c("Countrycode",
                            "Area_of_Residence",
                            "Gender",
                            "Age",
                            "Education",
                            "Employment",
                            "GDP_PPP",
                            "Food_Insecurity",
                            "Water_Insecurity",
                            "Income_Insecurity",
                            "Asset_Ownership")
#start recoding urban rural
ord_reg_data <- ord_reg_data |> 
  mutate(Area_of_Residence = case_when(
    Area_of_Residence %in% c(1, 3, 460) ~ 0,  # Treat 1, 3, 460 as Urban
    Area_of_Residence == 2 ~ 1,              # Treat 2 as Rural
    TRUE ~ NA_real_                          # Drop anything else
  ),
  Area_of_Residence = factor(Area_of_Residence,
                             levels = c(0, 1),
                             labels = c("Urban", "Rural")))

#start recoding gender 

ord_reg_data <- ord_reg_data |> 
  mutate(Gender = case_when(
    Gender == 1 ~ 0,
    Gender == 2 ~ 1,
    TRUE ~ NA_real_
  ),
  Gender = factor(Gender,
                  levels = c(0, 1),
                  labels = c("Male", "Female")))




# TREAT AGE AS NUMERIC 

ord_reg_data <- ord_reg_data |> 
  mutate(Age= case_when(
    Age %in% c(-1, 998, 999) ~ NA_real_,
    Age  >= 18 & Age <= 105 ~ as.numeric(Age),
    TRUE ~ NA_real_
  ))


# Treat education as factor 

ord_reg_data <- ord_reg_data |> 
  mutate(Education = factor(case_when(
    Education %in% c(-1, 98, 99) ~ NA_real_,
    Education %in% c(0, 1) ~ 0,
    Education %in% c(2, 3) ~ 1,
    Education %in% c(4, 5) ~ 2,
    Education %in% c(6, 7, 8, 9) ~ 3,
    TRUE ~ NA_real_
  ),
  levels = 0:3,
  labels = c("No formal education", "Primary", "Secondary", "Post-secondary")))

# Treat employment as factor
ord_reg_data <- ord_reg_data |> 
  mutate(Employment = factor(case_when(
    Employment %in% c(-1, 9, 98) ~ NA_real_,
    Employment == 2 ~ 1,
    Employment %in% c(0, 1) ~ 0,
    Employment == 3 ~ 2,
    TRUE ~ NA_real_
  ),
  levels = 0:2,
  labels = c("Unemployed", "Employed part-time", "Employed full-time")))

#Treat food insecurity as ordinal
ord_reg_data <- ord_reg_data |> 
  mutate(Food_Insecurity = factor(case_when(
    Food_Insecurity %in% c(-1, 9, 98) ~ NA_real_,
    Food_Insecurity %in% 0:4 ~ Food_Insecurity,
    TRUE ~ NA_real_
  ),
  levels = 0:4,
  labels = c("Never", "Once or twice", "Several times", "Many times", "Always"),
  ordered = TRUE))

# Treat water insecurity as numeric likert scale 
ord_reg_data <- ord_reg_data |> 
  mutate(Water_Insecurity = case_when(
    Water_Insecurity%in% c(-1, 9, 98) ~ NA_real_,
    Water_Insecurity %in% 0:4 ~ Water_Insecurity,
    TRUE ~ NA_real_
  ))

# Treat Income insecurity as numeric likert scale
ord_reg_data <- ord_reg_data |> 
  mutate(Income_Insecurity = case_when(
    Income_Insecurity %in% c(-1, 9, 98) ~ NA_real_,
    Income_Insecurity %in% 0:4 ~ Income_Insecurity,
    TRUE ~ NA_real_
  ))

#Here we make sure our asset are numeric 
ord_reg_data <- ord_reg_data |> 
  mutate(Asset_Ownership= case_when(
    Asset_Ownership == "Owns none"    ~ 0,
    Asset_Ownership == "Owns 1"       ~ 1,
    Asset_Ownership == "Owns 2"       ~ 2,
    Asset_Ownership == "Owns 3"       ~ 3,
    Asset_Ownership == "Owns all 4"   ~ 4,
    TRUE ~ NA_real_
  ))


# Here we take the gdp_ppp and sort it as groups 
ord_reg_data <- ord_reg_data |> 
  mutate(GDP_PPP_Group = case_when(
    GDP_PPP >= 4000 ~ "GDP PPP ($4,000 and more)",
    GDP_PPP >= 2000 & GDP_PPP < 4000 ~ "GDP PPP ($2,000 to $3,999)",
    GDP_PPP >= 1500 & GDP_PPP < 2000 ~ "GDP PPP ($1,500 to $1,999)",
    TRUE ~ "GDP PPP (Less than $1,500)"
  ),
  GDP_PPP_Group = factor(GDP_PPP_Group,
                         levels = c(
                           "GDP PPP ($4,000 and more)",
                           "GDP PPP ($2,000 to $3,999)",
                           "GDP PPP ($1,500 to $1,999)",
                           "GDP PPP (Less than $1,500)"
                         )))

ibrary(MASS)





library(MASS)
model_ordinal <- polr(Food_Insecurity ~ 
                        Water_Insecurity + 
                        Gender + 
                        Area_of_Residence + 
                        Age + 
                        Employment + 
                        Income_Insecurity + 
                        Education + 
                        Asset_Ownership + 
                        GDP_PPP_Group,
                      data = ord_reg_data,
                      Hess = TRUE)



summary(model_ordinal)

# Compute odds ratios and 95% confidence intervals

ctable <- coef(summary(model_ordinal))

# Compute p-values using normal approximation
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# Combine into a tidy results table
results_table <- data.frame(
  Term = rownames(ctable),
  Estimate = ctable[, "Value"],
  Std.Error = ctable[, "Std. Error"],
  t.value = ctable[, "t value"],
  p.value = round(p_values, 4),
  Odds.Ratio = round(exp(ctable[, "Value"]), 3),
  CI.lower = round(exp(ctable[, "Value"] - 1.96 * ctable[, "Std. Error"]), 3),
  CI.upper = round(exp(ctable[, "Value"] + 1.96 * ctable[, "Std. Error"]), 3)
)

# Display results
print(results_table)

