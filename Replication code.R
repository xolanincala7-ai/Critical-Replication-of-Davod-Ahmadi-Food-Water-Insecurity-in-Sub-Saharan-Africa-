library(haven) # Loading package to read .sav file
library(dplyr) #loading the dplyr package to help with selecting varibles from
                #the large Afro data set


Afrobaro.Roun6.Data <- read_sav("Afr.roun6.data.36coun.sav", encoding = "latin1") #loading the data onto data frame




# Convert labelled COUNTRY variable to character labels
Afrobaro.Roun6.Data<- Afrobaro.Roun6.Data |> 
  mutate(COUNTRY_NAME = as.character(as_factor(COUNTRY))) |>
  select(COUNTRY_NAME, everything()) 




selected_countries <- c(
  "Cape Verde", "Mauritius", "South Africa", "Botswana", "Namibia", "Gabon", "Zambia",
  "Ghana", "Nigeria", "Swaziland", "Senegal", "Tanzania", "Guinea", "Cote d'Ivoire",
  "Zimbabwe", "Kenya", "Cameroon", "Benin", "Lesotho", "Mali", "Sudan", "Burkina Faso",
  "Togo", "Sierra Leone", "Uganda", "Madagascar", "Niger", "Mozambique", "Malawi",
  "SÃ£o TomÃ© and PrÃ­ncipe", "Liberia", "Burundi"
)

selected_countries

Afr_roun6_subset32 <- Afrobaro.Roun6.Data |> 
  filter(COUNTRY_NAME %in% selected_countries)

Afr_roun6_subset32 |> 
  group_by(COUNTRY_NAME) |> 
  summarise(n = n()) |> 
  print(n = 32)







# Now we are selecting the data variables 
AFB.ROUND.6 <- Afr_roun6_subset32 |> 
  select(COUNTRY_NAME, ISO3, X2018, RESPNO, URBRUR, Q101, Q1, Q97, Q95, Q8E, EA_SVC_A, Q8B, Q8C, Q91A, Q91B, Q91C, Q91D,) |> 
  rename(
    country = COUNTRY_NAME,
    code = ISO3,
    gdp.ppp = x2018,
    resp.no = RESPNO,
    res.area = URBRUR,
    sex = Q101,
    Age = Q1,
    education = Q97,
    employment.status = Q95,
    income.available = Q8E,
    electricity = EA_SVC_A,
    water.insecurity = Q8B,
    food.insecurity = Q8C,
    radio = Q91A,
    tv = Q91B,
    car.or.bike = Q91C,
    phone = Q91D
  )

table(Afr_roun6_subset32$Q8A) 

  