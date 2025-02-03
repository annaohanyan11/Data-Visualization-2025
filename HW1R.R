
library(dplyr)
install.packages("lubridate")
library(lubridate)
list.files()

crime_data <- read.csv("/Users/annaohanyan/Downloads/crime_data.csv")

#Part1
# 1
head(crime_data, 5)

# 2
missing_counts <- colSums(is.na(crime_data))
columns_to_drop <- names(missing_counts[missing_counts > (0.5 * nrow(crime_data))])
crime_data_cleaned <- crime_data %>% select(-all_of(columns_to_drop))

# 3
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(DATE.OCC = as.character(DATE.OCC)) %>% 
  mutate(
    DATE_OCC = suppressWarnings(parse_date_time(DATE.OCC, orders = c("mdy", "dmy", "ymd"))),  
    Year = year(DATE_OCC),
    Month = month(DATE_OCC),
    Day = day(DATE_OCC),
    Hour = as.numeric(substr(TIME.OCC, 1, 2)) 
  )


# 4
crime_2023 <- crime_data_cleaned %>%
  filter(Year == 2023, Crm.Cd.Desc == "BURGLARY")
# 5
crime_summary <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(
    Total_Crimes = n(),
    Avg_Victim_Age = mean(Vict.Age, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Crimes))


print(crime_summary)

#Part2
# 1
crime_by_month <- crime_data_cleaned %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(Month)

print(crime_by_month)

# 2
weapon_crimes <- crime_data_cleaned %>%
  filter(!is.na(Weapon.Desc)) %>%
  summarise(Total_Crimes_With_Weapon = n())

print(weapon_crimes)

#print(colnames(crime_data_cleaned))

# 3
crime_by_premis <- crime_data_cleaned %>%
  group_by(Premis.Desc) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes))

print(crime_by_premis)

#Part4
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(Severity_Score = case_when(
    !is.na(Weapon.Desc) ~ 5,                  
    Crm.Cd.Desc == "BURGLARY" ~ 3,             
    TRUE ~ 1                                   
  ))

severity_by_area <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
  summarise(Total_Severity_Score = sum(Severity_Score, na.rm = TRUE)) 

print(severity_by_area)





