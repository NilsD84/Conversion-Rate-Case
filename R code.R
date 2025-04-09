# Loading in the relevant libraries
library(mice)
library(RPostgres)
library(gplots)
library(dplyr)
library(rstudioapi)
library(ggplot2)
library(psych)
library(scales)


#Import the queried Wehkamp data ----
CustomerConversion <- read.csv("C:/Users/nilsd/Downloads/Wehkamp_Final.csv")
CustomerConversion_original <- read.csv("C:/Users/nilsd/Downloads/Wehkamp_Final.csv")

#Add a concatenation of internet_session_id and customer_id to get a unique id per real session
CustomerConversion$actual_session_id <- paste0(CustomerConversion$internet_session_id, CustomerConversion$customer_id)

#Replace Null Text values with NA
CustomerConversion[CustomerConversion == "NULL"] <- NA

#Unhash the gender_code
CustomerConversion <- CustomerConversion %>%
  mutate(gender_code = case_when(
    gender_code == "mutKSpyMJM_ZQCATL6NlLjlnF3Y" ~ "Female",
    gender_code == "MkoiQQyrlgoFB0aa4gKu4zm73_w" ~ "Male",
    gender_code == "L8ptf3Nk2ejeZuxuRSeUD2EHDT4" ~ "Other",
    TRUE ~ gender_code
  ))

#Transforming text categories to numerical ----
CustomerConversion <- CustomerConversion %>%
  mutate(device_category_desc = case_when(
    device_category_desc == "mobile" ~ 1,
    device_category_desc == "desktop" ~ 2,
    device_category_desc == "tablet" ~ 3
  ))

CustomerConversion <- CustomerConversion %>%
  mutate(gender_code = case_when(
    gender_code == "Male" ~ 1,
    gender_code == "Female" ~ 2,
    gender_code == "Other" ~ 3,
  ))

CustomerConversion <- CustomerConversion %>%
  mutate(geom_household_age = case_when(
    geom_household_age == "0. Unknown" ~ 0,
    geom_household_age == "1. < 25 years" ~ 1,
    geom_household_age == "2. 25 - 29 years" ~ 2,
    geom_household_age == "3. 30 - 34 years" ~ 3,
    geom_household_age == "4. 35 - 39 years" ~ 4,
    geom_household_age == "5. 40 - 44 years" ~ 5,
    geom_household_age == "6. 45 - 49 years" ~ 6,
    geom_household_age == "7. 50 - 54 years" ~ 7,
    geom_household_age == "8. 55 - 59 years" ~ 8,
    geom_household_age == "9. 60 - 64 years" ~ 9,
    geom_household_age == "10. 65 - 69 years" ~ 10,
    geom_household_age == "11. 70 - 74 years" ~ 11,
    geom_household_age == "12. 75 - 79 years" ~ 12,
    geom_household_age == "13. >= 80 years" ~ 13
  ))

CustomerConversion <- CustomerConversion %>%
  mutate(geom_household_income = case_when(
    geom_household_income == "0. Unknown" ~ 0,
    geom_household_income == "1. < 18,000" ~ 1,
    geom_household_income == "2. 18,000 - 26,000" ~ 2,
    geom_household_income == "3. 26,000 - 35,000" ~ 3,
    geom_household_income == "4. 35,000 - 50,000" ~ 4,
    geom_household_income == "5. 50,000 - 75,000" ~ 5,
    geom_household_income == "6. 75,000 - 100,000" ~ 6,
    geom_household_income == "7. 100,000 - 200,000" ~ 7,
    geom_household_income == "8. >= 200,000" ~ 8
  ))

CustomerConversion <- CustomerConversion %>%
  mutate(geom_consumption_frequency = case_when(
    geom_consumption_frequency == "0. Unknown" ~ 0,
    geom_consumption_frequency == "1. Little" ~ 1,
    geom_consumption_frequency == "2. Average" ~ 2,
    geom_consumption_frequency == "3. Much" ~ 3
  ))

CustomerConversion <- CustomerConversion %>%
  mutate(geom_clothing_budget = case_when(
    geom_clothing_budget == "0. Unknown" ~ 0,
    geom_clothing_budget == "1. Little" ~ 1,
    geom_clothing_budget == "2. Below average" ~ 2,
    geom_clothing_budget == "3. Average" ~ 3,
    geom_clothing_budget == "4. Above average" ~ 4,
    geom_clothing_budget == "5. Much" ~ 5
  ))
CustomerConversion_original <- CustomerConversion

#Check for missing values in some columns and plot ----
md.pattern(CustomerConversion[, c("internet_session_id", "customer_id", "device_category_desc", "gender_code", "geom_household_age", "geom_household_age","geom_household_income","geom_consumption_frequency")])

# MICE to deal with missing values ----
# Load the mice package
library(mice)

# Assuming CustomerConversion is your data frame
# Convert columns 8, 9, and 10 to factors
CustomerConversion[, 7] <- as.factor(CustomerConversion[, 7])
CustomerConversion[, 8] <- as.factor(CustomerConversion[, 8])
CustomerConversion[, 9] <- as.factor(CustomerConversion[, 9])

# Define the number of columns
num_columns <- ncol(CustomerConversion)

# Create an empty predictor matrix
PredictorMatrix <- matrix(0, nrow = num_columns, ncol = num_columns)


# Allow columns 7, 8, and 9 to predict column 12
PredictorMatrix[7, 12] <- 1
PredictorMatrix[8, 12] <- 1
PredictorMatrix[9, 12] <- 1


# Set column 12 to predict 11:16
PredictorMatrix[12, 11:16] <- 1

# Allow columns 11 to 16 to predict each other
PredictorMatrix[11:16, 11:16] <- 1   
diag(PredictorMatrix)[11:16] <- 0

PredictorMatrix[11, 12] <- 0
PredictorMatrix[13, 12] <- 0
PredictorMatrix[14, 12] <- 0
PredictorMatrix[15, 12] <- 0
PredictorMatrix[16, 12] <- 0

# Print the predictor matrix
print(PredictorMatrix)

# Perform MICE imputation
MiceImputedData <- mice(CustomerConversion, m = 5, maxit = 5, method = 'pmm', predictorMatrix = PredictorMatrix, seed = 500)

# Summary of imputed data
summary(MiceImputedData)


sum(is.na(MiceImputedData))
complete(MiceImputedData)
CustomerConversion <- complete(MiceImputedData)
summary(complete(MiceImputedData))
#Check the table
summary(CustomerConversion)
summary(CustomerConversion_original)
head(CustomerConversion)


# Read the CSV directly from the URL, filter, transpose, and process the data ----
Covid_cases_Table <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  subset(Country.Region == "Netherlands" & Province.State == "") %>%
  { data.frame(t(.[,-c(1:4)])) } %>%
  transform(Date = as.Date(row.names(.), format = "X%m.%d.%y"))


#Import New Weather table and convert to right date format, only selecting the wanted columns ----
new_weather_table <- readxl::read_xlsx("/Users/nilsd/Downloads/KNMI Weather Table.xlsx")
new_weather_table$YYYYMMDD <- as.Date.character(new_weather_table$YYYYMMDD, format ="%Y%m%d")
new_weather_table <- new_weather_table[, c("YYYYMMDD", "TG", "FG", "SQ", "RH", "NG", "TX","TN")]

#Convert session_date to Date type
CustomerConversion$session_date<- as.Date(CustomerConversion$session_date)



#Join the weather table onto the CustomerConversion Table
Conversion_Weather <- left_join(x = CustomerConversion, y = new_weather_table, by = c("session_date" = "YYYYMMDD"))

#Join covid table on weather and Conversiontable 
Merged_Table <- left_join(x = Conversion_Weather, y = Covid_cases_Table, by = c("session_date" = "Date"))

#Renaming the Weather columns
Merged_Table <- Merged_Table %>%
  rename(mean_temp = TG) %>%
  rename(max_temp = TX) %>%
  rename(min_temp = TN) %>%
  rename(mean_cloud_cover = NG) %>%
  rename(sunshine_duration = SQ) %>%
  rename(precipitation_total = RH) %>%
  rename(wind_gust_speed = FG)

#Rename X201
Merged_Table <- rename(Merged_Table, Cumulative_Covid_Cases = X201)

#Add Year-month, Year, Day of the week and Year-Week column to the dataset
Merged_Table$Month <- format(Merged_Table$session_date, "%Y-%m")
Merged_Table$Year <- format(Merged_Table$session_date, "%Y")
Merged_Table$DayOfTheWeek <- weekdays(Merged_Table$session_date)
Merged_Table$DayOfTheWeekNumber <- as.numeric(factor(
  Merged_Table$DayOfTheWeek, 
  levels = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag")
))
Merged_Table$YearWeek <- format(as.Date(Merged_Table$session_date, format = "%Y%m%d"), "%Y-%V")


#Import Google Trend data ----
googletrend <- read.csv("/Users/nilsd/Downloads/multiTimeline-Google Trend.csv")
head(googletrend)

#Add a Year-week column to make joining on the Merged_Table possible
googletrend$YearWeek <- format(as.Date(googletrend$Week), "%Y-%V")

#Left join GoogleTrends onto the Merged_Table
Merged_Table <- left_join(x = Merged_Table, y = googletrend, by = "YearWeek")

n_distinct(Merged_Table$actual_session_id)
n_distinct(Merged_Table$internet_session_id)


#Check for average conversion rates per year and for the total dataset ----
Conversion_FullDataSetTest <- Merged_Table %>%
  group_by(actual_session_id, Year) %>%
  summarise(
    Conversion = max(conversion)
  )

data_2021 <- Conversion_FullDataSetTest[Conversion_FullDataSetTest$Year == "2021", ]
data_2022 <- Conversion_FullDataSetTest[Conversion_FullDataSetTest$Year == "2022", ]
summary(Conversion_FullDataSetTest)
summary(data_2021)
sum(data_2021$Conversion)
summary(data_2022)
sum(data_2022$Conversion)

#Check head and summary of Merged_Table
head(Merged_Table)
summary(Merged_Table)
describe(Merged_Table)

#Hypothesis 1 ----
actual_session_table <- CustomerConversion %>%
  arrange(desc(conversion)) %>%
  distinct(actual_session_id, .keep_all = TRUE)

actual_session_table$gender_code<- as.factor(actual_session_table$gender_code)
model3 <- lm(conversion ~ gender_code, data = actual_session_table)

summary(model3)

#Aggregate the total conversion and total sessions per gender
ConversionRatesPerGender <- actual_session_table %>%
  group_by(gender_code) %>%
  summarise(
    Conversion = sum(conversion),
    Sessions = length(unique(actual_session_id)),
    Conversion_Rate = signif((sum(conversion) / length(unique(actual_session_id))),4)
  )

#Plot the conversion rate per gender
ggplot(ConversionRatesPerGender, 
       aes(x = factor(gender_code),
           y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Conversion_Rate)), vjust = -0.5, color = "black") +
  labs(title = "Conversion Rates per GenderCode", x = "Gender", y = "Conversion Rates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

ggplot(data = actual_session_table, aes(x = gender_code)) +
  geom_bar(fill = "blue") +  # Creates a bar plot
  labs(title = "Distribution of Gender Codes",
       x = "Gender Code",  # Label for the x-axis
       y = "Count") +      # Label for the y-axis
  theme_minimal() +      # A clean theme
  scale_x_discrete(labels = c("1" = "1: Male", 
                              "2" = "2: Female", 
                              "3" = "3: Other")) +  geom_text(stat = "count", aes(label = ..count..), 
                                                              vjust = -0.5,  # Adjust vertical position of text
                                                              color = "black")  # Color of the text

#Hypothesis 2 ----
actual_session_table <- CustomerConversion %>%
  arrange(desc(conversion)) %>%
  distinct(actual_session_id, .keep_all = TRUE)
summary(actual_session_table)

actual_session_table$geom_household_income <- as.factor(actual_session_table$geom_household_income)


chisq_income_conversion <- chisq.test(table(actual_session_table$geom_household_income, actual_session_table$conversion))
print(chisq_income_conversion)


model <- lm(conversion ~ geom_household_income, data = actual_session_table)

summary(model)


#Aggregate the total conversion and total sessions per household_income
ConversionRatesPerHouseholdIncome <- actual_session_table %>%
  group_by(geom_household_income) %>%
  summarise(
    Conversion = sum(conversion),
    Sessions = length(unique(actual_session_id)),
    Conversion_Rate = signif((sum(conversion) / length(unique(actual_session_id))),4)
  )

#Plot the conversion rate per Household Income
ggplot(ConversionRatesPerHouseholdIncome, 
       aes(x = factor(geom_household_income),
           y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Conversion_Rate)), vjust = -0.5, color = "black") +
  labs(title = "Conversion Rates per Geom_Household_Income", x = "Geom_Household_Income", y = "Conversion Rates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

#Distribution of household incomes
ggplot(data = actual_session_table, aes(x = geom_household_income)) +
  geom_bar(fill = "blue") +  # Creates a bar plot
  labs(title = "Distribution of Geom_Household_Income",
       x = "Geom_Household_Income",  # Label for the x-axis
       y = "Count") +      # Label for the y-axis
  theme_minimal() +      # A clean theme
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5,  # Adjust vertical position of text
            color = "black")  # Color of the text


#Hypothesis 3 ----
actual_session_table <- CustomerConversion %>%
  arrange(desc(conversion)) %>%
  distinct(actual_session_id, .keep_all = TRUE)
summary(actual_session_table)

actual_session_table$geom_consumption_frequency <- as.factor(actual_session_table$geom_consumption_frequency)

S

model_consumption <- lm(conversion ~ geom_consumption_frequency, data = actual_session_table)

#Aggregate the total conversion and total sessions per consumption frequency
ConversionRatesPerConsumptionFrequency <- actual_session_table %>%
  group_by(geom_consumption_frequency) %>%
  summarise(
    Conversion = sum(conversion),
    Sessions = length(unique(actual_session_id)),
    Conversion_Rate = signif((sum(conversion) / length(unique(actual_session_id))),4)
  )

#Plot the conversion rate per consumption frequency
ggplot(ConversionRatesPerConsumptionFrequency, 
       aes(x = factor(geom_consumption_frequency),
           y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Conversion_Rate)), vjust = -0.5, color = "black") +
  labs(title = "Conversion Rates per geom_consumption_frequency", x = "geom_consumption_frequency", y = "Conversion Rates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10)) + scale_x_discrete(labels = c(
    "0" = "0: Unknown", "1" = "1: Little", 
    "2" = "2: Average", 
    "3" = "3: Much"))
# Plot the total conversion per consumption frequency without percent labels
ggplot(ConversionRatesPerConsumptionFrequency, 
       aes(x = factor(geom_consumption_frequency),
           y = Conversion)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Conversion), vjust = -0.5, color = "black") +  # Display raw numbers
  labs(title = "Conversion per Consumption Frequency", 
       x = "Consumption Frequency", 
       y = "Conversion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10)) + 
  scale_x_discrete(labels = c(
    "0" = "0: Unknown", 
    "1" = "1: Little", 
    "2" = "2: Average", 
    "3" = "3: Much"))



#Distribution of consumption frequency
ggplot(data = actual_session_table, aes(x = geom_consumption_frequency)) +
  geom_bar(fill = "blue") +  # Creates a bar plot
  labs(title = "Distribution of geom_consumption_frequency",
       x = "geom_consumption_frequency",  # Label for the x-axis
       y = "Count") +      # Label for the y-axis
  theme_minimal() +      # A clean theme
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5,  # Adjust vertical position of text
            color = "black") +  # Color of the text
  scale_x_discrete(labels = c(
    "0" = "0: Unknown", "1" = "1: Little", 
    "2" = "2: Average", 
    "3" = "3: Much"))

#Hypothesis 4 ----
Session_Covid_Merged_Table <- Merged_Table %>%
  group_by(actual_session_id) %>%
  summarise(Conversion = max(conversion),
            Sessions = length(unique(actual_session_id)),
            Covid = max(Cumulative_Covid_Cases),
            Day = max(session_date),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed),
  )

Day_Covid_Merged_Table <- Session_Covid_Merged_Table %>%
  group_by(Day) %>%
  summarise(Conversions = sum(Conversion), 
            Sessions = sum(Sessions),
            conversion_rate = sum(Conversion)/sum(Sessions),
            Covid = max(Covid),
            max_temp = mean(max_temp)
  )

Day_Covid_Merged_Table$Covid_increase <- Day_Covid_Merged_Table$Covid - lag(Day_Covid_Merged_Table$Covid,1)
H4 <- lm(conversion_rate ~ Covid_increase, data = Day_Covid_Merged_Table)
summary(H4)

ggplot(data = Day_Covid_Merged_Table)+
  geom_line(aes(x = Day, y=conversion_rate*100, color="conversion_rate"))+
  geom_line(aes(x = Day, y=Covid_increase/10000, color="Covid increase"))+
  labs(title = "Conversion rate per day with Covid new cases") +
  scale_y_continuous(
    name = "Conversion rate",
    sec.axis = sec_axis(~., name = "Covid counts (10k)")
  )+
  scale_color_manual(values = c("conversion_rate" = "black", "Covid increase" ="blue"))

# smoothen the outlier
Day_Covid_Merged_Table$Covid_increase_outlier <- Day_Covid_Merged_Table$Covid_increase
Day_Covid_Merged_Table$Covid_increase_outlier[403] <- mean(Day_Covid_Merged_Table$Covid_increase_outlier[402],Day_Covid_Merged_Table$Covid_increase_outlier[404])
H4_outlier <- lm(conversion_rate ~ Covid_increase_outlier, data = Day_Covid_Merged_Table)
summary(H4_outlier)

ggplot(data = Day_Covid_Merged_Table)+
  geom_line(aes(x = Day, y=conversion_rate*100, color="conversion_rate"))+
  geom_line(aes(x = Day, y=Covid_increase_outlier/10000, color="Covid increase"))+
  #  geom_line(aes(x = Day, y=Covid, color="Covid"))+
  labs(title = "Conversion rate per day with Covid new cases without outlier") +
  scale_y_continuous(
    name = "Conversion rate",
    sec.axis = sec_axis(~., name = "Covid counts (10k)")
  )+
  scale_color_manual(values = c("conversion_rate" = "black", "Covid increase" ="blue"))

Day_Covid_Merged_Table$std_covid <- (scale(Day_Covid_Merged_Table$Covid_increase_outlier))
Day_Covid_Merged_Table$std_max_temp <- (scale(Day_Covid_Merged_Table$max_temp))
summary(Day_Covid_Merged_Table$std_covid)
summary(Day_Covid_Merged_Table$std_max_temp)
H4_outlier_std <- lm(conversion_rate ~ std_covid + std_max_temp, data = Day_Covid_Merged_Table)
summary(H4_outlier_std)

#Hypothesis 5 ----
# Data Table Importation
setwd("C:/Users/nilsd/OneDrive/Documents/Data Engineering/Wehkamp Assignment")
CustomerConversion <- read.csv("Wehkamp_Final.csv")

# Brand name analysis ----
sum(is.na(CustomerConversion$brand_name))
num_brands <- CustomerConversion %>% 
  summarise(unique_brands = n_distinct(brand_name)) %>%
  pull(unique_brands)
cat("Number of unique brands:", num_brands, "\n")

#renaming hashed values to Brand#
unique_brands <- unique(CustomerConversion$brand_name)
new_names <- paste0("Brand", seq_along(unique_brands))
brand_mapping <- setNames(new_names, unique_brands)
CustomerConversion <- CustomerConversion %>%
  mutate(brand_name = brand_mapping[brand_name])

#table to see hatched names with Brand#
brand_mapping_df <- data.frame(
  Hashed_Brand_Value = unique_brands,
  Brand_Label = new_names
)
print(brand_mapping_df)


## chi square test

conversion_table_brand <- table(CustomerConversion$brand_name, CustomerConversion$conversion)
chi_test_brand <- chisq.test(conversion_table_brand)
print(chi_test_brand)

## test is significant, conversions differ across brands

# conversion rates per brand
brand_conversion_rates <- CustomerConversion %>%
  group_by(brand_name) %>%
  summarise(conversion_rate = mean(conversion) * 100) %>%
  arrange(desc(conversion_rate))
print(brand_conversion_rates)


# Bar plot of conversion rates per brand
ggplot(brand_conversion_rates, aes(x = reorder(brand_name, -conversion_rate), y = conversion_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Conversion Rates by Brand", x = "Brand", y = "Conversion Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization of brand distribution (Bar Plot)
ggplot(CustomerConversion, aes(x = brand_name)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Brand Distribution", x = "Brand", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization of conversion counts per brand 
conversion_counts <- CustomerConversion %>%
  group_by(brand_name, conversion) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(conversion_counts, aes(x = brand_name, y = count, fill = as.factor(conversion))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conversion Counts per Brand", x = "Brand", y = "Count", fill = "Conversion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hypothesis 6 ----
# H6: The conversion rate of beachwear increases when hot weather appears consecutively.
# the hot weather don't quite appears consecutively
library(vars)
library(lmtest)
actual_session_table <- CustomerConversion %>%
  arrange(desc(conversion)) %>%
  distinct(actual_session_id, .keep_all=TRUE)

Session_Weather_Merged_Table <- Merged_Table %>%
  group_by(actual_session_id) %>%
  summarise(Conversion = max(conversion),
            Sessions = length(unique(actual_session_id)),
            Day = max(session_date),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed),
  )

Day_Weather_Merged_Table <- Session_Weather_Merged_Table %>%
  group_by(Day) %>%
  summarise(Conversions = sum(Conversion), 
            Sessions = sum(Sessions),
            conversion_rate = sum(Conversion)/sum(Sessions),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed)
  )
Day_Weather_Merged_Table$hot_weather <- ifelse(Day_Weather_Merged_Table$max_temp>= quantile(Day_Weather_Merged_Table$max_temp, 0.90), 1, 0)
Day_Weather_Merged_Table$consecutive_hot_days <- ifelse(Day_Weather_Merged_Table$hot_weather==1 &  lag(Day_Weather_Merged_Table$hot_weather,1)==1,1,0)

t.test(conversion_rate ~ consecutive_hot_days, data=Day_Weather_Merged_Table)# insignificant

ggplot(data = Day_Weather_Merged_Table)+
  geom_line(aes(x = Day, y=conversion_rate*100, color="conversion_rate"))+
  geom_line(aes(x = Day, y=max_temp/10, color="max_temp"))+
  geom_line(aes(x = Day, y=max_temp*consecutive_hot_days/10, color = "consequtive_hot_days"))+
  labs(title = "Conversion rate per day with temperature and consequtive hot days") +
  scale_y_continuous(
    name = "Conversion rate (%)",
    sec.axis = sec_axis(~., name = "temperature (C)")
  )+
  scale_color_manual(values = c("conversion_rate" = "black", "max_temp" ="blue", "consequtive_hot_days"= "red"))

t.test(Conversions ~ consecutive_hot_days, data=Day_Weather_Merged_Table)
H6 <- lm(Conversions ~ consecutive_hot_days, data = Day_Weather_Merged_Table)
summary(H6) # significant

ggplot(data = Day_Weather_Merged_Table)+
  geom_line(aes(x = Day, y=Conversions, color="Conversions"))+
  geom_line(aes(x = Day, y=max_temp, color="max_temp"))+
  geom_line(aes(x = Day, y=max_temp*consecutive_hot_days, color = "consecutive_hot_days"))+
  labs(title = "Conversions per day with temperature and consecutive hot days") +
  scale_y_continuous(
    name = "Conversion (times)",
    sec.axis = sec_axis(~./10, name = "temperature (C)")
  )+
  scale_color_manual(values = c("Conversions" = "black", "max_temp" ="blue", "consecutive_hot_days"= "red"))

#Hypothesis 7 ----
# H7: The conversion rate of beachwear is positively related to temperature.
Session_Weather_Merged_Table <- Merged_Table %>%
  group_by(actual_session_id) %>%
  summarise(Conversion = max(conversion),
            Sessions = length(unique(actual_session_id)),
            Day = max(session_date),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed),
  )

Day_Weather_Merged_Table <- Session_Weather_Merged_Table %>%
  group_by(Day) %>%
  summarise(Conversions = sum(Conversion), 
            Sessions = sum(Sessions),
            conversion_rate = sum(Conversion)/sum(Sessions),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed)
  )

H7 <- lm(conversion_rate ~ max_temp, data = Day_Weather_Merged_Table)
summary(H7)
ggplot(data = Day_Weather_Merged_Table)+
  geom_line(aes(x = Day, y=conversion_rate*100, color="conversion_rate"))+
  geom_line(aes(x = Day, y=max_temp/10, color="max_temp"))+
  labs(title = "Conversion rate per day with temperature") +
  scale_y_continuous(
    name = "Conversion rate",
    sec.axis = sec_axis(~., name = "temperature")
  )+
  scale_color_manual(values = c("conversion_rate" = "black", "max_temp" ="blue"))

# 
Day_Weather_Merged_Table$hot_weather <- ifelse(Day_Weather_Merged_Table$max_temp>= quantile(Day_Weather_Merged_Table$max_temp, 0.90), 1, 0)
H7_hot <- lm(conversion_rate ~ max_temp*consecutive_hot_days, data = Day_Weather_Merged_Table)
summary(H7_hot)


# Granger causality tests
n <- 20
P_array <- c(0,0)
x <- c(1:n)
for (i in 1:n) {
  pvalue <- grangertest(conversion_rate ~ max_temp, order = i, data = Day_Weather_Merged_Table)
  P_array[i] <- pvalue[2,4]
}
plot(x, P_array, xlim = c(0,10), ylim = c(0,0.2), xlab="lags", ylab="P-value", main="Granger: Max temp on Conversion rate")
min(P_array)
which.min(P_array)

#Hypothesis 8 ----
# H8: The conversion rate of beachwear is positively related to sunlight.
Session_Weather_Merged_Table <- Merged_Table %>%
  group_by(actual_session_id) %>%
  summarise(Conversion = max(conversion),
            Sessions = length(unique(actual_session_id)),
            Day = max(session_date),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed),
  )

Day_Weather_Merged_Table <- Session_Weather_Merged_Table %>%
  group_by(Day) %>%
  summarise(Conversions = sum(Conversion), 
            Sessions = sum(Sessions),
            conversion_rate = sum(Conversion)/sum(Sessions),
            mean_temp = mean(mean_temp),
            max_temp = mean(max_temp),
            min_temp = mean(min_temp),
            sunshine_duration = mean(sunshine_duration),
            precipitation_total = mean(precipitation_total),
            wind_gust_speed = mean(wind_gust_speed)
  )
Day_Weather_Merged_Table$hot_weather <- ifelse(Day_Weather_Merged_Table$max_temp>= quantile(Day_Weather_Merged_Table$max_temp, 0.90), 1, 0)
Day_Weather_Merged_Table$consecutive_hot_days <- ifelse(Day_Weather_Merged_Table$hot_weather==1 &  lag(Day_Weather_Merged_Table$hot_weather,1)==1,1,0)

H8_rate <- lm(conversion_rate ~ sunshine_duration, data = Day_Weather_Merged_Table)
summary(H8_rate)

Day_Weather_Merged_Table$max_temp_std <- scale(Day_Weather_Merged_Table$max_temp)
Day_Weather_Merged_Table$sunshine_duration_std <- scale(Day_Weather_Merged_Table$sunshine_duration)

Hweather <- lm(conversion_rate ~ sunshine_duration + max_temp*consecutive_hot_days, data = Day_Weather_Merged_Table)
summary(Hweather)

Hweather_std <- lm(conversion_rate ~ sunshine_duration_std + max_temp_std*consecutive_hot_days, data = Day_Weather_Merged_Table)
summary(Hweather_std)

ggplot(data = Day_Weather_Merged_Table)+
  geom_line(aes(x = Day, y=conversion_rate*100, color="conversion_rate"))+
  geom_line(aes(x = Day, y=sunshine_duration/10, color="Sunshine_duration"))+
  labs(title = "Conversion rate per day with Sunshine duration") +
  scale_y_continuous(
    name = "Conversion rate (%)",
    sec.axis = sec_axis(~., name = "Time (hour)")
  )+
  scale_color_manual(values = c("conversion_rate" = "black", "Sunshine_duration" ="blue"))

#Hypothesis 9 ----
#H9: Customer purchases predominantly occur during the first half of the week compared to the later.

#Create a summarized table to do the analysis
#Create a summarized table to plot conversions per session per day of the week
Conversion_SDW <- Merged_Table %>%
  group_by(actual_session_id,DayOfTheWeekNumber) %>%
  summarise(
    Conversion = max(conversion),
    Views = sum(pdview)
  )

#Aggregate the total conversion and total sessions per day of the week
ConversionRatesPerDayOfTheWeek <- Conversion_SDW %>%
  group_by(DayOfTheWeekNumber) %>%
  summarise(
    Conversion = sum(Conversion),
    Sessions = length(unique(actual_session_id)),
    Conversion_Rate = signif((sum(Conversion) / length(unique(actual_session_id))),4)
  )

# Convert DayOfTheWeekNumber to a factor
Conversion_SDW$DayOfTheWeekNumber <- as.factor(Conversion_SDW$DayOfTheWeekNumber)

# Fitting the linear model again
model <- lm(Conversion ~ DayOfTheWeekNumber, data = Conversion_SDW)

# Display the summary of the model
summary(model)

#Plot the conversion rate per day of the week
ggplot(ConversionRatesPerDayOfTheWeek, 
       aes(x = factor(DayOfTheWeekNumber, levels = c("1", "2", "3", 
                                                     "4", "5", "6", "7")),
           y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Conversion_Rate)), vjust = -0.5, color = "black") +
  labs(title = "Conversion Rates per Day of the Week", x = "Day of the Week", y = "Conversion Rates") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))


#Hypothesis 10 ---
#Look at total conversion and sessions
sum(actual_session_table$conversion)
nrow(actual_session_table)

# Chisq test - Significant (p-value<0.05)
actual_session_table$device_category_desc <- as.factor(actual_session_table$device_category_desc)

conversion_device <- table(actual_session_table$device_category_desc, actual_session_table$conversion)
chi_test_device <- chisq.test(conversion_device)
print(chi_test_device)

#Aggregate the total conversion and total sessions per device
device_conversion <- actual_session_table %>%
  group_by(device_category_desc) %>%
  summarise(
    Conversion = sum(conversion),
    Sessions = length(unique(actual_session_id)),
    Conversion_Rate = signif((sum(conversion) / length(unique(actual_session_id))),4)
  )

#Convert the device category to a factor
device_conversion$device_category_desc <- as.factor(device_conversion$device_category_desc)

# Bar plot of conversion rates based on Device used
ggplot(device_conversion, 
       aes(x = factor(device_category_desc),
           y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Conversion_Rate)), vjust = -0.5, color = "black") +
  labs(title = "Conversion Rates per Device", x = "Device", y = "Conversion Rates") + 
  scale_x_discrete(labels = c("1" = "1: Mobile", 
                              "2" = "2: Desktop", 
                              "3" = "3: Tablet"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

#Plot the total sessions per device 
ggplot(data = actual_session_table, aes(x = device_category_desc)) +
  geom_bar(fill = "blue") +  # Creates a bar plot
  labs(title = "Distribution of Device",
       x = "Gender Code",  # Label for the x-axis
       y = "Count") +      # Label for the y-axis
  theme_minimal() +      # A clean theme
  scale_x_discrete(labels = c("1" = "1: Mobile", 
                              "2" = "2: Desktop", 
                              "3" = "3: Tablet")) +  geom_text(stat = "count", aes(label = ..count..), 
                                                               vjust = -0.5,  # Adjust vertical position of text
                                                               color = "black")  # Color of the text

#Hypothesis 11 ----

# Data Inspection
CustomerConversion %>% count(max_temp)

actual_session_table <- CustomerConversion %>%
  arrange(desc(conversion)) %>%
  distinct(actual_session_id, .keep_all = TRUE)

sum(actual_session_table$conversion)
nrow(actual_session_table)


# Check data types
str(actual_session_table$bikini)
str(actual_session_table$zwembroek)
str(actual_session_table$conversion)

# Convert conversion to numeric
actual_session_table$conversion <- as.numeric(as.character(actual_session_table$conversion))
actual_session_table$zwembroek <- as.numeric(as.character(actual_session_table$zwembroek))
actual_session_table$bikini <- as.numeric(as.character(actual_session_table$bikini))


# Plot the data (optional)
plot(actual_session_table$bikini, actual_session_table$conversion, main = "Google Trends - bikini vs. Conversion",
     xlab = "Google Search Trends", ylab = "Conversion (1 = Yes, 0 = No)")

# Perform the point-biserial correlation test
cor_test_result_bikini <- cor.test(actual_session_table$bikini, actual_session_table$conversion, method = "pearson")
cor_test_result_zwembroek <- cor.test(actual_session_table$zwembroek, actual_session_table$conversion, method = "pearson")

# Display the results
print(cor_test_result_bikini)
print(cor_test_result_zwembroek)


# Convert conversion to numeric
actual_session_table$Wehkamp <- as.numeric(as.character(actual_session_table$Wehkamp))
actual_session_table$Zalando <- as.numeric(as.character(actual_session_table$Zalando))
actual_session_table$CA <- as.numeric(as.character(actual_session_table$CA))


# Perform the point-biserial correlation test
cor_test_result_wehkamp <- cor.test(actual_session_table$Wehkamp, actual_session_table$conversion, method = "pearson")
cor_test_result_zalando <- cor.test(actual_session_table$Zalando, actual_session_table$conversion, method = "pearson")
cor_test_result_ca <- cor.test(actual_session_table$CA, actual_session_table$conversion, method = "pearson")

# Display the results
print(cor_test_result_wehkamp)
print(cor_test_result_zalando)
print(cor_test_result_ca)

