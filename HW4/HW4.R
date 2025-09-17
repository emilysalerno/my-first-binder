library(tidyverse)

table_a <- tibble(
  SKU = c(102345, 104567, 108912, 109876, 112233),
  Fruit = c("Apple", "Orange", "Mango", "Blueberry", "Watermelon"),
  Color = c("Red", "Orange", "Yellow", "Blue", "Green"),
  Price = c(1.20, 1.40, 1.70, 3.50, 4.40),
  In_Stock = c("Yes", "Yes", "No", "Yes", "No")
)

table_b <- tibble(
  SKU = c(102345, 105432, 106789, 104567, 107654),
  Fruit = c("Apple", "Banana", "Grape", "Orange", "Pear"),
  Color = c("Red", "Yellow", "Purple", "Orange", "Green"),
  Sale_Price = c(1.00, 0.50, 2.00, 1.20, 1.10),
  Number_in_Stock = c(50, 120, 0, 75, 0)
)

left_join_table <- left_join(table_a, table_b, by = "SKU")
print(left_join_table)

right_join_table <- right_join(table_a, table_b, by = "SKU")
print(right_join_table)

inner_join_table <- inner_join(table_a, table_b, by = "SKU")
print(inner_join_table)

full_join_table <- full_join(table_a, table_b, by = "SKU")
print(full_join_table)

anti_join_table <- anti_join(table_a, table_b, by = "SKU")
print(anti_join_table)

demo <- read.csv("C:/Users/emily/Desktop/BIOS512/HW4/demographics.csv")
full <- read.csv("C:/Users/emily/Desktop/BIOS512/HW4/full.csv")
hospitals <- read.csv("C:/Users/emily/Desktop/BIOS512/HW4/hospitals.csv")
patients <- read.csv("C:/Users/emily/Desktop/BIOS512/HW4/patient_names.csv")
treat <- read.csv("C:/Users/emily/Desktop/BIOS512/HW4/treatment_info.csv")

glimpse(demo)
glimpse(full)
glimpse(hospitals)
glimpse(patients)
glimpse(treat)

full_char  <- full %>%
  mutate(across(everything(), as.character))

full_char_long <- full_char %>%
  pivot_longer(
    cols = age:patient_zipcode,  
    names_to = "variable",
    values_to = "value"
  )
print(full_char_long)

full_char_long %>% 
  group_by(name) %>%  
  tally() %>% 
  arrange(desc(n))

##QUESTION 4
#first pivoted just character, just numeric, then just date in separate data sets
#after that joined with a full join 
#row number part is after each pivvot 
#step 3 do after a full join

#character
id_cols <- c("patient_id","name")

full <- full %>%
  mutate(across(c(admission_date,release_date),as.Date))
full <- full %>%
   mutate(across(c(patient_id),as.character))

character_long <- full %>%
  select(all_of(id_cols),where(is.character)) %>%
  pivot_longer(
      cols = -all_of(id_cols),
      names_to = "var_char",
      values_to = "val_char"
  )

character_long <- character_long %>%
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()


#number


numeric_long <- full %>%
  select(all_of(id_cols),where(is.numeric)) %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "var_num",
    values_to = "val_num"
  )

numeric_long <- numeric_long %>% 
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()


#date  
date_long <- full %>%
  select(all_of(id_cols),where(is.Date)) %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "var_date",
    values_to = "val_date"
  )

date_long <- date_long %>%
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()

#joining
joinone <- full_join(character_long, numeric_long, by = c("patient_id","name"))
print(joinone)

jointwo <- full_join(joinone, date_long, by = c("patient_id","name"))
print(joinetwo)

jointwo %>% 
  group_by(name,patient_id) %>%  
  tally() %>% 
  arrange(desc(n))

##Question 5
treatment_location <- left_join(patients, hospitals,  by = c("hospital_id"))

##question 6
table6 <- left_join(patients, demo,  by = c("patient_id"))
table7 <- left_join(table6, treat, by=c("condition_id"))
summary_table <- table7 %>%
  select(patient_id, name, age, gender, condition, treatment)

#graveyard
character_long <- character_long %>%
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()

numeric_long <- numeric_long %>% 
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()

date_long <- date_long %>%
  group_by(patient_id, name) %>%
  mutate(row = row_number()) %>%
  ungroup()