options(repos = c(CRAN = "https://cran.r-project.org"))
install.packages("tidyverse", dependencies = TRUE)
install.packages("rmarkdown", dependencies = TRUE)
install.packages("IRkernel", dependencies = TRUE)
install.packages("shiny", dependencies = TRUE)

IRkernel::installspec(user = FALSE)

# Load UFO sightings data from a GitHub CSV
df <- read_csv("https://raw.githubusercontent.com/Vincent-Toups/bios512/refs/heads/main/nuforc_workshop/nuforc_sightings.csv")

# Read column names
names(df)

# Count the occurrences of each unique 'shape' value
unique_vals <- df$shape %>% table()

# Sort the counts of shapes in descending order and get the names
unique_vals %>% sort(decreasing = T) %>% names()

# Store column names in a vector
column_names <- names(df)

# Total number of rows in the dataset
n_total <- nrow(df)

# Loop over each column to get basic summary stats
for(col in column_names) {
  values <- df[[col]];        # Extract column
  n_na <- sum(is.na(values))  # Count number of NA values
  
  unique_vals <- values %>% table() %>% sort(decreasing = T)  # Count unique values and sort them by frequency
  n_unique <- length(unique_vals)
  
  cat(sprintf("%s:\n", col))  # Print column name
  cat(sprintf("\tnumber of NA values %d (%0.2f %%)\n", n_na, 100*n_na/n_total)) # Print number and percent of NA values
  if(n_unique < 150) cat(sprintf("\t\t%s\n", names(unique_vals) %>% paste(collapse=", "))) # If column has fewer than 150 unique values, print them all
  cat(sprintf("\tnumber of unique values %d (%0.2f %%)\n", length(unique_vals), # Print number and percent of unique values
              100*length(unique_vals)/n_total))
}

# Count number of reports per state and sort ascending
df %>% group_by(state) %>% tally() %>% arrange(n)

# Extract the 'occurred' column as a vector
df %>% pull(occurred)

# Helper function: nth(n) returns a function that extracts the nth element of a vector
nth <- function(n) function(a) a[n]

# Custom function to parse date strings by splitting on - / space : characters
parse_date <- function(s){
  space_split <- s %>% str_split("[-/ :]")
  tibble(d1 = Map(nth(1), space_split) %>% as.character(),
         d2 = Map(nth(2), space_split) %>% as.character(),
         d3 = Map(nth(3), space_split) %>% as.character(),
         d4 = Map(nth(4), space_split) %>% as.character(),
         d5 = Map(nth(5), space_split) %>% as.character())
}

# Apply the parsing function to the 'occurred' column
date_stuff <- parse_date(df %>% pull(occurred))
head(date_stuff, 10)

# Histogram of the second component of the split date (likely month)
ggplot (date_stuff, aes(d2))+ geom_bar() + labs(x = "Month", y = "Count")

# Install and load the skimr package for a nicer summary
library(skimr)

# Quick summary of the dataset
skim_output <- skimr::skim(df)

# Count occurrences for categorical columns
df %>% count(country, sort = TRUE)
df %>% count(state, sort = TRUE)
df %>% count(shape, sort = TRUE)

# Convert 'occurred' and 'reported' to proper date-time format using lubridate
df <- df %>%
  mutate(
    occurred = lubridate::mdy_hm(occurred, quiet = TRUE),
    reported = lubridate::mdy_hm(reported, quiet = TRUE)
  )

# Plot UFO sightings per year
df %>%
  filter(!is.na(occurred)) %>%
  count(year = lubridate::year(occurred)) %>%
  ggplot(aes(year, n)) +
  geom_line() +
  labs(title = "UFO Sightings per Year", x = "Year", y = "Number of Reports")


##Question 7
##what observation are you looking for ? column for each character column sorted by ascending tally

unique_values_tbl <- df %>%
  summarise(across(everything(), ~list(unique(.)))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "column",
    values_to = "unique_values"
  ) %>%
  unnest(unique_values) 

unique_values_tbl
  
##Question 8
states <- df  %>%
  filter(country=="USA") %>%
  filter(!state %in% c("UM", "-", "FI","NB","VI","0","GU")) %>%
  mutate(state=recode(state,
                      "Fl"="FL",
                      "Ohio"="OH",
                      "Montana"="MN",
                      "Ca"="CA",
                      "New York" = "NY",
                      "West Virginia" = "WV",
                      "Wisconsin"="WI"))


sightings_per_state <- states  %>%
  count(state, name = "total_sightings") %>%
  filter(total_sightings!=1)
sightings_per_state

sightings_per_state  %>%
  ggplot(aes(x = state, y = total_sightings)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, size=5))+
  labs(
    title = "Sightings by State",
    x = "State",
    y = "Total Sightings"
  ) 
