library(tidyverse)
library(janitor)
library(lubridate)

## Load the reported clinical cases (rcc) data sets:
# Obtain file names.
rcc_files <- list.files(path = "Data/us_clinical_cases", 
                        pattern = ".csv", 
                        full.names = TRUE)

# Store all data sets in a list.
rcc <- rcc_files %>% 
  lapply(read_csv)

# Rename data sets by year.
rcc_names <- list.files(path = "Data/us_clinical_cases", 
                        pattern = ".csv") %>% 
  strsplit(".csv") %>% 
  unlist()
names(rcc) <- rcc_names

# Check if the number of columns is the same in each data set.
for (i in 1:(length(rcc) - 1)){
  print(dim(rcc[[i]])[2] == dim(rcc[[i + 1]])[2])
}
  # number of cols in 2019 daa does not match 2020 data
  # number of cols in 2020 does not match 2021 data

# check the columns in the 2020 data set.
rcc$reported_cases_2020 %>% 
  colnames()
  # Empty columns

# Remove empty columns in 2020 data set.
rcc$reported_cases_2020 <- rcc$reported_cases_2020 %>% 
  select(1:4)

# Check that number of columns match in all data sets after removing the empty 
# columns in the 2020 data set.
for (i in 1:(length(rcc) - 1)){
  print(dim(rcc[[i]])[2] == dim(rcc[[i + 1]])[2])
}
  # All data sets now have the same number of columns

# Check that the column names match across all data sets.
for (i in 1:(length(rcc) - 1)){
  print(colnames(rcc[[i]]) == colnames(rcc[[i + 1]]))
}
  # The columns contain the same data types, but the names do not match
  # across all data sets.


## Standardize the column names and variable types:
# Create a vector containing the new names.
rcc_col_names <- c("jurisdiction", "any_cases", "clinical_cases", "range")

# Assign the new names to all data sets.
rcc <- rcc %>% 
  lapply(setNames, rcc_col_names)


# Change the column variable types.
# for loop solution
for (i in 1:length(rcc)){
  rcc[[i]] <- rcc[[i]] %>% 
    mutate(across(where(is.character), factor))
}
# purr solution?


## Add a year column to all data sets:
# Obtain all years in the list of data sets.
rcc_years <- rcc_names %>% 
  str_remove("reported_cases_") %>% 
  as.integer()

# Add the year to all rows.
# for loop solution
for (i in 1:length(rcc)){
  rcc[[i]] <- rcc[[i]] %>% 
    mutate(year = rcc_years[i])
}

# purr solution
rcc <- rcc %>%
  map2(rcc_years, ~mutate(.x, year = .y))


# Merge the data sets:
# Drop any rows with non-clinical case counts.
all_reported_cases <- rcc %>% 
  bind_rows() %>% 
  relocate(year) %>% 
  arrange(year) %>% 
  filter(!is.na(jurisdiction),
         !clinical_cases == 0) %>% 
  select(!any_cases) 

write_csv(all_reported_cases, "all_reported_cases.csv")

all_rcc <- read_csv("Data/all_reported_cases.csv")


## Load the ncbi isolates data and select variables of interest:
ncbi <- read_csv("Data/ncbi_isolates.csv") %>% 
  clean_names() %>% 
  select(isolate, create_date, location, isolation_source, isolation_type, snp_cluster)

# Select all rows associated w/ US clinical cases
ncbi_clinical_isolates <- ncbi %>%
  filter(str_detect(location, "USA"),
         isolation_type == "clinical",) %>% 
  drop_na()

# Remove "USA: " and "USA:" from location column to get states
ncbi_clinical_isolates <- ncbi_clinical_isolates %>% 
  mutate(location = str_replace(location, "USA: ", "")) %>% 
  mutate(location = str_replace(location, "USA:", "")) %>% 
  mutate(location = str_replace(location, "Houston", "Texas")) %>% 
  mutate(location = str_replace(location, "Chicago", "Illinois")) %>% 
  mutate(location = str_replace(location, "New jersey", "New Jersey")) %>% 
  filter(location != "USA") %>% 
  rename(state = location) %>% 
  mutate(state = factor(state)) 
  
# Change isolation sources to blood and other
ncbi_clinical_isolates <- ncbi_clinical_isolates %>% 
  mutate(isolation_source = case_when(str_detect(isolation_source, "blood") ~ "blood",
                                      TRUE ~ "other"))

# Change column types
ncbi_clinical_isolates <- ncbi_clinical_isolates %>% 
  mutate(across(where(is.character), factor))


## Create an animated map for reported clinical cases:
library(maps)

# Load the state boundary basemap.
states <- map_data("state.vbm") %>% 
  tibble() %>% 
  mutate(region = factor(region))

USplot <- ggplot() +
  geom_polygon(data = states, aes(x=long, y = lat, group = group)) 

# Join the reported clinical cases data to the state boundary map data.
# rcc data has state abbreviations, not names
# need to add state name column
# do this by making a new data frame
state_key <- tibble(jurisdiction = state.abb, region = state.name)
all_reported_cases <- inner_join(all_reported_cases, 
                                 state_key, 
                                 by = "jurisdiction") %>% 
  mutate(region = factor(region))

# Join the reported clinical cases data to the state boundary map data.
statewide_cases <- inner_join(states, all_reported_cases, by = "region")
`%!in%` <- Negate(`%in%`)

# subset statewide reported cases by year
for (i in 1:6) {
  assign(paste("statewide_cases", rcc_years[i], sep = "_"), 
         statewide_cases %>% 
           filter(year == rcc_years[i]))
}

statewide_cases_list <- list(statewide_cases_2016,
                             statewide_cases_2017,
                             statewide_cases_2018,
                             statewide_cases_2019,
                             statewide_cases_2020,
                             statewide_cases_2021)

# extract states with no cases for each year
for (i in 1:6) {
  assign(paste("no_cases", rcc_years[i], sep = "_"), 
         state_key %>% 
           filter(region %!in% statewide_cases_list[[i]]$region) %>%  
           select(region) %>% 
           mutate(year = rcc_years[i]))
}

no_cases <- bind_rows(no_cases_2016,
                      no_cases_2017,
                      no_cases_2018,
                      no_cases_2019,
                      no_cases_2020,
                      no_cases_2021)

# join no cases data to state map data
no_cases <- inner_join(states, no_cases, by = "region")

# Create the map
fig <- ggplot() +
  geom_polygon(data = no_cases, 
               aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = statewide_cases, 
               aes(x = long, y = lat, group = group, fill = clinical_cases)) +
  labs(fill = "Count") +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  theme_void() +
  theme(legend.box.margin = margin(10,10,10,10),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18))
  

library(ggmap)
library(gganimate)
library(gifski)
library(transformr)
fig_animated <- fig +
  transition_time(year) +
  ggtitle('Clinical Cases of Candida auris in {frame_time}')

animate(fig_animated, nframes = 6, fps = 0.5, height = 450, width = 700)
anim_save("us_clinical_cases_map.gif")

# 2021 counts
center_coords <- state.vbm.center %>% 
  as.data.frame() %>% 
  tibble(long = x, lat = y) %>% 
  mutate(region = unique(states$region))

coords_2021 <- statewide_cases_2021 %>% 
  group_by(region) %>% 
  count(clinical_cases) %>% 
  select(-n) %>% 
  left_join(center_coords, by = "region") %>% 
  left_join(state_key, by = "region")

# 2021
ggplot() +
  geom_polygon(data = no_cases %>% 
                 filter(year == 2021), 
               aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = statewide_cases_2021, 
               aes(x = long, y = lat, group = group, fill = clinical_cases)) +
  #geom_text(data = counts_2021,
  #          aes(x = long, y = lat, label = clinical_cases),
  #          fontface = 2,
  #          color = "grey70") +
  geom_label(data = coords_2021,
            aes(x = long, y = lat, label = clinical_cases),
            fontface = "bold") +
  labs(fill = "Count") +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  theme_void() 

# No cases in Oregon but high freq of searches from google trends data
# voluntary reporting?

## Visualize the ncbi isolates data
# Proportion of blood isolates.
isolation_sources <- ncbi_clinical_isolates %>% 
  group_by(snp_cluster) %>% 
  count(isolation_source)

top_blood_clusters <- ncbi_clinical_isolates %>% 
  group_by(snp_cluster) %>% 
  tabyl(snp_cluster,isolation_source) %>% 
  arrange(desc(blood))

ggplot(data = isolation_sources) +
  geom_col(aes(x = fct_reorder(snp_cluster, n),
               y = n,
               fill = isolation_source)) +
  labs(x = "SNP Cluster",
       y = "Count",
       fill = "Isolation Source") +
  coord_flip() +
  scale_fill_uchicago() +
  theme_minimal()

# Find states with snp_clusters with the highest prop of bloodstream infections.
blood_clusters <- ncbi_clinical_isolates %>% 
  filter(snp_cluster %in% as.vector(head(top_blood_clusters$snp_cluster, 3)),
         isolation_source == "blood")

blood_clusters %>% 
 group_by(state) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


## Time series plots 
dates <- ncbi_clinical_isolates %>% 
  mutate(create_date = as_date(create_date)) %>%
  count(create_date) %>% 
  arrange(create_date)

ggplot(data = dates) +
  geom_line(aes(x = create_date, y = n))
  

## Microreact map
microreact_coords_distinct <- microreact %>% 
  filter(year <=2016, COUNTRY == "United States") %>% 
  select(Latitude, Longitude, year) %>% 
  arrange(year) %>% 
  distinct() %>% 
  mutate(dupe_count = 1)

microreact_coords_dup <- microreact %>% 
  filter(year <=2016, COUNTRY == "United States") %>% 
  select(Latitude, Longitude, year) %>% 
  arrange(year) %>%
  get_dupes %>% 
  distinct()

microreact_coords <- bind_rows(microreact_coords_distinct, 
                               microreact_coords_dup) %>% 
  arrange(year)

## Google Trends
# Related Queries:
related <- read_csv("Data/google_search_trends/searchterm_candidaauris/relatedQueries.csv", 
                    skip = 3,
                    col_names = TRUE)

# Separate top queries from rising queries
top_related <- related %>% 
  head(25)

rising_related <- related %>% 
  tail(26) %>%
  rename(query = TOP) %>% 
  slice(-1) %>% 
  mutate(query = str_remove(query, ",Breakout"))

# Obtain counts for words in top queries:
# Separate counts from queries.
top_counts <- top_related %>% 
  separate(TOP, c("query", "count"), ",") %>% 
  head(-2) # remove candida and auris

# Get counts for each word.
# Create a corpus
top_corpus <- Corpus(VectorSource(top_counts$query)) %>% 
  tm_map(removeWords, c("is", "of", "candida", "auris"))

# create a document term matrix from the corpus
top_dtm <- TermDocumentMatrix(top_corpus) %>% 
  as.matrix()

# get word frequencies from dtm
top_words <- rowSums(top_dtm) 
top_totals <- tibble(word = names(top_words), freq = top_words) 
# freq > 1 = duplicate
# multiply freq by count for each word

# combine with query counts to get totals for each word
top_totals <- top_totals %>% 
  mutate(count = c((100+14), 27, (25+8), 20, 19, 17, 17, 16, 14, (12+10), 10, 9, 8, 8)) %>% 
  mutate(n = freq*count)

# Create the word cloud.
library(ggwordcloud)
set.seed(1)
ggplot(data = top_totals) +
  geom_text_wordcloud(aes(label = word , size = n, color = word)) +
  scale_size_area(max_size = 28) +
  scale_color_manual(values = paletteer_c("grDevices::Teal", 14)) +
  theme_minimal()

# Obtain counts for words in rising queries:
# Create a corpus
rising_corpus <- Corpus(VectorSource(rising_related$query)) %>% 
  tm_map(removeWords, c("is", "of", "candida", "auris"))

# create a document term matrix from the corpus
rising_dtm <- TermDocumentMatrix(rising_corpus) %>% 
  as.matrix()

# get word frequencies from dtm
rising_words <- rowSums(rising_dtm) 
rising_totals <- tibble(word = names(rising_words), freq = rising_words) 
# freq > 1 = duplicate
# multiply freq by count for each word

# create the word cloud
ggplot(data = rising_totals) +
  geom_text_wordcloud(aes(label = word, size = freq, color = word)) +
  scale_size_area(max_size = 18) +
  scale_color_manual(values = paletteer_c("grDevices::Teal", 17)) +
  theme_minimal()

## Google trends map
# 2021
searches_2021 <- read_csv("Data/google_search_trends/searchterm_candidaauris/geoMap_2021.csv",
                          skip = 2,
                          col_names = TRUE) %>% 
  rename(region = Region,
         count = `Candida auris: (3/9/21 - 3/9/22)`)

searches_2021 <- inner_join(searches_2021, states, by = "region") %>% 
  inner_join(state_key, by = "region")
searches_2021_coords <- inner_join(searches_2021 %>% 
                                            select(region, jurisdiction, count), 
                                          center_coords,
                                          by = "region")

states_of_interest <- c("CA", "IL", "NY", "FL", "OR")
ggplot(data = searches_2021) +
  geom_polygon(data = searches_2021, 
               aes(x = long, y = lat, group = group, fill = count)) +
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "gray") +
  geom_label(data = searches_2021_coords %>% 
               filter(jurisdiction %in% states_of_interest),
             aes(x = long, y = lat, label = count),
             fontface = "bold") +
  labs(fill = "Queries") +
  theme_void()
