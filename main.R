library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(shiny)

# Importing the data
artists <- read_csv("artists.csv")
artworks <- read_csv("artworks.csv")
dic <- read_csv("MoMA_data_dictionary.csv")


## COMENCEMENT OF DATA CLEANING

# Remove columns that are not useful and rows with missing values
artists <- artists %>% 
  mutate(ConstituentID = as.character(ConstituentID)) %>%
  select(-`Wiki QID`, -ULAN)
artworks <- artworks %>% 
  select(-ArtistBio, -CreditLine, -AccessionNumber, -Department, -URL, -ObjectID,
         -ImageURL, -`Circumference (cm)`, -`Depth (cm)`, -`Diameter (cm)`,
         -`Height (cm)`, -`Length (cm)`, -`Weight (kg)`, -`Width (cm)`,
         -`Seat Height (cm)`, -`Duration (sec.)`, -Dimensions) %>% 
  filter(!is.na(Artist) & !is.na(Nationality) & !is.na(BeginDate))


# Standardizing the Date Column and Converting it to a Date
artworks <- artworks %>%
  mutate(
    # Standardize the Date string
    Date = str_squish(Date),
    Date = na_if(Date, "Unknown"),
    Date = na_if(Date, "n.d."),
    
    DateCreated = case_when(
      # 1. Century ranges, e.g. "4th-6th century C.E."
      str_detect(Date, "(\\d{1,2})(st|nd|rd|th)[-–](\\d{1,2})(st|nd|rd|th)\\s+century") ~ {
        nums <- as.numeric(unlist(str_extract_all(Date, "\\d{1,2}")))
        as.integer(mean(nums) * 100)
      },
      # 2. Single century references with optional qualifiers:
      # e.g., "16th century C.E.", "late 19th century", "early 20th century"
      str_detect(Date, "(late|early)?\\s*\\d{1,2}(st|nd|rd|th)\\s+century") ~ {
        cent <- as.numeric(str_extract(Date, "\\d{1,2}"))
        if_else(str_detect(Date, "late"),
                as.integer(cent * 100 - 1),
                if_else(str_detect(Date, "early"),
                        as.integer((cent - 1) * 100 + 1),
                        as.integer(((cent - 1) * 100 + (cent * 100 - 1)) / 2)
                )
        )
      },
      # 3. Two-digit year ranges: e.g., "1976-77" → 1977
      str_detect(Date, "\\d{4}-\\d{2}\\b") ~ as.integer(
        paste0(substr(str_extract(Date, "\\d{4}"), 1, 2),
               str_extract(Date, "(?<=-)\\d{2}\\b"))
      ),
      # 4. Full four-digit ranges: e.g., "1958–1964"
      str_detect(Date, "\\d{4}[-–]\\d{4}") ~ as.integer(str_extract(Date, "(?<=[-–])\\d{4}")),
      # 5. Circa ranges: e.g., "c. 1916-1920" 
      str_detect(Date, "c\\.?\\s*\\d{4}[-–]\\d{2,4}") ~ as.integer(
        if_else(str_detect(Date, "c\\.?\\s*\\d{4}-\\d{2}\\b"),
                paste0(substr(str_extract(Date, "c\\.?\\s*\\d{4}"),
                              if_else(str_detect(Date, "c\\.?\\s*"), 4L, 1L), 4),
                       str_extract(Date, "(?<=-)\\d{2}\\b")),
                str_extract(Date, "(?<=-)[0-9]{4}")
        )
      ),
      # 6. Single circa date: e.g., "c. 1917"
      str_detect(Date, "c\\.?\\s*\\d{4}") ~ as.integer(str_extract(Date, "\\d{4}")),
      # 7. "Before" and "After" dates:
      str_detect(Date, "before\\s+\\d{4}") ~ as.integer(str_extract(Date, "\\d{4}")) - 1,
      str_detect(Date, "after\\s+\\d{4}") ~ as.integer(str_extract(Date, "\\d{4}")) + 1,
      # 8. Decade descriptors: e.g., "early 1940s" → 1940, "late 1920s" → 1929
      str_detect(Date, "early\\s+\\d{4}s") ~ as.integer(str_extract(Date, "\\d{4}")),
      str_detect(Date, "late\\s+\\d{4}s") ~ as.integer(str_extract(Date, "\\d{4}")) + 9,
      # 9. Month-Year formats: e.g., "Sep-91" → 1991
      str_detect(Date, "^[A-Za-z]{3}-\\d{2}$") ~ as.integer(paste0("19", str_extract(Date, "\\d{2}$"))),
      # 10. If the date is exactly a 4-digit year (and nothing else)
      str_detect(Date, "^\\d{4}$") ~ as.integer(Date),
      # 11. If multiple 4-digit numbers appear, take the last one
      str_count(Date, "\\d{4}") > 1 ~ as.integer(str_extract(Date, "\\d{4}$")),
      # 12. Fallback: if any 4-digit number is found, use it; otherwise, set to NA
      str_detect(Date, "\\d{4}") ~ as.integer(str_extract(Date, "\\d{4}")),
      # Otherwise, set to NA
      TRUE ~ NA_integer_
    )
  ) %>% 
  select(-Date) %>% 
  select(Title, Artist, ConstituentID, Nationality, Gender, BeginDate, EndDate,
         DateCreated, DateAcquired, everything())


# Separating Art works created by single and multiple artists
artworks <- artworks %>%
  filter(!is.na(DateAcquired)) %>% 
  mutate(
    SingleArtist = ifelse((!is.na(ConstituentID) | !str_detect(ConstituentID, ",")) 
                          & !str_detect(Artist, ", "), TRUE, FALSE), # Check if there is only one artist
    
    MixedGender = ifelse((SingleArtist == FALSE & grepl("female", Gender, ignore.case = TRUE)
                          & grepl("male", Gender, ignore.case = TRUE)), TRUE, FALSE), # Check if multiple artists are mixed gender or not
    
    ConstituentID = as.numeric(ConstituentID), # Convert the ConstituentID to a numeric
    DateAcquired = as.Date(DateAcquired, format = "%m/%d/%Y") # Convert the DateAcquired to a date
  )


# Removing parentheses from `Nationality`, `BeginDate`, `EndDate`, `Gender`
artworks <- artworks %>% 
  mutate(
    # Replace ")(" or ") (" with ", " before removing parentheses
    Nationality = gsub("\\)\\s*\\(", ", ", Nationality),
    BeginDate = gsub("\\)\\s*\\(", ", ", BeginDate),
    EndDate = gsub("\\)\\s*\\(", ", ", EndDate),
    Gender = gsub("\\)\\s*\\(", ", ", Gender),
    
    # Remove parentheses from the columns
    Nationality = gsub("[()]", "", Nationality),
    BeginDate = gsub("[()]", "", BeginDate),
    EndDate = gsub("[()]", "", EndDate),
    Gender = gsub("[()]", "", Gender),
    
    # Remove trailing commas
    Nationality = gsub(" , ", "", Nationality),
    BeginDate = gsub(" , ", "", BeginDate),
    EndDate = gsub(" , ", "", EndDate),
    Gender = gsub(" , ", "", Gender),
    
    OnView = ifelse(is.na(OnView), FALSE, TRUE),
    
    # Convert the columns to integers
    BeginDate = as.integer(BeginDate),
    EndDate = as.integer(EndDate)
  )


# Installing the ConstituentIDs for Arts with multiple artists
artworks_fixed <- artworks %>% 
  separate_rows(Artist, sep = ",|\\+") %>%
  mutate(Artist = str_squish(Artist)) %>%
  group_by(Title) %>%
  summarise(
    Artist = paste(unique(Artist), collapse = ", "),
    ConstituentID = paste(unique(ConstituentID[!is.na(ConstituentID)]), collapse = ", ")
  ) %>% 
  ungroup()

artworks <- artworks %>%
  select(-Artist, -ConstituentID) %>%
  left_join(artworks_fixed, by = "Title") %>%
  select(Title, Artist, ConstituentID, Nationality, everything())


# Joining the `artists` table with the `artworks` table
artworks <- artworks %>%
  left_join(artists %>% select(ConstituentID, Nationality, Gender, BeginDate, EndDate), 
            by = c("ConstituentID")) %>%
  mutate(
    # Coalesce the columns (Replace missing values in Artworks with values from Artists)
    Nationality = coalesce(Nationality.x, Nationality.y),
    Gender = coalesce(Gender.x, Gender.y),
    BeginDate = coalesce(BeginDate.x, BeginDate.y),
    BeginDate = ifelse(BeginDate == 0, NA, BeginDate),
    EndDate = coalesce(EndDate.x, EndDate.y)
  ) %>%
  select(-Nationality.x, -Nationality.y, -Gender.x, -Gender.y, -BeginDate.x, 
         -BeginDate.y, -EndDate.x, -EndDate.y) %>% 
  select(Title, Artist, ConstituentID, Nationality, Gender, BeginDate,
         EndDate, DateCreated, everything())


artworks <- artworks %>% 
  mutate(
    Nationality = ifelse((is.na(Nationality) | Nationality == ""), "Unknown", Nationality),
    ConstituentID = ifelse((is.na(ConstituentID) | ConstituentID == ""), "Unknown", ConstituentID),
    Gender = ifelse((is.na(Gender) | Gender == ""), "Unknown", Gender),
    BeginDate = ifelse((is.na(BeginDate) | BeginDate == ""), 0, BeginDate),
    EndDate = ifelse((is.na(EndDate) | EndDate == ""), 0, EndDate),
    DateCreated = ifelse((is.na(DateCreated) | DateCreated == ""), 0, DateCreated)
  )





## VISUALIZING OUR CLEANED DATA

# Visualizing the number of artworks created over time
ggplot((artworks %>% filter(DateCreated >= 1800)), aes(x = DateCreated)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 50) +
  labs(title = "How Modern Are The Artworks in MoMA's Collection?",
       x = "Date Created", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Which artists have the most artworks in MoMA's collection?
total_artworks <- nrow(artworks)

artworks %>%
  count(Artist, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  mutate(percent = round((n / total_artworks) * 100, 1)) %>%  # Compute percentage before ggplot
  ggplot(aes(x = reorder(substr(Artist, 1, 25), n), y = n)) +
  geom_col(fill = "skyblue", alpha = 0.9) +
  geom_text(aes(label = paste0(percent, "%")), vjust = 0.5, hjust = -0.1, size = 5) +  # Use precomputed percent
  coord_flip() +  # Flip the coordinates
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Artists with the Most Artworks in MoMA's Collection",
       x = "Artist", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )


# Trends in the date of acquisition by months
artworks %>% 
  filter(!is.na(DateAcquired)) %>% 
  group_by(Month = month(DateAcquired)) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = Month, y = count)) +
  geom_col(fill = "steelblue", alpha = 0.7) +  # Add bars
  geom_line(group = 1, color = "blue", size = 1) +  # Ensure a single group
  scale_x_continuous(breaks = 1:12, labels = month.abb) + # Properly label months
  labs(title = "Trends in the Date of Acquisition by Month",
       x = "Month", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Visualizing the number of artworks acquired by MoMA over time
artworks %>% 
  filter(!is.na(DateAcquired)) %>% 
  group_by(Year = floor(as.numeric(year(DateAcquired)) / 10) * 10) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = Year, y = count)) +
  geom_col(fill = "skyblue", alpha = 0.7) +  # Add bars
  geom_line(group = 1, color = "steelblue", size = 1) +  # Ensure a single group
  scale_x_continuous(breaks = seq(1900, 2020, 10), labels = function(x) paste0(x, "s")) + # Properly label months
  labs(title = "Artworks Acquired by MoMA Over Time (by Decades)",
       x = "Decade Acquired", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Visualizizng the percentage of artworks that are on view
artworks %>%
  count(OnView) %>%
  ggplot(aes(x = factor(OnView), y = n, fill = factor(OnView))) +
  geom_col() +
  geom_text(aes(label = scales::percent(n / sum(n))), vjust = -0.5, size = 5) +
  labs(title = "Percentage of Artworks on Display in MoMA's Collection",
       x = "On Display", y = "Number of Artworks") +
  scale_fill_manual(values = c("skyblue", "steelblue")) +
  scale_fill_discrete(name = "On Display") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Visualizing the distribution of artists' nationalities
artworks %>% 
  filter(Nationality != "Unknown") %>%
  count(Nationality, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(Nationality, n), y = n, fill = Nationality)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(n, -3)), vjust = 0.5, hjust = -0.1, size = 5) +
  coord_flip() +  # Flip the bars for better readability
  labs(title = "Top 10 Nationalities of Artists in MoMA's Collection",
                  x = "Nationality", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Visualising Single Artisits vs Multiple Artists
artworks %>%
  count(SingleArtist) %>%
  ggplot(aes(x = factor(SingleArtist), y = n, fill = factor(SingleArtist))) +
  geom_col() +
  geom_text(aes(label = scales::percent(n / sum(n))), vjust = -0.5, size = 5) +
  labs(title = "Artist Collaboration Distribution",
       x = "Single Artist", y = "Number of Artworks") +
  scale_fill_manual(values = c("skyblue", "steelblue")) +
  scale_fill_discrete(name = "Single Artist") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )

# Visualizing the distribution of Gender in MoMA's collection
artworks %>% 
  filter(SingleArtist == TRUE, (Gender %in% c("male", "female"))) %>%
  count(Gender) %>% 
  ggplot(aes(x = Gender, y = n, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(n / sum(n))), vjust = -0.5, size = 5) +
  labs(title = "Distribution of Artists by Gender", x = "Gender", y = "Number of Artworks") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = 0.5, color = "gray"),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    plot.title = element_text(size = 35)
  )




write.csv(artworks, "artworks_cleaned.csv", row.names = FALSE)
