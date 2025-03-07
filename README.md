# MoMA
MoMA Art Collection Analysis and Visualization. Such as how modern are the artworks in their collection, as modern artworks are arts created after the 1860s. And lots more, so let's dig deep.

# Project Overview
This project focuses on cleaning, transforming, and analyzing data from the Museum of Modern Art (MoMA) collection. Using R and the tidyverse suite, we process artist and artwork data to standardize date formats, manage missing values, and classify artists based on attributes like nationality, gender, and collaboration types.

# Key Objectives
Data Cleaning & Transformation:
Remove unnecessary columns and handle missing values.
Standardize the DateCreated column using regex-based transformations.
Convert text-based date descriptions into structured numerical formats.
Differentiate between single and multiple artist contributions.
Classify artworks based on artist gender and nationality.
Exploratory Data Analysis (EDA):
Identify the most prolific artists in the MoMA collection.
Analyze the distribution of artists' nationalities.
Investigate trends in artwork acquisition over time.
Visualization & Reporting:
Create bar charts, and other visualizations using ggplot2

# Technologies & Libraries Used
R (tidyverse, ggplot2, stringr, lubridate, shiny)
Data Sources:
artists.csv (Artist information)
artworks.csv (Artwork metadata)
MoMA_data_dictionary.csv (Data dictionary for reference)

# Expected Outcomes
A clean, structured dataset ready for analysis.
Insights into MoMA’s artist demographics and collection trends.
