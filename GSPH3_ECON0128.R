# Install and/or load the necessary packages
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stargazer")
library(sf)
library(dplyr)
library(ggplot2)
library(stargazer)

# Load the data for commute time in New York

mobility_data <- read.csv("/Users/alexanderweston/Downloads/Travel_Time_to_Work.csv")

# Load the geodata for New York City Census Tracts

nyc_tracts <- st_read("/Users/alexanderweston/Desktop/UCL DSPP/Statistical Programming/untitled folder/Geodata")

# Adjust the category of the geoid variable in the mobility data

mobility_data$geoid <- as.character(mobility_data$geoid)

# Merge the mobility data to the geodata for NYC census tracts

merged_data <- left_join(nyc_tracts, mobility_data, by = c("GEOID" = "geoid"))

# Erase unneeded columns from the mobility data

merged_data <- select(merged_data, -c(hh_tot, hh_0009, hh_1019, hh_2029, hh_3044, hh_4559, hh_6089, hh_90up))

# Since the commute time is provided in time intervals, we need to set the midpoints
# for these.

midpoints <- c(4.5, 14.5, 24.5, 37, 52, 74.5, 105)

# We can now compute the mean travel time
merged_data$mean_travel_time <- (merged_data$hh_0009_p * midpoints[1] +
                                   merged_data$hh_1019_p * midpoints[2] +
                                   merged_data$hh_2029_p * midpoints[3] +
                                   merged_data$hh_3044_p * midpoints[4] +
                                   merged_data$hh_4559_p * midpoints[5] +
                                   merged_data$hh_6089_p * midpoints[6] +
                                   merged_data$hh_90up_p * midpoints[7]) / 100

# Now we can load the income data

income_data <- read.csv("/Users/alexanderweston/Desktop/UCL DSPP/Statistical Programming/untitled folder/Income_data_clean_test2.csv", sep = ";")

income_data$GEOID <- gsub("1400000US", "", income_data$GEOID)

# We should inspect the data set to make sure the variables are the right 
# category

str(merged_data)

str(income_data)

# After inspection we see we have to adjust the income columns

income_cols_to_convert <- c("USD_10000_or_less", "USD_10000_14999", "USD_15000_24999", 
                            "USD_25000_34999", "USD_35000_49999", "USD_50000_74999", 
                            "USD_75000_99999", "USD_100000_149999", "USD_150000_199999", 
                            "USD_200000_or_more", "Median_Income_USD", "Mean_Income_USD")

income_data[income_cols_to_convert] <- lapply(income_data[income_cols_to_convert], as.numeric)

# We can now merge the income data to the merged data
merged_data <- left_join(merged_data, income_data, by = c("GEOID" = "GEOID"))

# Since there are so many columns, we can clean the dataset of several unneeded columns
merged_data <- select(merged_data, -c(CDEligibil, tractid, namelsad))

# Lets also omit any NA rows since they will affect our analysis

merged_data <- na.omit(merged_data)

# We can  plot a visualisation of both MTT and MHI on a census tract map of NYC
# in order to have a visual, spatial representation of the relationship

ggplot(merged_data) +
  geom_sf(aes(fill = mean_travel_time), color = NA) +
  scale_fill_gradientn(colors = c("#64B5F6", "red"), 
                       name = "Mean Travel Time (min)") +
  labs(title = "Mean Travel Time in NYC Census Tracts") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

breakpoints <- seq(0, 200000, by = 25000)
color_palette <- colorRampPalette(c("red", "#64B5F6"))(length(breakpoints) - 1)

ggplot(merged_data) +
  geom_sf(aes(fill = Median_Income_USD), color = NA) +
  scale_fill_gradientn(colors = color_palette, 
                       values = scales::rescale(breakpoints),
                       name = "Median Household Income (USD)",
                       breaks = breakpoints,
                       labels = scales::dollar_format()) +
  labs(title = "Median Household Income in NYC Census Tracts") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

# Now, let us plot some graphs to visualise the Mean travel time

ggplot(merged_data, aes(x = mean_travel_time)) +
  geom_histogram(binwidth = 2.5, fill = "#4D7EA8", color = "white") +  # Smaller binwidth
  theme_minimal() +
  labs(
    title = "Histogram of Mean Travel Time in NYC Census Tracts",
    x = "Mean Travel Time (minutes)",
    y = "Number of Census Tracts",
    caption = "Source: Department of Transportation"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0)
  ) +
  scale_x_continuous(breaks = seq(0, max(merged_data$mean_travel_time, na.rm = TRUE), by = 5)) +
  scale_y_continuous(labels = scales::label_number())

#The histogram of Mean Travel Time shows some skewness, deviating slightly from a perfect normal distribution

# We should do the same for the median income levels

ggplot(merged_data, aes(x = Median_Income_USD)) +
  geom_histogram(binwidth = 5000, fill = "#4D7EA8", color = "white") +
  theme_minimal() +
  labs(
    title = "Histogram of Median Household Income in USD",
    x = "Median Household Income per census tract (USD)",
    y = "Number of Census Tracts",
    caption = "Source: United States Census Bureau"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# The histogram of Median Household Income likewise appears to be skewed, 
# indicating that it is not perfectly normally distributed

# Modelling the correlation

model <- lm(mean_travel_time ~ Median_Income_USD, data = merged_data)

ggplot(merged_data, aes(x = Median_Income_USD, y = mean_travel_time)) + 
  geom_point(shape = 3, alpha = 0.5) +  # Crosses and transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Linear Regression of Mean Travel Time vs Median Household Income",
    x = "Median Household Income (USD)",
    y = "Mean Travel Time (minutes)"
  ) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +  # Custom formatting
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#The scatter plot between Median Income and Mean Commute Time shows a spread
#that suggests a negative correlation, as observed earlier. Higher median incomes
# seem to be associated with shorter commute times

#We can analyse the summary statistics of this using the stargazer package
stargazer(model, type = "text")
