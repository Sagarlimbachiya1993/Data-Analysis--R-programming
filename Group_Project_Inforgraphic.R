library('dplyr')
library('tidyverse')
library('lubridate')
library(ggplot2) # Plot the graph using ggplot2
#Prosperity
income=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Income_and_Tax_2020_2021_Canada.csv")
income_growth=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Province-wise-income-growth-canada.csv")
gdp=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/GDP_Canada.csv")
#Social
social_belonging=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Social Belonging_Canada.csv")

#Health
life_expectancy=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Life Expectancy Canada.csv")
#Environment
air_pollutant=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/emissions-harmful-substances-air.csv")
#Good Governance
crime_data=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Crimes_Canada.csv")

#Graph-1

income = income %>% filter(Income.concept %in% c("Median total income", "Median income tax") & 
                             Economic.family.type == "Economic families and persons not in an economic family")

plot <- ggplot(income, aes(x = REF_DATE, y = VALUE, fill = Income.concept)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = VALUE), vjust = -0.5, position = position_dodge(width = 0.8), size = 4) +  # Add data labels
  labs(title = "Income Growth and Taxes (2020-2021)",
       x = "Year",
       y = "Value",
       fill = "Income Concept") +
  scale_fill_manual(values = c("Median total income" = "blue", "Median income tax" = "red")) +
  theme_minimal()

print(plot)

ggsave("income_growth_taxes_bar_with_labels.png", plot, width = 10, height = 6, dpi = 300)

#Graph-2

income_growth$REF_DATE <- as.factor(income_growth$REF_DATE)

# Calculate total income growth by province
province_growth <- income_growth %>%
  group_by(`Province.Name`) %>%
  summarise(total_growth = max(VALUE) - min(VALUE)) %>%
  arrange(desc(total_growth)) %>%
  top_n(5)

# Filter the data for top 5 provinces
top_province_data <- income_growth %>%
  filter(`Province.Name` %in% province_growth$`Province.Name`) %>% select(REF_DATE,Province.Name,VALUE)%>%group_by(REF_DATE,Province.Name,.drop = TRUE) %>%
  summarise(AvgValue = mean(VALUE))

# Define custom colors for the provinces
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

# Create a plot
plot <- ggplot(top_province_data, aes(x = REF_DATE, y = AvgValue, color = Province.Name, shape = Province.Name)) +
  geom_point(size = 3) +  # Set point size
  labs(title = "Top 5 Province-Wise Average Income (2018-2022)",
       x = "Year",
       y = "Average Income Value",
       color = "Province",
       shape = "Province") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = 1:length(unique(top_province_data$Province.Name)))

print(plot)

# Save the plot as an image
ggsave("top_province_average_income.png", plot, width = 10, height = 6, dpi = 300)

#Graph 3

social_belonging

# Filter the data for Gender "Total, all persons" and specific Sociodemographic characteristics
filtered_data <- social_belonging %>%
  filter(Gender == "Total, all persons" & `Sociodemographic.characteristics` %in% c(
    "15 to 24 years", "25 to 54 years", "55 to 64 years", "65 years and over"
  ))

# Create a list of pie charts for each age group
pie_charts <- lapply(unique(filtered_data$`Sociodemographic.characteristics`), function(age_group) {
  age_data <- filtered_data %>%
    filter(`Sociodemographic.characteristics` == age_group)
  
  ggplot(age_data, aes(x = "", fill = Indicators, y = VALUE)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = paste("Sense of Belonging in Canada -", age_group),
         fill = "Indicators") +
    geom_text(aes(label = sprintf("%.1f%%", VALUE)), position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    theme(legend.position = "bottom")
})

# Print the pie charts
for (i in seq_along(pie_charts)) {
  print(pie_charts[[i]])
}

#graph 5 #Health


# Filter the data for the specified characteristics
filtered_data <- life_expectancy %>%
  filter(GEO == "Canada" & Age.group == "At birth" & Sex %in% c("Males", "Females"))

# Create a bar chart for both males and females with separate bars for each year
plot <- ggplot(filtered_data, aes(x = REF_DATE, y = VALUE, fill = Sex, label = VALUE)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  labs(title = "Life Expectancy for Males and Females (2010-2017)",
       x = "Year",
       y = "Life Expectancy (Years)") +
  theme_minimal() +
  scale_fill_manual(values = c("Males" = "blue", "Females" = "pink"))

print(plot)

#Graph 6 

#Good Governance
crime_data=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/Crimes_Canada.csv")


# Group the data by REF_DATE and Cyber-related violation and sum VALUE
grouped_data <- crime_data %>%
  group_by(REF_DATE, violations) %>%
  summarise(total_crime = sum(VALUE))

# Create a line chart with different colors and legend
plot <- ggplot(grouped_data, aes(x = REF_DATE, y = total_crime, group = violations, color = violations)) +
  geom_line() +
  geom_point() +
  labs(title = "Growth in Crime(2018-2022)",
       x = "Year",
       y = "Number of Crimes") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Use a color palette for different lines
  theme(legend.position = "top")  # Position the legend at the top

print(plot)


#Environment

#Harmful substance emission
air_pollutant=read.csv("C:/Users/USER/Dropbox/PC/Desktop/Humber -BIA/Semester -3/Big Data -2/Group Project/emissions-harmful-substances-air.csv")

# Create the line graph using ggplot
p <- ggplot(air_pollutant, aes(x = Year)) +
  geom_line(aes(y = Mercury, color = "Mercury"), size = 1) +
  geom_line(aes(y = Lead, color = "Lead"), size = 1) +
  geom_line(aes(y = Cadmium, color = "Cadmium"), size = 1) +
  labs(title = "Percentage Change of Metals Over Years",
       x = "Year",
       y = "Percentage Change") +
  scale_color_manual(values = c("Mercury" = "blue", "Lead" = "red", "Cadmium" = "green")) +
  theme_minimal()

# Display the plot
print(p)


