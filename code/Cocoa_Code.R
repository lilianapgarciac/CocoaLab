#load and attach data
data = read.csv("Dataset 4 — Chocolate bar ratings.csv")
attach(data)

#Let's analyze the dataset by making some visualizations.

data$Bean.Type[data$Bean.Type == ""] <- "Unknown"
data$Bean.Type[data$Bean.Type == " "] <- "Unknown"
data$Bean.Type[grepl("^\\s*$", data$Bean.Type)] <- "Unknown"


# Data Cleaning and EDA
#Making Rating as categorical to analyze each behavior per variable
data$Rating <- as.factor(data$Rating)

# Let's analyze the categorical variables first
categorical_data <- data[, !sapply(data, is.numeric)]
categorical_data$Cocoa.Percent <- as.numeric(gsub("%", "", categorical_data$Cocoa.Percent))/ 100
categorical_data <- categorical_data[, !sapply(categorical_data, is.numeric)]
category_counts <- sapply(data, function(x) length(unique(x)))
print(category_counts)


## As we have two many categories let's try to see if we can reduce them in some way
## Let's create a frequency distribution of each of the categorical variables

## Company.Maker.if.known
freq_table <- sort(table(data$Company...Maker.if.known.), decreasing = TRUE)
print(freq_table)
library(ggplot2)
ggplot(data.frame(Category = names(freq_table), Count = as.numeric(freq_table)), 
       aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency Distribution of Companies")


# Define a threshold for rare categories
threshold <- 7

# Identify rare categories
rare_categories <- names(freq_table[freq_table < threshold])

# Replace rare categories with "Other"
data$Company...Maker.if.known. <- as.character(data$Company...Maker.if.known.)
data$Company...Maker.if.known.[data$Company...Maker.if.known. %in% rare_categories] <- "Other"
data$Company...Maker.if.known. <- as.factor(data$Company...Maker.if.known.)

## Company.Maker.if.known
freq_table <- sort(table(data$Company...Maker.if.known.), decreasing = TRUE)
print(freq_table)
library(ggplot2)
ggplot(data.frame(Category = names(freq_table), Count = as.numeric(freq_table)), 
       aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency Distribution of Companies")

## Count category of 'Other' 
sum(data$Company...Maker.if.known. == "Other")

# Specific Bean origin will be dropped since it represents the same information as Broader.Bean.Origin but with a higher level of granularity
freq_table <- sort(table(data$Specific.Bean.Origin.or.Bar.Name), decreasing = TRUE)
print(freq_table)
library(ggplot2)
ggplot(data.frame(Category = names(freq_table), Count = as.numeric(freq_table)), 
       aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency Distribution of Companies")

unique_beanorigin <- unique(data$Specific.Bean.Origin.or.Bar.Name)
length(unique_beanorigin)
# so, I will keep only Broader.Bean.Origin for the analysis 

data <- subset(data, select = -Specific.Bean.Origin.or.Bar.Name)

# REF variable indicates how recent is the rating but we already have the Review.Date to measure that, so
# For the sake of the analysis, I will drop REF as well.

data <- subset(data, select = -REF)


## I will reduce the number of categories of Company Location by categorizing them by the Continent Region they belong to
# Extract unique categories from a Company Location

unique_locations <- unique(data$Company.Location)

# View the result
print(unique_locations)

continent_dict <- list(
  "France" = "Europe",
  "U.S.A." = "North America",
  "Fiji" = "Oceania",
  "Ecuador" = "South America",
  "Mexico" = "North America",
  "Switzerland" = "Europe",
  "Netherlands" = "Europe",
  "Spain" = "Europe",
  "Peru" = "South America",
  "Canada" = "North America",
  "Italy" = "Europe",
  "Brazil" = "South America",
  "U.K." = "Europe",
  "Australia" = "Oceania",
  "Wales" = "Europe",
  "Belgium" = "Europe",
  "Germany" = "Europe",
  "Russia" = "Europe/Asia",
  "Puerto Rico" = "North America",
  "Venezuela" = "South America",
  "Colombia" = "South America",
  "Japan" = "Asia",
  "New Zealand" = "Oceania",
  "Costa Rica" = "North America",
  "South Korea" = "Asia",
  "Amsterdam" = "Europe",  # If referring to the city, it's in the Netherlands.
  "Scotland" = "Europe",
  "Martinique" = "North America",  # Caribbean
  "Sao Tome" = "Africa",
  "Argentina" = "South America",
  "Guatemala" = "North America",
  "South Africa" = "Africa",
  "Bolivia" = "South America",
  "St. Lucia" = "North America",  # Caribbean
  "Portugal" = "Europe",
  "Singapore" = "Asia",
  "Denmark" = "Europe",
  "Vietnam" = "Asia",
  "Grenada" = "North America",  # Caribbean
  "Israel" = "Asia",
  "India" = "Asia",
  "Czech Republic" = "Europe",
  "Domincan Republic" = "North America",  # Caribbean
  "Finland" = "Europe",
  "Madagascar" = "Africa",
  "Philippines" = "Asia",
  "Sweden" = "Europe",
  "Poland" = "Europe",
  "Austria" = "Europe",
  "Honduras" = "North America",
  "Nicaragua" = "North America",
  "Lithuania" = "Europe",
  "Niacragua" = "North America",  # Assuming this is a typo for Nicaragua.
  "Chile" = "South America",
  "Ghana" = "Africa",
  "Iceland" = "Europe",
  "Eucador" = "South America",  # Assuming this is a typo for Ecuador.
  "Hungary" = "Europe",
  "Suriname" = "South America",
  "Ireland" = "Europe"
)

# Add a new column for continent
data$Company.Location.Continent <- sapply(data$Company.Location, function(x) continent_dict[[x]])

# I will drop Company.Location and keep Company.Location.Continent
data <- subset(data, select = -Company.Location)


## Review.Date
freq_table <- sort(table(data$Review.Date), decreasing = TRUE)
print(freq_table)
ggplot(data.frame(Category = names(freq_table), Count = as.numeric(freq_table)), 
       aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency Distribution of Review Date")

## Cocoa.Percent
unique_cocoapercent <- unique(data$Cocoa.Percent)
print(unique_cocoapercent)
data$Cocoa.Percent <- as.numeric(gsub("%", "", data$Cocoa.Percent))

## Rating
unique_rating <- unique(data$Rating)
print(unique_rating)

## Bean.Type
freq_table <- sort(table(data$Bean.Type), decreasing = TRUE)
ggplot(data.frame(Category = names(freq_table), Count = as.numeric(freq_table)), 
       aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Frequency Distribution of Bean Type")

# For almost 50% of the data, there is no information about the Bean.Type
# so, I will drop this variable. 

data <- subset(data, select = -Bean.Type)

## Now, we have a cleaner data set!!!
## Since our first goal is to identify which types of cluster and we only have categorical variables,
## I will apply hierarchical clustering to identify them.

## However, since the goal is to identify the cluster where the rating belongs to. I will drop the Rating for the analysis because the idea 
## is to use the cluster as a new variable to predict the Rating of the Chocolate Bar using Logistic Regression as a classification model.

data_to_use = subset(data, select = -Rating)
Ratings = data['Rating']

## Now let's let's plot the dendrogram
hc = hclust(dist(data_to_use))
plot(hc, hang=-1, cex=0.6)

category_counts_2 <- sapply(data_to_use, function(x) length(unique(x)))
print(category_counts_2)

## The dendrogram is not aesthetically visually speaking and the it is not clear from where to cut the clusters.
## So, let's see if the Broad Bean Origin can be reduced in number of categories.

#Broad Bean Origin
unique_broadbean <- unique(data_to_use$Broad.Bean.Origin)
print(unique_broadbean)

## Now, let's create the column Bean.Origin.Continent

data_to_use$Broad.Bean.Origin <- ifelse(trimws(data_to_use$Broad.Bean.Origin) == "" | is.na(data_to_use$Broad.Bean.Origin), "Unknown", data_to_use$Broad.Bean.Origin)
data_to_use$Broad.Bean.Origin <- ifelse(trimws(data_to_use$Broad.Bean.Origin) == " " | is.na(data_to_use$Broad.Bean.Origin), "Unknown", data_to_use$Broad.Bean.Origin)
data_to_use$Broad.Bean.Origin <- ifelse(gsub("\\s+", "", data_to_use$Broad.Bean.Origin) == "" | is.na(data_to_use$Broad.Bean.Origin), "Unknown", data_to_use$Broad.Bean.Origin)

continent_dict2 <- list(
  "Sao Tome" = "Africa",
  "Togo" = "Africa",
  "Peru" = "South America",
  "Venezuela" = "South America",
  "Cuba" = "North America",
  "Panama" = "North America",
  "Madagascar" = "Africa",
  "Brazil" = "South America",
  "Ecuador" = "South America",
  "Colombia" = "South America",
  "Burma" = "Asia",
  "Papua New Guinea" = "Oceania",
  "Bolivia" = "South America",
  "Fiji" = "Oceania",
  "Mexico" = "North America",
  "Indonesia" = "Asia",
  "Trinidad" = "North America",
  "Vietnam" = "Asia",
  "Nicaragua" = "North America",
  "Tanzania" = "Africa",
  "Dominican Republic" = "North America",
  "Ghana" = "Africa",
  "Belize" = "North America",
  " " = "Unknown",  # Empty or blank values
  "Jamaica" = "North America",
  "Grenada" = "North America",
  "Guatemala" = "North America",
  "Honduras" = "North America",
  "Costa Rica" = "North America",
  "Domincan Republic" = "North America",
  "Haiti" = "North America",
  "Congo" = "Africa",
  "Philippines" = "Asia",
  "Malaysia" = "Asia",
  "Dominican Rep., Bali" = "Multiple Regions",
  "Venez,Africa,Brasil,Peru,Mex" = "Multiple Regions",
  "Gabon" = "Africa",
  "Ivory Coast" = "Africa",
  "Carribean" = "North America",
  "Sri Lanka" = "Asia",
  "Puerto Rico" = "North America",
  "Uganda" = "Africa",
  "Martinique" = "North America",
  "Sao Tome & Principe" = "Africa",
  "Vanuatu" = "Oceania",
  "Australia" = "Oceania",
  "Liberia" = "Africa",
  "Ecuador, Costa Rica" = "Multiple Regions",
  "West Africa" = "Africa",
  "Hawaii" = "Oceania",
  "St. Lucia" = "North America",
  "Cost Rica, Ven" = "Multiple Regions",
  "Peru, Madagascar" = "Multiple Regions",
  "Venezuela, Trinidad" = "Multiple Regions",
  "Trinidad, Tobago" = "North America",
  "Ven, Trinidad, Ecuador" = "Multiple Regions",
  "South America, Africa" = "Multiple Regions",
  "India" = "Asia",
  "Africa, Carribean, C. Am." = "Multiple Regions",
  "Tobago" = "North America",
  "Ven., Indonesia, Ecuad." = "Multiple Regions",
  "Trinidad-Tobago" = "North America",
  "Peru, Ecuador, Venezuela" = "South America",
  "Venezuela, Dom. Rep." = "Multiple Regions",
  "Colombia, Ecuador" = "South America",
  "Solomon Islands" = "Oceania",
  "Nigeria" = "Africa",
  "Peru, Belize" = "Multiple Regions",
  "Peru, Mad., Dom. Rep." = "Multiple Regions",
  "Unknown" = "Unknown",  # Placeholder for explicitly marked Unknown
  "PNG, Vanuatu, Mad" = "Multiple Regions",
  "El Salvador" = "North America",
  "South America" = "South America",
  "Samoa" = "Oceania",
  "Ghana, Domin. Rep" = "Multiple Regions",
  "Trinidad, Ecuador" = "Multiple Regions",
  "Cameroon" = "Africa",
  "Venezuela, Java" = "Multiple Regions",
  "Venezuela/ Ghana" = "Multiple Regions",
  "Venezuela, Ghana" = "Multiple Regions",
  "Indonesia, Ghana" = "Multiple Regions",
  "Peru(SMartin,Pangoa,nacional)" = "South America",
  "Principe" = "Africa",
  "Central and S. America" = "Multiple Regions",
  "Ven., Trinidad, Mad." = "Multiple Regions",
  "Carribean(DR/Jam/Tri)" = "North America",
  "Ghana & Madagascar" = "Multiple Regions",
  "Ven.,Ecu.,Peru,Nic." = "Multiple Regions",
  "Madagascar & Ecuador" = "Multiple Regions",
  "Guat., D.R., Peru, Mad., PNG" = "Multiple Regions",
  "Peru, Dom. Rep" = "Multiple Regions",
  "Dom. Rep., Madagascar" = "Multiple Regions",
  "Gre., PNG, Haw., Haiti, Mad" = "Multiple Regions",
  "Mad., Java, PNG" = "Multiple Regions",
  "Ven, Bolivia, D.R." = "Multiple Regions",
  "DR, Ecuador, Peru" = "Multiple Regions",
  "Suriname" = "South America",
  "Peru, Ecuador" = "South America",
  "Ecuador, Mad., PNG" = "Multiple Regions",
  "Ghana, Panama, Ecuador" = "Multiple Regions",
  "Venezuela, Carribean" = "Multiple Regions"
)

# Add a new column for continent
data_to_use$Bean.Origin.Continent <- sapply(data_to_use$Broad.Bean.Origin, function(x) continent_dict2[[x]])


## Let's drop Broad.Bean.Origin
data_to_use = subset(data_to_use, select = -Broad.Bean.Origin)

allowed_categories <- c("Africa", "South America", "North America", "Asia", "Oceania", "Multiple Regions")

# Filter the data
data_to_use <- data_to_use[data_to_use$Bean.Origin.Continent %in% allowed_categories, ]

## Let's check the Cocoa.Percent to see if we can create Cocoa Level variable
hist(data_to_use$Cocoa.Percent, main = "Distribution of Cocoa Percent", xlab = "Cocoa Percent")

## Based on what it was seen in the histogram of the Cocoa percent, I will create anothe variable categorizing the cocoa.percent by low (<60), medium (60-80) and high (>80)
data_to_use <- data_to_use %>%
  mutate(Cocoa.Level = case_when(
    Cocoa.Percent < 60 ~ "Low",
    Cocoa.Percent >= 60 & Cocoa.Percent <= 80 ~ "Medium",
    Cocoa.Percent > 80 ~ "High"
  ))

data_to_use = subset(data_to_use, select = -Cocoa.Percent)

data_to_use$Review.Date <- as.factor(data_to_use$Review.Date)

## Dropping Company Maker 

data_to_use = subset(data_to_use, select = -Company...Maker.if.known.)

## Now let's let's plot the dendrogram using a sample data 
# Perform stratified sampling
sampled_data <- data_to_use %>%
  group_by(Cocoa.Level) %>%  # Group by the category
  sample_n(size = 28 / n_distinct(Cocoa.Level), replace = FALSE) %>%  # Adjust size per group
  ungroup()

sampled_data$Combined <- paste(sampled_data$Cocoa.Level, sampled_data$Company.Location.Continent, sampled_data$Bean.Origin.Continent, sampled_data$Review.Date, sep = " ")

# Select unique rows based on a specific column
sampled_data <- sampled_data[!duplicated(sampled_data$Combined), ]

rownames(sampled_data)= sampled_data$Combined

## Now, let's plot the dendrogram for the sample data for report and insights purposes 
## IMPORTANT: This dendrogram was made for visual purposes. If try to plot it again, it may show different because it is based on the sample datase above from the original data.
hc = hclust(dist(sampled_data))
plot(hc, hang=-1, cex=0.6)
cutree(hc, k=4)


## Now let's plot the dendrogram for the entire dataset
hc = hclust(dist(data_to_use))
plot(hc, hang=-1, cex=0.6)

## Now let's go back to the original data and assign the clusters for each row
data_to_use$Cluster <- cutree(hc, k = 4) 


## PART 2: Now let's build our regression model to predict the chocolate rating using the new 'Cluster' variable
## Let's bring the chocolate rating back to our dataset.

data_to_use$RowIndex <- rownames(data_to_use)
Ratings$RowIndex <- rownames(Ratings)

# Perform the join on the RowIndex column
joined_data <- merge(data_to_use, Ratings, by = "RowIndex")

joined_data$Rating <- as.numeric(as.character(joined_data$Rating))

library(dplyr)
joined_data <- joined_data %>%
  mutate(Rating = case_when(
    Rating <= 1 ~ "Unpleasant",
    Rating >  1 & Rating <= 2 ~ "Disappointing",
    Rating >  2 & Rating <= 3 ~ "Satisfactory",
    Rating >  3 & Rating <= 4 ~ "Premium",
    Rating >  4               ~ "Elite"
  ))

## Let's see Review Date by Rating Category
# Summarize the data by category
grouped_count <- joined_data %>%
  group_by(Review.Date, Rating) %>%
  count()

# Create a grouped bar chart
ggplot(grouped_count, aes(x = Review.Date, y = n, fill = Rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Review Date by Rating", x = "Review Date", y = "Count") +
  theme_minimal()

## Let's see Cocoa Level by Rating Category
# Summarize the data by category
grouped_count <- joined_data %>%
  group_by(Cocoa.Level, Rating) %>%
  count()

# Create a grouped bar chart
ggplot(grouped_count, aes(x = Cocoa.Level, y = n, fill = Rating)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Cocoa Level by Rating", x = "Cocoa Level", y = "Count") +
  theme_minimal()

joined_data$Rating <- as.factor(joined_data$Rating)
# Drop the 'RowIndex' column
joined_data <- joined_data[ , !(names(joined_data) %in% "RowIndex")]

## Let's drop Elite and Unpleasant Rating category as there very few data point of these categories
joined_data <- joined_data[!(joined_data$Rating %in% c("Elite","Unpleasant")), ]
joined_data$Rating <- droplevels(joined_data$Rating)

joined_data$Bean.Origin.Continent <- as.factor(unlist(joined_data$Bean.Origin.Continent))

## Building the Random Forest (Classification) Model
## Note: When you also run this, it will generate a different random forest model but with similar accuracy (around ~60%) of the model that is shown in the report.
install.packages('randomForest')

library(randomForest)

rf_model <- randomForest(
  Rating ~ Cluster + Cocoa.Level + Company.Location.Continent  + Review.Date,
  data = joined_data,
  cp= 0.01,
  ntree = 100,
  importance = TRUE,
  na.action = na.omit
)

rf_model

# Feature Importance
importance(rf_model)
varImpPlot(rf_model)
