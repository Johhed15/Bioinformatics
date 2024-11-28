install.packages("ade4")
library(ade4)
data(carni70)

data <- carni70$tab
data$animal <- rownames(data)
head(carni70$tab)

# first rows in the dataframe
knitr::kable(head(carni70$tab))

#seems to be about carnivores, each row is contains the latin names of the carnivores, 
# their size in kg and range geographically in km.

library(ggplot2)
library(dplyr)
 

# Identify outliers using IQR
Q1 <- quantile(data$size, 0.1)
Q3 <- quantile(data$size, 0.9)
IQR <- Q3 - Q1

outliers <- data %>%
  filter(size < (Q1 - 2 * IQR) | size > (Q3 + 2 * IQR))

outliers$index <- which(data$animal %in% outliers$animal) # get index

ggplot(data, aes(x = 1:nrow(data), y = size)) +
  geom_point(color = "blue", size=2) +
  geom_text(data = outliers, aes(x = index, y = size, label = gsub("_", " ", animal)), 
            vjust = -0.5, hjust = 0, color = "black", size=4.5) +
  labs(title = "Size of Carnivores (with the heaviest labeled)", x = "Index", y = "Size (kg)") +
  theme_minimal()+theme(
    plot.title = element_text(hjust = 0.5))



# Identify outliers using IQR
Q11 <- quantile(data$range, 0.3)
Q33 <- quantile(data$range, 0.7)
IQR <- Q33 - Q11

outlier <- data %>%
  filter(range < (Q11 -  IQR) | range > (Q33 + IQR))

outlier$index <- which(data$animal %in% outlier$animal) # get index

ggplot(data, aes(x = 1:nrow(data), y = range)) +
  geom_point(color = "blue", size=2) +
  geom_text(data = outlier, aes(x = index, y = range, label = gsub("_", " ", animal)), 
            vjust = -0.5, hjust = 0, color = "black", size=4.5) +
  labs(title = "Range of Carnivores (with the longest span labeled)", x = "Index", y = "Range (km)") +
  theme_minimal()+theme(
    plot.title = element_text(hjust = 0.5))

# puma has clearly the longest geographical range of the carnivores. 
# There seems to be a group of carnivores that has a range between 14-28 and a
#group where the range is inbetween 0 and 7

labels  <- rbind(outlier, outliers)


ggplot(data, aes(x = size, y = range)) +
  geom_point(aes(size = size), alpha = 1) + # Adjust size 
  geom_text(data = labels, aes(x = size, y = range, label = gsub("_", " ", animal)), 
            vjust = -0.9, hjust = 0.7, color = "black", size=4.5) +
  labs(title = "Range vs. Size of Carnivores ", 
       x = "Size(kg)", y = "Range (km)") +
  theme_minimal() +
  theme(legend.position = "bottom")+theme(
    plot.title = element_text(hjust = 0.5))


# Categorize the range into three groups
data$range_group <- cut(data$range,
                        breaks = c(0, 10, 30, 50), # Define intervals
                        labels = c("0-10", "11-30", " >30"),
                        right = TRUE)
# Calculate the mean size for each range group
mean_size_by_group <- data %>%
  group_by(range_group) %>%
  summarize(mean_size = mean(size, na.rm = TRUE))
# Create a table summarizing the number of animals in each group
range_table <- table(data$range_group)

# Display the table
knitr::kable(range_table, col.names = c("Range Group", "Count"))

# Create the bar plot for the range categories with mean size labels
ggplot(data, aes(x = range_group)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(data = mean_size_by_group, aes(x = range_group, y = Inf, label = paste('Group mean size of:',round(mean_size, 2))), 
            vjust = 22, color = "black") + # Place text above the bars
  labs(title = "Distribution of Range Categories with Mean Size", 
       x = "Range Group", y = "Count") +
  theme_minimal()+theme(
    plot.title = element_text(hjust = 0.5))


# In general smaller carnivares in the group with a small range, but as we previous
# saw, there are a few carnivores with really high weight in this interval that makes '
# the mean weight grow. Puma concolor is alone in its group



