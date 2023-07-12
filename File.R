library(ggplot2)
library(ggpubr)
library(corrplot)

# Suppress warnings
options(warn=-1)
data<-read.csv("~/STA 631/Activities/Linear-Regression/advertising.csv")
head(data)

# Get the shape of the DataFrame
dim(data)

# Get the information about the DataFrame
str(data)
summary(data)

# Calculate the percentage of missing values
missing_percentage <- colSums(is.na(data)) * 100 / nrow(data)

# Print the result
print(missing_percentage)

library(ggplot2)
library(gridExtra)

# Create subplots with box plots
p1 <- ggplot(data, aes(x = "", y = TV)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(y = "TV") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- ggplot(data, aes(x = "", y = Newspaper)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(y = "Newspaper") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p3 <- ggplot(data, aes(x = "", y = Radio)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(y = "Radio") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Combine the plots into a grid
grid.arrange(p1, p2, p3, nrow = 3)

# Create a box plot
ggplot(data, aes(x = "", y = Sales)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(y = "Sales") +
  theme_bw()


# Create scatter plots
p1 <- ggplot(data, aes(x = TV, y = Sales)) +
  geom_point() +
  labs(x = "TV", y = "Sales") +
  theme_bw()

p2 <- ggplot(data, aes(x = Newspaper, y = Sales)) +
  geom_point() +
  labs(x = "Newspaper", y = "Sales") +
  theme_bw()

p3 <- ggplot(data, aes(x = Radio, y = Sales)) +
  geom_point() +
  labs(x = "Radio", y = "Sales") +
  theme_bw()

# Combine the plots into a grid
grid.arrange(p1, p2, p3, nrow = 1)



# Compute the correlation matrix
cor_matrix <- cor(data)

# Create a correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("white", "blue"))(100),
         addCoef.col = "black", number.cex = 0.8)

# Create a subset of the data with only the "TV" and "Sales" columns
subset_data <- data[, c("TV", "Sales")]

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(subset_data), 0.7 * nrow(subset_data))
train_data <- subset_data[train_indices, ]
test_data <- subset_data[-train_indices, ]

# Fit a linear regression model
model <- lm(Sales ~ TV, data = train_data)

# Predict the Sales values using the test data
predictions <- predict(model, newdata = test_data)

# Calculate the RMSE
rmse <- sqrt(mean((test_data$Sales - predictions)^2))

# Calculate the R-squared
rsquared <- summary(model)$r.squared

# Print the RMSE and R-squared
cat("RMSE:", rmse, "\n")
cat("R-squared:", rsquared, "\n")

# Visualize the relationship between "TV" and "Sales"
p1 <- ggplot(subset_data, aes(x = TV, y = Sales)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Relationship between TV and Sales") +
  theme_bw()

p2 <- ggplot(train_data, aes(x = Sales, y = residuals(model))) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Sales") +
  theme_bw()

grid.arrange(p1, p2, nrow = 2)











