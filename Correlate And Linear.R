
the.data <- read_excel("/Users/xiaoxiaohuang/Desktop/student-mat.csv")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students


setwd("/Users/xiaoxiaohuang/Desktop/")

data.mat <- read.csv("student-mat.csv",sep = ";", quote = '"', header = TRUE)
data.pro <- read.csv("student-por.csv",sep = ";", quote = '"', header = TRUE)

# View the first few rows of the data
head(data.mat)
head(data.pro)

selected_data <- data.pro[, c("studytime", "schoolsup", "higher","internet",
                              "health","absences", "failures","paid")]


out_dat = data.pro[,c("G1","G2","G3")]
out_dat <- (out_dat / 20)*100
head(out_dat)

combined_data <- cbind(selected_data, out_dat)
head(combined_data)

# Select numeric columns from the dataset
numeric_data <- combined_data[, sapply(combined_data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)


library(psych)  # for phi coefficient

# Data selection
selected_data <- data.pro[, c("studytime", "schoolsup", "higher", "internet", 
                                "health", "absences", "failures", "paid")]
head(selected_data)
  
# Outcome variables 
out_dat = data.pro[, c("G1", "G2", "G3")]
out_dat <- (out_dat / 20) * 100
head(out_dat)

combined_data <- cbind(selected_data, out_dat)
head(combined_data)

numeric_data <- combined_data[, sapply(combined_data, is.numeric)]

correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)



# Calculate the Phi coefficient correlation matrix for binary variables. 
# Pearson's correlation coeffecient
binary_correlation_matrix <- phi(binary_vars)
print(binary_correlation_matrix)

# Convert binary variables to numeric (0/1)
binary_vars <- combined_data[, c("schoolsup", "higher", "internet", "paid")]
binary_vars <- data.frame(lapply(binary_vars, function(x) as.numeric(as.factor(x)) - 1))

# Extract the original numeric variables (e.g., studytime, health, absences, failures, G1, G2, G3)
numeric_vars <- combined_data[, c("studytime", "health", "absences", "failures", "G1", "G2", "G3")]

#  Combine binary and numeric variables into one dataset
all_vars <- cbind(binary_vars, numeric_vars)

#  Calculate the correlation matrix for both binary and numeric variables
combined_correlation_matrix <- cor(all_vars, use = "complete.obs")
print(combined_correlation_matrix)





# linear
linear_model <- lm(G3 ~ studytime + health + absences + failures + G1 + G2, data = numeric_data)
summary(linear_model)

# Make predictions on the training data
predicted_G3 <- predict(linear_model, newdata = numeric_data)

# Compare predicted values to actual values
head(data.frame(Actual = numeric_data$G3, Predicted = predicted_G3))

# Mean Squared Error (MSE)
mse <- mean((numeric_data$G3 - predicted_G3)^2)
mse
SST <- sum((numeric_data$G3 - mean(numeric_data$G3))^2)
SSE <- sum((numeric_data$G3 - predicted_G3)^2)
r_squared <- 1 - (SSE / SST)
r_squared
