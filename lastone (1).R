##RESEARCH STATEMENT##
##Learning environments that encourage interaction improve student engagement.##

##INSTALL BLOCK##
install.packages(c(
  "janitor",
  "dplyr",
  "ggplot2",
  "corrplot",
  "car",
  "caret",
  "e1071"
))
install.packages("effsize")
install.packages("lm.beta")
install.packages("mediation")
install.packages(c("randomForest"))

library(randomForest)
library(e1071)

##LOAD LIBRARIES##
library(janitor)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(caret)
library(effsize)
library(lm.beta)
library(mediation)

##PART 1 — DATA COLLECTION & CLEANING (LO1, LO4 – Apply)##

##Step 1: Load Data##

getwd()
setwd("D:/university/3 yr-1 sem/TPSM/assignment/FinalAssignment")
data <- read.csv("StudentEngagement.csv")

data <- clean_names(data)

##STEP 2 — Convert categorical variables to factors##
data$gender <- as.factor(data$gender)
data$previous_art_experience <- as.factor(data$previous_art_experience)
data$teaching_method <- as.factor(data$teaching_method)
data$feedback_received <- as.factor(data$feedback_received)
data$adaptive_content_used <- as.factor(data$adaptive_content_used)
data$teaching_effectiveness_label <- as.factor(data$teaching_effectiveness_label)

## Select numeric variables
numeric_vars <- sapply(data, is.numeric)
num_data <- data[, numeric_vars]

## Summary statistics
summary(num_data)

## Standard deviation
sapply(num_data, sd)

## Skewness (important for normality assumption)
library(e1071)
sapply(num_data, skewness)

### see which columns are constant##
zero_var_cols <- sapply(num_data, function(x) sd(x, na.rm = TRUE) == 0)
names(num_data)[zero_var_cols]          
num_data_clean <- num_data[, !zero_var_cols]

cor_matrix <- cor(num_data_clean, use = "complete.obs")
print(cor_matrix)

##Correlation Heatmap (Correlation Mapping)##
install.packages("corrplot")   # run once
library(corrplot)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.cex = 0.7)


eng_cor <- sort(cor_matrix[, "engagement_score"], decreasing = TRUE)
eng_cor

##Only variables with correlation >= 0.70 will be considered strong predictors##
threshold <- 0.70
candidate <- eng_cor[names(eng_cor) != "engagement_score"]
selected_X <- names(candidate[abs(candidate) >= threshold])

selected_X

##STEP 3 — Data quality checks (missing + duplicates+ Outlier Detection)##
sum(is.na(data))
sum(duplicated(data))

numeric_vars <- sapply(data, is.numeric)
num_data <- data[, numeric_vars]

boxplot(num_data, las=2)

##Boxplot without student_id##
num_data2 <- num_data[, colnames(num_data) != "student_id"]
boxplot(num_data2, las=2, main="Numeric Variables (without student_id)")

##Correlation Mapping##

numeric_vars <- sapply(data, is.numeric)
num_data <- data[, numeric_vars]
num_data2 <- num_data[, colnames(num_data) != "student_id"]

##Create Correlation Matrix##
cor_matrix <- cor(num_data2)

##Extract Correlation with DV##
cor_with_dv <- cor_matrix["engagement_score", ]

sort(cor_with_dv, decreasing = TRUE)

##Feature selection rule (numeric)##
##Pick variables with strong relationship, e.g. |r| ≥ 0.30:#3
selected_numeric <- names(cor_with_dv[abs(cor_with_dv) >= 0.30])
selected_numeric

##Descriptive Analytics##
##Data Structure and Quality##
str(data)
## Number of observations and variables
dim(data)

sum(is.na(data))
sum(duplicated(data))

##Descriptive Statistics (Numeric Variables)##
##Add SD explicitly:##
sapply(data[sapply(data, is.numeric)], sd)

##Add skewness:##
library(e1071)
sapply(data[sapply(data, is.numeric)], skewness)

##Descriptive Statistics for Core Numeric Variables

##Focus deeply only on:
  
  ##engagement_score

  ##participation_rate

  ##assignment_completion_time_min

## Mean, Median, Min, Max
summary(data$engagement_score)
summary(data$participation_rate)
summary(data$assignment_completion_time_min)

## Standard Deviation
sd(data$engagement_score)
sd(data$participation_rate)
sd(data$assignment_completion_time_min)

## Skewness
library(e1071)
skewness(data$engagement_score)
skewness(data$participation_rate)
skewness(data$assignment_completion_time_min)

##Distribution Visualization
hist(data$engagement_score)
hist(data$participation_rate)
hist(data$assignment_completion_time_min)

##Normality Assumption Check##
shapiro.test(data$engagement_score)
shapiro.test(data$participation_rate)

##Outlier Detection##
boxplot(data$engagement_score)
boxplot(data$participation_rate)
boxplot(data$assignment_completion_time_min)

##Descriptive for Learning Environment Variables - Teaching Method##
table(data$teaching_method)
prop.table(table(data$teaching_method))

##Feedback Received##
table(data$feedback_received)
prop.table(table(data$feedback_received))

##Adaptive Content Used##
table(data$adaptive_content_used)
prop.table(table(data$adaptive_content_used))

##Preliminary Group Comparisons (VERY IMPORTANT)##
##Engagement by Teaching Method##
aggregate(engagement_score ~ teaching_method, data=data, mean)
aggregate(engagement_score ~ teaching_method, data=data, sd)

##Boxplot:
ggplot(data, aes(x=teaching_method, y=engagement_score)) +
  geom_boxplot()

##Participation by Teaching Method##
aggregate(participation_rate ~ teaching_method, data=data, mean)

##Completion Time by Teaching Method##
aggregate(assignment_completion_time_min ~ teaching_method, data=data, mean)

##Correlation Analysis (Descriptive Relationship)##
cor(data[, c("engagement_score",
             "participation_rate",
             "assignment_completion_time_min",
             "skill_improvement",
             "motivation_increase")])

##Equal Variance Assumption (For ANOVA)##
library(car)
leveneTest(engagement_score ~ teaching_method, data=data)

##Create the Learning Environment Score (Composite Variable)##
env_data <- data.frame(
  teaching = ifelse(data$teaching_method == "AIGC", 1, 0),
  feedback = ifelse(data$feedback_received == "Yes", 1, 0),
  adaptive = ifelse(data$adaptive_content_used == "Yes", 1, 0),
  visual = data$visual_quality_rating,
  relevance = data$perceived_content_relevance
)

data$learning_environment_score <- rowMeans(env_data)

##Descriptive Statistics for Learning Environment##
summary(data$learning_environment_score)
sd(data$learning_environment_score)

##Histogram:
hist(data$learning_environment_score)

##Boxplot:
boxplot(data$learning_environment_score)

##Descriptive Statistics for Key Variables in the Pathway

##These variables must be described:
  
  ##learning_environment_score

  ##participation_rate

  ##assignment_completion_time_min

  ##engagement_score

summary(data[,c(
  "learning_environment_score",
  "participation_rate",
  "assignment_completion_time_min",
  "engagement_score"
)])

##Standard deviation:
sapply(data[,c(
  "learning_environment_score",
  "participation_rate",
  "assignment_completion_time_min",
  "engagement_score"
)], sd)

##Distribution Check
##Histograms:

hist(data$participation_rate)
hist(data$assignment_completion_time_min)
hist(data$engagement_score)

##Outlier Detection
boxplot(data$learning_environment_score)


##Correlation Analysis (Important for Pathways)
cor(data[,c(
"learning_environment_score",
"participation_rate",
"assignment_completion_time_min",
"engagement_score"
)])

##Descriptive Evidence for Pathway 1
##Learning Environment → Participation → Engagement
cor(data$learning_environment_score, data$participation_rate)

cor(data$participation_rate, data$engagement_score)

##Descriptive Evidence for Pathway 2
##Learning Environment → Completion Time → Engagement

cor(data$learning_environment_score, data$assignment_completion_time_min)

cor(data$assignment_completion_time_min, data$engagement_score)

##Visual Evidence for Pathways
plot(data$learning_environment_score, data$participation_rate)

plot(data$participation_rate, data$engagement_score)

plot(data$learning_environment_score, data$assignment_completion_time_min)

plot(data$assignment_completion_time_min, data$engagement_score)


##Add QQ plots (better than Shapiro alone)##
qqnorm(data$engagement_score); qqline(data$engagement_score)
qqnorm(data$participation_rate); qqline(data$participation_rate)


##Add descriptive table (Mean ± SD) by environment group
aggregate(cbind(participation_rate, engagement_score, assignment_completion_time_min) ~ teaching_method,
          data=data, FUN=mean)

aggregate(cbind(participation_rate, engagement_score, assignment_completion_time_min) ~ teaching_method,
          data=data, FUN=sd)

##A) Correlation method choice (Pearson vs Spearman)

##Because normality is not perfect, include Spearman too:
cor(data$learning_environment_score, data$participation_rate, method="spearman")
cor(data$participation_rate, data$engagement_score, method="spearman")
cor(data$learning_environment_score, data$assignment_completion_time_min, method="spearman")
cor(data$assignment_completion_time_min, data$engagement_score, method="spearman")


##One descriptive table
library(dplyr)

data %>%
  summarise(
    n = n(),
    env_mean = mean(learning_environment_score), env_sd = sd(learning_environment_score),
    part_mean = mean(participation_rate), part_sd = sd(participation_rate),
    time_mean = mean(assignment_completion_time_min), time_sd = sd(assignment_completion_time_min),
    eng_mean = mean(engagement_score), eng_sd = sd(engagement_score)
)


##Inferential Test##

##Test whether engagement differs by teaching method

##Group means
aggregate(engagement_score ~ teaching_method, data=data, mean)

##Group standard deviations
aggregate(engagement_score ~ teaching_method, data=data, sd)

##Levene’s Test
leveneTest(engagement_score ~ teaching_method, data=data)

t.test(engagement_score ~ teaching_method, data = data, var.equal = TRUE)

##Inferential Correlation Analysis

##participation_rate ↔ engagement_score → very strong
##assignment_completion_time_min ↔ engagement_score → strong negative
cor.test(data$learning_environment_score, data$engagement_score, method="spearman")

cor.test(data$participation_rate, data$engagement_score, method = "spearman")

cor.test(data$assignment_completion_time_min, data$engagement_score, method = "spearman")

##Regression Analysis-Multiple Linear Regression
##engagement_score = f(participation_rate, completion_time)

model <- lm(engagement_score ~ participation_rate + assignment_completion_time_min, data = data)

summary(model)

##model including learning environment
model2 <- lm(engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min, data = data)
summary(model2)

##ANOVA TEST-teaching_method
anova_model <- aov(engagement_score ~ teaching_method, data = data)
summary(anova_model)

##Confidence Intervals (CI)
##(a) CI for correlations (Spearman → use Pearson approx or bootstrap)

##Simple (Pearson approximation)
cor.test(data$participation_rate, data$engagement_score, method = "pearson")
cor.test(data$assignment_completion_time_min, data$engagement_score, method = "pearson")
cor.test(data$learning_environment_score, data$engagement_score, method = "pearson")

##Better (bootstrap for Spearman):
library(boot)

spearman_fn <- function(d, i) cor(d[i,1], d[i,2], method="spearman")

boot_ci <- function(x, y){
  d <- data.frame(x, y)
  b <- boot(d, statistic = function(d,i) spearman_fn(d, i), R=1000)
  boot.ci(b, type="perc")
}

boot_ci(data$participation_rate, data$engagement_score)
boot_ci(data$assignment_completion_time_min, data$engagement_score)
boot_ci(data$learning_environment_score, data$engagement_score)

##(b) CI for regression coefficients

confint(model)   # for model 1
confint(model2)  # for final model

##Effect Size
##(a) Effect size for t-test → Cohen’s d
library(effsize)
cohen.d(engagement_score ~ teaching_method, data = data)

##(b) Effect size for regression → standardized coefficients
library(lm.beta)
lm.beta(model2)

##Mediation Analysis

library(mediation)

# Model for mediator (Participation)
med_model1 <- lm(participation_rate ~ learning_environment_score, data = data)

# Outcome model
out_model1 <- lm(engagement_score ~ participation_rate + learning_environment_score, data = data)

mediation1 <- mediate(med_model1, out_model1, treat="learning_environment_score", mediator="participation_rate", boot=TRUE)
summary(mediation1)

##For completion time:

med_model2 <- lm(assignment_completion_time_min ~ learning_environment_score, data = data)

out_model2 <- lm(engagement_score ~ assignment_completion_time_min + learning_environment_score, data = data)

mediation2 <- mediate(med_model2, out_model2, treat="learning_environment_score", mediator="assignment_completion_time_min", boot=TRUE)
summary(mediation2)



##Predective analysis
##Define the predictive problem and create train/test data
##Prepare Dataset
set.seed(123)

library(caret)

predictive_data <- data[, c(
  "engagement_score",
  "learning_environment_score",
  "participation_rate",
  "assignment_completion_time_min"
)]

##Train/Test Split
train_index <- createDataPartition(predictive_data$engagement_score, p = 0.8, list = FALSE)

train_data <- predictive_data[train_index, ]
test_data  <- predictive_data[-train_index, ]

dim(train_data)
dim(test_data)
summary(train_data)
summary(test_data)

##Build Main Predictive Model (Engagement)
model_pred <- lm(
  engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min,
  data = train_data
)

summary(model_pred)


##Predict Test Data
predictions <- predict(model_pred, newdata = test_data)
head(predictions)


##Evaluate Model (RMSE)
library(Metrics)

rmse_value <- rmse(test_data$engagement_score, predictions)
rmse_value


##PATH-BASED PREDICTIVE ANALYSIS
##PATH 1-Learning Environment → Participation
model_participation <- lm(
  participation_rate ~ learning_environment_score,
  data = train_data
)

summary(model_participation)

##PATH 2-Learning Environment → Completion Time
model_completion <- lm(
  assignment_completion_time_min ~ learning_environment_score,
  data = train_data
)

summary(model_completion)

##FINAL PATH MODEL-Participation & Completion → Engagement
model_path_final <- lm(
  engagement_score ~ participation_rate + assignment_completion_time_min,
  data = train_data
)

summary(model_path_final)

##Linear Regression comparison
lm_model <- lm(engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min, data=train_data)

lm_pred <- predict(lm_model, test_data)
rmse_lm <- rmse(test_data$engagement_score, lm_pred)


##Train Random Forest Model

library(randomForest)

rf_model <- randomForest(
  engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

rf_model

##Random Forest Prediction Accuracy
rf_predictions <- predict(rf_model, newdata = test_data)
rmse_rf <- rmse(test_data$engagement_score, rf_predictions)
rmse_rf

##Predict using Random Forest
rf_predictions <- predict(rf_model, newdata = test_data)

head(rf_predictions)

##Evaluate Random Forest (RMSE)
library(Metrics)

rmse_rf <- rmse(test_data$engagement_score, rf_predictions)

rmse_rf

##Feature Importance
importance(rf_model)

varImpPlot(rf_model)


##K-FOLD CROSS-VALIDATION

library(caret)

set.seed(123)

# Define cross-validation method
train_control <- trainControl(method = "cv", number = 10)

# Train model using cross-validation
cv_model <- train(
  engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min,
  data = predictive_data,
  method = "lm",
  trControl = train_control
)

cv_model

##SVM MODEL
library(e1071)

svm_model <- svm(
  engagement_score ~ learning_environment_score + participation_rate + assignment_completion_time_min,
  data = train_data
)

svm_predictions <- predict(svm_model, newdata = test_data)

library(Metrics)
rmse_svm <- rmse(test_data$engagement_score, svm_predictions)

rmse_svm
