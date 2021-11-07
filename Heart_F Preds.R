library(DataExplorer)
library(dlookr)
library(tidymodels)
library(flextable)

heart <- read.csv('heart.csv', stringsAsFactors = TRUE)

# Data peak
head(heart)
summary(heart)
overview(heart) %>% flextable()
table(heart$HeartDisease)
plot_intro(heart)
create_report(heart, y = "HeartDisease") #html report


# converting Heartdisease to factor
heart$HeartDisease <- ifelse(heart$HeartDisease == 1, "yes", "no")
heart$HeartDisease <- as.factor(heart$HeartDisease)

# Data Diagnosis
overview(heart)
plot_intro(heart)
describe(heart)
diagnose(heart) %>% flextable()
diagnose_category(heart) %>% flextable()
diagnose_numeric(heart) %>% flextable()
diagnose_outlier(heart) %>% flextable()
diagnose_web_report(heart)

# Variables distribution
plot_bar_category(heart)
plot_hist_numeric(heart)

# Categories Distribution By outcome variable
plot_bar(heart, by  = "HeartDisease")

#correlations
plot_correlate(heart)
plot_histogram(heart)
plot_boxplot(heart, by = "HeartDisease")


# Running a logistic Model
set.seed(1223)

#logistic model
heartlog <- logistic_reg() %>%
  set_engine('glm') %>%
  set_mode('classification')

#Data split
heart_split <-
  initial_split(heart, prop = 0.75, strata = HeartDisease)
heart_train <- heart_split %>% training()
heart_test <- heart_split %>% testing()

#heart disease fit model
heartfit <- heartlog %>%
  fit(HeartDisease ~ ., data = heart)
vip(heartfit)

# Probability and class preds
class_preds <- predict(heartfit, new_data = heart_test,
                       type = 'class')
prob_preds <- predict(heartfit, new_data = heart_test,
                      type = 'prob')

 # A tibble binding actual and predicted results
heartresults <- heart_test %>%
  select(HeartDisease) %>%
  bind_cols(class_preds, prob_preds)

# Confusion matrix
heartconfmatrix <- conf_mat(heartresults,
                  truth = HeartDisease,estimate =.pred_class)

custom_metrics <- metric_set(accuracy, sens, spec)

heartconfmatrix %>% autoplot(type = 'heatmap')

#roc curve
roc_curve(heartresults, truth = HeartDisease,
          estimate = .pred_no) %>%
  autoplot()

