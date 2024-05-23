library(pROC)

data <- read.csv('~/Downloads/Dropout and Success/student_data.csv', 
                 sep = ';')
str(data)
table(data$Output)
# Remove 'Enrolled' to have a binomial problem

# La nostra domanda sarà:
# Vogliamo creare un modello che classifichi se uno studente
# quando s'iscrive all'università si laurei in tempo oppure no
# Quindi vogliamo prevedere solo chi si laurea in tempo
index_enrolled <- which(data$Output == 'Enrolled')
# data <- data[-index, ]
data$Output[index_enrolled] <- 'Dropout'

# Rimuoviamo le variabili Curricular
data <- data[, -c(20:31)]
# Dato che raggruppiamo i corsi non c'interessa quali siano serali
data <- data[, -5]
# Rimuoviamo Application.mode e order che sono confusionarie
index <- which(colnames(data) == 'Application.mode' | 
                 colnames(data) == 'Application.order')
data <- data[, -index]
### Collapse some categorical factor

## Marital status
table(data$Marital.status)
# Only 4 widower and 6 legally separated we collapse all
# in just 3 categories: (single, married and other)
index_single <- which(data$Marital.status == 1) # Single
index_married <- which(data$Marital.status == 2) # Married
data$Marital.status[index_single] <- 'Single'
data$Marital.status[index_married] <- 'Married'
data$Marital.status[-c(index_single, index_married)] <- 'Others'

chisq.test(table(data$Marital.status, 
                 data$Output)[1:2, 1:2])
## Raggruppiamo married e others perche non sono particolarmente diversi
# e in più sono pochi
# data$Marital.status[-index_single] <- 'Others'



## Course
table(data$Course)
index_stem <- which(data$Course == 1 |
                      data$Course == 4 |
                      data$Course == 6 |
                      data$Course == 7 |
                      data$Course == 8 |
                      data$Course == 12 |
                      data$Course == 13)
data$Course[index_stem] <- 'Stem'
data$Course[-index_stem] <- 'No Stem'



## Previous qualification
# Group on (no secondary education, secondary education, bachelor's degree, 
# master's degree, phd or highter)
table(data$Previous.qualification)
index_secondary <- which(data$Previous.qualification == 1 | 
                           data$Previous.qualification == 6 |
                           data$Previous.qualification == 14 |
                           data$Previous.qualification == 16)
# Aggiungiamo '6' perchè se frequenti l'uni ti sei già diplomato e 
# se fai dei corsi da specializzando tecnico dovresti aver 
# fatto le superiori, però non valgono come la laurea

index_higher <- which(data$Previous.qualification == 2 |
                        data$Previous.qualification == 3 |
                        data$Previous.qualification == 15 |
                        data$Previous.qualification == 4 | # Since just 8
                        data$Previous.qualification == 5| # Since just 1 phd
                        data$Previous.qualification == 17)
data$Previous.qualification[index_secondary] <- 'Secondary'
data$Previous.qualification[index_higher] <- 'Higher'
data$Previous.qualification[-c(index_secondary, 
                               index_higher)] <- 'No Secondary'


## Nationality
table(data$Nacionality)
data$Nacionality[data$Nacionality != 1] <- 'Others' # just 2%
data$Nacionality[data$Nacionality == 1] <- 'Portoguese'


## Mother's qualificatoin
table(data$Mother.s.qualification)
index_m_secondary <- which(data$Mother.s.qualification == 1 | 
                             data$Mother.s.qualification == 6 |
                             data$Mother.s.qualification == 13 |
                             data$Mother.s.qualification == 15 |
                             data$Mother.s.qualification == 22 |
                             data$Mother.s.qualification == 23 |
                             data$Mother.s.qualification == 29 |
                             data$Mother.s.qualification == 31 |
                             data$Mother.s.qualification == 32)
index_m_higher <- which(data$Mother.s.qualification == 2 |
                          data$Mother.s.qualification == 3 |
                          data$Mother.s.qualification == 4 |
                          data$Mother.s.qualification == 5 |
                          data$Mother.s.qualification == 30 |
                          data$Mother.s.qualification == 33 |
                          data$Mother.s.qualification == 34)
data$Mother.s.qualification[index_m_secondary] <- 'Secondary'
data$Mother.s.qualification[index_m_higher] <- 'Higher'
data$Mother.s.qualification[-c(index_m_secondary, 
                               index_m_higher)] <- 'No Secondary'


## Father's qualification
table(data$Father.s.qualification)
index_f_secondary <- which(data$Father.s.qualification == 1 | 
                             data$Father.s.qualification == 6 |
                             data$Father.s.qualification == 13 |
                             data$Father.s.qualification == 15 |
                             data$Father.s.qualification == 22 |
                             data$Father.s.qualification == 23 |
                             data$Father.s.qualification == 29 |
                             data$Father.s.qualification == 31 |
                             data$Father.s.qualification == 32)
index_f_higher <- which(data$Father.s.qualification == 2 |
                          data$Father.s.qualification == 3 |
                          data$Father.s.qualification == 4 |
                          data$Father.s.qualification == 5 |
                          data$Father.s.qualification == 30 |
                          data$Father.s.qualification == 33 |
                          data$Father.s.qualification == 34)
data$Father.s.qualification[index_f_secondary] <- 'Secondary'
data$Father.s.qualification[index_f_higher] <- 'Higher'
data$Father.s.qualification[-c(index_f_secondary, 
                               index_f_higher)] <- 'No Secondary'


## Mother's occupation
table(data$Mother.s.occupation)
# Slpit in white / blue collar
index_m_no_worker <- which(data$Mother.s.occupation == 1| # Student
                             data$Mother.s.occupation == 12 | # Other
                             data$Mother.s.occupation == 13) # Blanck
index_m_white <- which(data$Mother.s.occupation == 2 |
                         data$Mother.s.occupation == 3 |
                         data$Mother.s.occupation == 4 |
                         data$Mother.s.occupation == 5 |
                         data$Mother.s.occupation == 6 |
                         data$Mother.s.occupation == 14 | # Armed forces Officers
                         data$Mother.s.occupation == 15 | # Armed forces sergeants
                         data$Mother.s.occupation ==  17 |
                         data$Mother.s.occupation == 19 |
                         data$Mother.s.occupation == 20 |
                         data$Mother.s.occupation == 21 |
                         data$Mother.s.occupation == 22 |
                         data$Mother.s.occupation == 23 |
                         data$Mother.s.occupation == 24 |
                         data$Mother.s.occupation == 25 |
                         data$Mother.s.occupation == 26 |
                         data$Mother.s.occupation == 27 |
                         data$Mother.s.occupation == 28 |
                         data$Mother.s.occupation == 29)
data$Mother.s.occupation[index_m_no_worker] <- 'Others'
data$Mother.s.occupation[index_m_white] <- 'White Collar'
data$Mother.s.occupation[-c(index_m_no_worker, 
                            index_m_white)] <- 'Blue Collar'

## Father's occupation
table(data$Father.s.occupation)
# Slpit in white / blue collar
index_f_no_worker <- which(data$Father.s.occupation == 1| # Student
                             data$Father.s.occupation == 12 | #Other
                             data$Father.s.occupation == 13) # Blanck
index_f_white <- which(data$Father.s.occupation == 2 |
                         data$Father.s.occupation == 3 |
                         data$Father.s.occupation == 4 |
                         data$Father.s.occupation == 5 |
                         data$Father.s.occupation == 6 |
                         data$Father.s.occupation == 14 | # Armed forces Officers
                         data$Father.s.occupation == 15 | # Armed forces sergeants
                         data$Father.s.occupation ==  17 |
                         data$Father.s.occupation == 19 |
                         data$Father.s.occupation == 20 |
                         data$Father.s.occupation == 21 |
                         data$Father.s.occupation == 22 |
                         data$Father.s.occupation == 23 |
                         data$Father.s.occupation == 24 |
                         data$Father.s.occupation == 25 |
                         data$Father.s.occupation == 26 |
                         data$Father.s.occupation == 27 |
                         data$Father.s.occupation == 28 |
                         data$Father.s.occupation == 29)
data$Father.s.occupation[index_f_no_worker] <- 'Others'
data$Father.s.occupation[index_f_white] <- 'White Collar'
data$Father.s.occupation[-c(index_f_no_worker, 
                            index_f_white)] <- 'Blue Collar'

# Gender
index_male <- which(data$Gender == 1)
data$Gender[index_male] <- 'Male'
data$Gender[-index_male] <- 'Female'

str(data)

# Change features as categorical
colnames(data)
categorical_names <- colnames(data)[-c(15, 17:19)]

for(i in categorical_names){
  data[, i] <- as.factor(data[, i])
}

## Valutiamo quale variabile prende come riferimento
# Marital Status
levels(data$Marital.status)
data$Marital.status <- relevel(data$Marital.status, ref = "Single")
levels(data$Course)
levels(data$Previous.qualification)
levels(data$Previous.qualification)
data$Previous.qualification <- relevel(data$Previous.qualification, 
                                       ref = 'Higher')
levels(data$Nacionality)
data$Nacionality <- relevel(data$Nacionality, 
                            ref = 'Portoguese')
levels(data$Mother.s.qualification)
levels(data$Father.s.qualification)
levels(data$Mother.s.occupation)
levels(data$Father.s.occupation)
levels(data$Gender)
levels(data$Output)

table(data$Nacionality, data$International)
data <- data[, -which(colnames(data) == 'International')]


# Analisi esplorativa


## Models (Poi questa parte si farà con il train e test)
fit1 <- glm(Output ~ ., 
            family = binomial, 
            data = data)
summary(fit1)
# fit2 <- update(fit1, 
#                .~. - Application.order - 
#                  International - 
#                  Nacionality - 
#                  Father.s.qualification - 
#                  Father.s.occupation - 
#                  Displaced - 
#                  Mother.s.qualification - 
#                  Mother.s.occupation - 
#                  Age.at.enrollment - 
#                  Inflation.rate)

fit2 <- step(fit1, 
             direction = 'backward')
summary(fit2)
anova(fit2, fit1)

table(data$Output, round(fitted(fit2)))
prop.table(table(data$Output, round(fitted(fit2)))) %>% 
  diag %>% sum # 68%

# ROC
library(pROC)

logistic.prob <- fitted(fit2, type = 'response')
roc.out <- roc(data$Output, logistic.prob, levels=c("Dropout", 
                                                    "Graduate"))

# different ways of plotting the ROC curve
plot(roc.out) # check values on the x axis

# legacy.axes=TRUE   1 - specificity on the x axis
plot(roc.out, legacy.axes=TRUE)

# compute AUC = Area Under the Curve
plot(roc.out,  print.auc=TRUE, legacy.axes=TRUE, xlab="False Positive Rate", ylab="True Positive Rate")
auc(roc.out)

# specificity (TNR) and sensitivity (TPR) for a given threshold
coords(roc.out, 0.5)

coords(roc.out, seq(0.1, 0.9, by=0.1))

# threshold that maximizes the sum of specificity (TNR) and sensitivity (TPR)
coords(roc.out, "best")


table(data$Output, 
      data$Marital.status) %>% 
  prop.table() %>%  
  barplot(beside = T, 
          legend.text = T, 
          args.legend = list(x = "topleft", 
                             bty = "n"))



# Pulizia del dataset
set.seed(71)
index <- sample(1:nrow(data), 
                round(0.75 * nrow(data)))
train <- data[index, ]
test <- data[-index, ]

# Rimozione degli outliers
boxplot(train$Age.at.enrollment ~ train$Output)
max_age_drop <- min(boxplot(train$Age.at.enrollment[train$Output == 'Dropout'])$out)
index_age_drop <- which(train$Age.at.enrollment[train$Output == 'Dropout'] >= max_age_drop)
max_age_grad <- min(boxplot(train$Age.at.enrollment[train$Output == 'Graduate'])$out)
index_age_grad <- which(train$Age.at.enrollment[train$Output == 'Graduate'] >= max_age_grad)
train <- train[-c(index_age_drop, index_age_grad), ]

boxplot(train$Unemployment.rate ~ train$Output)
boxplot(train$Inflation.rate ~ train$Output)
boxplot(train$GDP ~ train$Output)


# Models ------------------------------------------------------------------

######################################
# Logistic Regression
######################################
fit1 <- glm(Output ~ ., 
            family = binomial, 
            data = train)
summary(fit1)
fit2 <- step(fit1, 
             direction = 'backward')
summary(fit2)

pred.logit <- predict(fit2, 
                      newdata = test, 
                      type = 'response')
roc.out.logit <- roc(test$Output ~ pred.logit, 
                     levels = c('Dropout', 'Graduate'))
plot(roc.out.logit,  
     print.auc=TRUE, 
     legacy.axes=TRUE, 
     xlab="False Positive Rate", 
     ylab="True Positive Rate")
auc(roc.out.logit)



######################################
# Linear Discriminant Analysis (LDA)
######################################

library(MASS)
lda.fit <- lda(Output ~ ., 
               data = train)
lda.fit

# plot the values of the discriminant function for the two groups
# 
plot(lda.fit, 
     type="histogram")
plot(lda.fit) # histogram is the default value

plot(lda.fit, type="density")
plot(lda.fit, type="both")


pred.lda <- predict(lda.fit, 
                    newdata = test, 
                    type = 'response')
roc.out.lda <- roc(test$Output ~ pred.lda$posterior[, 2], 
                   levels = c('Dropout', 'Graduate'))
plot(roc.out.lda,  
     print.auc=TRUE, 
     legacy.axes=TRUE, 
     xlab="False Positive Rate", 
     ylab="True Positive Rate", 
     main = 'LDA')
auc(roc.out.lda)


######################################
# Quadratic Discriminant Analysis (QDA)
######################################

qda.fit <- qda(Output ~ ., 
               data = train)
qda.fit
pred.qda <- predict(qda.fit, 
                    newdata = test, 
                    type = 'response')
roc.out.qda <- roc(test$Output ~ pred.qda$posterior[, 2], 
                   levels = c('Dropout', 'Graduate'))
plot(roc.out.qda,  
     print.auc=TRUE, 
     legacy.axes=TRUE, 
     xlab="False Positive Rate", 
     ylab="True Positive Rate", 
     main = 'LDA')
auc(roc.out.qda)



###########################
# K-Nearest Neighbors (KNN)
###########################

library(class)
# KNN non applicabile per via di variabili categoriali, 
# ci sarebbe da modificare il dataset, ma non l'abbiamo visto nel corso


###########################
# Ridge Regression
###########################

library(glmnet)


design_matrix_train <- model.matrix(Output ~ .,
                                    data = train)

design_matrix_test <- model.matrix(Output ~ .,
                                   data = test)

# Adattamento del modello Ridge con regressione Binomiale
ridge_model <- glmnet(x = design_matrix_train, 
                      y = train$Output,
                      alpha = 0, 
                      intercept = F, 
                      family = "binomial")
# Find the best lambda

predictions <- predict(ridge_model, 
                       newx = design_matrix_test, 
                       type = 'response')
##########################

accuracies <- c()

for (i in 1:ncol(predictions)) {
  predicted_classes <- round(predictions[, i])
  confusion_matrix <- table(predicted_classes, test$Output)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracies <- c(accuracies, accuracy)
}

best_lambda_index <- which.max(accuracies)
ridge.pred <- matrix(predictions[, best_lambda_index], 
                     ncol = 1)
best_accuracy <- accuracies[best_lambda_index]

colnames(ridge.pred) <- 'ridge.pred'


roc.out.ridge <- roc(test$Output ~ as.numeric(ridge.pred), 
                     levels = c('Dropout', 'Graduate'))
plot(roc.out.ridge,  
     print.auc=TRUE, 
     legacy.axes=TRUE, 
     xlab="False Positive Rate", 
     ylab="True Positive Rate", 
     main = 'LDA')
auc(roc.out.ridge)


###########################
# Lasso Regression
###########################

# Adattamento del modello Lasso con regressione Binomiale
lasso_model <- glmnet(x = design_matrix_train, 
                      y = train$Output,
                      alpha = 1, 
                      intercept = F, 
                      family = "binomial")
# Find the best lambda

predictions <- predict(lasso_model, 
                       newx = design_matrix_test, 
                       type = 'response')
##########################

accuracies <- c()

for (i in 1:ncol(predictions)) {
  predicted_classes <- round(predictions[, i])
  confusion_matrix <- table(predicted_classes, test$Output)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracies <- c(accuracies, accuracy)
}

best_lambda_index <- which.max(accuracies)
lasso.pred <- matrix(predictions[, best_lambda_index], 
                     ncol = 1)
best_accuracy <- accuracies[best_lambda_index]

colnames(lasso.pred) <- 'lasso.pred'


roc.out.lasso <- roc(test$Output ~ as.numeric(lasso.pred), 
                     levels = c('Dropout', 'Graduate'))
plot(roc.out.lasso,  
     print.auc=TRUE, 
     legacy.axes=TRUE, 
     xlab="False Positive Rate", 
     ylab="True Positive Rate", 
     main = 'LDA')
auc(roc.out.lasso)

##à DA vedere se lancia
# add labels to identify the variables
plot(ridge_model, xvar="lambda", label=TRUE)

plot(ridge_model, xvar = "norm", label=TRUE)
plot(ridge_model, xvar = "dev",  label=TRUE)

plot(ridge_model, xvar = 'norm')





# Analisi esplorativa -----------------------------------------------------

str(data)
col_grad <- c('red', 'darkgreen')
barplot(table(data$Output), 
        col = col_grad)
barplot(as.matrix(prop.table(table(data$Marital.status))), 
        horiz = T)
barplot(as.matrix((table(data$Output, data$Marital.status))), 
        beside = T,
        horiz = T, 
        legend = T, 
        col = col_grad, 
        las = 2)
barplot(table(data$Course, 
              data$Output), 
        horiz = T, 
        beside = T, 
        col = col_grad)
barplot(table(data$Output, 
              data$Previous.qualification), 
        horiz = T, 
        beside = T, 
        col = col_grad)
barplot(table(data$Output, 
              data$Nacionality), 
        horiz = T, 
        beside = T, 
        col = col_grad)
