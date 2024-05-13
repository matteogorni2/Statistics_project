data <- read.csv('~/Downloads/Dropout and Success/student_data.csv', 
                 sep = ';')
str(data)
table(data$Output)
# Remove 'Enrolled' to have a binomial problem
index <- which(data$Output == 'Enrolled')
data <- data[-index, ]

# Change features as categorical
cat_names <- colnames(data)
categorical_names <- cat_names[-c(18, 20:34)]
for(i in categorical_names){
  data[, i] <- as.factor(data[, i])
}


# Models
fit1 <- glm(Output ~ ., 
            family = binomial, 
            data = data)
summary(fit1)
table(data$Output, round(fitted(fit1)))
prop.table(table(data$Output, round(fitted(fit1)))) # 92% Accuracy

fit2 <- MASS::stepAIC(fit1, 
                      direction = 'backward')
summary(fit2)
table(data$Output, round(fitted(fit2)))
prop.table(table(data$Output, round(fitted(fit2)))) # 92% Accuracy

fit3 <- update(fit2, 
               .~. - Course)
summary(fit3)


# Proviamo a togliere i curricular
data2 <- data[, -c(20:31)]
fit1b <- glm(Output ~ ., 
             family = binomial, 
             data = data2)
summary(fit1b)
table(data$Output, round(fitted(fit1b)))
prop.table(table(data$Output, round(fitted(fit1b))))  # 80% Accuracy

fit2b <- MASS::stepAIC(fit1b, 
                      direction = 'backward')
summary(fit2b)
table(data$Output, round(fitted(fit2b)))
prop.table(table(data$Output, round(fitted(fit2b)))) # 80% Accuracy


table(data$Course, data$Output)


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


## Application mode
table(data$Application.order, data$Application.mode)
# Da capire cosa significa. Probabilmente possiamo toglierli


## Course
table(data$Course)
# Per il momento lasciamolo così


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
                             data$Mother.s.occupation == 12 | #Other
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
data$Mother.s.occupation[index_m_no_worker] <- 'No Worker'
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
data$Father.s.occupation[index_f_no_worker] <- 'No Worker'
data$Father.s.occupation[index_f_white] <- 'White Collar'
data$Father.s.occupation[-c(index_f_no_worker, 
                            index_f_white)] <- 'Blue Collar'


## Analisi esplorativa