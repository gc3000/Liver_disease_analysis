#Assignment INDIAN LIVER DISEASE
library(purrr)
library(tidyr)
library(ggplot2)
library(boot)

liver.data <- read.csv("indian_liver_patient.csv")
# NAMES predictors. 
# [1] "Age"                        "Gender"                     "Total_Bilirubin"           
# [4] "Direct_Bilirubin"           "Alkaline_Phosphotase"       "Alamine_Aminotransferase"  
# [7] "Aspartate_Aminotransferase" "Total_Protiens"             "Albumin"                   
# [10] "Albumin_and_Globulin_Ratio" "Dataset" 
# Dataset variable is whether or not you have liver disease, yes (1) or no (2).

#1. Age Age of the patient Yrs.
#2. Gender Gender of the patient M/F
#3. TB Total Bilirubin mg/dL
#4. DB Direct Bilirubin mg/dL
#5. Alkphos Alkaline Phosphotase IU/L
#6. Sgpt Alamine Aminotransferase IU/L
#7. Sgot Aspartate Aminotransferase IU/L
#8. TP Total Protiens g/dL
#9. ALB Albumin g/dL
#10. A/G Ratio Albumin and Globulin Ratio A/G
#11. Selector field used to split the data into two sets (labeled by the experts)

# =================== #
#     CLEAN DATA      #
# =================== #
# Check for NA values.
clean.liver <- liver.data
which(is.na(clean.liver), arr.ind = TRUE)
# HAD TO REMOVE ROWS WHERE ALBUMIN AND GLOBULIN RATIO WAS MISSING "NA". 

clean.liver <- na.omit(clean.liver)
names(clean.liver)[ncol(clean.liver)] <- "Liver_Disease"
# Turn response variable into 1 or 0. 
clean.liver[, "Liver_Disease"] <- ifelse(clean.liver$Liver_Disease == 1, 1, 0)
# Male and Female as 1 and 0 respectively.
clean.liver[ , "Gender"] <- ifelse(clean.liver$Gender == "Male", 1, 0)
# Turn all variables into "numeric"
#as.numeric(clean.liver$Age)
#as.numeric(clean.liver$Gender)
clean.liver$Age
clean.liver$Gender

# SPLIT INTO TRAINING AND TEST DATA. 
# The data has been split 80/20. 
set.seed(1)
train_idx <- sample(nrow(clean.liver), floor(0.8*nrow(clean.liver)), replace = FALSE)
liver.train <- clean.liver[train_idx, ]

# REINDEX
row.names(liver.train) <- 1:nrow(liver.train)
liver.test <- clean.liver[-train_idx, ]
row.names(liver.test) <- 1:nrow(liver.test)

# EXPLAIN THAT AGE FOR PEOPLE OVER 89 IS CATEGORIZED AS 90. 

# ================= #
#  DATA EXPLORATION #
# ================= #

# NOTE REMEMBER THIS IS ALL TRAINING DATA. 
# Data Distribution

liver.train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Summary of data (training).
summary(liver.train)

# Create Table
gender_table <- table(liver.train$Gender, liver.train$Liver_Disease)

# Mean values of variables no disease and disease.
disease <- subset(liver.train, subset = liver.train$Liver_Disease == 1)[ , -c(2, 11)]
no.disease <- subset(liver.train, subset = liver.train$Liver_Disease == 0)[ , -c(2, 11)]
disease_mean <- apply(disease,2, FUN = mean)
no.disease_mean <- apply(no.disease, 2, FUN = mean)
disease_mean <- data.frame(disease_mean)
no.disease_mean <- data.frame(no.disease_mean)
predictors_mean <- cbind(disease_mean, no.disease_mean)
variables <- data.frame(Variables = rownames(predictors_mean))
predictors_mean <- cbind(variables, predictors_mean)
rownames(predictors_mean) <- c(1:nrow(predictors_mean))
pivot <- predictors_mean[ , c(2,3)]
rownames(pivot) <- predictors_mean[ , 1]
par(mar=c(1,9,2,1))
barplot(t(as.matrix(pivot)), names.arg = rownames(pivot), xlab = "Value", beside = TRUE, col = c("lightblue", "lightgreen"),
        horiz = TRUE, las = 2, cex.names = 0.6, legend = TRUE)
# Distribution of People with Liver Disease vs No Disease

par(mar=c(5, 4,4,2)+.1)
barplot(table(liver.train$Liver_Disease), main = "Patients with Liver Disease", names.arg = c("No", "Yes"),
        ylab = "Number of Patients", xlab = "Liver Disease", col = c("lightblue", "lightgreen"))

# Create a graph showing the percentage of men vs women who have liver disease. 
barplot(gender_table, main = "Liver Disease by Gender", xlab = "Gender", col = c("red", "darkred"), names.arg = c("Female", "Male"), beside = TRUE)
legend(x = 1, y = 250, c("Disease", "No Disease"), fill = c("darkred", "red"))

# Boxplot of distribution of ages with liver disease versus no liver disease

par(mar=c(6, 4,4,2)+.1)
boxplot(Age~Liver_Disease, data = liver.train, xlab = "Liver Disease", ylab = "Age (Years)", main = "Age given liver disease or not", ylim = c(0, 100),
        col = c('lightblue','lightgreen'), names = c("No", "Yes"))

# create graph with liver disease vs not liver disease given Bilirubin.
plot(liver.train$Total_Bilirubin, liver.train$Liver_Disease, col = (liver.train$Liver_Disease+1))

# Shows that there is a threshold from which somebody will most likely have liver disease. 

# CORRELATION MATRIX (HELPS US TO FIND MULTICOLLINEARITY)
corr_matrix <- liver.train
names(corr_matrix) <- c("Age", "Gender", "T.Bilirubin", "D.Bilirubin", "Alkaline.Phos", 
                        "Alamine.A", "Aspartate.A", "Total.Proteins", "Albumin", "A/G Ratio", "Liver.Disease")
cm <- corr_matrix[ , c("Age", "T.Bilirubin", "D.Bilirubin", "Alkaline.Phos", 
                       "Alamine.A", "Aspartate.A", "Total.Proteins", "Albumin", "A/G Ratio")]
cor(cm)
corrplot(cor(cm), type="upper", order="hclust", col=brewer.pal(n=8, name="RdYlBu"))
# Plots of co-linear variables
plot(liver.train$Total_Protiens, liver.train$Albumin, pch = 21, bg = 'lightblue', xlab = "Total Proteins g/dL", ylab = "Albumin g/dL")
legend(x =3, y =5, legend = "r=0.79")
plot(liver.train$Albumin_and_Globulin_Ratio, liver.train$Albumin, pch = 21, bg = 'lightblue', xlab = "A/G Ratio", ylab = "Albumin g/dL")
legend(x=2, y=5, legend = "r = 0.7")
plot(liver.train$Alamine_Aminotransferase, liver.train$Aspartate_Aminotransferase, pch = 21, bg = 'lightblue', xlab = "Alamine Aminotransferase IU/L", ylab = "Aspertate Aminotransferase IU/L")
legend(x = 0, y = 4000, legend = "r = 0.73")
plot(liver.train$Total_Bilirubin, liver.train$Direct_Bilirubin, pch = 21, bg = 'lightblue', 
     xlab = "Total Bilirubin g/dL", ylab = "Direct Bilirubin g/dL")
legend(x =0, y = 16.5, legend = "r =0.85")

cor(liver.train$Total_Protiens, liver.train$Albumin)
cor(liver.train$Albumin_and_Globulin_Ratio, liver.train$Albumin)
cor(liver.train$Alamine_Aminotransferase, liver.train$Aspartate_Aminotransferase)
cor(liver.train$Total_Bilirubin, liver.train$Direct_Bilirubin)

# ================================== #
# BUILDING LOGISTIC REGRESSION MODEL #
# ================================== #

# VARIABLES TO KEEP 

# Albumin_and_Globulin_Ratio + Total_Protiens + Age + Alamine_Aminotransferase + Alkaline_Phosphotase +
# Total_Bilirubin

lr.model7 <- glm(formula = Liver_Disease~Albumin_and_Globulin_Ratio + Total_Protiens + Age + Alamine_Aminotransferase + Alkaline_Phosphotase +Total_Bilirubin, family = binomial(link = 'logit'), data = liver.train)
lr.model8 <- glm(formula = Liver_Disease ~ Total_Bilirubin + Alamine_Aminotransferase, 
                 family = binomial(link = "logit"), data = liver.train)
lr.model9 <- glm(formula = Liver_Disease ~ Total_Bilirubin + Alamine_Aminotransferase +
                   I(Total_Bilirubin*Alamine_Aminotransferase), family = binomial, data = liver.train)
lr.model10 <- glm(formula = Liver_Disease~Total_Bilirubin+Alamine_Aminotransferase+Age,
                  family = binomial(link = "logit"), data = liver.train)
lr.model11 <- glm(formula = Liver_Disease~Total_Bilirubin+Alamine_Aminotransferase+Age+
                    I(Total_Bilirubin*Alamine_Aminotransferase), family = binomial,
                  data = liver.train) 
summary(lr.model8)
summary(lr.model9)
summary(lr.model10)
summary(lr.model11)

# ============#
#  K FOLD CV  #
# ============#
set.seed(10)
cost2 <- function(r ,pi =0) mean(abs(r-pi) > 0.6 )
models <- list(lr.model8, lr.model9, lr.model10, lr.model11)
cv.models2 <- c()
# Threshold 0.6
for(i in 1:length(models)){
  cv.models2[i] <- cv.glm(liver.train, models[[i]], cost = cost2, K=10)$delta[1]
}
cv.models2
# ============= #
#   PREDICTION  #
# ============= #
prex11 <- predict(lr.model11, newdata = liver.test, type = 'response')
classx11 <- ifelse(prex11 >= 0.60, 1, 0)
conf_matrix11 <- table(classx11, liver.test$Liver_Disease)
# ============== #
#     TREES                 #
# ============== #
set.seed(1)
disease.train <- ifelse(liver.train$Liver_Disease == 1, "Yes", "No")
disease.test <- ifelse(liver.test$Liver_Disease == 1, "Yes", "No")
l.train <- cbind(liver.train, disease.train)
l.test <- cbind(liver.test, disease.test)

treex <- tree(disease.train~.-Liver_Disease, data = l.train)
treepred <- predict(treex, l.test, type = 'class')
plot(treex)
text(treex, pretty=0)

# confusion matrix
cm.tree <- table(treepred, l.test$disease.test)

# CV tree
cv.treex <- cv.tree(treex, FUN = prune.misclass)
plot(cv.treex$size, cv.treex$dev, type = 'b', main = "Deviance vs Tree Size", xlab = "Terminal Nodes", ylab = "Deviance",  pch =21, bg = 'lightgreen', ylim = c(140, 153))
plot(cv.treex$k, cv.treex$dev, type = 'b', main = "Deviance vs alpha value", xlab = "Alpha paramater", ylab= "Deviance", pch = 21,bg = 'lightgreen', ylim = c(140, 153))  
prune.treex <- prune.misclass(treex, best =7)
summary(prune.treex)
plot(prune.treex)
text(prune.treex, pretty =0)

# predict with tree given best alpha. 
treeprune.predict <- predict(prune.treex, l.test, type = 'class')
cm.treeCV <- table(treeprune.predict, l.test$disease.test)
# PLOT TRAINING ERROR, CROSS VALIDATION ERROR, TEST SET ERROR.


# ==============================#
# PRINCIPAL COMPONENT ANALYSIS
# ============================= #

pca_liver <- prcomp(liver.train[ , -ncol(liver.train)])
barplot(summary(pca_liver)$importance[2, ], main = "Proportion of Variance Explained",
        col = "lightblue")
plot(pca_liver$x[ ,1], pca_liver$x[, 2], col = (liver.train$Liver_Disease+2), xlab = "PC1", ylab = "PC2")

