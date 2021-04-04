Classification - with Unbalanced Data
================
Hoyal Kim(<hoyal@another.works>)
2021-04-04

 

### Loading

``` r
library(caret)
```

 

#### Unbalanced data

``` r
data("BreastCancer", package="mlbench")

bc_data <- BreastCancer
colnames(bc_data) <- c("sample_code_number", 
                       "clump_thickness", 
                       "uniformity_of_cell_size", 
                       "uniformity_of_cell_shape", 
                       "marginal_adhesion", 
                       "single_epithelial_cell_size", 
                       "bare_nuclei", 
                       "bland_chromatin", 
                       "normal_nucleoli", 
                       "mitosis", 
                       "classes") # diagnosis
str(bc_data)
```

    ## 'data.frame':    699 obs. of  11 variables:
    ##  $ sample_code_number         : chr  "1000025" "1002945" "1015425" "1016277" ...
    ##  $ clump_thickness            : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
    ##  $ uniformity_of_cell_size    : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
    ##  $ uniformity_of_cell_shape   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
    ##  $ marginal_adhesion          : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
    ##  $ single_epithelial_cell_size: Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
    ##  $ bare_nuclei                : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
    ##  $ bland_chromatin            : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
    ##  $ normal_nucleoli            : Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
    ##  $ mitosis                    : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
    ##  $ classes                    : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...

#### 결측치 확인

``` r
colSums(is.na(bc_data)) # bare_nuclei 16
```

    ##          sample_code_number             clump_thickness 
    ##                           0                           0 
    ##     uniformity_of_cell_size    uniformity_of_cell_shape 
    ##                           0                           0 
    ##           marginal_adhesion single_epithelial_cell_size 
    ##                           0                           0 
    ##                 bare_nuclei             bland_chromatin 
    ##                          16                           0 
    ##             normal_nucleoli                     mitosis 
    ##                           0                           0 
    ##                     classes 
    ##                           0

``` r
# plot(bc_data$bare_nuclei)
```

#### ID 성격 컬럼은 제거

``` r
bc_data <- dplyr::select(bc_data, -("sample_code_number"))
# Label만 factor, 나머지는 모두 numeric으로 
cols <- names(bc_data)[1:9]
bc_data[cols] <- lapply(bc_data[cols], as.numeric)
```

#### 결측치 대체 with DMwR

``` r
# bc_data <- DMwR::knnImputation(bc_data, k=5)
bc_data <- DMwR::centralImputation(bc_data)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
# 결측치 대체 with MICE(Multivariate Imputation by Chained Equations)
# library(mice)
# # 숫자형으로 바꾸기
# bc_data[, 2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
# # 데이터 복제본
# dataset_impute <- mice(bc_data[, 2:10], print=FALSE)
# # 
# bc_data <- cbind(bc_data[, 11, drop=FALSE], mice::complete(dataset_impute, 1))
# 
# bc_data$classes <- as.factor(bc_data$classes)
```

``` r
# how many benign and malignant cases are there?
summary(bc_data$classes)
```

    ##    benign malignant 
    ##       458       241

### Modeling

#### Split train data

``` r
set.seed(seed)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]
```

#### Simple all-data

``` r
ctrl = trainControl(method = "repeatedcv", 
                    number = 10, 
                    repeats = 10, 
                    verboseIter = FALSE)

set.seed(seed)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         # preProcess = c("scale", "center"),
                         trControl = ctrl)

final <- data.frame(actual = test_data$classes,
                    predict(model_rf, newdata = test_data, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")

cm_original <- confusionMatrix(as.factor(final$predict), test_data$classes)
```

#### Under-sampling

``` r
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(seed)
model_rf_under <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)

final_under <- data.frame(actual = test_data$classes,
                    predict(model_rf_under, newdata = test_data, type = "prob"))
final_under$predict <- ifelse(final_under$benign > 0.5, "benign", "malignant")

cm_under <- confusionMatrix(as.factor(final_under$predict), test_data$classes)
```

#### Over-sampling

``` r
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(seed)
model_rf_over <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)

final_over <- data.frame(actual = test_data$classes,
                          predict(model_rf_over, newdata = test_data, type = "prob"))
final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")

cm_over <- confusionMatrix(as.factor(final_over$predict), test_data$classes)
```

#### Hybrid methods - ROSE

``` r
# install.packages("ROSE")
library(ROSE)
```

``` r
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(seed)
model_rf_rose <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)

final_rose <- data.frame(actual = test_data$classes,
                         predict(model_rf_rose, newdata = test_data, type = "prob"))
final_rose$predict <- ifelse(final_rose$benign > 0.5, "benign", "malignant")

cm_rose <- confusionMatrix(as.factor(final_rose$predict), test_data$classes)
```

#### Hybrid methods - SMOTE

``` r
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")  

set.seed(seed)
model_rf_smote <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
```

    ## Loading required package: grid

``` r
final_smote <- data.frame(actual = test_data$classes,
                         predict(model_rf_smote, newdata = test_data, type = "prob"))
final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")

cm_smote <- confusionMatrix(as.factor(final_smote$predict), test_data$classes)
```

### Predictions

``` r
models <- list(original = model_rf,
               under = model_rf_under,
               over = model_rf_over,
               smote = model_rf_smote,
               rose = model_rf_rose)

resampling <- resamples(models)
bwplot(resampling)
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
library(dplyr)
```

``` r
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))
```

``` r
for (name in names(models)) {
  model <- get(paste0("cm_", name))

  # Error: Problem with 'mutate()' input 'Sensitivity'.
  comparison[comparison$model==name, ] %>% filter(comparison, model==name) %>%
    mutate(Sensitivity = model$byClass["Sensitivity"],
           Specificity = model$byClass["Specificity"],
           Precision = model$byClass["Precision"],
           Recall = model$byClass["Recall"],
           F1 = model$byClass["F1"])
}
```

``` r
library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 3)
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Imbalnaced Classification with ROSE

  - From:
    <https://goodtogreate.tistory.com/entry/Handling-Class-Imbalance-with-R-and-Caret-An-introduction>
    \[GOOD to GREAT\]

<!-- end list -->

``` r
# install.packages("ROSE")
library(ROSE)   # Up/DownSampling, SMOTE
library(rpart)  # classifier
```

#### imbalanced data set

``` r
data(hacide) 
str(hacide.train)
```

    ## 'data.frame':    1000 obs. of  3 variables:
    ##  $ cls: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ x1 : num  0.2008 0.0166 0.2287 0.1264 0.6008 ...
    ##  $ x2 : num  0.678 1.5766 -0.5595 -0.0938 -0.2984 ...

``` r
str(hacide.test)
```

    ## 'data.frame':    250 obs. of  3 variables:
    ##  $ cls: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ x1 : num  0.0556 -0.7453 -0.1849 -0.98 0.1063 ...
    ##  $ x2 : num  2.0987 -2.849 0.3807 0.0189 0.9021 ...

#### check table

``` r
table(hacide.train$cls)
```

    ## 
    ##   0   1 
    ## 980  20

``` r
table(hacide.test$cls)
```

    ## 
    ##   0   1 
    ## 245   5

#### check classes distribution

``` r
prop.table(table(hacide.train$cls))
```

    ## 
    ##    0    1 
    ## 0.98 0.02

``` r
prop.table(table(hacide.test$cls))
```

    ## 
    ##    0    1 
    ## 0.98 0.02

#### plot unbalanced data

  - highlighting the majority and minority class examples.

<!-- end list -->

``` r
plot(hacide.train[, 2:3], main="Unbalanced data", xlim=c(-4,4),
     ylim=c(-4,4), col=as.numeric(hacide.train$cls), pch=20)
legend("topleft", c("Majority class","Minority class"), pch=20, col=1:2)
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

  - only 2% of data is positive. It is a severely imbalanced data set.

#### classification with rpart

``` r
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)
```

#### check model accuracy

``` r
accuracy.meas(hacide.test$cls, pred.treeimb[,2])
```

    ## 
    ## Call: 
    ## accuracy.meas(response = hacide.test$cls, predicted = pred.treeimb[, 
    ##     2])
    ## 
    ## Examples are labelled as positive when predicted is greater than 0.5 
    ## 
    ## precision: 1.000
    ## recall: 0.200
    ## F: 0.167

``` r
roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)
```

    ## Area under the curve (AUC): 0.600

#### Generating data according to ROSE

``` r
# p=0.5 as default
data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 1)$data
table(data.rose$cls)
```

    ## 
    ##   0   1 
    ## 520 480

#### over sampling

``` r
# 갯수 지정
data_balanced_over <- ovun.sample(cls~., data = hacide.train, method = "over", N=1960)$data
# 비율 지정
data_balanced_over <- ovun.sample(cls~., data = hacide.train, method = "over", p=0.5, seed=1)$data
# N refers to number of observations in the resulting balanced set.
table(data_balanced_over$cls)
```

    ## 
    ##   0   1 
    ## 980 941

#### under sampling

``` r
data_balanced_under <- ovun.sample(cls~., data = hacide.train, method = "under", N=40)$data
```

#### under and over smapling (both)

``` r
# the minority class is oversampled with replacement 
# and majority class is undersampled without replacement
# 건수 지정
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=1000, seed=1)$data
# 비율 지정
data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=nrow(hacide.train), seed = 1)$data
table(data_balanced_both$cls)
```

    ## 
    ##   0   1 
    ## 520 480

#### Build decision tree models

``` r
tree.rose <- rpart(cls ~ ., data = data.rose)
tree.over <- rpart(cls ~ ., data = data_balanced_over)
tree.under <- rpart(cls ~ ., data = data_balanced_under)
tree.both <- rpart(cls ~ ., data = data_balanced_both)
```

#### make predictions on unseen data

``` r
pred.tree.rose <- predict(tree.rose, newdata = hacide.test)
pred.tree.over <- predict(tree.over, newdata = hacide.test)
pred.tree.under <- predict(tree.under, newdata = hacide.test)
pred.tree.both <- predict(tree.both, newdata = hacide.test)
```

#### AUC

``` r
# AUC ROSE
# roc.curve(hacide.test$cls, pred.tree.rose[,2])

# AUC Oversampling
# roc.curve(hacide.test$cls, pred.tree.over[,2])

# AUC Undersampling
# roc.curve(hacide.test$cls, pred.tree.under[,2])

# AUC Both
roc.curve(hacide.test$cls, pred.tree.both[,2])
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

    ## Area under the curve (AUC): 0.798

  - ROSE: 0.989, Oversampling: 0.798, Undersampling: 0.876, Both: 0.798

#### ROSE with GLM

  - from:
    <https://www.rdocumentation.org/packages/ROSE/versions/0.0-3/topics/ROSE>

<!-- end list -->

``` r
# model estimation using logistic regression
glm.rose <- glm(cls~., data=data.rose, family="binomial")

# prediction using test set
pred.glm.rose <- predict(glm.rose, newdata=hacide.test)

# Half circle depleted data
roc.curve(hacide.test$cls, pred.glm.rose)
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

    ## Area under the curve (AUC): 0.908

``` r
# plot new data generated by ROSE highlighting the 
# majority and minority class examples.
par(mfrow=c(1,2))
plot(data.rose[, 2:3], main="Balanced data by ROSE",
     xlim=c(-6,6), ylim=c(-6,6), col=as.numeric(data.rose$cls), pch=20)
legend("topleft", c("Majority class","Minority class"), pch=20, col=1:2)

glm.rose <- glm(cls~., data=data.rose, family="binomial")
pred.rose <- predict(glm.rose, data=data.rose, type="response")
roc.curve(data.rose$cls, pred.rose, 
          main="ROC curve \n (Half circle depleted data balanced by ROSE)")
```

![](Classification-with_Imbalanced_Data_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

    ## Area under the curve (AUC): 0.894

``` r
par(mfrow=c(1,1))
```

 

-----

#### **References**

  - Tutorials:
      - [Dealing with unbalanced data in machine
        learning](https://www.r-bloggers.com/2017/04/dealing-with-unbalanced-data-in-machine-learning/)
      - <https://shiring.github.io/machine_learning/2017/03/31/webinar_code>  
      - <https://goodtogreate.tistory.com/entry/Handling-Class-Imbalance-with-R-and-Caret-An-introduction>
      - <https://www.rdocumentation.org/packages/ROSE/versions/0.0-3/topics/ROSE>
