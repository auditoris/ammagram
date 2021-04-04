Classification - Ensemble
================
Hoyal Kim(<hoyal@another.works>)
2021-04-04

  - Classification - Ensemble
      - caretList
      - caretEnsemble
      - caretStack

 

### \[0\] Load Dataset

``` r
library(mlbench)
library(caret)
library(caretEnsemble)
```

``` r
# Load the dataset
data(Ionosphere)
dataset <- Ionosphere
dim(dataset)  # 351 x 35
```

    ## [1] 351  35

``` r
head(dataset, 3)
```

    ##   V1 V2      V3       V4      V5       V6       V7       V8      V9      V10
    ## 1  1  0 0.99539 -0.05889 0.85243  0.02306  0.83398 -0.37708 1.00000  0.03760
    ## 2  1  0 1.00000 -0.18829 0.93035 -0.36156 -0.10868 -0.93597 1.00000 -0.04549
    ## 3  1  0 1.00000 -0.03365 1.00000  0.00485  1.00000 -0.12062 0.88965  0.01198
    ##       V11      V12     V13      V14      V15      V16     V17      V18     V19
    ## 1 0.85243 -0.17755 0.59755 -0.44945  0.60536 -0.38223 0.84356 -0.38542 0.58212
    ## 2 0.50874 -0.67743 0.34432 -0.69707 -0.51685 -0.97515 0.05499 -0.62237 0.33109
    ## 3 0.73082  0.05346 0.85443  0.00827  0.54591  0.00299 0.83775 -0.13644 0.75535
    ##        V20      V21      V22      V23      V24      V25      V26      V27
    ## 1 -0.32192  0.56971 -0.29674  0.36946 -0.47357  0.56811 -0.51171  0.41078
    ## 2 -1.00000 -0.13151 -0.45300 -0.18056 -0.35734 -0.20332 -0.26569 -0.20468
    ## 3 -0.08540  0.70887 -0.27502  0.43385 -0.12062  0.57528 -0.40220  0.58984
    ##        V28      V29      V30      V31      V32      V33      V34 Class
    ## 1 -0.46168  0.21266 -0.34090  0.42267 -0.54487  0.18641 -0.45300  good
    ## 2 -0.18401 -0.19040 -0.11593 -0.16626 -0.06288 -0.13738 -0.02447   bad
    ## 3 -0.22145  0.43100 -0.17365  0.60436 -0.24180  0.56045 -0.38238  good

``` r
dataset <- dataset[,-2] # V2 삭제
dataset$V1 <- as.numeric(as.character(dataset$V1))
str(dataset)
```

    ## 'data.frame':    351 obs. of  34 variables:
    ##  $ V1   : num  1 1 1 1 1 1 1 0 1 1 ...
    ##  $ V3   : num  0.995 1 1 1 1 ...
    ##  $ V4   : num  -0.0589 -0.1883 -0.0336 -0.4516 -0.024 ...
    ##  $ V5   : num  0.852 0.93 1 1 0.941 ...
    ##  $ V6   : num  0.02306 -0.36156 0.00485 1 0.06531 ...
    ##  $ V7   : num  0.834 -0.109 1 0.712 0.921 ...
    ##  $ V8   : num  -0.377 -0.936 -0.121 -1 -0.233 ...
    ##  $ V9   : num  1 1 0.89 0 0.772 ...
    ##  $ V10  : num  0.0376 -0.0455 0.012 0 -0.164 ...
    ##  $ V11  : num  0.852 0.509 0.731 0 0.528 ...
    ##  $ V12  : num  -0.1776 -0.6774 0.0535 0 -0.2028 ...
    ##  $ V13  : num  0.598 0.344 0.854 0 0.564 ...
    ##  $ V14  : num  -0.44945 -0.69707 0.00827 0 -0.00712 ...
    ##  $ V15  : num  0.605 -0.517 0.546 -1 0.344 ...
    ##  $ V16  : num  -0.38223 -0.97515 0.00299 0.14516 -0.27457 ...
    ##  $ V17  : num  0.844 0.055 0.838 0.541 0.529 ...
    ##  $ V18  : num  -0.385 -0.622 -0.136 -0.393 -0.218 ...
    ##  $ V19  : num  0.582 0.331 0.755 -1 0.451 ...
    ##  $ V20  : num  -0.3219 -1 -0.0854 -0.5447 -0.1781 ...
    ##  $ V21  : num  0.5697 -0.1315 0.7089 -0.6997 0.0598 ...
    ##  $ V22  : num  -0.297 -0.453 -0.275 1 -0.356 ...
    ##  $ V23  : num  0.3695 -0.1806 0.4339 0 0.0231 ...
    ##  $ V24  : num  -0.474 -0.357 -0.121 0 -0.529 ...
    ##  $ V25  : num  0.5681 -0.2033 0.5753 1 0.0329 ...
    ##  $ V26  : num  -0.512 -0.266 -0.402 0.907 -0.652 ...
    ##  $ V27  : num  0.411 -0.205 0.59 0.516 0.133 ...
    ##  $ V28  : num  -0.462 -0.184 -0.221 1 -0.532 ...
    ##  $ V29  : num  0.2127 -0.1904 0.431 1 0.0243 ...
    ##  $ V30  : num  -0.341 -0.116 -0.174 -0.201 -0.622 ...
    ##  $ V31  : num  0.4227 -0.1663 0.6044 0.2568 -0.0571 ...
    ##  $ V32  : num  -0.5449 -0.0629 -0.2418 1 -0.5957 ...
    ##  $ V33  : num  0.1864 -0.1374 0.5605 -0.3238 -0.0461 ...
    ##  $ V34  : num  -0.453 -0.0245 -0.3824 1 -0.657 ...
    ##  $ Class: Factor w/ 2 levels "bad","good": 2 1 2 1 2 1 2 1 2 1 ...

``` r
seed <- 1234

set.seed(seed)
intrainset <- createDataPartition(dataset$Class, p=.75, list=FALSE)
trainset <- dataset[ intrainset,]
testset  <- dataset[-intrainset,]
dim(trainset)  # 264 x 34
```

    ## [1] 264  34

``` r
dim(testset)   # 87 x 34
```

    ## [1] 87 34

 

### \[1\] Bagging Algorithms

  - 원본 훈련 세트에서 Bootstrap(중복 허용 random) sample 사용  
  - 고차원의 분류 문제에서 모델 분산 감소에 효과적이나  
  - 편향을 낮추는데는 한계가 있음.

 

Two of the most popular bagging machine learning algorithms:  
\- Bagged CART  
\- Random Forest

``` r
# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
```

``` r
# Bagged CART
set.seed(seed)
fit.treebag <- train(Class~., data=trainset, method="treebag", 
                     metric=metric, trControl=control)
```

``` r
# Random Forest
set.seed(seed)
fit.rf <- train(Class~., data=trainset, method="rf", 
                metric=metric, trControl=control)
```

``` r
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
```

    ## 
    ## Call:
    ## summary.resamples(object = bagging_results)
    ## 
    ## Models: treebag, rf 
    ## Number of resamples: 30 
    ## 
    ## Accuracy 
    ##            Min. 1st Qu.  Median    Mean 3rd Qu. Max. NA's
    ## treebag 0.81481 0.88462 0.92308 0.91662 0.96154    1    0
    ## rf      0.84615 0.89744 0.92593 0.93176 0.96154    1    0
    ## 
    ## Kappa 
    ##            Min. 1st Qu.  Median    Mean 3rd Qu. Max. NA's
    ## treebag 0.61095 0.74746 0.82069 0.81662 0.91275    1    0
    ## rf      0.64138 0.78010 0.83934 0.84845 0.91671    1    0

  - random forest가 92.6%로 조금 더 정확한 모델을
생성했음.

<!-- end list -->

``` r
dotplot(bagging_results)
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
- Confidence level 0.95에서 Accuracy, Kappa 값을 보여줌.

 

### \[2\] Boosting Algorithms

  - Bootstrap re-sampling 과정에서
      - 각 데이터에 동일한 확률을 부여하는 것이 아니라  
      - 앞에서 잘못 분류된 훈련 sample에 대해 큰 가중치를 주어 추출함.  
  - 편향과 분산을 감소시킬 수 있음.

 

Two of the most popular boosting machine learning algorithms:  
\- C5.0  
\- GBM (Stochastic Gradient Boosting)

``` r
# Example of Boosting Algorithms
# Three separate 10-fold cross-validations
# The resampling method: "boot", "cv", "LOOCV", "LGOCV", "repeatedcv", 
#                        "timeslice", "none" and "oob"
control <- trainControl(method="repeatedcv", 
                        number=10, repeats=3)
metric <- "Accuracy"
```

``` r
# C5.0
set.seed(seed)
fit.c50 <- train(Class~., data=trainset, method="C5.0", 
                 metric=metric, trControl=control)
```

``` r
# GBM (Stochastic Gradient Boosting)
set.seed(seed)
fit.gbm <- train(Class~., data=trainset, method="gbm", 
                 metric=metric, trControl=control, verbose=FALSE)
```

``` r
# Plotting the Resampling Profile: to examine the relationship 
# between the estimates of performance and the tuning parameters
trellis.par.set(caretTheme())
plot(fit.c50)  
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(fit.gbm, metric="Kappa")  # default Accuracy
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->
- C50의 성능 추정치와 튜닝 파라미터 간의 관련성: 부트스트랩 반복에 따른 트리 최대 깊이별 Kappa 보기

``` r
# heatmap of the results
trellis.par.set(caretTheme())
plot(fit.gbm, metric="Accuracy", plotType="level",
     scales=list(x=list(rot=90)))
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
- 위와 같은 GBM 결과의 관련성 정보를 트리맵으로 보기

``` r
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
```

    ## 
    ## Call:
    ## summary.resamples(object = boosting_results)
    ## 
    ## Models: c5.0, gbm 
    ## Number of resamples: 30 
    ## 
    ## Accuracy 
    ##         Min. 1st Qu.  Median    Mean 3rd Qu. Max. NA's
    ## c5.0 0.84615 0.92308 0.92593 0.93936 0.96296    1    0
    ## gbm  0.84615 0.92308 0.92593 0.93305 0.96261    1    0
    ## 
    ## Kappa 
    ##         Min. 1st Qu.  Median    Mean 3rd Qu. Max. NA's
    ## c5.0 0.67500 0.82069 0.84118 0.86269 0.91991    1    0
    ## gbm  0.64138 0.82069 0.84118 0.85038 0.91738    1    0

  - C5.0이 정확도 93.9%로 살짝 더 정확한 모델로
나타났음

<!-- end list -->

``` r
dotplot(boosting_results)
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

 

### \[3\] Stacking Algorithms

 

Creating 5 sub-models for the trainset, specifically:  
\- LDA (Linear Discriminate Analysis)  
\- CART (Classification and Regression Trees)  
\- Logistic Regression (via Generalized Linear Model or GLM)  
\- kNN (k-Nearest Neighbors)  
\- SVM (Support Vector Machine with a Radial Basis Kernel Function)

 

**Example of Stacking algorithms**

``` r
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
```

``` r
set.seed(seed)
models <- caretList(Class~., data=trainset, 
                    trControl=control, 
                    methodList=algorithmList)
```

``` r
results <- resamples(models)
summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: lda, rpart, glm, knn, svmRadial 
    ## Number of resamples: 30 
    ## 
    ## Accuracy 
    ##              Min. 1st Qu.  Median    Mean 3rd Qu.    Max. NA's
    ## lda       0.73077 0.84615 0.85185 0.85218 0.88462 0.92308    0
    ## rpart     0.77778 0.84615 0.88462 0.88148 0.92308 0.96296    0
    ## glm       0.74074 0.81481 0.84900 0.85366 0.88462 0.96154    0
    ## knn       0.76923 0.80769 0.84615 0.83837 0.85185 0.92308    0
    ## svmRadial 0.88462 0.92593 0.96154 0.94962 0.96296 1.00000    0
    ## 
    ## Kappa 
    ##              Min. 1st Qu.  Median    Mean 3rd Qu.    Max. NA's
    ## lda       0.34532 0.62567 0.65385 0.65169 0.72340 0.82069    0
    ## rpart     0.45638 0.66569 0.74217 0.73766 0.83671 0.91892    0
    ## glm       0.45533 0.58814 0.66013 0.67270 0.74412 0.91275    0
    ## knn       0.45070 0.55314 0.62044 0.61617 0.66500 0.82069    0
    ## svmRadial 0.74172 0.83606 0.91275 0.89018 0.91892 1.00000    0

  - SVM이 정확도 95.0%로 가장 나은 모델을
생성했음.

<!-- end list -->

``` r
dotplot(results)
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
- 위 결과를 시각적으로 보여줌.

 

**correlation between results**

  - 스태킹을 사용하여 다른 모델의 예측을 결합 할 때  
    하위 모델에 의한 예측의 상관 관계가 낮은(\< 0.75) 것이 바람직함.  
  - 즉 하위 모델이 서로 다른 방식(바람직한 조합)임을 시사하기 때문임.

<!-- end list -->

``` r
modelCor(results)
```

    ##               lda   rpart     glm     knn svmRadial
    ## lda       1.00000 0.26785 0.16969 0.19963   0.23208
    ## rpart     0.26785 1.00000 0.26960 0.12964   0.29432
    ## glm       0.16969 0.26960 1.00000 0.32271   0.35511
    ## knn       0.19963 0.12964 0.32271 1.00000   0.17983
    ## svmRadial 0.23208 0.29432 0.35511 0.17983   1.00000

  - 예측 간의 상관 관계가 가장 높은 두 가지 방법은
      - 로지스틱 회귀(GLM)와 SVM의 상관관계 0.355이며  
      - 이러한 상관관계가 높지는 않은(\< 0.75) 것으로
간주함.

<!-- end list -->

``` r
splom(results)
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->
- 위 결과를 시작화 하여 보여줌.

 

**테스트 데이터 6개만 예측해 보기**

``` r
p <- as.data.frame(predict(models, newdata=head(testset[,-34])))
print(p)
```

    ##         lda    rpart      glm knn svmRadial
    ## 1 0.0320980 0.065868 0.025536 0.2 0.0071403
    ## 2 0.0096251 0.065868 0.028270 0.2 0.0040409
    ## 3 0.9999825 0.933333 1.000000 1.0 0.9869717
    ## 4 0.0619167 0.756757 0.917038 0.0 0.7540557
    ## 5 0.4637391 0.756757 1.000000 0.6 0.9833500
    ## 6 0.0062878 0.065868 0.017337 0.0 0.0633839

  - 각 모델별 0 ~ 1.0의 예측 값을 보여줌.

 

#### Stacking with caret

 

**GLM을 사용하여 분류기의 예측을 결합**

``` r
# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, 
                             savePredictions=TRUE, classProbs=TRUE)

set.seed(seed)
stack.glm <- caretStack(models, method="glm", 
                        metric="Accuracy", 
                        trControl=stackControl)
print(stack.glm)
```

    ## A glm ensemble of 5 base models: lda, rpart, glm, knn, svmRadial
    ## 
    ## Ensemble results:
    ## Generalized Linear Model 
    ## 
    ## 792 samples
    ##   5 predictor
    ##   2 classes: 'bad', 'good' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 712, 713, 713, 713, 712, 713, ... 
    ## Resampling results:
    ## 
    ##   Accuracy  Kappa  
    ##   0.94741   0.88527

  - 정확도가 위 caretList SVM 보다 높은 94.7%로 증가

 

**RF 알고리즘을 사용하여 예측을 결합**

``` r
# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", 
                       metric="Accuracy", trControl=stackControl)
print(stack.rf)
```

    ## A rf ensemble of 5 base models: lda, rpart, glm, knn, svmRadial
    ## 
    ## Ensemble results:
    ## Random Forest 
    ## 
    ## 792 samples
    ##   5 predictor
    ##   2 classes: 'bad', 'good' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 712, 713, 713, 713, 712, 713, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy  Kappa  
    ##   2     0.95667   0.90544
    ##   3     0.95709   0.90626
    ##   5     0.95079   0.89280
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 3.

  - RF try=3에서 정확도 96.5%까지 향상(Bagging RF는 92.6%)

 

### \[4\] Caret Ensemble

``` r
library("nnet")
library("rpart")
library("caretEnsemble")
```

 

#### \[4.1\] Caret List

``` r
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(trainset$Class, 25),
  summaryFunction=twoClassSummary
)

model_list <- caretList(
  Class~., data=trainset,
  trControl=my_control,
  methodList=c("glm", "rpart")
)
```

 

**예측**

  - type(=row, prob, class..) 지정 안됨.

<!-- end list -->

``` r
p <- as.data.frame(predict(model_list, newdata=head(testset[,-34])))
print(p)
```

    ##        glm    rpart
    ## 1 0.025536 0.065868
    ## 2 0.028270 0.065868
    ## 3 1.000000 0.933333
    ## 4 0.917038 0.756757
    ## 5 1.000000 0.756757
    ## 6 0.017337 0.065868

 

**참고**

  - 모델에 대한 Spec을 지정

<!-- end list -->

``` r
# model_list_big <- caretList(
#   Class~., data=trainset,
#   trControl=my_control,
#   methodList=c("glm", "rpart"),
#   # 추가
#   metric="ROC",
#   tuneList=list(
#     rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
#     rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
#     nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
#   )
# )
```

 

#### \[4.2\] Caret Ensemble

 

**모델의 상관성 및
ROC(AUC)**

``` r
xyplot(resamples(model_list))
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->
- 특정한 패턴을 보이지 않아 상관성은 없고,  
오른쪽 아래로 치우쳐 glm의 AUC가 상대적으로 낮아 보인다.

 

**모델간 상관관계**

``` r
modelCor(resamples(model_list))
```

    ##          glm  rpart
    ## glm   1.0000 0.1399
    ## rpart 0.1399 1.0000

  - 상관관계가 없어 2 모델간 앙상블을 할 수 있다.

<!-- end list -->

``` r
# linear greedy optimization on AUC
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)
```

    ## The following models were ensembled: glm, rpart 
    ## They were weighted: 
    ## 2.8054 -2.3584 -3.5127
    ## The resulting ROC is: 0.906
    ## The fit for each individual model on the ROC is: 
    ##  method     ROC    ROCSD
    ##     glm 0.80937 0.045478
    ##   rpart 0.87762 0.043694

  - 훈련 셋에 대한 ROC가 87.8% (개별 모델의 ROC와 비교하여 더 나은지 비교)  
  - 그래프에서 볼 수 있었던 것처럼 rpart의 ROC가 높은 것을 확인할 수 있다.

<!-- end list -->

``` r
library("caTools")
model_preds <- lapply(model_list, predict, newdata=testset[,-34], type="prob")
model_preds <- lapply(model_preds, function(x) x[, "good"])
model_preds <- data.frame(model_preds)

ens_preds <- predict(greedy_ensemble, newdata=testset[,-34], type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testset$Class)
```

    ##                  glm   rpart ensemble
    ## bad vs. good 0.95219 0.93548  0.96313

  - 테스트 셋에서는 rpart \< glm \< 앙상블의 값이 좀 더 낫게 나왔다.

 

각 모델과 앙상블에서의 변수의 중요도

``` r
vars <- caret::varImp(greedy_ensemble)
head(vars)
```

    ##     overall     glm rpart
    ## V33 0.18581 0.46256     0
    ## V31 0.30570 0.76104     0
    ## V12 0.45898 1.14260     0
    ## V13 0.47133 1.17336     0
    ## V20 0.48955 1.21871     0
    ## V28 0.53907 1.34200     0

#### \[4.3\] Caret Stack

  - caretStack은 단순한 모델 혼합을 넘어  
  - “메타 모델”을 사용하여 예측 모델 모음을 통합 할 수 있음.

<!-- end list -->

``` r
# 앙상블에서 사용한 trainControl을 사용하면 안됨.
# resampling index가 잘못될 수 있음
glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, 
                                 newdata=testset[,-34], type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, testset$Class)
```

    ##                  glm   rpart ensemble
    ## bad vs. good 0.95219 0.93548  0.96313

  - 위 caretEnsemble과 결과가 같음.

<!-- end list -->

``` r
CF/sum(CF)
```

    ##     glm   rpart 
    ## 0.40169 0.59831

  - glm과 rpart 모델의 가중치도 위 caretEnsemble과 비슷(하다고 함).

**참고**

  - 아래와 같이 더 정교한 앙상블을 사용한 모델은 과적합 되기 쉬움

<!-- end list -->

``` r
gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10, # <= 위 코드에서 추가
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, 
                                 newdata=testset[,-34], type="prob")
colAUC(model_preds3, testset$Class)
```

    ##                  glm   rpart ensemble
    ## bad vs. good 0.95219 0.93548   0.9663

  - glm, rpart는 위와 같지만, 앙상블은 0.03 올랐음.

 

### \[5\] Unnecessary Comments

 

**Extracting Predictions and Class Probabilities**

``` r
# caretEnsemble
pred1 <- predict(greedy_ensemble, newdata=testset[,-34], type="raw")
head(pred1)
```

    ## [1] good good bad  bad  bad  good
    ## Levels: bad good

``` r
# caretStack
pred2 <- predict(glm_ensemble, newdata=testset[,-34], type="raw")
head(pred2)
```

    ## [1] good good bad  bad  bad  good
    ## Levels: bad good

``` r
pred3 <- predict(gbm_ensemble, newdata=testset[,-34], type="raw")
head(pred3)
```

    ## [1] good good bad  good bad  good
    ## Levels: bad good

``` r
# caretList: type 지정이 안됨
pred4 <- predict(model_list, newdata=testset[,-34])
head(pred4)
```

    ##           glm    rpart
    ## [1,] 0.025536 0.065868
    ## [2,] 0.028270 0.065868
    ## [3,] 1.000000 0.933333
    ## [4,] 0.917038 0.756757
    ## [5,] 1.000000 0.756757
    ## [6,] 0.017337 0.065868

``` r
pred.c50 <- predict(fit.c50, newdata=testset[,-34]) #, type = "prob")
head(pred.c50)
```

    ## [1] good good bad  bad  bad  good
    ## Levels: bad good

``` r
pred.gbm <- predict(fit.gbm, newdata=testset[,-34]) #, type = "prob")
head(pred.gbm)
```

    ## [1] good good bad  bad  bad  good
    ## Levels: bad good

 

**Confusion matrix & AUC**

``` r
# Confusion matrix
library(ROCR)
```

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
(cm.c50 <- confusionMatrix(pred.c50, testset$Class, positive='good'))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction bad good
    ##       bad   26    0
    ##       good   5   56
    ##                                         
    ##                Accuracy : 0.943         
    ##                  95% CI : (0.871, 0.981)
    ##     No Information Rate : 0.644         
    ##     P-Value [Acc > NIR] : 4.86e-11      
    ##                                         
    ##                   Kappa : 0.87          
    ##                                         
    ##  Mcnemar's Test P-Value : 0.0736        
    ##                                         
    ##             Sensitivity : 1.000         
    ##             Specificity : 0.839         
    ##          Pos Pred Value : 0.918         
    ##          Neg Pred Value : 1.000         
    ##              Prevalence : 0.644         
    ##          Detection Rate : 0.644         
    ##    Detection Prevalence : 0.701         
    ##       Balanced Accuracy : 0.919         
    ##                                         
    ##        'Positive' Class : good          
    ## 

``` r
# ROC Curve
roc.c50 <- ROCR::prediction(as.numeric(pred.c50),
                            as.numeric(testset$Class))
pf.c50 <- ROCR::performance(roc.c50, "tpr", "fpr")
plot(pf.c50, lwd=2, col='blue')
# AUC
(auc.c50 <- paste0("AUC = ", 
                   round(as.numeric(ROCR::performance(roc.c50,
                                                      "auc")@y.values),
                         2)))
```

    ## [1] "AUC = 0.92"

``` r
text(0.5, 0.8, auc.c50, cex=1.2, col='blue')
```

![](Classification--Ensemble_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

 

-----

#### **References**

  - code from:  
    o
    <https://machinelearningmastery.com/machine-learning-ensembles-with-r/>  
    o <https://topepo.github.io/caret/model-trainset-and-tuning.html>  
    o
    <https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html>

  - data from:  
    o [Practice Machine Learning with Datasets from the UCI Machine
    Learning
    Repository](https://machinelearningmastery.com/practice-machine-learning-with-small-in-memory-datasets-from-the-uci-machine-learning-repository/)
