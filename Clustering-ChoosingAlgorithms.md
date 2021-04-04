Clustering- Choosing the Best Clustering Algorithms
================
Hoyal Kim(<hoyal@another.works>)
2021-04-04

 

### 1\. 클러스터 유효성 측정

  - **내부 측정** : 클러스터링 프로세스의 내부 정보를 사용하여 클러스터링 구조의 장점을 평가하는 방법
  - **외부 측정** : 클러스터 분석의 결과를 외부에서 제공된 클래스 레이블과 같은 외부 적으로 알려진 결과와 비교하는
    방법  
  - **상대 측정** : 동일한 알고리즘에 대해 서로 다른 매개 변수 값을 변경하여 클러스터링 구조를 평가하는 방법

 

#### 1.1 내부 척도

  - 클러스터 내의 관측치간의 평균 거리가 가능한 한 작고, 클러스터 사이의 평균 거리는 가능한 한 큰 것이 바람직함.  

  - 일반적으로 내부 척도는 응집력과 분할성을 결합하여 측정함.
    
    1.  응집력(Compactness; cohesion)은 다른 군집 관측치들과의 거리보다 동일 군집내 관측치 간 거리가
        얼마나 가까운지를 측정하는 지표로, 군집 단위별 센터 값과 관측치 간의 거리를 측정함.  
    2.  분할성(Separation)은 클러스터가 다른 클러스터와 얼마나 잘 분리 되었는지 측정하는 지표로 클러스터 센터간의
        거리, 서로 다른 클러스터에 있는 관측치 쌍별 최소 거리 등을 사용함.  
    3.  연결성(Connectivity)은 관측치가 데이터 공간에서 가장 가까운 이웃과 동일한 클러스터에 배치되는 범위를
        나타내는 지표로 0과 무한대 사이의 값을 가지며 이를 최소화 하는 것이 바람직함.

 

**실루엣 계수**

  - 실루엣 분석은 관측치가 얼마나 잘 군집되어 있는지를 측정하고 군집 간의 평균 거리를 추정함.  
  - \-1 ≤ s(i) ≤ 1 값을 가지며 1에 가까우면 군집이 잘되었다고 평가하고, -1의 관측치는 잘못된 클러스터에 배정된
    것을 의미함.  
  - 실루엣 플롯은 한 군집의 각 점이 인접한 군집의 점에 얼마나 가까운 지 측정 값을 표시함.

 

**Dunn 인덱스**

  - 관측치가 잘 응집되고, 잘 분리 된 군집은 직경은 작고, 군집 사이의 거리는 더 클 것으로 예상함.
  - 따라서 Dunn 인덱스는 최대화 되어야 함.

 

#### 1.2 외부 측정

  - k-Means, PAM, 계측적 클러스터링 등에 의해 식별된 군집을 외부 참조와 비교하는 방법임.  
  - fpc::cluster.stats()에 구현 된 Adjusted Rand Index 및 Meila’s variation
    index VI를 사용하여 분할한 클러스터와 외부 참조 간의 일치를 정량화 할 수 있음.  
  - Adjusted Rand Index는 -1(일치 없음)에서 1(완벽한 일치)까지 값을 가짐.

 

### 2\. 클러스터 유효성 측정 방법

  - k-means, PAM, SOM 및 Hierarchical Clustering 적용

 

#### 2.1 클러스터링 분석

``` r
# 필수 R 패키지
library(factoextra)  # 시각화
library(fpc)         # 클러스터링 유효성 지표 계산
library(NbClust)     # 최적 클러스터 수 결정
```

``` r
# 데이터 준비
# iris 데이터 사용: 5번째 "Species" 컬럼은 제거
df <- iris[, -5]
# Standardize
df <- scale(df)
```

 

**factoextra::eclust()**

  - PAM, Hierarchical Clustering 등에서 클러스터링 수 추정 통계 자동 계산  
  - silhouette 정보 제공  
  - eclust(x, FUNcluster = “kmeans”, hc\_metric = “euclidean”, …)
      - FUNcluster : 군집 알고리즘  
      - hc\_metric : 관측치 간의 불일치 계산 척도(FUNcluster가 “hclust”, “agnes”,
        “diana” 등 계층적 군집인 경우 dist() 함수에 허용되는 다음 값)
          - “euclidean”, “manhattan”, “maximum”, “canberra”, “binary”,
            “minkowski”, “pearson”, “spearman”  
      - 아래 정보를 포함한 표준 함수의 결과를 리턴함.
          - cluster : 나무 절단 후 관측 값의 할당된 군집 번호  
          - nbclust : 클러스터 수  
          - silinfo : 관측치의 실루엣 정보  
          - size : 클러스터의 크기  
          - data : 원본 또는 표준화 된 데이터를 포함하는 행렬 (stand = TRUE 인 경우)  
          - gap\_stat : 갭 통계 포함

<!-- end list -->

``` r
# 클러스터링 분석 - kmeans
# K-means clustering
km.res <- eclust(df,             # vector, matrix, data.frame
                 "kmeans",       # "pam", "clara", "fanny", "hclust", "agnes", "diana"
                 k = 3, 
                 nstart = 25, 
                 graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, 
             geom = "point", 
             ellipse.type = "norm",
             palette = "jco", 
             ggtheme = theme_minimal())
```

![](Clustering-ChoosingAlgorithms_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Hierarchical clustering
hc.res <- eclust(df, "hclust", 
                 k = 3, 
                 hc_metric = "euclidean", 
                 hc_method = "ward.D2", 
                 graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, 
          show_labels = FALSE,
          palette = "jco", 
          as.ggplot = TRUE)
```

![](Clustering-ChoosingAlgorithms_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

 

#### 2.2 내부 측정 유효성 검증

 

**실루엣 그래프**

``` r
# Visualize silhouhette information
require("cluster")
```

    ## Loading required package: cluster

``` r
sil <- silhouette(km.res$cluster, dist(df))
fviz_silhouette(sil,
                print.summary = TRUE)
```

    ##   cluster size ave.sil.width
    ## 1       1   50          0.64
    ## 2       2   53          0.39
    ## 3       3   47          0.35

![](Clustering-ChoosingAlgorithms_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
- 실루엣 계수 전체 평균 0.46  
\- 실루엣 계수는 i번째 관측치가 자신의 군집 내 다른 관측치와 인접 군집에서의 관측치와 얼마나 유사한지를 나타내는 값으로 -1
~ 1의 값을 가짐.  
\- 각 관측치의 Si 값이 1에 가까우면 그만큼 관측치가 잘 클러스터링 되었음을 보여줌.  
\- -1에 가까운 관측치는 제대로 군집이 할당되지 않아 다른 클러스터에 할당하면 전체 결과가 향상될 수 있음.

 

**실루엣 계수 정보 추출**

``` r
# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)  ## 아래 3개 정보 이름
```

    ## [1] "widths"          "clus.avg.widths" "avg.width"

``` r
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
```

    ##    cluster neighbor sil_width
    ## 1        1        2   0.73419
    ## 41       1        2   0.73333
    ## 8        1        2   0.73082
    ## 18       1        2   0.72875
    ## 5        1        2   0.72847
    ## 40       1        2   0.72470
    ## 38       1        2   0.72442
    ## 12       1        2   0.72179
    ## 28       1        2   0.72151
    ## 29       1        2   0.71452

``` r
# Average silhouette width of each cluster
silinfo$clus.avg.widths 
```

    ## [1] 0.63632 0.39338 0.34739

``` r
# The total average (mean of all individual silhouette widths)
silinfo$avg.width  ## 0.4599
```

    ## [1] 0.45995

``` r
# The size of each clusters
km.res$size        ## 50 53 47
```

    ## [1] 50 53 47

``` r
# Silhouette width of observation
sil <- km.res$silinfo$widths[, 1:3]             # 실루엣 값만 추출
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)  # 실루엣 값이 음수인 객체 찾기
sil[neg_sil_index, , drop = FALSE]
```

    ##     cluster neighbor sil_width
    ## 112       3        2 -0.010584
    ## 128       3        2 -0.024894

  - 음수 값을 가지는 3번 군집의 2개 관측치는 2번 군집에 인접해 있다는 것을 알 수 있음.

**Validation statistics**

  - 클러스터링 품질을 평가하는 특성 통계  
  - fpc::cluster.stats() 함수 : 클러스터링의 고유한 특성 분석을 위한 정보 제공
      - cluster.number : 클러스터 수  
      - cluster.size : 각 군집의 크기를 포함하는 벡터  
      - average.distance, median.distance : 클러스터 단위별 평균/중앙 거리를 포함하는 벡터  
      - average.between : 클러스터 간의 거리 평균(크면 좋음)  
      - average.within : 클러스터 내의 거리 평균(작으면 좋음)  
      - clus.avg.silwidths : 클러스터 평균 실루엣 너비의 벡터(-1 ~ 1(매우 양호))  
      - within.cluster.ss : d가 유클리드 거리 행렬인 경우 군집 내 제곱합(k-means)  
      - dunn, dunn2 : Dunn 인덱스  
      - corrected.rand, vi : 두 개의 클러스터링의 유사성을 평가하기 위한 Corrected Rand
        Index와 Meila’s VI 두 개 인덱스

<!-- end list -->

``` r
library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),  km.res$cluster)
# Dun index
km_stats$dunn
```

    ## [1] 0.026497

``` r
km_stats
```

  - within.cluster.ss (within clusters sum of squares) : 138.8884  
  - average.within (average distance within clusters) : 1.224431
  - clus.avg.silwidths (vector of cluster average silhouette widths) :
    0.6363162, 0.3933772, 0.3473922

 

#### 2.3 외부 측정 유효성 검증

  - k-means의 군집화 결과가 실제 분류 데이터와 일치할까?

<!-- end list -->

``` r
table(iris$Species, km.res$cluster)
```

    ##             
    ##               1  2  3
    ##   setosa     50  0  0
    ##   versicolor  0 39 11
    ##   virginica   0 14 36

  - 두 군집의 유사성을 평가하기 위한 두 개의 정량적 지표가 있음
      - Corrected Rand Index : 우연에 의해 할당된 군집과의 유사성을 평가하는 척도; -1~1(완벽한
        일치)  
      - Meila’s VI

**k-means Cluster 비교**

``` r
library("fpc")
# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df),                # 관측치의 거리 
                             clustering = km.res$cluster, # 군집번호 벡터
                             species)                     # 비교 군집 벡터
# Corrected Rand index
clust_stats$corrected.rand
```

    ## [1] 0.62014

``` r
# VI
clust_stats$vi
```

    ## [1] 0.74777

**PAM Cluster 비교**

``` r
# Agreement between species and PAM clusters
pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
table(iris$Species, pam.res$cluster)
```

    ##             
    ##               1  2  3
    ##   setosa     50  0  0
    ##   versicolor  0  9 41
    ##   virginica   0 36 14

``` r
cluster.stats(d = dist(df), 
              species, pam.res$cluster)$vi
```

    ## [1] 0.7129

**hclust 비교**

``` r
# Agreement between species and HC clusters
res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
```

    ##             
    ##               1  2  3
    ##   setosa     49  1  0
    ##   versicolor  0 27 23
    ##   virginica   0  2 48

``` r
cluster.stats(d = dist(df), 
              species, res.hc$cluster)$vi
```

    ## [1] 0.6945

  - 이러한 외부 클러스터링 유효성 검사를 사용하여 주어진 데이터 세트에 적합한 클러스터링 알고리즘을 선택할 수도 있음.

 

### 3\. 최적 클러스터링 알고리즘 선택

``` r
library(clValid)
# clValid(df,                      # dataframe 혹은 matrix with scaled data
#        2:10,                     # 평가할 클러스터의 수
#        clMethods="hierarchical", # 클러스터링 방법: kmeans, som, pam, agnes, diana
#        validation="stability",   # 유효성 검사 측정 유형:  internal, biological
#        maxitems=600,             # The maximum number of items(rows in matrix)
#        metric="euclidean",       # 거리측정 방법: correlation, manhattan
#        method="average")         # 군집(hclust & agnes) 방법: ward, single, complete
```

 

#### 3.1 알고리즘 비교 측정

  - 두 가지의 클러스터 유효성 검사 측정 방법
    1.  내부 측정 : 데이터의 고유 정보를 사용하여 클러스터링의 품질을 평가함. 내부 측정에는 연결성, 실루엣 계수 및
        Dunn 인덱스 등이 포함됨.
          - the connectivity : 0에서 ∞ 값을 가지며 최소화 시키는 것이 바람직함.  
          - silhouette width : -1 ≤ s(i) ≤ 1(o), 1에 가까우면 군집이 잘되었다고
            평가함.  
          - Dunn index : Dunn 인덱스는 최대화 시키는 것이 바람직함.
    2.  안정성 측정 : 내부 측정의 특수 버전으로, 각 열이 제거 된 후 얻은 클러스터와 한 번에 하나씩 비교하여
        클러스터링 결과의 일관성을 평가함.
          - APN(The average proportion of non-overlap) : ’비중첩 평균 비율’은 전체
            데이터를 기반으로 클러스터링하고 하나의 열이 제거 된 데이터를 기반으로 클러스터링하여 동일한 클러스터에
            배치되지 않은 관측치의 평균 비율을 측정함.  
          - AD(The average distance) : ’평균 거리’는 두 경우(전체 데이터 세트 및 하나의 열
            제거)에서 동일한 군집에 배치된 관측치 간의 평균 거리를 측정함.  
          - ADM(The average distance between means) : ’평균간 평균 거리’는 두 경우
            모두 동일한 군집에 배치 된 관측치에 대해 군집 중심 간의 평균 거리를 측정함.  
          - FOM(The figure of merit) : ’성능 지수’는 삭제 된 열의 클러스터 내 분산 평균을
            측정함. 이 때 클러스터링은 삭제되지 않은 나머지 열을 기반으로 함.

 

#### 3.2 클러스터링 알고리즘 비교

``` r
# Iris data set: Remove Species column and scale
# df <- scale(iris[, -5])

# Compute clValid
clmethods <- c("hierarchical", "kmeans", "pam", "som")
# for somgrid only
rownames(df) <- 1:nrow(df)
```

**cluster internal measures** - 다른 군집 관측치들과의 거리보다 동일 군집내 관측치 간 거리가 가까운지를
측정하는 지표  
   
\- the connectivity : 0에서 ∞ 값을 가지며 최소화 시키는 것이 바람직함.  
\- **silhouette width** : -1 ≤ s(i) ≤ 1(o), 1에 가까우면 군집이 잘되었다고 평가함.  
\- Dunn index : Dunn 인덱스는 최대화 시키는 것이 바람직함.

``` r
# Internal measures
intern <- clValid(df, nClust = 2:8, 
                  clMethods = clmethods, 
                  validation = "internal")
# Summary
summary(intern)
```

    ## 
    ## Clustering Methods:
    ##  hierarchical kmeans pam som 
    ## 
    ## Cluster sizes:
    ##  2 3 4 5 6 7 8 
    ## 
    ## Validation Measures:
    ##                                 2      3      4      5      6      7      8
    ##                                                                            
    ## hierarchical Connectivity   0.976  5.596  7.549 18.051 24.731 36.017 41.903
    ##              Dunn           0.267  0.187  0.206  0.070  0.076  0.098  0.102
    ##              Silhouette     0.582  0.480  0.407  0.375  0.325  0.330  0.329
    ## kmeans       Connectivity   0.976 23.815 25.904 40.306 40.138 45.521 52.694
    ##              Dunn           0.267  0.026  0.070  0.081  0.081  0.094  0.119
    ##              Silhouette     0.582  0.460  0.419  0.346  0.344  0.346  0.340
    ## pam          Connectivity   0.976 23.073 31.807 35.796 44.541 50.352 61.620
    ##              Dunn           0.267  0.057  0.057  0.064  0.036  0.043  0.043
    ##              Silhouette     0.582  0.457  0.409  0.357  0.340  0.326  0.332
    ## som          Connectivity   0.976 23.815 34.768 42.051 65.709 58.564 59.003
    ##              Dunn           0.267  0.026  0.040  0.084  0.048  0.084  0.086
    ##              Silhouette     0.582  0.460  0.387  0.342  0.286  0.310  0.308
    ## 
    ## Optimal Scores:
    ## 
    ##              Score Method       Clusters
    ## Connectivity 0.976 hierarchical 2       
    ## Dunn         0.267 hierarchical 2       
    ## Silhouette   0.582 hierarchical 2

  - Connectivity, Dunn, Silhouette 세 가지 점수 모두 “hierarchical” 방법의 “2”개
    클러스터를 최적으로 산출하였음.

 

  - **The stability measures**:
    
      - **APN**(The average proportion of non-overlap) : ’비중첩 평균 비율’은 전체
        데이터를 기반으로 클러스터링하고 하나의 열이 제거 된 데이터를 기반으로 클러스터링하여 동일한 클러스터에 배치되지
        않은 관측치의 평균 비율을 측정함.  
      - **AD**(The average distance) : ’평균 거리’는 두 경우(전체 데이터 세트 및 하나의 열
        제거)에서 동일한 군집에 배치된 관측치 간의 평균 거리를 측정함.  
      - **ADM**(The average distance between means) : ’평균간 평균 거리’는 두 경우
        모두 동일한 군집에 배치 된 관측치에 대해 군집 중심 간의 평균 거리를 측정함.  
      - **FOM**(The figure of merit) : 삭제 된 열의 평균 클러스터 내 분산을 측정함. 여기서
        클러스터링은 삭제되지 않은 나머지 열을 기반으로 함.

<!-- end list -->

``` r
# Stability measures
clmethods <- c("hierarchical", "kmeans", "pam", "som")
stab <- clValid(df, nClust = 2:8, 
                clMethods = clmethods, 
                validation = "stability")
# Show only optimal Scores
optimalScores(stab)
```

    ##         Score       Method Clusters
    ## APN 0.0032667 hierarchical        2
    ## AD  0.9375574          pam        8
    ## ADM 0.0160871 hierarchical        2
    ## FOM 0.4464462          som        8

  - APN, ADM은 ‘hierarchical’ 방법 ’2’개 클러스터가 일정하게 결과로 나오나,  
  - AD의 Method는 ‘pam’, FOM은 ‘kmeans’ 혹은 ’som’로 나오고, Clusters의 수는 항상 지정하는
    최대 클러스터 수로 바뀌어서 나옴..  
  - 위 내부지표에서도 hierarchical 2가 최적이라고 나왔었음.

 

-----

#### **References**

  - source from:  
    o
    <https://www.datanovia.com/en/lessons/choosing-the-best-clustering-algorithms/>  
    o
    <https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/>
