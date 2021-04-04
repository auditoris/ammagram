Detect Anomalies in Time Series
================
Hoyal Kim (<hoyal@another.works>)
2021-04-04

 

  - 이상 탐지(anomaly detection)란?
    
      - 비정상적인 데이터는 금융 사기, 소프트웨어 문제를 탐지하거나  
      - 또는 최종 사용자 구매 패턴의 변화와 같은 잠재적 기회를 탐색  
         

  - 시계열 이상 탐지 프로세스
    
    1.  시계열을 주요 변수로 분해: Trend, Seasonality, Residuals(Random)  
    2.  어떤 임계 값을 가진 상하한 값 만들기  
    3.  임계 값을 벗어난 데이터 포인트를 이상으로 식별하기  
         

  - 시계열 이상탐지 R 패키지
    
      - anomalize  
      - timetk  
      - tibbletime

<!-- end list -->

``` r
# pkg <- c('tidyquant', 'tibbletime', 'anomalize') 
# install.packages(pkg) 
library (tidyverse) 
library(tidyquant) 
library(lubridate)
library (tibbletime) 
library (anomalize) 
library (timetk)
```

``` r
# User interaction
interact = FALSE
# Random seed
seed <- 2021
```

 

### 0\. Problem Case

  - 전력 이상 사용 탐지  
  - 데이터: 주거 유형별 가정용 총 전력 사용량(GWh)
      - 다운로드: [싱가폴 정부
        웹사이트](https://storage.data.gov.sg/total-household-electricity-consumption-by-dwelling-type/resources/total-household-electricity-consumption-by-dwelling-type-2005-to-2020-2020-11-25T04-00-58Z.csv)  
      - 2005.01 ~ 2020.07 월별 전력 사용량

 

### 1\. Load Data

``` r
df <- read.csv( "./data/total-household-electricity-consumption.csv") 
head (df, 3)
```

    ##     month overall public_housing X1.room_2.room X3.room X4.room
    ## 1 2019-01   594.1          343.4           11.5    61.1   143.3
    ## 2 2019-02   613.6          356.0           11.7    62.3   148.5
    ## 3 2019-03   598.2          340.9           11.2    59.6   142.0
    ##   X5.room_and_executive private_housing private_apts_and_condo
    ## 1                 127.4           249.8                  163.0
    ## 2                 133.4           256.7                  167.6
    ## 3                 128.0           256.5                  167.8
    ##   landed_properties others
    ## 1              86.9    0.8
    ## 2              89.1    0.8
    ## 3              88.7    0.8

  - 데이터가 month별로 정렬되어 있지 않음.

<!-- end list -->

``` r
str(df) # 187 x 11
```

    ## 'data.frame':    187 obs. of  11 variables:
    ##  $ month                 : Factor w/ 187 levels "2005-01","2005-02",..: 169 170 171 172 173 174 175 176 177 178 ...
    ##  $ overall               : num  594 614 598 653 670 ...
    ##  $ public_housing        : num  343 356 341 375 386 ...
    ##  $ X1.room_2.room        : num  11.5 11.7 11.2 12.3 12.8 13.4 12.8 13.2 13.3 13 ...
    ##  $ X3.room               : num  61.1 62.3 59.6 66.2 68.3 70.3 66.4 68.8 69.6 66.6 ...
    ##  $ X4.room               : num  143 148 142 156 161 ...
    ##  $ X5.room_and_executive : num  127 133 128 140 144 ...
    ##  $ private_housing       : num  250 257 256 278 284 ...
    ##  $ private_apts_and_condo: num  163 168 168 184 190 ...
    ##  $ landed_properties     : num  86.9 89.1 88.7 94 93.7 96 89.6 93.5 98.2 95.1 ...
    ##  $ others                : num  0.8 0.8 0.8 0.9 0.9 0.9 0.8 0.8 0.9 0.9 ...

 

### 2\. Data Preparation

``` r
# Factor를 Date 형식으로 변경 
df$month <- paste(df$month, "01", sep="-") # -01일 덧붙이기
df$month <- as.Date(df$month, format="%Y-%m-%d")
```

``` r
# 새 data.frame에서 관련 column만 선택 : 사용 월과 총량
df <- df %>% select(month, overall)
```

``` r
# 정렬 - 파일의 원본 데이터가 month로 정렬되어 있지 않음!!
df <- df[order(df$month), ]
```

``` r
# df를 tibble로 변환: require the object be created as a time tibble
df <- as_tibble(df)
class(df)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
head(df, 3)
```

    ## # A tibble: 3 x 2
    ##   month      overall
    ##   <date>       <dbl>
    ## 1 2005-01-01    448.
    ## 2 2005-02-01    437.
    ## 3 2005-03-01    480.

 

### 3\. 시계열 데이터 진단

 

**전체 사용량 그래프**

``` r
df %>%
  # group_by(year(month)) %>%
  summarise_by_time(
    month, 
    .by = "month",
    overall = FIRST(overall)
  ) %>%
  plot_time_series(month, overall, .facet_ncol=2, .interactive=interact)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

 

**그룹별 사용량 그래프**

``` r
df %>%
  group_by(quarter(month)) %>%
  plot_time_series(month, log(overall), # Apply a Log Transformation
        .color_var = quarter(month), # Color applied to Week transformation
        .smooth = FALSE, # Eliminate smoother(blue line)
        # Facet formatting
        .facet_ncol = 2, 
        .facet_scales = "free", 
        .interactive=interact)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

 

**AutoCorrelation**

``` r
df %>%
    plot_acf_diagnostics(month, overall, 
                         .interactive=interact)  
```

    ## Max lag exceeds data available. Using max lag: 186

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

  - Lag별 자기 상관관계 ACF, PACF 그래프.. boundary 표시가 없다.

\======

**Forecasting Time Series with R base and forecast package**

  - Forecasting-TimeSeries.Rmd

<!-- end list -->

``` r
# ts 형식으로 바꾸고 시계열 분해와 ACF, PACF
# frequency 년(1), 분기(4), 월(12), 주(52)
# 2019.01 ~ 2020.07 월별
df_ts <- ts(df$overall, start=c(2005,1), frequency=12)
plot(df_ts)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
df_comp <- stats::decompose(df_ts)
# str(df_comp)
plot(df_comp)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
acf(df_ts, lag.max=30)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
pacf(df_ts, lag.max=30)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

  - ACF에서 wave 형식을 보이고, PACF에서도 그러하다..

<!-- end list -->

``` r
forecast::auto.arima(df_ts)
```

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

    ## Series: df_ts 
    ## ARIMA(1,0,2)(0,1,1)[12] with drift 
    ## 
    ## Coefficients:
    ##         ar1     ma1     ma2    sma1  drift
    ##       0.905  -0.048  -0.419  -0.831  1.004
    ## s.e.  0.091   0.123   0.111   0.075  0.212
    ## 
    ## sigma^2 estimated as 515:  log likelihood=-799.43
    ## AIC=1610.9   AICc=1611.4   BIC=1629.8

  - 추정결과 ARIMA(2,1,0)(1,1,1)\[12\]

<!-- end list -->

``` r
fit_ts <- arima(df_ts, order=c(2,1,0), 
             seasonal=list(order=c(1,1,1), period=12))
forecast::checkresiduals(fit_ts)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(2,1,0)(1,1,1)[12]
    ## Q* = 17.3, df = 20, p-value = 0.63
    ## 
    ## Model df: 4.   Total lags used: 24

  - 융\-박스 검정 결과, Ho Accept(잔차는 White Noise 이다).

<!-- end list -->

``` r
fcast_ts <- forecast::forecast(fit_ts, h=12)
fcast_ts
```

    ##          Point Forecast  Lo 80  Hi 80  Lo 95  Hi 95
    ## Aug 2020         784.95 756.21 813.69 740.99 828.91
    ## Sep 2020         763.90 724.37 803.43 703.44 824.36
    ## Oct 2020         807.29 765.64 848.95 743.58 871.00
    ## Nov 2020         808.45 764.28 852.63 740.89 876.01
    ## Dec 2020         742.24 693.12 791.36 667.12 817.37
    ## Jan 2021         729.19 676.02 782.35 647.88 810.50
    ## Feb 2021         735.68 679.84 791.52 650.28 821.08
    ## Mar 2021         732.67 674.05 791.28 643.02 822.31
    ## Apr 2021         788.90 727.17 850.63 694.49 883.31
    ## May 2021         815.34 750.77 879.91 716.59 914.09
    ## Jun 2021         854.36 787.28 921.44 751.77 956.95
    ## Jul 2021         834.07 764.50 903.64 727.67 940.47

``` r
autoplot(fcast_ts)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

  - 예측이 추세를 어느 정도 반영하는 것으로 보임.

\======

 

**Seasonal 단위별 그래프**

  - day, wday, week, month 각 단위별 그래프

<!-- end list -->

``` r
# 데이터는 일 데이터가 되어야...
# df %>%
    # plot_seasonal_diagnostics(month, overall, .interactive=interact)
```

 

**STL(Seonal-Trend-Loess) 분해**

  - STL 분해는 “관측치”로부터 “계절성(season)” 및 “추세(trend)” 컴포넌트를 분리하여 “White
    Noise(remainder)”만을 남겨 이상탐지에 사용함.

<!-- end list -->

``` r
df %>%
#   group_by(year(month)) %>%
    plot_stl_diagnostics(
    month, overall,
    .frequency = "auto", 
    .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive=interact)
```

    ## frequency = 12 observations per 1 year

    ## trend = 60 observations per 5 years

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

 

#### 시계열 이상탐지 2단계 프로세스

 

**Step 1: Detrend & Remove Seasonality using STL Decomposition**

  - STL 분해를 통한 추세 및 계절성 제거하기
  - 분해는 “관측치”로부터 “season” 및 “trend” 컴포넌트를 분리하여 “remainder”를 남겨둠.  
  - frequency/trend 파라미터 제어 가능
      - .frequency: 관측치에서 제거된 계절 컴포넌트 조정
    
      - .trend: 추세 윈도우(stats::stl()을 사용한 t.window)를 조정
    
      - tk\_time\_scale\_template()를 활용하여 척도에 따른 frequency/trend를 미리
        결정하여,  
    
      - .frequency와 .trend 파라미터에 시간 기반의 기간(예: ‘6주’), 숫자 값(예: 180), 혹은
        ’auto’로 설정 가능함.

 

**Step 2: Anomaly Detection**

  - “trend”와 “season(seasonality)”을 제거한 후, “remainder”에서 이상치를 감지함.  

  - 이상치를 식별하기 위한 경계(recomposed\_l1과 recomposed\_l2)가 결정함.

  - 이상치 탐지 방법
    
      - 사분위 범위(IQR = 중앙값(median) +/- 25)을 사용함.

  - IQR을 조정하기 위한 alpha 파라미터
    
      - 기본 값 alpha=0.05는 IQR Factor=3(3X)으로, 25/75의 기준선을 한계로 설정함.

  - IQR Factor=3(=0.15/alpha, 따라서 alpha=0.05일때 3X)는
    
      - 한계를 제어하는 IQR Factor를 크게하면 alpha가 작아져서 이상치가 줄어듦.  
      - 즉 alpha를 크게하면 이상치가 더 많아짐.

  - IQR 이상치 탐지는 forecast::tsoutliers() 메소드를 사용함.
    
      - 똑같은 이상치 탐지 방법이 Twitter의 AnomalyDetection 패키지에서 사용됨.

  - Business Science’s anomalize 패키지에
    
      - Twitter와 Forecast::tsoutliers() 메소드 2가지 모두 구현되어 있음.

 

### 4\. anomalize 활용 이상탐지

  - 이상 감지 workflow 지원  
  - 주요 함수:
      - time\_decompose(): 이상치 감지를 위한 시계열 분해  
      - anomalize(): trend나 seasonality를 나타내지 않는 데이터에서 이상치를 추출  
      - time\_recompose(): normal 관측치로부터 이상치를 분리하는 밴드를 재구성

<!-- end list -->

``` r
df_anomalized <- df %>%
    time_decompose(overall, 
        merge=TRUE) %>%  # 결과를 원본 데이터에 추가
    anomalize(remainder) %>%
    time_recompose()
df_anomalized %>% glimpse()
```

    ## Rows: 187
    ## Columns: 11
    ## $ month         <date> 2005-01-01, 2005-02-01, 2005-03-01, 2005-04-01, 2005-0…
    ## $ overall       <dbl> 447.8, 437.1, 479.7, 533.6, 535.0, 560.2, 537.4, 528.8,…
    ## $ observed      <dbl> 447.8, 437.1, 479.7, 533.6, 535.0, 560.2, 537.4, 528.8,…
    ## $ season        <dbl> -48.81017, -56.41089, -58.09824, 0.43021, 24.39233, 55.…
    ## $ trend         <dbl> 505.82, 506.05, 506.28, 506.50, 506.73, 506.96, 507.19,…
    ## $ remainder     <dbl> -9.207216, -12.535563, 31.522725, 26.665211, 3.874038, …
    ## $ remainder_l1  <dbl> -83.696, -83.696, -83.696, -83.696, -83.696, -83.696, -…
    ## $ remainder_l2  <dbl> 84.19, 84.19, 84.19, 84.19, 84.19, 84.19, 84.19, 84.19,…
    ## $ anomaly       <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "…
    ## $ recomposed_l1 <dbl> 373.31, 365.94, 364.48, 423.24, 447.43, 478.64, 459.69,…
    ## $ recomposed_l2 <dbl> 541.20, 533.83, 532.37, 591.12, 615.32, 646.52, 627.57,…

  - 내부적으로 tbl\_df를 tbl\_time으로 변환, index를 month로 하고, 오름차순 가정  
  - frequency=12 months, trend=60 months  
  - 시계열 분해를 통해 remainder를 분리하여 이상치를 판단하는 밴드를 계산하여 이상치를 판정함.

 

#### 4.1 Visualize the Anomalies

  - 시각화 함수: plot\_anomalies()

<!-- end list -->

``` r
df_anomalized %>% 
  plot_anomalies(ncol=1,
        time_recomposed=T,  # show band 
        alpha_dots=0.75)    # dot transparency
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

  - 밴드 바깥의 4개 포인트를 이상치로 식별하여 보여줌.

 

##### 4.2 Adjusting Trend and Seasonality

  - 일자 혹은 일시 정보를 기반으로 의미있는 시간 간격(예: 5분, 1개월)을 직관적으로 선택 가능함.  
  - 빈도(frequency)와 추세(trend)가 자동 설계에 따라 선택됨.

 

**시계열 분해 시각화**

  - plot\_anomaly\_decomposition() : 이상치를 올바르게 감지하고 있는지, 분해 방법, 이상치 식별
    방법, alpha, frquency 등의 매개 변수를 조정해야 하는지 여부를 파악하기 위해 사용

<!-- end list -->

``` r
# frequency='auto', trend='auto' default
p1 <- df_anomalized %>%
    plot_anomaly_decomposition() + 
    ggtitle("Freq/Trend = 'auto'")  
p1
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

  - freq/trend를 ’auto’로 사용한 경우, get\_time\_scale\_template() 함수로  
  - 데이터의 스케일에 따라 논리적으로 frequency와 trend 범위를 결정할 수 있음.

<!-- end list -->

``` r
get_time_scale_template()
```

    ## # A tibble: 8 x 3
    ##   time_scale frequency trend   
    ##   <chr>      <chr>     <chr>   
    ## 1 second     1 hour    12 hours
    ## 2 minute     1 day     14 days 
    ## 3 hour       1 day     1 month 
    ## 4 day        1 week    3 months
    ## 5 week       1 quarter 1 year  
    ## 6 month      1 year    5 years 
    ## 7 quarter    1 year    10 years
    ## 8 year       5 years   30 years

  - 이 표에서, 만약 척도가 day(일; 각 데이터 포인트 간격이 1일)이면,
      - 빈도(frequency)는 1주(혹은 7일)  
      - 추세(trend)는 3개월(약 90일)이라는 것을 의미함.
  - 이러한 로직은 2가지 방법으로 조정될 수 있음.
      - Local 파라미터 조정 혹은  
      - Global 파라미터 조정

 

**Adjusting Local Parameters**

  - Local 파라미터 조정은 함수의 파라미터를 조정하는 것을 말함.  
  - 아래에서 추세를 좀 많이 하여 trend = ’12 months’로 조정함.

<!-- end list -->

``` r
p2 <- df %>%
    time_decompose(overall,
        frequency = "auto",
        trend    = "12 months") %>%  # 2 weeks
    anomalize(remainder) %>%
    plot_anomaly_decomposition() +
    ggtitle("Trend = 12 Months (Local)")
# Show plots
# p1  # previous "auto"
p2
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

 

**Adjusting the Global Parameter**

  - Global 파라미터 조정은 set\_time\_scale\_template() 함수를 이용하고,  
  - 템플릿을 갱신하여 원하는 값을 전역적으로 사용할 수 있도록 함.

<!-- end list -->

``` r
# 템플릿의 시간 척도가 1일(scale='day')이면, 추세(trend)를 '3개월'에서 '2주'로 변경하기
time_scale_template() %>%
    mutate(trend = ifelse(time_scale == "day", "2 weeks", trend)) %>%
    set_time_scale_template()
get_time_scale_template()
```

    ## # A tibble: 8 x 3
    ##   time_scale frequency trend   
    ##   <chr>      <chr>     <chr>   
    ## 1 second     1 hour    12 hours
    ## 2 minute     1 day     14 days 
    ## 3 hour       1 day     1 month 
    ## 4 day        1 week    2 weeks 
    ## 5 week       1 quarter 1 year  
    ## 6 month      1 year    5 years 
    ## 7 quarter    1 year    10 years
    ## 8 year       5 years   30 years

  - time\_scale=’day’의 trend가 ’2 weeks’로 바뀌어 전역적으로 적용되게 함.

 

``` r
# 변경된 Global 파라미터 적용 확인
p3 <- df %>%
    time_decompose(overall) %>%
    anomalize(remainder) %>%
    plot_anomaly_decomposition() +
    ggtitle("Trend = 2 Weeks (Global)")
p3
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

  - 위 템플릿 값은 바뀌었지만, 내부적으로 index=month로 자동 인식하여 frequency=12 months,
    trend=60 months를 적용하고 있다.  
  - index 지정은 API에 설명이 없고, frequency, trend를 직접 지정하는 설명만 있다.

 

**템플릿 기본 값을 원래 값으로 되돌리기**

``` r
time_scale_template() %>%
    set_time_scale_template()
# Verify the change
get_time_scale_template()
```

    ## # A tibble: 8 x 3
    ##   time_scale frequency trend   
    ##   <chr>      <chr>     <chr>   
    ## 1 second     1 hour    12 hours
    ## 2 minute     1 day     14 days 
    ## 3 hour       1 day     1 month 
    ## 4 day        1 week    3 months
    ## 5 week       1 quarter 1 year  
    ## 6 month      1 year    5 years 
    ## 7 quarter    1 year    10 years
    ## 8 year       5 years   30 years

 

#### 4.3 Extracting the Anomalous Data Points

 

**이상치를 가진 실제 데이터 추출**

``` r
df %>% 
  time_decompose(overall) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')
```

    ## # A time tibble: 2 x 10
    ## # Index: month
    ##   month      observed season trend remainder remainder_l1 remainder_l2 anomaly
    ##   <date>        <dbl>  <dbl> <dbl>     <dbl>        <dbl>        <dbl> <chr>  
    ## 1 2020-06-01     870.   55.4  648.      167.        -83.7         84.2 Yes    
    ## 2 2020-07-01     887.   36.2  649.      202.        -83.7         84.2 Yes    
    ## # … with 2 more variables: recomposed_l1 <dbl>, recomposed_l2 <dbl>

  - 4개의 이상치 리스트를 보여준다.  
  - 관측치에 대한 시계열 분해 후 season, trend를 제거한  
  - 잔차(remainder)의 값이 경계(remainder\_l1 ~ remainder\_l2) 경계를 벗어난 데이터가
    이상치로 판정됨.

 

#### 4.4 Adjusting Alpha and Max Anoms

  - anomalize()를 제어하는 2개의 파라미터
      - alpha : “normal” 범위 지정; 밴드 표시
      - max\_anoms : 허용할 이상치의 최대 비율 지정

 

**Alpha**

  - alpha의 기본 값 0.05에 따른 정상 범위의 밴드(band)를 표시함.

<!-- end list -->

``` r
p4 <- df %>%
    time_decompose(overall) %>% 
       # frequency = "12 months",
       # trend = "60 months") %>%
    anomalize(remainder, alpha = 0.075, max_anoms = 0.2) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p4
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

  - alpha 값을 작게 하면, 밴드가 넓어져서 이상치가 적어짐.
  - 반대로 이 값을 크게 하면 밴드는 좁아지고 이상치가 많아지게 됨.

 

  - alpa 값을 절반으로 작게 설정한 사례

<!-- end list -->

``` r
p5 <- df %>%
    time_decompose(overall) %>%
    anomalize(remainder, alpha = 0.025, max_anoms = 0.2) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p5
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

 

  - 적정(?) alpa 값의 결정
      - 아래 임의 100개 데이터에 대한 테스트 결과를 살펴볼 때  
      - 데이터의 성격에 따라 해당 업무를 고려하여(rule of thumb?) 결정해야 할 것으로 보임.

<!-- end list -->

``` r
set.seed(seed)
seq1 <- sample(0:100, 90, replace=T)
seq2 <- sample(-500:0, 5, replace=T)
seq3 <- sample(0:500, 5, replace=T)
seq100 <- c(seq2, seq1, seq3)

fivenum(seq100)
```

    ## [1] -388.0   21.0   45.0   76.5  381.0

``` r
lowerQ <- fivenum(seq100)[2]  # .25%
upperQ <- fivenum(seq100)[4]  # .75%
IQR    <- IQR(seq100)    # [50%]
lidx <- which(seq100 < (lowerQ-IQR*1.5))  # -48.5
uidx <- which(seq100 > (upperQ+IQR*1.5))  # 149.75
outidx <- c(lidx, uidx)
seq100[outidx]
```

    ## [1] -116 -354 -136 -388  -81  381  200  328

``` r
ov <- boxplot(seq100)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
ov$out
```

    ## [1] -116 -354 -136 -388  -81  381  200  328

``` r
date100 <- as.Date(seq(0, 99), origin='2021-01-01')
df100 <- data.frame(Date=date100, Value=seq100)
df100 <- as_tibble(df100)
str(df100)
```

    ## tibble [100 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ Date : Date[1:100], format: "2021-01-01" "2021-01-02" ...
    ##  $ Value: int [1:100] -116 -354 -136 -388 -81 6 37 45 57 11 ...

``` r
df100 %>%
    time_decompose(Value) %>%
    # anomalize(remainder, alpha=0.5, max_anoms=0.2) %>%
    anomalize(remainder, alpha=0.08, max_anoms=0.2) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed=TRUE) +
    ggtitle("alpha = 0.05")
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

 

**Max Anoms**

  - max\_anoms 파라미터는 이상치가 되는 데이터의 최대 비율을 통제함.

  - 아래 코드에서,
    
      - alpha=0.3으로 조정하여 많은 데이터가 이상치가 되도록 하고,  
      - max\_anoms=0.2 (이상치 허용 비율 20%)와 max\_anoms=0.05 (이상치 허용 비율 5%)
        결과를 비교해 봄.

<!-- end list -->

``` r
p6 <- df %>%
    time_decompose(overall) %>%
    anomalize(remainder, alpha = 0.3, max_anoms = 0.2) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("20% Anomalies")
#> frequency = 7 days
#> trend = 91 days
p7 <- df %>%
    time_decompose(overall) %>%
    anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
    time_recompose() %>%
    plot_anomalies(time_recomposed = TRUE) +
    ggtitle("5% Anomalies")
#> frequency = 7 days
#> trend = 91 days
p6
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
p7
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->

  - <span style="color:red">*허용 비율을 지정했을 때 **이상치로 식별되는 판단 기준**은 무엇인가*?
      - </span><span style="color:blue">In my guess… band로부터 거리 값 정렬해서
        비율만큼?</span>

 

### 5\. timetk 패키지 이상탐지

 

  - timetk는 시계열 및 기계학습 예측을 위한 시계열 데이터의 준비와 시각화 작업을 도와주는 툴킷임.

 

#### 5.1 Interactive Anomaly Visualization

  - timetk::plot\_anomaly\_diagnostics()
      - 사용자가 직관적으로 파악하고 매개변수를 조정할 수 있도록 도와줌

<!-- end list -->

``` r
str(df)
```

    ## tibble [187 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ month  : Date[1:187], format: "2005-01-01" "2005-02-01" ...
    ##  $ overall: num [1:187] 448 437 480 534 535 ...

``` r
df %>% 
  timetk::plot_anomaly_diagnostics(month, 
            overall, 
            .facet_ncol = 2,
            .interactive=interact)
```

![](DetectAnomaliesInTimeSeries_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

  - frequency=12 observations per 1 year, trend=60 observations per 5
    years

 

#### 5.2 Interactive Anomaly Detection

  - timetk::tk\_anomaly\_diagnostics()
      - 이상치를 갖는 데이터 포인트 찾기

<!-- end list -->

``` r
df %>% 
  timetk::tk_anomaly_diagnostics(month, overall) %>% 
  filter(anomaly=='Yes')
```

    ## # A tibble: 2 x 11
    ##   month      observed season trend remainder seasadj remainder_l1 remainder_l2
    ##   <date>        <dbl>  <dbl> <dbl>     <dbl>   <dbl>        <dbl>        <dbl>
    ## 1 2020-06-01     870.   55.4  648.      167.    814.        -83.7         84.2
    ## 2 2020-07-01     887.   36.2  649.      202.    851.        -83.7         84.2
    ## # … with 3 more variables: anomaly <chr>, recomposed_l1 <dbl>,
    ## #   recomposed_l2 <dbl>

 

### 6\. Conclusion

  - 공개 데이터 세트를 이용하여,  
  - 시계열 데이터의 이상치 식별 및 시각화 패키지 살펴봤음.

 

-----

### *References*

  - R code:  
    o
    <https://www.analyticsvidhya.com/blog/2020/12/a-case-study-to-detect-anomalies-in-time-series-using-anomalize-package-in-r/>  
    o
    <https://www.r-bloggers.com/2020/09/time-series-in-5-minutes-part-5-anomaly-detection/>

  - 본 문서는 위 두 참조 문서를 정리한 “Detect Anomalies.Rmd” 에서 “PART 2. A Case Study
    To Detect Anomalies In Time Series”만 뽑아서 정리한 문서임.
