EDA
================
Mark Liu & Friends
2026-02-12

### Loading in the dataset and some libraries

## Including Plots

Do the trend plot, as per Lang’s instruction

``` r
#Plot is too crowded, filter out some observations for good measures

dataset_plottable <- filter(dataset_org, ID < 100 | ID > 1900)

#Comparing effect of copper

ggplot(dataset_plottable, aes(x = Time, y = OD, group = ID, color = Cu)) +
geom_line() +
geom_point() +
labs(title = "OD trends", x = "Time", y = "Concentration")
```

![](EDA_files/figure-gfm/cars-1.png)<!-- -->

``` r
#Comparing effect of mine

dataset_plottable <- filter(dataset_org, ID < 1000, ID > 750)
ggplot(dataset_plottable, aes(x = Time, y = OD, group = ID, color = Mine)) +
geom_line() +
geom_point() +
labs(title = "OD trends, Colored by Mine", x = "Time", y = "Concentration")
```

![](EDA_files/figure-gfm/cars-2.png)<!-- -->

``` r
#Comparing effect of storage

dataset_plottable <- filter(dataset_org, ID > 300, ID < 650)
ggplot(dataset_plottable, aes(x = Time, y = OD, group = ID, color = Storage)) +
geom_line() +
geom_point() +
labs(title = "OD trends, COlored by Storage Method", x = "Time", y = "Concentration")
```

![](EDA_files/figure-gfm/cars-3.png)<!-- -->

## Including more Plots

Do some cross section plots, as per Lang’s instruction

![](EDA_files/figure-gfm/pressure-1.png)<!-- -->![](EDA_files/figure-gfm/pressure-2.png)<!-- -->![](EDA_files/figure-gfm/pressure-3.png)<!-- -->

## Including even more Plots

Do some more side by side box plots, just for good measures

``` r
#maximum concentration

boxplot(dataset_mod$Maximum_Concentration ~ dataset_mod$Cu, outline=FALSE, xlab = "Cu concentration", ylab = "Maximum Concentration", main = "Comparing Maximum Concentration via Cu")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
boxplot(dataset_mod$Maximum_Concentration ~ dataset_mod$Mine, outline=FALSE, xlab = "Mine", ylab = "Maximum Concentration", main = "Comparing Maximum Concentration via Mine")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
boxplot(dataset_mod$Maximum_Concentration ~ dataset_mod$Storage, outline=FALSE, xlab = "Storage", ylab = "Maximum Concentration", main = "Comparing Maximum Concentration via Storage")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
#mu max

boxplot(dataset_mod$mu_max ~ dataset_mod$Cu, outline=FALSE, xlab = "Cu concentration", ylab = "mu max", main = "Comparing Mu Max via Cu")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
boxplot(dataset_mod$mu_max ~ dataset_mod$Mine, outline=FALSE, xlab = "Mine", ylab = "Mu Max", main = "Comparing Mu Max via Mine")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
boxplot(dataset_mod$mu_max ~ dataset_mod$Storage, outline=FALSE, xlab = "Storage", ylab = "Mu Max", main = "Comparing Mu Max via Storage")
```

![](EDA_files/figure-gfm/unnamed-chunk-1-6.png)<!-- -->

``` r
# lag time

boxplot(as.integer(dataset_mod$lag_time) ~ dataset_mod$Cu, outline=FALSE, xlab = "Cu concentration", ylab = "Lag Time", main = "Comparing Lag Time via Cu")
```

    ## Warning in eval(predvars, data, env): NAs introduced by coercion

![](EDA_files/figure-gfm/unnamed-chunk-1-7.png)<!-- -->

``` r
boxplot(as.integer(dataset_mod$lag_time) ~ dataset_mod$Mine, outline=FALSE, xlab = "Mine", ylab = "Lag Time", main = "Comparing Lag Time via Mine")
```

    ## Warning in eval(predvars, data, env): NAs introduced by coercion

![](EDA_files/figure-gfm/unnamed-chunk-1-8.png)<!-- -->

``` r
boxplot(as.integer(dataset_mod$lag_time) ~ dataset_mod$Storage, outline=FALSE, xlab = "Storage", ylab = "Lag Time", main = "Comparing Lag Time via Storage")
```

    ## Warning in eval(predvars, data, env): NAs introduced by coercion

![](EDA_files/figure-gfm/unnamed-chunk-1-9.png)<!-- -->
