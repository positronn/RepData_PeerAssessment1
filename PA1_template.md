---
title: "Reproducible Research: Peer Assessment 1"
author: "Marco de los Santos"
output: 
  html_document:
    keep_md: true
---

## Load libraries

```r
library("data.table")
library(ggplot2)
```

## Loading and preprocessing the data
Unzip data to obtain a csv file.


```r
unzip("activity.zip",exdir = "data")
activity <- data.table::fread(input = "data/activity.csv")
```


## What is mean total number of steps taken per day?

```r
steps <- activity[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(steps, 10)
```

```
##           date steps
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
```
Histogram

```r
ggplot(steps, aes(x = steps)) +
    geom_histogram(fill = "steelblue", binwidth = 1000, alpha=0.6, color='black') +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

mean and median of the total number of steps taken per day

```r
steps[, .(mean = mean(steps, na.rm = TRUE), median = median(steps, na.rm = TRUE))]
```

```
##        mean median
## 1: 10766.19  10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
