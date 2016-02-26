---
title: "Reproducible Research: Peer Assessment 1"
author: "Owusu"
date: "February 26, 2016"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
```{r}

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## Loading and preprocessing the data
### 1. Load data (i.e read.csv())
```{r}
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
### 1. Make a histogram of the total number of steps taken each day
```{r echo = TRUE}
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps", main = "Histogram of Total Number of Steps Taken Each Day", col = "blue")
```
### 2. Calculate and report the mean and median total number of steps taken per day
```{r echo = TRUE}
mean(steps.date$steps)
```

```{r echo = TRUE}
median(steps.date$steps)
```

## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r echo=TRUE}
steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l", main = "Time Series Plot of the 5-minute Intervals", col = "blue")
```
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE}
steps.interval$interval[which.max(steps.interval$steps)]
```

## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset
```{r echo=TRUE}
sum(is.na(activity))
```

### 2. Devise a strategy for filling in all of the missing values in the dataset.
I will use the means for the 5-minute intervals as fillers for missing values.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r echo=TRUE}
activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```

### 4a. Make a histogram of the total number of steps taken each day
```{r echo=TRUE}
steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps", main = "Histogram of Total Number of Steps Taken Each Day (Imputed Data)", col = "blue")

```

### 4b. Calculate and report the mean and median total number of steps taken per day.
```{r echo=TRUE}
mean(steps.date$steps)
```

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r echo=TRUE}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }}
activity$daytype <- as.factor(sapply(activity$date, daytype))

```

### 2. Make a panel plot containing a time series plot
```{r echo=TRUE}
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type, col = "blue") 
}

```

