# TestingWinGitHubDesk


---
title: "Most Harmful To Population Health & With Greatest Economic Consequences Historical Storms"
author: "Ricardo Romo Encino"
date: "Sep 4th, 2017"
output: html_document
---

## Synopsis

The goal of this research is to analyze the US National Weather Service Storm Data and find out the most harmful to population health and with greatest economic consequences event types recorded in this database. Regarding people health the variables analyzed are __Casualities__ and __Injures__. About economic consequences are __Property Damage__ and __Crop Damage__. The data set contains information from 1950 to 2011. Not all the event type started to be recorded since 1950. The results show that for people health a single event type is the most dangerous. About economic consequences there are two different that cause the greatest impact in Property and Crop damage.


## Getting and Processing the Raw Data

There should be a section titled Data Processing which describes (in words and code) how the data were loaded into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks.

The pre-processed Historical Storm data was downloaded from the [coursera cloud servers]("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"). The data was saved in the **StormDataset.bz2** file:

```{r downloading the data, eval=FALSE}

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","StormDataset.bz2")

```

### Loading the storm data

The data was read directly from the BZ2 compressed file:

```{r loading data, cache=TRUE}

sd <- read.csv("StormDataset.bz2")

```

### Analyzing the data

After the data was loaded, the **sd** dimension and structure were checked to see how many variables and observations it contains, their data types and sample values: 

```{r checking data frame dimension}

dim(sd)
str(sd)

```

There are __902,267__ observation and __37__ variables in this frame. The __EVTYPE__ variables stores the event types recorded. The number of distint event types in the data set was obtained to see if filtering the information was required:

```{r showing unique EVTYPE values}
length(unique(sd$EVTYPE))
```

From the National Weather service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) in the section __2.1.1__ the official list of event types is defined. There are __48__ official event types. In the data set __985__ were found. Therefore a cleanning based on the even type is required.

### Cleaning the data

Because of the data manipulation would start, the DPLYR package was loaded:

```{r loading dplyr package, message=FALSE, warning=FALSE}
library(dplyr)
```

A [CSV file](https://www.dropbox.com/s/l5jc1ra2jrf2xk9/EventTypes.csv?dl=0) was created with the official list of event types and was loaded into a DF and the values were converted to upper case:

```{r loading official EVTYPE values}
evntTyps <- read.csv2("EventTypes.csv",header =TRUE)
# Converting evntTyps to uppercase
evntTyps <- mutate(evntTyps, EVTYPE = toupper(EVTYPE))
```

Also the values in the __sd__ DF were converted to uppercase and stored in the same column:

```{r converting EVTYPE values to uppercase}
sd <- mutate(sd, EVTYPE = toupper(EVTYPE))
```

With these data frames loaded, further data transformation occured. A merge between the official data types and the NWS data was executed to get rid of the records that didn't match with the official event type list.

Also the amounts in the relevant variables regarding economic impact: __PROPDMG__ and __CROPDMG__, were transformed. Using the __PROPDMGEXP__ and __CROPDMGEXP__ variables, arithmetic operations were executed multiply by 1,000 when the EXP was and __K__, by 1,000,000 when the EXP was a __M__ and by 1,000,000,000 when the EXP was a __B__.

Finally, in the __%>%__ chain, the __SELECT__ function was used to pick only the relevant variables for the rest of the analysis.

```{r filtering and transforming the amounts, cache=TRUE}
filtered <- merge(sd, evntTyps, by.x = "EVTYPE", by.y = "EVTYPE") %>% 
            mutate(PROPDMG = ifelse(PROPDMGEXP == 'K',PROPDMG * 1000,ifelse(PROPDMGEXP == 'M',PROPDMG * 1000000,ifelse(PROPDMGEXP == 'B',PROPDMG * 1000000000,PROPDMG))),
                   CROPDMG = ifelse(CROPDMGEXP == 'K',CROPDMG * 1000,ifelse(CROPDMGEXP == 'M',CROPDMG * 1000000,ifelse(CROPDMGEXP == 'B',CROPDMG * 1000000000,CROPDMG)))) %>%
            select(EVTYPE,BGN_DATE, FATALITIES, INJURIES, PROPDMG, CROPDMG, REFNUM)
```

At this moment the tidy and filtered data is stored in the __filtered__ DF and ready to be used in the analysis and plotting of the information.

## Results

There should be a section titled Results in which your results are presented.

You may have other sections in your analysis, but Data Processing and Results are required.

You must show all your code for the work in your analysis document. This may make the document a bit verbose, but that is okay. In general, you should ensure that echo = TRUE for every code chunk (this is the default setting in knitr).

The __filtered__ DF was grouped by __EVTYPE__ in order to summarize the amounts of the four relevant variables. A new DF was created with the summarized data.

```{r grouping and summarizing by EVTYPE, cache=TRUE}
# Grouping by EVTYPE
filtered <- group_by(filtered, EVTYPE)

# Summarizing the sum of fatalities and injuries and total amount of damages by EventType
stormDataSummaryByEvntype <- summarize(filtered, TOTAL_FATALITIES = sum(FATALITIES), TOTAL_INJURIES = sum(INJURIES), TOTAL_PROPDMG = sum(PROPDMG), TOTAL_CROPDMG = sum(CROPDMG))
```

### Figures

The analysis document must have at least one figure containing a plot.

Your analysis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), but there cannot be more than three figures total.

The tools package is necessary for the plotting code.

```{r loading tools package, message=FALSE, warning=FALSE}
library(tools)
```

The most dangarous events for the population health were found with the following figure:

```{r figure1, fig.width=40, fig.height=50}
par(mfrow = c(2, 2), mar = c(10, 4, 20, 1), oma = c(10, 1, 20, 1), ps = 20, family = "sans")

stormDataSummaryByEvntype <- stormDataSummaryByEvntype[with(stormDataSummaryByEvntype,order(-TOTAL_INJURIES, EVTYPE)),]
barplot(stormDataSummaryByEvntype$TOTAL_INJURIES, names.arg = toTitleCase(tolower(stormDataSummaryByEvntype$EVTYPE)), las = 3, col = rainbow(46), main = "Total Injuries by Event Type (# of People)", ylab = "", yaxt="n", cex.main = 1.5)
axis(2, at=axTicks(2), labels=sprintf("%s", axTicks(2)), las = 1)

stormDataSummaryByEvntype <- stormDataSummaryByEvntype[with(stormDataSummaryByEvntype,order(-TOTAL_FATALITIES, EVTYPE)),]
barplot(stormDataSummaryByEvntype$TOTAL_FATALITIES, names.arg = toTitleCase(tolower(stormDataSummaryByEvntype$EVTYPE)), las = 3, col = rainbow(46), main = "Total Fatalities by Event Type (# of People)", ylab = "", yaxt="n", cex.main = 1.5)
axis(2, at=axTicks(2), labels=sprintf("%s", axTicks(2)), las = 1)

stormDataSummaryByEvntype <- stormDataSummaryByEvntype[with(stormDataSummaryByEvntype,order(-TOTAL_PROPDMG, EVTYPE)),]
barplot(stormDataSummaryByEvntype$TOTAL_PROPDMG / 1000000, names.arg = toTitleCase(tolower(stormDataSummaryByEvntype$EVTYPE)), las = 3, col = rainbow(46), main = "Amount of Total Property Damage by Event Type (Millions of USD)", ylab = "", yaxt="n", cex.main = 1.5)
axis(2, at=axTicks(2), labels=sprintf("%s M", axTicks(2)), las = 1)

stormDataSummaryByEvntype <- stormDataSummaryByEvntype[with(stormDataSummaryByEvntype,order(-TOTAL_CROPDMG, EVTYPE)),]
barplot(stormDataSummaryByEvntype$TOTAL_CROPDMG / 1000000, names.arg = toTitleCase(tolower(stormDataSummaryByEvntype$EVTYPE)), las = 3, col = rainbow(46), main = "Amount of Total Crop Damage by Event Type (Millions of USD)", ylab = "", yaxt="n", cex.main = 1.5)
axis(2, at=axTicks(2), labels=sprintf("%s M", axTicks(2)), las = 1)

mtext("Most Harmful to Population Health (Top) / Greatest Economic Consequences (Bottom)",outer = TRUE, cex = 2, col = "blue")
```

## Conclusions

Final words