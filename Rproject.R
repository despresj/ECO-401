---
title: "FREDR"
author: "Joe"
date: "October, 22, 2019"
---

``` {r Load}
library(wesanderson)
library(ggplot2)
library(tidyverse)
library(fredr)
library(purrr)
library(lubridate)
library(ggrepel)
library(knitr)
library(kableExtra)
aspect_ratio<- 1.75
#https://support.rstudio.com/hc/en-us/articles/360004672913-Rendering-PowerPoint-Presentations-with-RStudio

#Package Manual
#  https://cran.r-project.org/web/packages/fredr/fredr.pdf

#color chart
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

#git config --global credential.helper 'cache --timeout 604800'
```

``` {r key}
fredr_set_key("5e546bf9dc0d38cf6e2db34bb0e9dcef")
```
# R -----------------------------------------------------------------------

```{r REAL-GDP}
fredr_request(
  endpoint = "series/observations",
  series_id = "A191RL1Q225SBEA",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date)) %>% 
  arrange(desc(date))%>%
 ggplot(mapping = aes( x = date, y = value), ) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Real GDP", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","gdp.jpeg", height = 8, width = 8*aspect_ratio)

```

```{r Household_Consumption}
#Household Consumption
fredr_request(
  endpoint = "series/observations",
  series_id = "DPCERL1Q225SBEA",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
filter_variable = "seasonal_adjustment",
  frequency = "q",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Household Consumption", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Household_Consumption.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Durables}
#Durables
fredr_request(
  endpoint = "series/observations",
  series_id = "PCDGCC96",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Consumption of Durable Goods", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  geom_hline(yintercept = -1, color = "white") + 
  geom_hline(yintercept = 4, color = "white") + 
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Durables.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r non-Durables}
#Non-Durables
fredr_request(
  endpoint = "series/observations",
  series_id = "PCNDGC96",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Consumption of Non-Durable Goods", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
    geom_hline(yintercept = -1, color = "white") + 
  geom_hline(yintercept = 4, color = "white") + 
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Non-Durables.jpeg", height = 8, width = 8*aspect_ratio) 
```

```{r Investment}
#Investment
fredr_request(
  endpoint = "series/observations",
  series_id = "A006RL1Q225SBEA",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Private Investment", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Investment.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r non-res-fixed}
#Real Private Nonresidential Fixed Investment
fredr_request(
  endpoint = "series/observations",
  series_id = "PNFIC1",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Non-Residential Fixed Investment", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","non-res-fixed.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r res-fixed}
#Private Residential Fixed Investment 
fredr_request(
  endpoint = "series/observations",
  series_id = "PRFIC1",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Residential Fixed Investment", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","res-fixed.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r gov-con}
#Real Government Consumption Expenditures and Gross Investment
fredr_request(
  endpoint = "series/observations",
  series_id = "GCEC1",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date)) %>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Government Consumption and Investment", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","gov-con.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r net-export}
#Real Net Exports of Goods and Services
fredr_request(
  endpoint = "series/observations",
  series_id = "NETEXC",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Net Exports", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Billions of US Dollars") +
  geom_hline(yintercept = 0, size = .75, color = "white", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","net-export.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Exchange-rate}
#FOREX
fredr_request(
  endpoint = "series/observations",
  series_id = "TWEXB",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
filter_variable = "seasonal_adjustment",
  frequency = "m",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Exchange Rate Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Index Jan 1997=100") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
  scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","Exchange-rate.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r unemp}
#Unemployment rate
fredr_request(
  endpoint = "series/observations",
  series_id = "UNRATE",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
ggplot() + 
  geom_line( mapping = aes(x = date, y = value), color = "black", size = 1)  +
  labs(title = "Unemployment Rate", caption = "Source: Federal Reserve Bank of St. Louis")+
  labs(x= "Year", y = "Rate")+
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  + scale_y_continuous(limits=c(0, 18)) + ggsave(path = "Plots","unemp.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r unemp_load}
unemp <- fredr_request(
  endpoint = "series/observations",
  series_id = "UNRATE",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```


```{r u6}
#Total Unemployed, Plus All Marginally Attached Workers Plus Total Employed Part Time for Economic Reasons, as a Percent of All Civilian Labor Force Plus All Marginally Attached Workers 
u6 <-fredr_request(
  endpoint = "series/observations",
  series_id = "U6RATE",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```

```{r unemp-u6}
ggplot() + 
  geom_line( data = unemp, mapping = aes(x = date, y = value), color = "black", size = 1)  +
  geom_line( data = u6, mapping = aes(x = date, y = value), color = "black", size = 1, linetype = "twodash")  +
  labs(title = "Unemployment Rate and U6 Rate", caption = "Source: Federal Reserve Bank of St. Louis")+
  labs(x= "Year", y = "Rate") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +scale_y_continuous(limits=c(0, 18))  +  ggsave(path = "Plots","unemp_wu6.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r LFP-rate}
#Civilian Labor Force Participation Rate: 25 to 54 years
fredr_request(
  endpoint = "series/observations",
  series_id = "LNS11300060",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Civilian Labor Force Participation Rate, Ages 25-54", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Rate") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","LFP-rate.jpeg", height = 8, width = 8*aspect_ratio)
```
options(scipen=100000000)
```{r unemployment-insurance}
#Unemployment insurance claims
options(scipen=100000000)
fredr_request(
  endpoint = "series/observations",
  series_id = "ICSA",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "w",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value),  scale_y_continuous(labels = comma)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "New Unemployment Insurance Claims", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Claims") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +
 ggsave(path = "Plots","unemployment-insurance.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r payroll}
#Average Hourly Earnings
fredr_request(
  endpoint = "series/observations",
  series_id = "CES0500000003",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "pc1") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Average Hourly Earnings", caption = "Source: Federal Reserve Bank of St. Louis")  +
  geom_hline(yintercept = 0, color = "white") +
  labs(x= "Year", y = "Percent Change from Year Ago") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","payroll.jpeg", height = 8, width = 8*aspect_ratio)
```


```{r new-housing-permits}
#New private housing units authorized
fredr_request(
  endpoint = "series/observations",
  series_id = "PERMIT",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "New Private Housing Permits", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Thousands") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","Permits.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r PCE-core}
#PCE core
pcecore<-fredr_request(
  endpoint = "series/observations",
  series_id = "PCEPI",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```

```{r PCE-headline}
#PCE headline
pcehead <- fredr_request(
  endpoint = "series/observations",
  series_id = "PCEPILFE",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "ch1") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```

```{r both}
ggplot() + 
  geom_line(data = pcehead, mapping = aes( x = date, y = value), color = "black", size = 1)  +
  labs(title = "PCE Headline Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change From Year Ago") +
  geom_hline(yintercept = 0, color = "white") + 
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","PCE-headline.jpeg", height = 8, width = 8*aspect_ratio)
```
```{r both-w-click}
ggplot() + 
  geom_line(data = pcehead, mapping = aes( x = date, y = value), color = "black", size = 1)  +
  labs(title = "PCE Headline Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change From Year Ago") +
  geom_hline(yintercept = 0, color = "white") + 
  geom_hline(yintercept = 2, color = "red") + 
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","PCE-headline-click.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r PPI}
#PPI
fredr_request(
  endpoint = "series/observations",
  series_id = "PPIACO",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
filter_variable = "seasonal_adjustment",
  frequency = "q",
  units = "ch1") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Producer Price Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change from Year Ago") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5)+
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","PPI.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r pceheadline}
fredr_request(
  endpoint = "series/observations",
  series_id = "PCEPILFE",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "ch1") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Headline PCE", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change from Year Ago") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","pceheadline.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Capacity-Utilization}
#Capacity Utilization 
fredr_request(
  endpoint = "series/observations",
  series_id = "TCU",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Capacity Utilization Rate", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Rate") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Capacity-Utilization.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r REPO-market}
#repo
fredr_request(
  endpoint = "series/observations",
  series_id = "SOFR",
  observation_start = "2000-06-01",
  observation_end = "9999-01-01",
  frequency = "d",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = .75) +
  geom_jitter( size = .1, color = "black", alpha = .1) +
  labs(title = "Secured Overnight Financing Rate", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs( x= "Year", y = "Rate") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2018-6-1","2019-1-1", "2019-9-1")), labels = c("2018","2019","Sep-2019")) + scale_y_continuous(limits=c(1.5, 5.5)) +  ggsave(path = "Plots","REPOmarket.jpeg", height = 8, width = 8*aspect_ratio)
```


```{r Monetary-Base}
#Monetaty Base
fredr_request(
  endpoint = "series/observations",
  series_id = "BASE",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Monetary Base", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Billions of US Dollars") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Monetary-Base.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Fedfunds}
#Federal Funds Rate
fredr_request(
  endpoint = "series/observations",
  series_id = "FF",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "w",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "FED Funds Rate", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) + scale_y_continuous(limits=c(0, 6))  +  ggsave(path = "Plots","Fedfunds.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r prime}
#Prime Lending Rate
fredr_request(
  endpoint = "series/observations",
  series_id = "DPRIME",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "w",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Prime Lending Rate", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","prime.jpeg", height = 8, width = 8*aspect_ratio)
```




```{r clickininterest}
prime<-fredr_request(
  endpoint = "series/observations",
  series_id = "DPRIME",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "w",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
fedfunds <- fredr_request(
  endpoint = "series/observations",
  series_id = "FF",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "w",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```

```{r click-in}
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line(data = fedfunds, mapping = aes( x = date, y = value), color = "black", size = 1)  +
  geom_line(data = prime, mapping = aes( x = date, y = value), color = "red", size = 1)  +
  labs(title = "FED Funds Rate and Prime Rate", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) + scale_y_continuous(limits=c(0, 6))   +  ggsave(path = "Plots","clickin.jpeg", height = 8, width = 8*aspect_ratio)
```


```{r tenyr-3mth}
#Yield Curve 10-year/3-months
fredr_request(
  endpoint = "series/observations",
  series_id = "T10Y3M",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "d",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "10 year - 3 month Treasury Yield Spread", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Interest Rate Spread")+ 
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  + scale_y_continuous(limits=c(-1, 4)) +  ggsave(path = "Plots","10y3m.jpeg", height = 8, width = 8*aspect_ratio)
```


```{r Yield-Curve}
#Interest rates
ten<-fredr_request(
  endpoint = "series/observations",
  series_id = "T10Y2Y",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "d",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
three<-fredr_request(
  endpoint = "series/observations",
  series_id = "T10Y3M",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "d",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
  mutate(value = as.numeric(value))
```

```{r YieldCurve}
#Interest rates
three<-fredr_request(
  endpoint = "series/observations",
  series_id = "T10Y3M",
  observation_start = "2010-01-01",
  observation_end = "2020-09-15",
  frequency = "d",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))
```

```{r yield-curves}
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line(data = ten, mapping = aes( x = date, y = value), color = "red", size = 1)  +
  geom_line(data = three, mapping = aes( x = date, y = value), color = "black", size = 1)  +
  labs(title = "10 year - 3 month and 10 year - 2 year Treasury Yields", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Interest Rate Spread") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +scale_y_continuous(limits=c(-1, 4)) +  ggsave(path = "Plots","Yield-Curve.jpeg", height = 8, width = 8*aspect_ratio)
```


```{r Consumer-confidence}
#Consumer Confidence
fredr_request(
  endpoint = "series/observations",
  series_id = "UMCSENT",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
filter_variable = "seasonal_adjustment",
  frequency = "m",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "University of Michigan: Consumer Sentiment Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Index 1966:Q1=100") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019"))  +  ggsave(path = "Plots","Consumer-confidence.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r business-sent}
#Business Tendency Surveys 
fredr_request(
  endpoint = "series/observations",
  series_id = "BSCICP03USM665S",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value)) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Business Sentiment Index", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Normalised (Normal=100)") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","bus-sen.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r non-farm-payroll}
fredr_request(
  endpoint = "series/observations",
  series_id = "PAYEMS",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "chg") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value), ) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Total Nonfarm Payroll", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Thousands of Persons") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","non-farmpay.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Eurozone}
fredr_request(
  endpoint = "series/observations",
  series_id = "EUNNGDP",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "q",
  filter_variable = "seasonal_adjustment",
  units = "pch") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value), ) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Euro Zone GDP", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Percent Change") +
  geom_hline(yintercept = 0, size = .75, color = "red", alpha = .5) +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","euro.jpeg", height = 8, width = 8*aspect_ratio)
```

```{r Industrial-Production}
fredr_request(
  endpoint = "series/observations",
  series_id = "IPMAN",
  observation_start = "2010-01-01",
  observation_end = "9999-01-01",
  frequency = "m",
  filter_variable = "seasonal_adjustment",
  units = "lin") %>%
  mutate(value = as.numeric(value))%>%
  mutate(date = ymd(date))%>%
 ggplot(mapping = aes( x = date, y = value), ) + 
  geom_line( color = "black", size = 1)  +
  labs(title = "Industrial Production: Manufacturing", caption = "Source: Federal Reserve Bank of St. Louis")  +
  labs(x= "Year", y = "Index 2012=100") +
  theme_test(base_size = 20) +   scale_x_date(breaks = as.Date(c("2010-1-1","2011-1-1","2012-1-1", "2013-1-1", "2014-1-1", "2015-1-1", "2016-1-1","2017-1-1", "2018-1-1", "2019-01-01")), labels = c("2010","2011", "2012","2013", "2014", "2015", "2016", "2017", "2018", "2019")) +  ggsave(path = "Plots","industrialproduction.jpeg", height = 8, width = 8*aspect_ratio)
```
```{r}
fedfunds %>% 
  head() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")
```
this is interesting here lets see how it goes

