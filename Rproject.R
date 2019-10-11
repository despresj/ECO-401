# Load

``` {r}
library(wesanderson)
library(ggplot2)
library(tidyverse)

fredgraph<- na.omit(fredgraph)

#FED FUNDS
ggplot(data = FEDFUNDS, mapping = aes( x = DATE, y = FEDFUNDS)) + labs(title = "Fed Funds Rate", subtitle = "From Janurary 2018 to present", caption = "Source: Federal Reserve Bank of St. Louis" ) + theme_test()   
ab <- a + geom_smooth(color ="Blue", size = .1, linetype = 7, se=FALSE) 
ab + geom_point(shape = 0)

#GDP
(ggplot(data = GDPC1_2_, mapping = aes(x = DATE, y = GDPC1)) + 
    geom_line(color = "turquoise", size = 2) +
    labs(title = "Real Gross Domestic Product (in $ billions)",  caption = "Source: Federal Reserve Bank of St. Louis")+
    xlab("Year") + 
    ylab("GDP (in $ billions)") +
    theme_test(base_size = 30)
)

fredgraph$DSGG10 <- as.numeric(fredgraph$DGS10)
fredgraph$DSGG2 <- as.numeric(fredgraph$DGS2)


#YEILD CURVE

ggplot(fredgraph)+
  geom_ribbon(aes(ymin = DSGG2, ymax = DSGG10, x= DATE)) +
  geom_line(data=fredgraph,aes(y=DSGG2,x= DATE,colour="blue"), size=1 )+
  geom_line(data=fredgraph,aes(y=DSGG10,x= DATE,colour="red"), size=1) +
  scale_color_discrete(name = "  ", labels = c("2-YEAR", "10-YEAR")) +
  labs(title = "Treasury Yield Curve Inversion",  caption = "Source: Federal Reserve Bank of St. Louis") +
  xlab("Year") + 
  ylab("Percent") + theme_test(base_size = 30)








