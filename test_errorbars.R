# Attempt 1: -----
## source: https://ggplot2.tidyverse.org/reference/geom_linerange.html
# create df
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

# set base
p <- ggplot(df, aes(trt, resp, colour = group))

# one type of error
p + geom_pointrange(aes(ymin = lower, ymax = upper))

# another
p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)

# Because the bars and errorbars have different widths
# we need to specify how wide the objects we are dodging are
dodge <- position_dodge(width=0.9)
p +
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25)

# When using geom_errorbar() with position_dodge2(), extra padding will be
# needed between the error bars to keep them aligned with the bars.
p +
  geom_col(position = "dodge2") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge2(width = 0.5, padding = 0.5))

    
# Attempt 2: -----
## source: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization


#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++

# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

## summarize -----

# summarize data
df2 <- data_summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))


# Convert dose to a factor variable
df2$dose <- as.factor(df2$dose)
head(df2)

## default ggplot ----
library(ggplot2)
# Default bar plot
p<- ggplot(df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))
 
print(p)
# Finished bar plot
p+labs(title="Tooth length per dose", x="Dose (mg)", y = "Length")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#E69F00'))

## line plot with error bar -----
# Default line plot
p<- ggplot(df2, aes(x=dose, y=len, group=supp, color=supp)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(0.05))

print(p)

# Finished line plot
p+labs(title="Tooth length per dose", x="Dose (mg)", y = "Length")+
  theme_classic() +
  scale_color_manual(values=c('#999999','#E69F00'))


