# TECHINICAL ASSIGNMENT
# BY Atticus Patrick

############### Library Necessary Packages ############################
library(tidyverse) # to use dplyr for preprocessing, ggplot for visualization
library(openxlsx) # to write to new xlsx files
library(ggrepel) # to annotate graphs
library(ggpubr) # to graph multiple plots in a window
options(scipen = 999) # prevent scientific notation in graphs

######################### Load Data ###################################
cartAdds <- read.csv('DataAnalyst_Ecom_data_addsToCart.csv')
sessionCounts <- read.csv('DataAnalyst_Ecom_data_sessionCounts.csv')

######################### Exploratory Data Analysis ###################

# check for missing values in each data frame
sum(is.na(cartAdds))
sum(is.na(sessionCounts))
# there are no NAs, so we'll move on

# check for 0 sessions with >0 transactions or >0 QTY
# do this separately to make sure there isn't a large quantity of rows under either category
count_check1 <- sessionCounts %>%
  filter(sessions == 0, transactions > 0)
count_check1

count_check2 <- sessionCounts %>%
  filter(sessions == 0, QTY > 0)
count_check2

# remove these rows as there are only 5 of them (a very small proportion of the entire data set so we won't lose much information) 
# and they will cause arithmetic errors with ECR
sessionCounts <- sessionCounts %>%
  filter(!(sessions == 0 & transactions > 0))

sessionCounts <- sessionCounts %>%
  filter(!(sessions == 0 & QTY > 0))

# verify that they were removed
count_check1 <- sessionCounts %>%
  filter(sessions == 0, transactions > 0)
count_check1

count_check2 <- sessionCounts %>%
  filter(sessions == 0, QTY > 0)
count_check2
# Rows have been removed

############# Explore cartAdds #########

# Adds to cart by year
cart_boxplot <- ggplot(data = cartAdds, mapping = aes(x = factor(dim_year), y = addsToCart, fill = factor(dim_year)))+
  geom_boxplot()+
  xlab('Year')+
  ylab('Adds to Cart')+
  labs(title = 'Adds to Cart (per Month) by Year', fill = 'Year')
cart_boxplot
# It appears the median number of adds to cart was higher in 2012 than 2013.
# It does not look like there are any outliers

# Visualize months
cartAddsMonthly <- cartAdds %>%
  mutate(month = factor(dim_month, levels = dim_month))

# Adds to cart by month
adds_per_month <- ggplot(data = cartAddsMonthly, mapping = aes(x = month, y = addsToCart, fill = factor(dim_year)))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('Adds to Cart')+
  labs(title = 'Adds to Cart by Month', fill = 'Year')
adds_per_month

# Statistical summary for total adds to cart, and also by each year
cartAdds %>%
  summarise(Minimum = min(addsToCart),
            Q1 = quantile(addsToCart, 0.25),
            Average = mean(addsToCart),
            Standard_dev = sd(addsToCart),
            Q3 = quantile(addsToCart, 0.75),
            Maximum = max(addsToCart))

# 2012 Summary
cartAdds %>%
  filter(dim_year == 2012) %>%
  summarise(Minimum = min(addsToCart),
            Q1 = quantile(addsToCart, 0.25),
            Average = mean(addsToCart),
            Standard_dev = sd(addsToCart),
            Q3 = quantile(addsToCart, 0.75),
            Maximum = max(addsToCart))

# 2013 Summary
cartAdds %>%
  filter(dim_year == 2013) %>%
  summarise(Minimum = min(addsToCart),
            Q1 = quantile(addsToCart, 0.25),
            Average = mean(addsToCart),
            Standard_dev = sd(addsToCart),
            Q3 = quantile(addsToCart, 0.75),
            Maximum = max(addsToCart))

########### Create Both Sheets ###################

# First, feature engineer a month and year column
sessionCounts1 <- sessionCounts %>%
  mutate(dim_date = as.Date.character(dim_date, '%m/%d/%Y'))%>% # dim_date is a char type column, so we'll convert it to date type
  mutate(month = sub('^0+', '', format(dim_date, '%m')))%>% # take only the month (and remove leading zeros with sub()) for the month column
  mutate(Year = format(dim_date, '%Y')) # take only the year for the year column

# Create the first sheet, with a Month * Device aggregation of the data with the following metrics:
# Sessions, Transactions, QTY, and ECR (= Transactions / Sessions)
sessionCountsAgg <- sessionCounts1 %>%
  group_by(month, dim_deviceCategory)%>%
  summarise(Year = Year,
            Total_Sessions = sum(sessions),
            Total_Transactions = sum(transactions),
            Total_QTY = sum(QTY),
            ECR = Total_Transactions / Total_Sessions)%>% 
  distinct(Year, month, dim_deviceCategory, Total_Sessions, Total_Transactions, Total_QTY, ECR)

# verify results by manually filtering and calculating the first row of data in sessionCountsAgg
verify <- sessionCounts1 %>%
  filter(month == 1, dim_deviceCategory == 'desktop')%>%
  summarise(Total_Sessions = sum(sessions),
            Total_Transactions = sum(transactions),
            Total_QTY = sum(QTY),
            ECR = Total_Transactions / Total_Sessions)
verify
# These are equal to the values from the created table sessionCountsAgg (for Month = 1, device = desktop), so we can move forward with this table

# Create the second sheet with a Month over Month comparison (for the most recent two months
# in the data) for all available metrics (including Adds to Cart), showing: the most recent month’s
# value, the prior month’s value, and both the absolute and relative differences between them
sessionCountsMonthly <- sessionCounts1 %>%
  left_join(cartAddsMonthly, by=c('month'))%>%
  group_by(month) %>%
  summarise(Year = Year,
            sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            addsToCart = addsToCart,
            ECR = transactions / sessions)%>%
  distinct(Year, month, sessions, transactions, QTY, addsToCart, ECR)

sessionCountsMonthComp <- as.data.frame(sessionCountsMonthly) # Convert back to a dataframe as reversing rows was not working with a grouped_df data frame type

# Reverse each column to take the absolute difference for all metrics, and the relative difference (x1 - x2 / x2) for each metric
sessionCountsMonthComp56 <- sessionCountsMonthComp %>%
  filter(month == 5 | month == 6) %>% # Select May and June as these are the last months from 2013 (and there is no May or June for 2012)
  # reverse each column to calculate difference and relative difference
  mutate(revSessions = rev(sessions), 
         revTransactions = rev(transactions),
         revQTY = rev(QTY),
         revAddsToCart = rev(addsToCart),
         revECR = rev(ECR),
         diffSessions = sessions - revSessions,
         diffTransactions = transactions - revTransactions,
         diffQTY = QTY - revQTY,
         diffAddsToCart = addsToCart - revAddsToCart,
         diffECR = ECR - revECR,
         relDiffSessions = (sessions - revSessions) / revSessions,
         relDiffTransactions = (transactions - revTransactions) / revTransactions,
         relDiffQty = (QTY - revQTY) / revQTY,
         relDiffAddsToCart = (addsToCart - revAddsToCart) / revAddsToCart,
         relDiffECR = (ECR - revECR) / revECR)%>%
  select(month,	sessions, transactions, QTY, addsToCart, ECR, diffSessions, diffTransactions, diffQTY, diffAddsToCart, diffECR,
         relDiffSessions, relDiffTransactions, relDiffQty, relDiffAddsToCart, relDiffECR)

# Aggregate by year instead here
sessionCountsAggYr <- sessionCounts1 %>%
  left_join(cartAddsMonthly, by=c('month'))%>%
  group_by(Year)%>%
  summarise(Year = Year,
            sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            addsToCart = sum(addsToCart),
            ECR = transactions / sessions)%>% 
  distinct(Year, sessions, transactions, QTY, addsToCart, ECR)
# convert to data.frame from grouped_df type
sessionCountsAggYr <- data.frame(sessionCountsAggYr)

# Find the difference and absolute difference between 2012 and 2013 in the same
# way that I did this for month 5 and 6
sessionCountsCompYr <- sessionCountsAggYr %>%
  mutate(revSessions = rev(sessions), 
         revTransactions = rev(transactions),
         revQTY = rev(QTY),
         revAddsToCart = rev(addsToCart),
         revECR = rev(ECR),
         diffSessions = sessions - revSessions,
         diffTransactions = transactions - revTransactions,
         diffQTY = QTY - revQTY,
         diffAddsToCart = addsToCart - revAddsToCart,
         diffECR = ECR - revECR,
         relDiffSessions = (sessions - revSessions) / revSessions,
         relDiffTransactions = (transactions - revTransactions) / revTransactions,
         relDiffQty = (QTY - revQTY) / revQTY,
         relDiffAddsToCart = (addsToCart - revAddsToCart) / revAddsToCart,
         relDiffECR = (ECR - revECR) / revECR)%>%
  select(Year,	sessions, transactions, QTY, addsToCart, ECR, diffSessions, diffTransactions, diffQTY, diffAddsToCart, diffECR,
         relDiffSessions, relDiffTransactions, relDiffQty, relDiffAddsToCart, relDiffECR)

# Quick check to make sure the number of sessions is still the same in this aggregation
sum(sessionCounts$sessions)
sum(sessionCountsCompYr$sessions)
# These values are equal

sessionCountsAgg <- sessionCountsAgg %>%
  mutate(month = as.numeric(month))%>%
  arrange(Year, month)%>%
  mutate(month = factor(month))

# convert from grouped_by to data.frame and make month a factor variable
sessionCountsAgg <- as.data.frame(sessionCountsAgg)
# make month a factor variable and sort by year and month
sessionCountsAgg <- sessionCountsAgg %>%
  mutate(month = as.numeric(month))%>%
  arrange(Year, month)%>%
  mutate(month = factor(month))

# Write all three sheets to one xlsx file in current working directory
df_list = list("Month_Device_Aggregations" = sessionCountsAgg, "May_vs_June_2013" = sessionCountsMonthComp56, "2012_vs_2013" = sessionCountsCompYr)
write.xlsx(df_list, "ReportAnalytics.xlsx", rowNames=FALSE)

############# Continue EDA ################

# create a manual order of months so that they appear chronologically
months <- c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6)

# graph each aggregated variable per month
sessions_per_month <- ggplot(data = sessionCountsAgg, mapping = aes(x = month, y = Total_Sessions, fill = dim_deviceCategory))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab('Month')+
  ylab('Sessions')+
  labs(title = 'Total Sessions Per Month', fill = 'Device')+
  scale_x_discrete(limits = factor(months))
sessions_per_month

# Filter for only months in 2013
sessionCountsAgg2013 <- sessionCountsAgg %>%
  filter(month %in% c(1:6))
# compare transactions per month in 2013
transactions_per_month <- ggplot(data = sessionCountsAgg2013, mapping = aes(x = month, y = Total_Transactions, fill = dim_deviceCategory))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab('Month')+
  ylab('Transactions')+
  labs(title = 'Total Transactions Per Month', fill = 'Device')
transactions_per_month

# QTY per month
QTY_per_month <- ggplot(data = sessionCountsAgg, mapping = aes(x = month, y = Total_QTY, fill = dim_deviceCategory))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab('Month')+
  ylab('QTY')+
  labs(title = 'Total QTY Per Month', fill = 'Device')+
  scale_x_discrete(limits = factor(months))
QTY_per_month

# ECR per month
ECR_per_month <- ggplot(data = sessionCountsAgg, mapping = aes(x = month, y = ECR, fill = dim_deviceCategory))+
  geom_bar(stat = 'identity', position = 'dodge')+
  xlab('Month')+
  ylab('ECR')+
  labs(title = 'ECR (Transactions / Sessions) Per Month', fill = 'Device')+
  scale_x_discrete(limits = factor(months))
ECR_per_month

# Descriptive Statistics for 2012 and 2013 Transactions
# Monthly Summary
sessionCountsAgg %>%
  group_by(month) %>%
  summarise(Minimum = min(Total_Transactions),
            Q1 = quantile(Total_Transactions, 0.25),
            Average = mean(Total_Transactions),
            Standard_dev = sd(Total_Transactions),
            Q3 = quantile(Total_Transactions, 0.75),
            Maximum = max(Total_Transactions),
            Sum = sum(Total_Transactions))

# Device Summary
sessionCountsAgg %>%
  group_by(dim_deviceCategory) %>%
  summarise(Minimum = min(Total_Transactions),
            Q1 = quantile(Total_Transactions, 0.25),
            Average = mean(Total_Transactions),
            Standard_dev = sd(Total_Transactions),
            Q3 = quantile(Total_Transactions, 0.75),
            Maximum = max(Total_Transactions),
            Sum = sum(Total_Transactions))

# Yearly Summary
sessionCountsAgg %>%
  group_by(Year) %>%
  summarise(Minimum = min(Total_Transactions),
            Q1 = quantile(Total_Transactions, 0.25),
            Average = mean(Total_Transactions),
            Standard_dev = sd(Total_Transactions),
            Q3 = quantile(Total_Transactions, 0.75),
            Maximum = max(Total_Transactions),
            Sum = sum(Total_Transactions))

# Overall trends
g1 <- ggplot(data = sessionCountsMonthly, mapping = aes(x = month, y = sessions, fill = Year))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('Sessions')+
  labs(title = 'Sessions by Month')+
  scale_x_discrete(limits = factor(months))
g2 <- ggplot(data = sessionCountsMonthly, mapping = aes(x = month, y = addsToCart, fill = Year))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('Adds to Cart')+
  labs(title = 'Adds to Cart by Month')+
  scale_x_discrete(limits = factor(months))
g3 <- ggplot(data = sessionCountsMonthly, mapping = aes(x = month, y = transactions, fill = Year))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('Transactions')+
  labs(title = 'Transactions by Month')+
  scale_x_discrete(limits = factor(months))
g4 <- ggplot(data = sessionCountsMonthly, mapping = aes(x = month, y = QTY, fill = Year))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('QTY')+
  labs(title = 'QTY by Month')+
  scale_x_discrete(limits = factor(months))
g5 <- ggplot(data = sessionCountsMonthly, mapping = aes(x = month, y = ECR, fill = Year))+
  geom_bar(stat = 'identity')+
  xlab('Month')+
  ylab('ECR')+
  labs(title = 'ECR by Month')+
  scale_x_discrete(limits = factor(months))

ggarrange(g1, g2, g3, g4, g5, ncol = 3, nrow = 2)

# 2013 Analysis

# first quarter
sessionCounts2013_1 <- sessionCountsMonthly %>%
  filter(month == 1 | month == 2 | month == 3)%>%
  mutate(transactions_total = sum(transactions))

# second quarter
sessionCounts2013_2 <- sessionCountsMonthly%>%
  filter(month == 4 | month == 5 | month == 6)%>%
  mutate(transactions_total = sum(transactions))

# difference and relative difference in sessions between these quarters
sum(sessionCounts2013_2$transactions) - sum(sessionCounts2013_1$transactions)
# 39766 more transactions in second quarter of 2013
(sum(sessionCounts2013_2$transactions) - sum(sessionCounts2013_1$transactions)) / sum(sessionCounts2013_1$transactions)
# relative increase in transactions of 0.7428732 from Q1 to Q2

######################## Data Analysis ##########################

# Transactions vs sessions
ggplot(data = sessionCountsAgg, mapping = aes(x = Total_Sessions, y = Total_Transactions, color = dim_deviceCategory))+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_point()+
  xlab('Total Sessions (Monthly)')+
  ylab('Total Transactions (Monthly)')+
  labs(title = 'Transactions vs Sessions', color = 'Device')+ 
  geom_text_repel(data = sessionCountsAgg, aes(label = month))+ #add months as annotations
  scale_x_continuous(labels = scales::comma) # change from scientific notation

# Fit a linear model for each device category to compare their performances
models1 = sessionCountsAgg %>%
  group_by(dim_deviceCategory)%>%
  do(lm_pred = lm(Total_Transactions ~ Total_Sessions, data = .))

# Linear model for desktop
summary(models1[[2]][[1]])
# Linear model for mobile
summary(models1[[2]][[2]])
# Linear model for tablet
summary(models1[[2]][[3]])
# R-squared for each model is 90% or better, suggesting these models are valid for interpretation as majority of 
# variability is explained by the models

# QTY vs transactions
ggplot(data = sessionCountsAgg, mapping = aes(x = Total_Transactions, y = Total_QTY, color = dim_deviceCategory))+
  geom_point()+
  xlab('Total Transactions (Monthly)')+
  ylab('Total QTY (Monthly)')+
  labs(title = 'QTY vs Transactions', fill = 'Device')

# Fit a linear model for each device category to compare their performances
models2 = sessionCountsAgg %>%
  group_by(dim_deviceCategory)%>%
  do(lm_pred = lm(Total_QTY ~ Total_Sessions, data = .))

# Linear model for desktop
summary(models2[[2]][[1]])
# Linear model for mobile
summary(models2[[2]][[2]])
# Linear model for tablet
summary(models2[[2]][[3]])
# R-squared for each model is 90% or better, suggesting these models are valid for interpretation as majority of 
# variability is explained by the models
