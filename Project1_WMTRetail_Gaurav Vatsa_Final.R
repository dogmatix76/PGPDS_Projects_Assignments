library(dplyr)
library(lubridate)

WMTsales <- read.csv('Walmart_Store_sales.csv') # Read csv
WMTsales$Date <- as.Date(WMTsales$Date,format = c("%d-%m-%Y")) # Treat dates
str(WMTsales)

# Q1 - Which store has the maximum sales? (multiple ways)
max(WMTsales$Weekly_Sales)  # Finding max sales - for any week across the time period
arrange(WMTsales,desc(Weekly_Sales))[1:5,]  # Finding which store has max sales for a week - method two
WMTsales %>% group_by(Store) %>% arrange(desc(Weekly_Sales)) # Finding which store has max sales for a week - method three
WMTsales %>% group_by(Store) %>% summarise(total_sales = sum(Weekly_Sales)) %>% arrange(desc(total_sales)) # Which store has max overall sales for the entire duration - method four

# Q2 - Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation
WMTsales %>% group_by(Store) %>% summarise(Mean_sales = mean(Weekly_Sales), Std_Dev = sd(Weekly_Sales)) %>% arrange(desc(Std_Dev)) # Std Dev in descending order
WMTsales %>% group_by(Store) %>% summarise(Mean_sales = mean(Weekly_Sales), Std_Dev = sd(Weekly_Sales)) %>% mutate(CV = Std_Dev/Mean_sales*100) %>% arrange(desc(CV)) # Calculating CV and arranging in desc order

# Q3 - Which store/s has good quarterly growth rate in Q3'2012
Q2_2012 <- WMTsales %>% group_by(Store) %>% filter(Date >= "2012-04-01" & Date <= "2012-06-30") %>% summarise(sum(Weekly_Sales)) # Fetch Q2 sales
Q3_2012 <- WMTsales %>% group_by(Store) %>% filter(Date >= "2012-07-01" & Date <= "2012-09-30") %>% summarise(sum(Weekly_Sales)) # Fetch Q3 sales
Q3_Growth <- mutate(Q3_2012, QoQGrowth = ((Q3_2012$`sum(Weekly_Sales)` - Q2_2012$`sum(Weekly_Sales)`)/Q2_2012$`sum(Weekly_Sales)`)*100) # Growth Rate = (Weekly_Sales.2012Q3 - Weekly_Sales.2012Q2)/Weekly_Sales.2012Q2
arrange(Q3_Growth, desc(QoQGrowth)) # Arrange in descending order

# Q4 - Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
Mean_Non_Holiday <- WMTsales %>% filter(Holiday_Flag == "0") %>% summarise(mean(Weekly_Sales)) # Mean of Non-holiday sales
Mean_Non_Holiday # View the mean value
mean(WMTsales$Weekly_Sales)# Mean of Weekly Sales

Holiday_impact <- WMTsales %>% group_by(Date) %>% filter(Holiday_Flag == "1") %>% summarise(Holiday_Week_Sales = sum(Weekly_Sales)) %>% mutate(Greater_than_NonHoliday_Mean = Holiday_Week_Sales > Mean_Non_Holiday) # Add a column to see which holidays have a posiive impact (TRUE / FALSE)
Holiday_impact  # View output
Holiday_impact$Holiday <- ifelse(month(ymd(Holiday_impact$Date)) == 2,"Super Bowl",
                                 ifelse(month(ymd(Holiday_impact$Date)) == 9, "Labor Day",
                                        ifelse(month(ymd(Holiday_impact$Date)) == 11, "Thanksgiving", "Christmas")))  # Identifying which Holidays have positive impact using the flag
Holiday_impact # View output

# Q5 - Provide a monthly and semester view of sales in units and give insights
WMTSales_M_Y  <- mutate(WMTsales,Year_Sale = as.numeric(format(Date,"%Y")), Month_Sale = as.numeric(format(Date,"%m"))) # Adding cols to indicate Year and Month for Weekly Sales
head(WMTSales_M_Y) # Viewing output header
Summarised_WMTSales <- aggregate(Weekly_Sales ~ Year_Sale + Month_Sale, WMTSales_M_Y, sum) %>% arrange(desc(Weekly_Sales)) # Summarising sum of Weekly sales by month and arranging in descending order 
head(Summarised_WMTSales) # View output header

WMTsales_Sem <- mutate(WMTsales,Sem = semester(WMTsales$Date,2010)) # Adding col to indicate semester
Summarised_WMTsales_Sem <- aggregate(Weekly_Sales ~ Sem, WMTsales_Sem, sum) %>% arrange(desc(Weekly_Sales)) # Summarising to show  aggregated sales by semester and arranged in descending order
Summarised_WMTsales_Sem # Viewing output header


#------------------------------------------------------------
# Statistical Model
#------------------------------------------------------------
# For Store 1 - Build  prediction models to forecast demand
# 1.	Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# 2.	Change dates into days by creating new variable.
# Select the model that gives best accuracy.

View (WMTsales)
str(WMTsales)
summary(WMTsales)
head(WMTsales)

WMTSales_days <- mutate(WMTsales, Days = yday(Date)-35) %>% filter(Days >=1) %>% arrange(Days) # Restructuring for dates to days -- starting at 1 for 5Feb2010, sorting by Days
head(WMTSales_days) # View output 1

# Store1 data
Store1_data <- filter(WMTSales_days, Store == 1) # Filtering out Store 1 data only
head(Store1_data) # View output 2

# Ho : There is no impact of Temperature, Fuel_Price, CPI, Unemployment on Store1 sales
# H1 : There is an impact of Temperature, Fuel_Price, CPI, Unemployment on Store1 sales

Store1_model <- lm(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment, data = Store1_data) # MLR on all variables for Store1 data
Store1_model # View model equation
summary(Store1_model) # View model summary (Output 3)

Store1_model1 <- lm(formula = Weekly_Sales ~ Temperature + CPI, data = Store1_data ) # Removing Fuel_Price and Unemployment as their corresponding p value is > alpha (using the backward selection method)
Store1_model1 # View model1 equation (Output 4)
summary(Store1_model1)# View model1 summary (Output 4)
