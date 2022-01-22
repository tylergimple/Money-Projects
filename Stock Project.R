#Load Libraries
library(tidyquant)
library(timetk)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(reshape)
library(plotly)
library(gcookbook)
library(lattice)  

#Collect Data
netflix <- tq_get("NFLX",                    
                  from = '2011-11-01',
                  to = "2021-11-01",
                  get = "stock.prices")
#Graph Data
netflix %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  ggtitle("Netflix since 2011") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Adjusted Price") +
  theme_bw()

#Calculate Daily/Monthly Returns
netflix_daily_returns <- netflix %>%
  tq_transmute(select = adjusted,           # this specifies which column to select   
               mutate_fun = periodReturn,   # This specifies what to do with that column
               period = "daily",      # This argument calculates Daily returns
               col_rename = "nflx_returns") # renames the column

# Produce a line chart for daily returns
netflix_daily_returns %>%
  ggplot(aes(x = date, y = nflx_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Daily returns") +
  ggtitle("Daily Returns for Netflix") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) 

# Charting the monthly returns for Netflix. Using bar charts
netflix_daily_returns %>%
  ggplot(aes(x = nflx_returns)) +
  geom_histogram(binwidth = 0.015) +
  theme_classic() +
  labs(x = "Daily returns") +
  ggtitle("Daily Returns for Netflix") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) +
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")

# Charting the monthly returns for Netflix. Using bar charts
netflix_monthly_returns %>%
  ggplot(aes(x = date, y = nflx_returns)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Date", y = "Monthly returns") +
  ggtitle("Monthly Returns for Netflix") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-0.6,0.8,0.1),
                     labels = scales::percent) +
  scale_x_date(date_breaks = "years", date_labels = "%Y")


netflix_cum_returns <- netflix_daily_returns %>%
  mutate(cr = cumprod(1 + nflx_returns)) %>%      # using the cumprod function
  mutate(cumulative_returns = cr - 1)

netflix_cum_returns %>%
  ggplot(aes(x = date, y = cumulative_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Cumulative returns for Netflix since 2009",
          subtitle = "$1 investment in 2009 grew to $85")


# Setting up the variables for Tom

interest <- 0.075
annual_deposit <- 12000
n = 50  

# Making an empty Table where we will hold the values

fv_table <- tibble(Year = 1:n,
                   beg_value = 0,
                   deposit = 0,
                   int = 0,
                   end_value = 0)

# Assigning the value for the first row of the table

fv_table[1,2] <- 0
fv_table[1,3] <- annual_deposit
fv_table[1,4] <- annual_deposit * interest
fv_table[1,5] <- fv_table[1,3] + fv_table[1,4]


# Running a for loop to calculate the first part where Tom save $3600 per year
for(i in 2:50) {
  
  fv_table[i,2] <- fv_table[c(i-1),5]
  fv_table[i,3] <- annual_deposit
  fv_table[i,4] <- (fv_table[i,2] + annual_deposit) * interest
  fv_table[i,5] <- fv_table[i,2] + fv_table[i,3] + fv_table[i,4]
  
}

# Running a for loop to calculate the second part where Tom saves nothing but 
# his investments continue to grow at 7.5%

#for(i in 21:50) {
  
  #fv_table[i,2] <- fv_table[c(i-1),5]
  #fv_table[i,3] <- 0
  #fv_table[i,4] <- (fv_table[i,2] + 0) * interest
  #fv_table[i,5] <- fv_table[i,2] + fv_table[i,3] + fv_table[i,4]
  
#}

fv_table_tom <- fv_table 


#-------------------------------------------------------------------------------

# Setting up the variable for Jerry

interest <- 0.075
annual_deposit <- 3600
n = 50 

# Empty table for jerry is from Year 30 to Year 50 when Jerry will retire

fv_table <- tibble(Year = 1:n,
                   beg_value = 0,
                   deposit = 0,
                   int = 0,
                   end_value = 0)

# Assigning the value  to the first row

fv_table[1,2] <- 0
fv_table[1,3] <- annual_deposit
fv_table[1,4] <- annual_deposit * interest
fv_table[1,5] <- fv_table[1,3] + fv_table[1,4]

# Running the for loop for the rest of the years

for(i in 2:50) {
  
  fv_table[i,2] <- fv_table[c(i-1),5]
  fv_table[i,3] <- annual_deposit
  fv_table[i,4] <- (fv_table[i,2] + annual_deposit) * interest
  fv_table[i,5] <- fv_table[i,2] + fv_table[i,3] + fv_table[i,4]
  
}


fv_table_jerry <- fv_table

#--------------------------------------------------------------------------------------

fv_table_both <- left_join(fv_table_tom, fv_table_jerry, by = "Year")

colnames(fv_table_both) <- paste(c("Year", "Tom's initial value", "Tom's Deposit", "Tom's Interest", "Tom's Ending value", "Jerry's initial value", "Jerry's Deposit", "Jerry's Interest", "Jerry's Ending value"))

options(digits = 2)

fv_table_both %>%
  gather(`Tom's initial value`:`Jerry's Ending value`, key = Name, value = save, factor_key = TRUE) %>%
  mutate(save = if_else(is.na(save), 0, save)) %>%
  spread(Name, value = save) %>%
  datatable(caption = "Comparing the Savings and investment growth \n for Tom
            and Jerry", rownames = FALSE,options = list(pageLength = 10,
                                                        autoWidth = TRUE,
                                                        scrollX = TRUE)) %>%
  formatRound(columns = c("Tom's initial value", "Tom's Deposit", "Tom's Interest", "Tom's Ending value", "Jerry's initial value", "Jerry's Deposit", "Jerry's Interest", "Jerry's Ending value"), digits = 2)

fv_table_both <- mutate(fv_table_both, Cumulative2 = fv_table_both$Total_Interest + lag(fv_table_both$`Tom's Interest`))

Investment <- read.csv("Investment.csv")

Total_Deposit<-Investment$Total.Deposit
Total_Interest<-Investment$Cumulative.Interest
Year<-Investment$Year


second <- data.frame(Deposit=Total_Deposit, Investment=Total_Interest, Year=Year)

msecond<-melt.list(second)
msecond
DF <- as.data.frame(second)

class(second)

mydata = data.frame(V1=com$Year, V2=com$Money.value,V3=com$Money.L1)

com<-data.frame(Year=Year,Money=msecond)

my_table <- table(com)

ggplot(com, aes(fill=Money.L1, y=Money.value, x=Year)) + 
  geom_bar(position="stack", stat="identity")

ggplot(com, aes(x=Year),aes(y=Mon)) +
  geom_bar(aes(fill = Money.L1), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")

tabl <- table(Total_Deposit, Total_Interest, Year)
tabl

ggplot(data=second, aes(x=Year, y=Deposit))+geom_point()

                                                     