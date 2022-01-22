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
library(formattable)

Ticker <- "MNST" #Insert Company Ticker in ""
Investment_Start <- "1990-01-01" #Enter as "YYYY-MM-DD"
Investment_End <- "2020-01-01" #Enter as "YYYY-MM-DD"
Initial_Deposit <- 0 #Enter Starting Amount
Investment_Frequency <- 30 #Insert your Investment Frequency in Days, must be >1.
Investment_Amount <- 2000

Stock_Table <- tq_get(Ticker,                    
                  from = Investment_Start,
                  to = Investment_End,
                  get = "stock.prices")
Stock_Table_df<-as.data.frame(Stock_Table)
Stock_Table_df_cut <- Stock_Table_df %>%
  filter(row_number() %% Investment_Frequency == 1)

deposit <- as.data.frame(seq(Investment_Amount, Investment_Amount, length = count(Stock_Table_df_cut)))

Stock_Table_wDeposit<-as.data.frame(c(Stock_Table_df_cut,deposit))
Stock_Table_wDeposit[1, 9] = Initial_Deposit
colnames(Stock_Table_wDeposit)[9] <- "Deposit"

M1Spread <- Stock_Table_wDeposit %>% mutate(Shares_Purchased = Deposit / close)
M2Spread <- M1Spread %>% mutate(Cumulative_Shares = cumsum(Shares_Purchased))
M3Spread <- M2Spread %>% mutate(Cumulative_Deposit = cumsum(Deposit))
M4Spread <- M3Spread %>% mutate(Value = (Cumulative_Shares*close))

clean_spread<-M4Spread[, c("Cumulative_Deposit", "Value")]
colnames(clean_spread)[1] <- "Deposit"
colnames(clean_spread)[2] <- "Investment"
m_clean_spread<-melt(clean_spread)
colnames(m_clean_spread)[1] <- "Money_Type"
colnames(m_clean_spread)[2] <- "Value"
final_spread<-cbind(m_clean_spread,M4Spread$date)
colnames(final_spread)[3] <- "Date"

Fin<-as.character(count(M4Spread))

Final_Value <- M4Spread[Fin,13]
Final_Deposit <- M4Spread[Fin,12]
Final_Difference <- Final_Value-Final_Deposit


ggplot(final_spread, aes(x=Date, y=Value, color=Money_Type)) + 
  geom_line() +
  ggtitle("Your Investment is Worth",currency(Final_Value)) +
  xlab("Year") + 
  ylab("Value")
paste("Your Final Value is",currency(Final_Value),"Your Final Deposit it", currency(Final_Deposit),"This is a difference of",currency(Final_Difference))


fig <- plot_ly(data = final_spread, x = ~Date, y = ~Value, color = ~Money_Type)
fig
