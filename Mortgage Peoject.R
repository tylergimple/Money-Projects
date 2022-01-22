library(dplyr)
library(ggplot2)
library(data.table)
library(DataCombine)
library(plotly)
library(formattable)


Cost_of_Property <- 500000 #Enter cost of Property
Down_Payment <- .2 #Enter Down Payment in %
Mortgage_Length <- 15 #Enter Length of Mortgage in Years
Interest_Rate <- .027 #Enter the Interest Rate in %
Monthly_Interest_Rate<-Interest_Rate/12
Mortgage_Periods <- Mortgage_Length*12
Mortgage_Amount <- Cost_of_Property-(Cost_of_Property*Down_Payment)
Monthly_Payment <- Mortgage_Amount*((Monthly_Interest_Rate*(1+Monthly_Interest_Rate)^Mortgage_Periods)/(((1+Monthly_Interest_Rate)^Mortgage_Periods)-1))
one_year <- ((Mortgage_Amount)-(Monthly_Payment-(Mortgage_Amount*Monthly_Interest_Rate)))
new_year <- Mortgage_Amount
i=0
x <- vector(length=Mortgage_Periods)

while (i < Mortgage_Periods) {
  new_year=(((new_year)-(Monthly_Payment-(new_year*Monthly_Interest_Rate))))
  x[i] <- new_year
  i = i+1
}

Results<-data.frame(x)
Balance <- InsertRow(Results, NewRow = one_year, RowNum = 1)
resizeBalance<-data.frame(Balance[-c(Mortgage_Periods),])


Payment <- rep(Monthly_Payment, Mortgage_Periods)
Principal <- c(1:(Mortgage_Periods))
Period <- c(1:(Mortgage_Periods))
Interest <- c(1:(Mortgage_Periods))
Spread <- as.data.frame(cbind(Period,Payment,Principal,Interest,resizeBalance))
colnames(Spread)[5] <- "Balance"
Spread2 <- rbind(c(0,0,0,0,Mortgage_Amount), Spread)
M1Spread <- Spread %>% mutate(Interest = lag(Balance, n=1L) * Monthly_Interest_Rate)
M1Spread[1, 4] = Mortgage_Amount*Monthly_Interest_Rate
M2Spread <- M1Spread %>% mutate(Principal = Payment - Interest)

Slim_Spread <- as.data.frame(cbind(M2Spread$Principal, M2Spread$Interest))
colnames(Slim_Spread)[1] <- "Principal"
colnames(Slim_Spread)[2] <- "Interest"
mSpread <- cbind(melt(Slim_Spread),Period)

plotlydat <- as.data.frame(cbind(Slim_Spread,Period,Payment))

fig <- plot_ly(plotlydat, x = ~Period, y = ~Principal, type = 'bar', name = 'Principal')
fig <- fig %>% add_trace(y = ~Interest, name = 'Interest')
fig <- fig %>% layout(yaxis = list(title = 'Dollars'), barmode = 'stack')
fig
paste("Your Monthly Payment is",currency(Monthly_Payment), "For a total mortgage payment of", currency((Monthly_Payment*Mortgage_Periods)+(Cost_of_Property*Down_Payment)), "over",Mortgage_Length,"Years")


pie_princ<-sum(Slim_Spread$Principal)
pie_int<-sum(Slim_Spread$Interest)
pie<-as.data.frame(cbind(pie_int,pie_princ))
m_pie<-melt(pie)
pie_num<-as.numeric(m_pie$value)

pie(pie_num, labels = currency(pie_num), col=rainbow(length(pie_num)), main="Final") + legend("topleft", c("Total Interest", "Total Principal"), cex = 0.5, fill = rainbow(length(m_pie$value)))


