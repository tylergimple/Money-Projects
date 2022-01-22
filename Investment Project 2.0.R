library(ggplot2)
library(reshape)
library(readxl)
library(dplyr)
library(DataCombine)
library(formattable)
library(data.table)
library(plotly)

Years <- 30 #Input a number of Years
Initial <- 10000 #Input the Initial Value
Monthly_Deposit <- 2237.15 #Input the Montly Deposit
Return <- .06 #Input the Yearly Return
Yearly_Investment <- Monthly_Deposit*12
Iyear<-(((Initial)*(1+Return))+Yearly_Investment)
twoyear<-(((Iyear)*(1+Return))+Yearly_Investment)
new_year<-(((Initial)*(1+Return))+Yearly_Investment)
i=0
x <- vector(length=Years)
while (i < Years) {
  new_year=(((new_year)*(1+Return))+Yearly_Investment)
  x[i] <- new_year
  i = i+1
}

Results<-data.frame(x)
tworesults<-InsertRow(Results, NewRow = twoyear, RowNum = 1)
IResults <- InsertRow(tworesults, NewRow = Iyear, RowNum = 1)
resizeIresults<-data.frame(IResults[-c(Years+1,Years+2),])
FinalValue<-resizeIresults[Years, 1]

STotal_Deposit<-c(1:(nrow(resizeIresults)))
SYear<-c(1:(Years))
SInterest<-c(1:(Years))
Sstarting<-c(Initial)
Spread <- data.frame(SYear, resizeIresults, Sstarting, STotal_Deposit, SInterest)

M1Spread <- Spread %>% mutate(STotal_Deposit = SYear * Yearly_Investment)
M2Spread <- M1Spread %>% mutate(SInterest = (M1Spread$IResults..c.Years...1..Years...2.... - (STotal_Deposit+Sstarting)))

Interest<-M2Spread$SInterest
Deposit<-M2Spread$STotal_Deposit
Initial<-M2Spread$Sstarting
DF<-cbind(Interest,Deposit,Initial)
resize <- DF[-c(Years+1,Years+2),]
MDF<-melt(resize)
Pie<-tail(resize,1)
mPie<-melt(Pie)
mPie2 <- subset( mPie, select = -X1 )
df_Pie <- as.data.frame(mPie2)
df_Pie_num<-as.numeric(df_Pie$value)

ggplot(MDF, aes(y=value, x=Var1, fill=factor(Var2, levels=c("Interest","Deposit","Initial" )))) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.title = element_blank(),axis.line = element_line(colour = "black")) +
  scale_y_continuous(name="Dollars", labels = scales::comma) +
  scale_x_continuous(name="Years", labels = scales::comma) +
  ggtitle("Your Final Value is",currency(FinalValue))

pie(df_Pie_num, labels = currency(df_Pie_num), col=rainbow(length(df_Pie_num)), main="Final Money Allocation") + legend("topleft", c("Total Interest", "Total Deposit", "Starting Value"), cex = 0.5, fill = rainbow(length(df_Pie$value)))

plotly_data<-cbind(resize, SYear)
df_plotly_data <- as.data.frame(plotly_data)
fig<-plot_ly(df_plotly_data, x = ~SYear, y = ~Initial, type = 'bar', name = 'Initial')
fig <- fig %>% add_trace(y = ~Deposit, name = 'Deposit')
fig <- fig %>% add_trace(y = ~Interest, name = 'Interest')
fig <- fig %>% layout(yaxis = list(title = 'Dollars'), barmode = 'stack')
fig <- fig %>% layout(title = "Investment Calendar ",
                      uniformtext = "Hello",
                      xaxis = list(title = "Year"),
                      yaxis = list(title = "Dollars"))
fig

