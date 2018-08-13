#for loading in Microsfot Excel(.xlsx) files
library(xlsx)
#for using smartBind to combine the stock dataframes
library(gtools)
#loads this library in order to get stock price
library(quantmod)
library(DMwR)
library(mice)

#col names should be in the table as a variable with title=date and the row names should be columns 
stockDataMunging=function(dataframe, stockName){
  #changes first col name
  
  names(dataframe)[1]="stat_name"
  
  newColNames<- dataframe$stat_name
  
  # transpose all but the first column (name)
  dataframe <- as.data.frame(t(dataframe[,-1]))
  colnames(dataframe) <- newColNames
  
  #create the date column from the row names then remove the row names
  dataframe$Date=rownames(dataframe)
  row.names(dataframe)=NULL
  
  return(dataframe)
}
createstockDataframeQ=function(stockNames,stockFileNames){

  #loops through the stocks and the files for the stocks and creates a dataframe for each
  #then it merges the dataframes into a single dataframe
  #dataFrame=data.frame of the individual files,individualStockframe=data.frame of the individual stock
  #stockDataframeQ=data.frame of all the stocks combined
  for(stock in stockNames){
    
    print(stock)
    stockImpDateList=NULL
    for(fileName in stockFileNames){
      
      #takes in a single dataframe then adds it to the individualStockframe
      dataframe=read.xlsx(paste("./stockrow_stock_data_quarterly",stock,fileName,sep="/"),1)
      dataframe=stockDataMunging(dataframe,stock);
      
      #changes date from numeric value with an X in front of it to a date value EX: X39813 to 2008-09-27, substring is use to remove the x
      dataframe$Date=as.Date(as.numeric(substring(dataframe$Date,2)), origin="1899-12-30")
      
      #adds a year date value to the data.frame
      dataframe$Year=substring(dataframe$Date,1,4)
      year=dataframe$Year
      
      #only keep first 10 rows
      dataframe=dataframe[year!=2008 & year!=2009 & !is.na(year),]
      
      #the month of stockImpDateList is changed because the stock data is not collected at the same time for each stock, gets month number from the data column
      #months come from the year 2015
      month1=substring(dataframe[length(row.names(dataframe)),"Date"],6,7)
      month2=substring(dataframe[length(row.names(dataframe))-1,"Date"],6,7)
      month3=substring(dataframe[length(row.names(dataframe))-2,"Date"],6,7)
      month4=substring(dataframe[length(row.names(dataframe))-3,"Date"],6,7)
      
      
      stockImpDateList=c(paste("2010",month1,"01",sep="-"),paste("2010",month2,"01",sep="-"),paste("2010",month3,"01",sep="-"),paste("2010",month4,"01",sep="-"),paste("2011",month1,"01",sep="-"),paste("2011",month2,"01",sep="-"),paste("2011",month3,"01",sep="-"),paste("2011",month4,"01",sep="-"),paste("2012",month1,"01",sep="-"),paste("2012",month2,"01",sep="-"),paste("2012",month3,"01",sep="-"),paste("2012",month4,"01",sep="-"),paste("2013",month1,"01",sep="-"),paste("2013",month2,"01",sep="-"),paste("2013",month3,"01",sep="-"),paste("2013",month4,"01",sep="-"),paste("2014",month1,"01",sep="-"),paste("2014",month2,"01",sep="-"),paste("2014",month3,"01",sep="-"),paste("2014",month4,"01",sep="-"),paste("2015",month1,"01",sep="-"),paste("2015",month2,"01",sep="-"),paste("2015",month3,"01",sep="-"),paste("2015",month4,"01",sep="-"),paste("2016",month1,"01",sep="-"),paste("2016",month2,"01",sep="-"),paste("2016",month3,"01",sep="-"),paste("2016",month4,"01",sep="-"),paste("2017",month1,"01",sep="-"),paste("2017",month2,"01",sep="-"),paste("2017",month3,"01",sep="-"),paste("2017",month4,"01",sep="-"),paste("2018",month1,"01",sep="-"))
      
      
      
      
      #only get from Q1 2015 to Q1 2018
      year=dataframe$Year
      
      year2010=which(year==2010)[4:1]
      year2011=which(year==2011)[4:1]
      year2012=which(year==2012)[4:1]
      year2013=which(year==2013)[4:1]
      year2014=which(year==2014)[4:1]
      year2015=which(year==2015)[4:1]
      year2016=which(year==2016)[4:1]
      year2017=which(year==2017)[4:1]
      year2018=which(year==2018)
      year2018=year2018[length(year2018)]
      
      dataframe=dataframe[c(year2010,year2011,year2012,year2013,year2014,year2015,year2016,year2017,year2018),]
      
      
      #if it is the first fileName then set individualStockframe=dataframe else merge the two dataframes
      if(fileName==stockFileNames[1]){
        individualStockframe=dataframe
      }else{
        if(fileName=="financials (5).xlsx"){
          #get rid of duplicated columns between metrics and trailing metrics
          dataframe[,c("Interest Coverage","Income Quality","Payout Ratio","Selling, General and Administrative Expense of Revenue","Research and Development Expense of Revenue","FCF per Share")]=NULL
        }
        
        
        individualStockframe=merge(individualStockframe,dataframe,by.x=c("Date","Year"))
      }
    }
    
    
    
    #add stock identfier to the table
    stockColumn=rep(stock,length(dataframe[,1]))
    individualStockframe=cbind(stockColumn,individualStockframe)
    colnames(individualStockframe)[1]="Stock"
    
    
    
    #add fnancial quarters to the data frame
    quarters=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1)
    individualStockframe$Quarter=quarters
    
    individualStockframe$Index=1:33
    
    #add price to stock using QuantMod - accesses the environment returned from getSymbol in the QuantMod then picks
    #out the specific stock from the environment using get() then returns the col=Ticker.Close where ticker is the actual
    #ticker of the specific stock and rows that access the price annually from 2009-2018(stockImpDateList).
    #this data is then added to the dataframe
    stockPriceColName=paste(stock,"Close",sep=".")
    stockDataPrice=get(stock,envir=stockDataEnv)[stockImpDateList,stockPriceColName]
    individualStockframe$Price=as.numeric(stockDataPrice)
    
    
    
    #adds a column that shows the price increase year over year as a percent, 12% is average
    #subtracts the price from the price before it, the price change for the first row will not be able to be calculated because there is no trailing price for it
    #EX: 1,4,10,4 : price=4,10,4 : trailingPrice=1,4,10 : priceChange=3,6,-6
    price=individualStockframe[,"Price"]
    futurePrice=individualStockframe[-1,"Price"]
    
    #add another stock price to future price that is for the last quarter available for 2018 by adding 3 to the 2018 month and using yahoo finance api
    month2018=as.numeric(substring(individualStockframe[length(stockImpDateList),"Date"],6,7))+3
    if(month2018<10){
      month2018=paste("0",month2018,sep="")
    }else{
      month2018=as.character(month2018)
    }
    stockImpDateList=paste("2018",month2018,"01",sep="-")
    
    stockDataPrice=get(stock,envir=stockDataEnv)[stockImpDateList,stockPriceColName]
    futurePrice=c(futurePrice,as.numeric(stockDataPrice))
    
    
    priceChange=futurePrice-price
    #changes priceChange to a percent
    priceChangePercent=(priceChange/price)*100
    #adds 0 as the price change for the last row
    priceChangePercentPresentYear=c(priceChangePercent)
    individualStockframe$"Price Change %"=priceChangePercentPresentYear
    #adds 0 as the price change for the first row
    priceChangePercentPreviousYear=c(0,priceChangePercent[-length(priceChangePercent)])
    individualStockframe$"Price Change % Previous Year"=priceChangePercentPreviousYear
    priceChangePercent=c(priceChangePercent[-1],0)
    individualStockframe$"Price Change % Next Year"=priceChangePercent
    priceChangePercent=c(priceChangePercent[-c(1,2)],0,0)
    individualStockframe$"Price Change % 2 Years Later"=priceChangePercent
    
    
    
    #if it is the first stock then set stockDataframeQ=individualStockframe else merge the two dataframes
    if(stock==stockNames[1]){
      stockDataframeQ=individualStockframe
    }else{
      stockDataframeQ=smartbind(stockDataframeQ,individualStockframe, fill=NA)
    }
  }
  return(stockDataframeQ)
}

#creates vectors that include the stocks that will be used and the different files for each stock
stockNames=list.files("./stockrow_stock_data_quarterly")
numberOfStocks=length(stockNames)

#correspond to Income, Balance Sheet, Cash Flow, Metrics, Growth, metrics(trailing)
stockFileNames=c("financials.xlsx","financials (1).xlsx","financials (2).xlsx","financials (3).xlsx","financials (4).xlsx","financials (5).xlsx")

#creates a new environment to hold the results(stock prices) from quantmod 
stockDataEnv <- new.env()
getSymbols(stockNames,env=stockDataEnv,src='yahoo',periodicity = "monthly",from = "2010-01-01",
           to = Sys.Date(),curl.options = list())
#NOTE: can't do periodicity="annually" with getSymbols()- so this array is used to select annual dates from the environment
stockImpDateList=c()
numberOfDates=length(stockImpDateList)

#creates the stockDataframeQ
stockDataframeQ=createstockDataframeQ(stockNames,stockFileNames)
stockTrain=stockDataframeQ



lastIndex=33

#Impute missing values
imputationData=stockDataframeQ[,c("EPS","Weighted Average Shares Diluted","Accumulated Retained Earnings (Deficit)","Investments Non-Current","Short-term debt","Liabilities Non-Current","Assets Non-Current","Current Ratio","Working Capital","ROIC","Long-term debt","Current Assets","Current Liabilities","Inventory","Investments Current")]

#MICE can't use vars with spaces or -'s
names(imputationData) <- gsub(x = names(imputationData), pattern = " ", replacement = ".")  
names(imputationData) <- gsub(x = names(imputationData), pattern = "\\(", replacement = "10")  
names(imputationData) <- gsub(x = names(imputationData), pattern = "\\)", replacement = "20")  
names(imputationData) <- gsub(x = names(imputationData), pattern = "-", replacement = "30")  

tempData <- mice(imputationData,m=5,maxit=50,meth='cart',seed=500)
imputationData <- complete(tempData,1)

names(imputationData) <- gsub(x = names(imputationData), pattern = "\\.", replacement = " ")
names(imputationData) <- gsub(x = names(imputationData), pattern = "10", replacement = "(")  
names(imputationData) <- gsub(x = names(imputationData), pattern = "20", replacement = ")") 
names(imputationData) <- gsub(x = names(imputationData), pattern = "30", replacement = "-")  

stockDataframeQ[,c("EPS","Weighted Average Shares Diluted","Accumulated Retained Earnings (Deficit)","Investments Non-Current","Short-term debt","Liabilities Non-Current","Assets Non-Current","Current Ratio","Working Capital","ROIC","Long-term debt","Current Assets","Current Liabilities","Inventory",
                   "Investments Current")]=imputationData
stockDataframeQ$"Cash and short-term investments"=stockDataframeQ$"Cash and cash equivalents"+stockDataframeQ$"Investments Current"




#adds key stock metrics to the price that were not retrieved from stockrow such as P/E and P/B
indexNoPrice=which(is.na(stockDataframeQ$Price))
stockDataframeQ[indexNoPrice,]=stockDataframeQ[(indexNoPrice-1),"Price"]

#P/E - Market Price per Share / Earnings per Share
price=stockDataframeQ[,"Price"]
EPS=stockDataframeQ[,"EPS"]
PE=price/(EPS+.001); stockDataframeQ$PE=PE


#P/B - Market Price per Share / Book Value per Share
bookValue=stockDataframeQ[,"Book Value per Share"]
PB=price/bookValue; stockDataframeQ$PB=PB

#P/S - Market Price per Share / sales
salesPerShare=stockDataframeQ[,"Revenue"]/stockDataframeQ[,"Weighted Average Shares"]
which((stockDataframeQ[,"Revenue"]==0))
PS=price/salesPerShare; stockDataframeQ$PS=PS

#PEG - P/E to growth(EPS increase year over year), *100 to convert from decimal to percent representation of data
earningsGrowth=stockDataframeQ[,"Net Income Growth"]*100
PEG=PE/(earningsGrowth+.001)
stockDataframeQ$PEG=PEG
stockDataframeQ[which(earningsGrowth==0),"PEG"]=0





#price to operating cash flow
operatingCashFlow=stockDataframeQ[,"Operating Cash Flow"]/stockDataframeQ[,"Weighted Average Shares"]
PCF=price/operatingCashFlow;
stockDataframeQ$PCF=PCF
stockDataframeQ[which(operatingCashFlow==0),"PCF"]=0


#profitability ratios
sales=stockDataframeQ$"Revenue"+1
operatingCashFlow=stockDataframeQ$"Operating Cash Flow"+1
capital=stockDataframeQ$"Working Capital"+1
netIncome=stockDataframeQ$"Net Income"+1
grossIncome=stockDataframeQ$"Gross Profit"+1
fixedAssets=stockDataframeQ[,"Property, Plant & Equipment Net"]+1
netAssets=fixedAssets+capital
longTermDebt=stockDataframeQ$"Long-term debt"+1
#add 1 to avoid / by 0
research=stockDataframeQ$"Research and Development (R&D) Expenses"+1
researchPrev=c(1,research[-length(research)])
stockDataframeQ$"Return on Capital Employed"=stockDataframeQ$EBIT/capital
stockDataframeQ$"Operating Profit Margin"=stockDataframeQ$"Operating Income"/sales
stockDataframeQ$"Return on Operating Cash Flow"=operatingCashFlow/sales
stockDataframeQ$"Return on Retained Earnings"=netIncome/(stockDataframeQ$"Accumulated Retained Earnings (Deficit)"+1)
stockDataframeQ[which(is.na(stockDataframeQ$"Return on Retained Earnings")),"Return on Retained Earnings"]=0
stockDataframeQ$"Cash Flow Return on Investment"=operatingCashFlow/capital
stockDataframeQ[which(is.na(stockDataframeQ$"Cash Flow Return on Investment")),"Cash Flow Return on Investment"]=0
stockDataframeQ$"Return on Debt"=netIncome/longTermDebt
stockDataframeQ[which(is.na(stockDataframeQ$"Return on Debt")),"Return on Debt"]=0
stockDataframeQ$"Return on Average Equity"=netIncome/stockDataframeQ$"Average Equity"
stockDataframeQ$"Cash Return on Assets"=operatingCashFlow/stockDataframeQ$"Average Assets"
stockDataframeQ$"Return on Average Assets"=netIncome/stockDataframeQ$"Average Assets"
stockDataframeQ$"Return on Research Capital"=grossIncome/researchPrev
stockDataframeQ$"Cash Return On Capital Invested"=stockDataframeQ$"EBITDA"/stockDataframeQ$"Invested Capital"
stockDataframeQ$"Return on Net Assets"=netIncome/netAssets
stockDataframeQ[which(is.na(stockDataframeQ$"Return on Net Assets")),"Return on Net Assets"]=0


#valuation ratios
stockDataframeQ$"Cash EPS"=operatingCashFlow/stockDataframeQ$"Weighted Average Shares Diluted"
stockDataframeQ[which(is.na(stockDataframeQ$"Cash EPS")),"Cash EPS"]=0
stockDataframeQ$"Net Asset Value per Share"=capital/stockDataframeQ$"Weighted Average Shares"
stockDataframeQ[which(is.na(stockDataframeQ$"Net Asset Value per Share")),"Net Asset Value per Share"]=0
stockDataframeQ$"Times Preferred Dividends Earned"=stockDataframeQ$"Dividends per Common Share"/stockDataframeQ$"Price"
stockDataframeQ$"PEGY"=stockDataframeQ$"PE"/(stockDataframeQ$"EPS Growth"+.0001)*100
stockDataframeQ$"Dividend Payout Ratio"=stockDataframeQ$"Dividends per Common Share"/(stockDataframeQ$"EPS Growth"+.0001)
stockDataframeQ[which(is.na(stockDataframeQ$"Dividend Payout Ratio")),"Dividend Payout Ratio"]=0

#activity ratios
cash=stockDataframeQ[-1,"Cash and cash equivalents"]
cash2=stockDataframeQ[-length(rownames(stockDataframeQ)),"Cash and cash equivalents"]
averageCash=c(1,(cash+cash2)/2)
stockDataframeQ$"Average Cash"=averageCash

capital=stockDataframeQ[-1,"Working Capital"]
capital2=stockDataframeQ[-length(rownames(stockDataframeQ)),"Working Capital"]
averageCapital=c(1,(capital+capital2)/2)
stockDataframeQ$"Average Working Capital"=averageCapital

stockDataframeQ$"Cash Turnover"=sales/(averageCash+1)
which(is.na(stockDataframeQ$"Days Sales Outstanding"))
stockDataframeQ$"Days Sales Outstanding"=stockDataframeQ$"Average Receivables"/sales*(stockDataframeQ$"Average Days of Receivables"+1)
stockDataframeQ$"Days Payable Outstanding"=stockDataframeQ$"Average Payables"/sales*(stockDataframeQ$"Average Days of Payables"+1)
stockDataframeQ$"Days Inventory Outstanding"=stockDataframeQ$"Average Inventory"/sales*(stockDataframeQ$"Days of Inventory on Hand"+1)
stockDataframeQ$"Operating Cash Flow to Sales"=operatingCashFlow/sales
stockDataframeQ$"Days Working Capital"=averageCapital*365/sales
stockDataframeQ[which(is.na(stockDataframeQ$"Days Working Capital")),"Days Working Capital"]=0
stockDataframeQ$"Days Cash on Hand"=stockDataframeQ$"Cash and cash equivalents"/(stockDataframeQ$"Operating Expenses"+stockDataframeQ$"Depreciation & Amortization"+1)*365
stockDataframeQ$"Sales to Administrative Expenses"=sales/(stockDataframeQ$"Selling, General and Administrative (SG&A) Expenses"+1)
stockDataframeQ$"Investment Turnover"=sales/(stockDataframeQ$"Shareholders Equity"+stockDataframeQ$"Total Debt")
stockDataframeQ$"Sales to Equity"=sales/stockDataframeQ$"Average Equity"
stockDataframeQ$"Inventory to Sales"=stockDataframeQ$"Average Inventory"/sales
stockDataframeQ$"Sales to Operating Income"=sales/stockDataframeQ$"Operating Income"
stockDataframeQ$"Goodwill to Assets"=stockDataframeQ$"Goodwill and Intangible Assets"/stockDataframeQ$"Total Assets"
stockDataframeQ$"Free Cash Flow to Sales"=stockDataframeQ$"Free Cash Flow"/sales



#Liquidity ratios
currentLiabilities=stockDataframeQ[,"Current Liabilities"]+1
currentAssets=stockDataframeQ[,"Current Assets"]+1
cash=stockDataframeQ[,"Cash and cash equivalents"]
workingCapital=stockDataframeQ[,"Average Receivables"]+stockDataframeQ[,"Inventory"]-stockDataframeQ[,"Average Payables"]+1
stockDataframeQ[,"Cash Ratio"]=cash/(currentLiabilities)
stockDataframeQ[,"Quick Ratio"]=(stockDataframeQ[,"Current Assets"]-stockDataframeQ[,"Inventory"])/currentLiabilities
stockDataframeQ[which(is.na(stockDataframeQ$"Quick Ratio")),"Quick Ratio"]=0
stockDataframeQ[,"Cash to Working Capital"]=cash/workingCapital
stockDataframeQ[,"Inventory to Working Capital"]=stockDataframeQ[,"Inventory"]/workingCapital
stockDataframeQ[,"Sales to Current Assets"]=stockDataframeQ[,"Revenue"]/currentAssets
stockDataframeQ[,"Sales to Working Capital"]=stockDataframeQ[,"Revenue"]/workingCapital
stockDataframeQ[,"Net Working Capital Ratio"]=stockDataframeQ[,"Working Capital"]/stockDataframeQ[,"Total Assets"]
stockDataframeQ[which(is.na(stockDataframeQ$"Net Working Capital Ratio")),"Net Working Capital Ratio"]=0
stockDataframeQ[,"Acid Test"]=(stockDataframeQ[,"Average Receivables"]+stockDataframeQ[,"Cash and short-term investments"])/currentLiabilities
stockDataframeQ[which(is.na(stockDataframeQ$"Acid Test")),"Acid Test"]=0
stockDataframeQ[,"Cash to Current Assets"]=(stockDataframeQ[,"Cash and cash equivalents"]+stockDataframeQ[,"Investments Current"])/currentAssets
stockDataframeQ[which(is.na(stockDataframeQ$"Cash to Current Assets")),"Cash to Current Assets"]=0
stockDataframeQ[,"Cash Flow Coverage"]=stockDataframeQ[,"Operating Cash Flow"]/(stockDataframeQ[,"Total Debt"]+1)


#Solvency Ratios
currentLiabilities=stockDataframeQ[-length(row.names(stockDataframeQ)),"Current Liabilities"]
futureCurrentLiabilities=stockDataframeQ[-1,"Current Liabilities"]
currentLiabilitiesAverage=(futureCurrentLiabilities+currentLiabilities)/2
currentLiabilitiesAverage=c(1,currentLiabilitiesAverage)
stockDataframeQ$"Current Liabilities Average"=currentLiabilitiesAverage

totalDebt=stockDataframeQ[,"Total Debt"]+1
totalAssets=stockDataframeQ[,"Total Assets"]
totalLiabilities=stockDataframeQ[,"Total Liabilities"]
longTermDebt=stockDataframeQ[,"Long-term debt"]+1

stockDataframeQ[,"Cash Flow to Debt"]=stockDataframeQ[,"Operating Cash Flow"]/totalLiabilities
stockDataframeQ[,"Debt Ratio"]=totalLiabilities/totalAssets
stockDataframeQ[,"Equity Ratio"]=(totalAssets-totalLiabilities)/totalAssets
stockDataframeQ[,"Working Capital to Debt"]=stockDataframeQ[,"Working Capital"]/totalDebt
stockDataframeQ[which(is.na(stockDataframeQ$"Working Capital to Debt")),"Working Capital to Debt"]=0
stockDataframeQ[,"Current Cash Debt Coverage"]=stockDataframeQ[,"Operating Cash Flow"]/(currentLiabilitiesAverage+1)
stockDataframeQ[,"Interest Coverage"]=stockDataframeQ[,"EBIT"]/(stockDataframeQ[,"Interest Expense"]+1)
stockDataframeQ[,"Asset Coverage"]=(totalAssets-stockDataframeQ[,"Goodwill and Intangible Assets"])-(stockDataframeQ[,"Current Liabilities"]-stockDataframeQ[,"Short-term debt"])/totalDebt
stockDataframeQ[which(is.na(stockDataframeQ$"Asset Coverage")),"Asset Coverage"]=0
stockDataframeQ[,"Interest Expense to Debt"]=stockDataframeQ[,"Interest Expense"]/totalDebt
stockDataframeQ[,"Capitalization Ratio"]=longTermDebt/(longTermDebt+stockDataframeQ[,"Shareholders Equity"])
stockDataframeQ[,"Debt to EBITDA"]=totalDebt/stockDataframeQ[,"EBITDA"]
stockDataframeQ[,"Long-term debt ratio"]=longTermDebt/totalAssets
stockDataframeQ[,"Net Debt to EBITDA"]=(longTermDebt+stockDataframeQ[,"Short-term debt"]-stockDataframeQ[,"Cash and cash equivalents"])/stockDataframeQ[,"EBITDA"]
stockDataframeQ[which(is.na(stockDataframeQ$"Net Debt to EBITDA")),"Net Debt to EBITDA"]=0
stockDataframeQ[,"Cash Flow Coverage"]=(stockDataframeQ[,"Capital Expenditure"]+stockDataframeQ[,"Payment of Dividends & Other Cash Distributions   "]+stockDataframeQ[,"Issuance (Repayment) of Debt Securities "])/stockDataframeQ[,"Operating Cash Flow"]
stockDataframeQ[,"Financial Leverage Index"]=stockDataframeQ[,"ROE"]/(stockDataframeQ[,"ROA"]+.01)
stockDataframeQ[which(is.infinite(stockDataframeQ$"Financial Leverage Index")),"Financial Leverage Index"]=0
stockDataframeQ[,"Non-Current Asset to Net Worth"]=stockDataframeQ[,"Assets Non-Current"]/(totalAssets-totalLiabilities)
stockDataframeQ[,"Long-term debt to Equity Ratio"]=longTermDebt/stockDataframeQ[,"Shareholders Equity"]
stockDataframeQ[,"Long-term debt to Total Assets"]=longTermDebt/totalAssets
stockDataframeQ[,"Fixed-Assets to Net Worth Ratio"]=(stockDataframeQ[,"Property, Plant & Equipment Net"]-stockDataframeQ[,"Depreciation & Amortization "])/(totalAssets-totalLiabilities-stockDataframeQ[,"Goodwill and Intangible Assets"])

#c("Total Asset Turnover Ratio","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio")

#Merchandise Inventory Ratio
costOfGoodsSold=stockDataframeQ[,"Cost of Revenue"]
inventory=stockDataframeQ[,"Average Inventory"]
merchandiseTurnoverRatio=costOfGoodsSold/inventory
stockDataframeQ[,"Merchandise Inventory Ratio"]=merchandiseTurnoverRatio
stockDataframeQ[which(inventory==0),"Merchandise Inventory Ratio"]=0

#create average working capital
workingCapital=stockDataframeQ[-length(row.names(stockDataframeQ)),"Working Capital"]
futureWorkingCapital=stockDataframeQ[-1,"Working Capital"]
workingCapitalAverage=(futureWorkingCapital+workingCapital)/2
workingCapitalAverage=c(1,workingCapitalAverage)
stockDataframeQ$"Working Capital Average"=workingCapitalAverage
#Working Capital Turnover Ratio
workingCapitalTurnoverRatio=sales/workingCapitalAverage
stockDataframeQ[,"Working Capital Turnover Ratio"]=workingCapitalTurnoverRatio
stockDataframeQ[which(!is.finite(workingCapitalTurnoverRatio)|is.na(workingCapitalTurnoverRatio)),"Working Capital Turnover Ratio"]=0

#Fixed Asset Turnover Ratio
fixedAssets=stockDataframeQ[,"Property, Plant & Equipment Net"]
fixedAssetTurnoverRatio=sales/fixedAssets
stockDataframeQ[,"Fixed Asset Turnover Ratio"]=fixedAssetTurnoverRatio
stockDataframeQ[which(fixedAssets==0),"Fixed Asset Turnover Ratio"]=0


#c(Degree of Financial Leverage,Equity Multiplier)
#Degree of Financial Leverage-Change in EPS / % Change in EBIT
EPSGrowth=stockDataframeQ[,"EPS Growth"]
EBITGrowth=stockDataframeQ[,"EBIT Growth"]
EBITGrowth[which(EBITGrowth==0)]=1
degreeOfFinancialLeverage=EPSGrowth/EBITGrowth
stockDataframeQ[,"Degree of Financial Leverage"]=degreeOfFinancialLeverage

#Equity Multiplier
totalAssets=stockDataframeQ[,"Total Assets"]
netEquity=stockDataframeQ[,"Current Assets"]-stockDataframeQ[,"Inventory"]-stockDataframeQ[,"Total Debt"]
stockDataframeQ[,"Equity Multiplier"]=totalAssets/netEquity






#Market Cap - Market Price per Share * total shares outstanding
outstandingShares=stockDataframeQ[,"Weighted Average Shares"]
marketCap=price * outstandingShares; stockDataframeQ$"Market Capital"=marketCap




#create a month and next month feature
month=substring(stockDataframeQ[,"Date"],6,7)
stockDataframeQ$Month=month
nextMonth=c(month[-1],0)
stockDataframeQ$"Next Month"=nextMonth

year=stockDataframeQ$Year
nextYear=c(as.character(year[-1]),0)
stockDataframeQ$"Next Year"=nextYear


#adds the feature to be predicted : Beat Market - any price increase of > 12%, Beat Market Next Year is a column that shows if the stock beat the market in a year from the row's date
stockDataframeQ$"Price Increased"=0
priceChange=stockDataframeQ[,"Price Change %"]
beatMarketIndexes=which(priceChange>0)
stockDataframeQ[beatMarketIndexes,"Price Increased"]=1
stockDataframeQ$"Beat Market"=0
stockDataframeQ$"Beat Market Next Year"=0
stockDataframeQ$"Beat Market Both Years"=0

marketAvg=read.csv(file="C:/Users/Kyle Peters/Desktop/R-Workspace/Market Historical Perfomance/Market Average Q.csv", header=TRUE, check.names=FALSE)

#to add more to the market average
#marketAvg2=data.frame(Date=c("06/1/2010","07/1/2010","08/1/2010","09/1/2010","10/1/2010","11/1/2010","12/1/2010","01/1/2011","02/1/2011","03/1/2011","04/1/2011","05/1/2011","06/1/2011","07/1/2011","08/1/2011","09/1/2011","10/1/2011","11/1/2011","12/1/2011","01/1/2012","02/1/2012","03/1/2012","04/1/2012","05/1/2012","06/1/2012","07/1/2012","08/1/2012","09/1/2012","10/1/2012","11/1/2012","12/1/2012","01/1/2013","02/1/2013","03/1/2013","04/1/2013","05/1/2013",as.character(marketAvg$Date)),"Market.Price"=c(1083.36,1079.80,1087.28,1122.08,1171.58,1198.89,1241.53,1282.62,1321.12,1304.49,1331.51,1338.31,1287.29,1325.19,1185.31,1173.88,1207.22,1226.42,1243.32,1300.58,1352.49,1389.24,1386.43,1341.27,1323.48,1359.78,1403.45,1443.42,1437.82,1394.51,1422.29,1480.40,1512.31,1550.83,1570.70,1639.84,marketAvg$"Market.Price"))
#write.csv(marketAvg2,file="C:/Users/Kyle Peters/Desktop/R-Workspace/Market Historical Perfomance/Market Average Q.csv",row.names=FALSE)

marketDate=marketAvg$Date
marketMonth=substring(marketDate,1,2)
marketYear=substring(marketDate,6,9)

beatMarketIndexes=c()


#uses the current month/year to get the price from the marketAvg df then use next quarter's month/year to do the same thing
#to find out how much the s&p500 increase over that quarter.  Then it decides if the change was less than the change
#by the stock, and if this is so, it adds it to the beatMarketIndexes
index=stockDataframeQ$Index
#don't use the latest quarter because that is the one we are trying to predict
for(i in 1:length(rownames(stockDataframeQ))){
  
  if(index[i]!=lastIndex){
    
    stockYear=year[i]
    stockMonth=month[i]
    stockNextYear=nextYear[i]
    stockNextMonth=nextMonth[i]
    marketPrice1=marketAvg[which(marketYear==stockYear & marketMonth==stockMonth),"Market.Price"]
    marketPrice2=marketAvg[which(marketYear==stockNextYear & marketMonth==stockNextMonth),"Market.Price"]

  
    marketPriceChange=((marketPrice2-marketPrice1)/marketPrice1)*100
    if(priceChange[i]>marketPriceChange){
      beatMarketIndexes=c(beatMarketIndexes,i)
    }
  }
}


stockDataframeQ[beatMarketIndexes,"Beat Market"]=1





dataLength=length(rownames(stockDataframeQ))

stat=stockDataframeQ[-dataLength,"Market Capital"]+.00000000001
futureStat=stockDataframeQ[-1,"Market Capital"]+.00000000001
statChange=futureStat-stat
statChangePercent=(statChange/stat)*100
statChangePercent=c(0,statChangePercent)
stockDataframeQ[,"Market Capital Growth"]=statChangePercent
stockDataframeQ[which(stockDataframeQ[,"Market Capital Growth"]==0),"Market Capital Growth"]=.1


stat=stockDataframeQ[c(-(1:3),-dataLength),"Price Change %"]
stat2=stockDataframeQ[c(-(1:2),-(seq(from=dataLength, to=dataLength-1, by=-1))),"Price Change %"]
stat3=stockDataframeQ[c(-1,-(seq(from=dataLength, to=dataLength-2, by=-1))),"Price Change %"]
stat4=stockDataframeQ[-seq(from=dataLength, to=dataLength-3, by=-1),"Price Change %"]
#add zeros to make up for the first three years inwhich we are unable to get a four year average
yearIncrease=c(0,0,0,0,(stat+stat2+stat3+stat4))
stockDataframeQ[,"Annual Price Change %"]=yearIncrease


#Examples for PE:pe,category pe, five year average pe, four year growth avg, pe growth, pe better than average, pe GT four year average
createIndividualStatFeatures=function(statName,categoryBreaks,betterLess,statAverage,statGrowthAverage,statFuture){
  
  stockDataframeQ[which(is.na(stockDataframeQ[,statName])),statName]=0
  stat=stockDataframeQ[,statName]
  categoryIndex=rep(0,times=length(stat))
  for(i in 1:length(categoryBreaks)){
    categoryIndex[which(categoryIndex==0 & stat<as.double(categoryBreaks[i]))]=i
  }
  #if no value in categoryBreaks is greater than the stat(therefore the index should have 0 as its value) then it should have the value equal to the length of the vector
  categoryIndex[which(categoryIndex==0)]=length(categoryBreaks)
  
  #no value can be 1 so this fixes that
  categoryIndex=categoryIndex-1
  stockDataframeQ[,paste("Category",statName,sep=" ")]=categoryIndex
  

  
  
  #calculates stat growth
  #addition by .000000001 so there is no NAN and INF due to num/0 or 0/0
  stat=stockDataframeQ[-dataLength,statName]+.00000000001
  futureStat=stockDataframeQ[-1,statName]+.00000000001
  
  #can't do growth if one of the stats is negative.  Therefore add the abs value of the stat to the future stat and
  #then set the stat to the abs value of the stat if the stat is negative
  futureStat=ifelse(stat>0,abs(futureStat),abs(stat)+abs(futureStat))
  stat=ifelse(stat>0,stat,abs(stat))
  
  statChange=futureStat-stat
  statChangePercent=(statChange/stat)*100
  statChangePercent=ifelse(stat==0,statChange,statChangePercent)
  statChangePercent=c(0,statChangePercent)
  

  stockDataframeQ[,paste(statName,"Growth",sep=" ")]=statChangePercent
  stockDataframeQ[,paste(statName,"Growth to MK Growth",sep=" ")]=c(0,statChange)/stockDataframeQ[,"Market Capital Growth"]




  #create a feature that decides if a stock is worth buying at its present state by dividing the stat by the PE
  stockDataframeQ[,paste(statName,"to PE",sep=" ")]=stockDataframeQ[,statName]/(stockDataframeQ[,"PE"]+.01)
 
  
  
  
  #calculate stat growth4 year average
  stat=stockDataframeQ[c(-(1:4)),paste(statName,"Growth",sep=" ")]
  stat2=stockDataframeQ[c(-(1:3),-dataLength),paste(statName,"Growth",sep=" ")]
  stat3=stockDataframeQ[c(-(1:2),-(seq(from=dataLength, to=dataLength-1, by=-1))),paste(statName,"Growth",sep=" ")]
  stat4=stockDataframeQ[c(-1,-(seq(from=dataLength, to=dataLength-2, by=-1))),paste(statName,"Growth",sep=" ")]
  #add zeros to make up for the first three years inwhich we are unable to get a four year average
  fourYearAvg=c(0,0,0,0,(stat+stat2+stat3+stat4)/4)
  stockDataframeQ[,paste("Four Year Average", statName,"Growth",sep=" ")]=fourYearAvg
  
  
  #future stat feature-shows what the stat could be in 20 years
  futureStat=stockDataframeQ[,statName]*(1+(stockDataframeQ[,paste("Four Year Average", statName,"Growth",sep=" ")]/100))**20
  futureStat[which(is.infinite(futureStat))]=.000001
  futureStat[which(is.na(futureStat))]=0
  stockDataframeQ[,paste("Future 20 Year",statName,sep=" ")]=futureStat
  
  
  #divides growth by growth last year
  previousGrowth=c(1,stockDataframeQ[-dataLength,paste(statName,"Growth",sep=" ")])
  growthToAvg=stockDataframeQ[,paste(statName,"Growth",sep=" ")]/(previousGrowth+.01)
  stockDataframeQ[,paste(statName,"Growth to Growth Average",sep=" ")]=growthToAvg
  
  #divide stat by 5 year average
  previousStat=c(1,stockDataframeQ[-dataLength,paste("Five Year Average",statName,sep=" ")])
  statToAvg=stockDataframeQ[,statName]/(previousStat+.01)
  stockDataframeQ[,paste(statName,"to Average",sep=" ")]=statToAvg
  
  
  #stat to the market average
  index=as.numeric(stockDataframeQ[,"Index"])
  stat=stockDataframeQ[,statName]
  stockDataframeQ[,paste(statName,"to Market Average",sep=" ")]=stat/statAverage[index]
  


  
  return(stockDataframeQ)
}

#create stat features with discrete vars
statList=c("Average Days of Receivables","Average Days of Payables","Account Payables Turnover","Account Receivables Turnover","Inventory Turnover","Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Cash Ratio","Quick Ratio","Equity Multiplier","Degree of Financial Leverage","ROA","PCF","PE","PB","PS","PEG","FCF per Share","Book Value per Share","ROE","EPS","Gross Margin","EBITDA Margin","EBIT Margin","Profit Margin","Free Cash Flow Margin","Cash per Share","Debt to Equity Ratio","Total Debt To Total Assets","Current Ratio","Income Quality","Payout Ratio","Selling, General and Administrative Expense of Revenue","Research and Development Expense of Revenue")
q=c(0,0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
breaks=list(unname(quantile(stockDataframeQ[,"Average Days of Receivables"],prob=q)),unname(quantile(stockDataframeQ[,"Average Days of Payables"],prob=q)),unname(quantile(stockDataframeQ[,"Account Payables Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Account Receivables Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Inventory Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Asset Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Merchandise Inventory Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Working Capital Turnover Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Fixed Asset Turnover Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Ratio"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Quick Ratio"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Equity Multiplier"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Degree of Financial Leverage"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"ROA"],prob=q)),unname(quantile(stockDataframeQ[,"PCF"],prob=q)),unname(quantile(stockDataframeQ[,"PE"],prob=q)),unname(quantile(stockDataframeQ[,"PB"],prob=q)),unname(quantile(stockDataframeQ[,"PS"],prob=q)),unname(quantile(stockDataframeQ[,"PEG"],prob=q)),unname(quantile(stockDataframeQ[,"FCF per Share"],prob=q)),unname(quantile(stockDataframeQ[,"Book Value per Share"],prob=q)),unname(quantile(stockDataframeQ[,"ROE"],prob=q)),unname(quantile(stockDataframeQ[,"EPS"],prob=q)),unname(quantile(stockDataframeQ[,"Gross Margin"],prob=q)),unname(quantile(stockDataframeQ[,"EBITDA Margin"],prob=q)),unname(quantile(stockDataframeQ[,"EBIT Margin"],prob=q)),unname(quantile(stockDataframeQ[,"Profit Margin"],prob=q)),unname(quantile(stockDataframeQ[,"Free Cash Flow Margin"],prob=q)),unname(quantile(stockDataframeQ[,"Cash per Share"],prob=q)),unname(quantile(stockDataframeQ[,"Debt to Equity Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Total Debt To Total Assets"],prob=q)),unname(quantile(stockDataframeQ[,"Current Ratio"],prob=q,na.rm=T)),unname(quantile(stockDataframeQ[,"Income Quality"],prob=q)),unname(quantile(stockDataframeQ[,"Payout Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Selling, General and Administrative Expense of Revenue"],prob=q)),unname(quantile(stockDataframeQ[,"Research and Development Expense of Revenue"],prob=q)))
betterLess=c(T,T,T,T,T,T,T,T,T,F,F,T,T,F,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,T,T,T,F,F,T,T)
dowJonesAvg=read.csv(file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Averages Q.csv", check.names=FALSE)
for(i in 1:length(statList)){
  stockDataframeQ=createIndividualStatFeatures(statList[i],breaks[[i]],betterLess[i],dowJonesAvg[[statList[i]]],dowJonesAvg[[paste(statList[i],"Growth",sep=" ")]],dowJonesAvg[[paste("Future 20 Year",statList[i],sep=" ")]])
}

#create another set of stat features with discrete vars
statList=c("Return on Sales","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings",
           "Cash Flow Return on Investment","Return on Debt","Cash Return on Assets","Return on Research Capital","Cash Return On Capital Invested",
           "Return on Net Assets","Cash EPS","Net Asset Value per Share","PEGY","Dividend Payout Ratio","Cash Turnover","Days Sales Outstanding"
           ,"Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand",
           "Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income",
           "Goodwill to Assets","Free Cash Flow to Sales","Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets",
           "Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash to Current Assets","Cash Flow Coverage","Cash Flow to Debt","Debt Ratio","Equity Ratio",
           "Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio",
           "Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth",
           "Long-term debt to Equity Ratio","Long-term debt to Total Assets","Fixed-Assets to Net Worth Ratio")

#quantiles to use
q=c(0,0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
breaks=list(unname(quantile(stockDataframeQ[,"Return on Sales"],prob=q)),unname(quantile(stockDataframeQ[,"Return on Capital Employed"],prob=q)),unname(quantile(stockDataframeQ[,"Operating Profit Margin"],prob=q)),unname(quantile(stockDataframeQ[,"Return on Operating Cash Flow"],prob=q)),unname(quantile(stockDataframeQ[,"Return on Retained Earnings"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Flow Return on Investment"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Return on Debt"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Cash Return on Assets"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Return on Research Capital"],na.rm=T,prob=q)),unname(quantile(stockDataframeQ[,"Cash Return On Capital Invested"],prob=q)),unname(quantile(stockDataframeQ[,"Return on Net Assets"],prob=q)),unname(quantile(stockDataframeQ[,"Cash EPS"],prob=q)),unname(quantile(stockDataframeQ[,"Net Asset Value per Share"],prob=q)),unname(quantile(stockDataframeQ[,"PEGY"],prob=q)),unname(quantile(stockDataframeQ[,"Dividend Payout Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Days Sales Outstanding"],prob=q)),unname(quantile(stockDataframeQ[,"Days Payable Outstanding"],prob=q)),unname(quantile(stockDataframeQ[,"Days Inventory Outstanding"],prob=q)),unname(quantile(stockDataframeQ[,"Operating Cash Flow to Sales"],prob=q)),unname(quantile(stockDataframeQ[,"Days Working Capital"],prob=q)),unname(quantile(stockDataframeQ[,"Days Cash on Hand"],prob=q)),unname(quantile(stockDataframeQ[,"Sales to Administrative Expenses"],prob=q)),unname(quantile(stockDataframeQ[,"Investment Turnover"],prob=q)),unname(quantile(stockDataframeQ[,"Sales to Equity"],prob=q)),unname(quantile(stockDataframeQ[,"Inventory to Sales"],prob=q)),unname(quantile(stockDataframeQ[,"Sales to Operating Income"],prob=q)),unname(quantile(stockDataframeQ[,"Goodwill to Assets"],prob=q,na.rm=T)),unname(quantile(stockDataframeQ[,"Free Cash Flow to Sales"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Quick Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Cash to Working Capital"],prob=q)),unname(quantile(stockDataframeQ[,"Inventory to Working Capital"],prob=q)),unname(quantile(stockDataframeQ[,"Sales to Current Assets"],prob=q)),unname(quantile(stockDataframeQ[,"Sales to Working Capital"],prob=q)),unname(quantile(stockDataframeQ[,"Net Working Capital Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Acid Test"],prob=q,na.rm=T)),unname(quantile(stockDataframeQ[,"Cash to Current Assets"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Flow Coverage"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Flow to Debt"],prob=q)),unname(quantile(stockDataframeQ[,"Debt Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Equity Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Working Capital to Debt"],prob=q)),unname(quantile(stockDataframeQ[,"Current Cash Debt Coverage"],prob=q)),unname(quantile(stockDataframeQ[,"Interest Coverage"],prob=q)),unname(quantile(stockDataframeQ[,"Asset Coverage"],prob=q)),unname(quantile(stockDataframeQ[,"Interest Expense to Debt"],prob=q)),unname(quantile(stockDataframeQ[,"Capitalization Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Debt to EBITDA"],prob=q)),unname(quantile(stockDataframeQ[,"Long-term debt ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Net Debt to EBITDA"],prob=q)),unname(quantile(stockDataframeQ[,"Cash Flow Coverage"],prob=q)),unname(quantile(stockDataframeQ[,"Financial Leverage Index"],prob=q)),unname(quantile(stockDataframeQ[,"Non-Current Asset to Net Worth"],prob=q)),unname(quantile(stockDataframeQ[,"Long-term debt to Equity Ratio"],prob=q)),unname(quantile(stockDataframeQ[,"Long-term debt to Total Assets"],prob=q)),unname(quantile(stockDataframeQ[,"Fixed-Assets to Net Worth Ratio"],prob=q)))
betterLess=c(T,T,T,T,T,F,F,T,T,F,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,T,T,T,F,F,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T)
dowJonesAvg=read.csv(file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Averages Q.csv", check.names=FALSE)
for(i in 1:length(statList)){
  stockDataframeQ=createIndividualStatFeatures(statList[i],breaks[[i]],betterLess[i],dowJonesAvg[[statList[i]]],dowJonesAvg[[paste(statList[i],"Growth",sep=" ")]],dowJonesAvg[[paste("Future 20 Year",statList[i],sep=" ")]])
}


#create stat features with continuous vars
statList=c("Weighted Average Shares","Long-term debt","Total Assets","Revenue","Net Income","Cost of Revenue","Gross Profit","Research and Development (R&D) Expenses","Selling, General and Administrative (SG&A) Expenses","Operating Expenses","EBITDA","EBIT","Cash and cash equivalents","Investments Current","Cash and short-term investments","Inventory","Current Assets","Goodwill and Intangible Assets","Assets Non-Current","Current Liabilities","Liabilities Non-Current","Total Debt","Total Liabilities","Shareholders Equity","Financing Cash Flow","Investing Cash Flow","Operating Cash Flow","Average Receivables","Average Payables")
breaks=list(unname(quantile(stockDataframeQ[,"Market Capital"])),unname(quantile(stockDataframeQ[,"Weighted Average Shares"])),unname(quantile(stockDataframeQ[,"Long-term debt"],na.rm=T)),unname(quantile(stockDataframeQ[,"Total Assets"])),unname(quantile(stockDataframeQ[,"Revenue"])),unname(quantile(stockDataframeQ[,"Net Income"])),unname(quantile(stockDataframeQ[,"Cost of Revenue"])),unname(quantile(stockDataframeQ[,"Gross Profit"])),unname(quantile(stockDataframeQ[,"Research and Development (R&D) Expenses"])),unname(quantile(stockDataframeQ[,"Selling, General and Administrative (SG&A) Expenses"])),unname(quantile(stockDataframeQ[,"Operating Expenses"])),unname(quantile(stockDataframeQ[,"EBITDA"])),unname(quantile(stockDataframeQ[,"EBIT"])),unname(quantile(stockDataframeQ[,"Cash and cash equivalents"])),unname(quantile(stockDataframeQ[,"Investments Current"],na.rm=T)),unname(quantile(stockDataframeQ[,"Cash and short-term investments"],na.rm=T)),unname(quantile(stockDataframeQ[,"Inventory"])),unname(quantile(stockDataframeQ[,"Current Assets"],na.rm=T)),unname(quantile(stockDataframeQ[,"Goodwill and Intangible Assets"])),unname(quantile(stockDataframeQ[,"Assets Non-Current"],na.rm=T)),unname(quantile(stockDataframeQ[,"Current Liabilities"],na.rm=T)),unname(quantile(stockDataframeQ[,"Liabilities Non-Current"],na.rm=T)),unname(quantile(stockDataframeQ[,"Total Debt"])),unname(quantile(stockDataframeQ[,"Total Liabilities"])),unname(quantile(stockDataframeQ[,"Shareholders Equity"])),unname(quantile(stockDataframeQ[,"Financing Cash Flow"])),unname(quantile(stockDataframeQ[,"Investing Cash Flow"])),unname(quantile(stockDataframeQ[,"Operating Cash Flow"])),unname(quantile(stockDataframeQ[,"Average Receivables"])),unname(quantile(stockDataframeQ[,"Average Payables"])))
betterLess=c(T,T,T,F,F,F,T,F,T,T,T,F,F,F,F,F,F,F,F,T,T,T,T,F,F,F,F,F,F,T)
dowJonesAvg=read.csv(file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Averages Q.csv", check.names=FALSE)
for(i in 1:length(statList)){
  stockDataframeQ=createIndividualStatFeatures(statList[i],breaks[[i]],betterLess[i],dowJonesAvg[[statList[i]]],dowJonesAvg[[paste(statList[i],"Growth",sep=" ")]],dowJonesAvg[[paste("Future 20 Year",statList[i],sep=" ")]])
}


write.csv(stockDataframeQ,file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dataframe Full Q.csv",row.names=FALSE)


stockDataframeQ$Index=as.factor(stockDataframeQ$Index)
stockDataframeQ$Year=as.factor(stockDataframeQ$Year)
stockDataframeQ$Quarter=as.factor(stockDataframeQ$Quarter)

stockDataframeQ$"Beat Market"=as.factor(stockDataframeQ$"Beat Market")
stockDataframeQ$"Price Increased"=as.factor(stockDataframeQ$"Price Increased")

index=stockDataframeQ$Index
stockDataframeQTemp=stockDataframeQ[index!=1&index!=2&index!=3&index!=4&index!=5,]
index=stockDataframeQTemp$Index
testSetQ=stockDataframeQTemp[index==7,]
index=stockDataframeQTemp$Index
trainingSetQ=stockDataframeQTemp[index==6,]



median=median(stockDataframeQTemp$"Annual Price Change %")
growthTrainingSetQ=trainingSetQ[trainingSetQ$"Annual Price Change %">=median,]
growthTestSetQ=testSetQ[testSetQ$"Annual Price Change %">=median,]




