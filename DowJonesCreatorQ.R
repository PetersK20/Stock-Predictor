#for loading in Microsfot Excel(.xlsx) files
library(xlsx)
#for using smartBind to combine the stock dataframes
library(gtools)
#loads this library in order to get stock price
library(quantmod)

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
createdowJonesQ=function(stockNames,stockFileNames){
  #loops through the stocks and the files for the stocks and creates a dataframe for each
  #then it merges the dataframes into a single dataframe
  #dataFrame=data.frame of the individual files,individualStockframe=data.frame of the individual stock
  #dowJonesQ=data.frame of all the stocks combined
  for(stock in stockNames){
    print(stock)
    stockImpDateList=NULL
    for(fileName in stockFileNames){
      
      #takes in a single dataframe then adds it to the individualStockframe
      dataframe=read.xlsx(paste("./Dow Jones",stock,fileName,sep="/"),1)
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
    
    
    
    #if it is the first stock then set dowJonesQ=individualStockframe else merge the two dataframes
    if(stock==stockNames[1]){
      dowJonesQ=individualStockframe
    }else{
      dowJonesQ=smartbind(dowJonesQ,individualStockframe, fill=NA)
    }
  }
  return(dowJonesQ)
}

#creates vectors that include the stocks that will be used and the different files for each stock
stockNames=list.files("./Dow Jones")
#stockNames=c("AAPL")
numberOfStocks=length(stockNames)

#correspond to Income, Balance Sheet, Cash Flow, Metrics, Growth, metrics(trailing)
stockFileNames=c("financials.xlsx","financials (1).xlsx","financials (2).xlsx","financials (3).xlsx","financials (4).xlsx","financials (5).xlsx")

#creates a new environment to hold the results(stock prices) from quantmod 
stockDataEnv <- new.env()
getSymbols(stockNames,env=stockDataEnv,src='yahoo',periodicity = "monthly",from = "2010-01-01",
           to = Sys.Date(),curl.options = list())
#NOTE: can't do periodicity="annually" with getSymbols()- so this array is used to select annual dates from the environment
stockImpDateList=c("2015-12-01","2016-03-01","2016-06-01","2016-09-01","2016-12-01","2017-03-01","2017-06-01","2017-09-01","2017-12-01","2018-03-01")
numberOfDates=length(stockImpDateList)

#creates the dowJonesQ
dowJonesQ=createdowJonesQ(stockNames,stockFileNames)

#adds key stock metrics to the price that were not retrieved from stockrow such as P/E and P/B
indexNoPrice=which(is.na(dowJonesQ$Price))
dowJonesQ[indexNoPrice,"Price"]=dowJonesQ[(indexNoPrice-1),"Price"]

#P/E - Market Price per Share / Earnings per Share
price=dowJonesQ[,"Price"]
EPS=dowJonesQ[,"EPS"]
PE=price/EPS; dowJonesQ$PE=PE


#P/B - Market Price per Share / Book Value per Share
bookValue=dowJonesQ[,"Book Value per Share"]
PB=price/bookValue; dowJonesQ$PB=PB

#P/S - Market Price per Share / sales
salesPerShare=dowJonesQ[,"Revenue"]/dowJonesQ[,"Weighted Average Shares"]
PS=price/salesPerShare; dowJonesQ$PS=PS

#PEG - P/E to growth(EPS increase year over year), *100 to convert from decimal to percent representation of data
earningsGrowth=dowJonesQ[,"Net Income Growth"]*100
PEG=PE/earningsGrowth;
dowJonesQ$PEG=PEG
dowJonesQ[which(earningsGrowth==0),"PEG"]=0





#price to operating cash flow
operatingCashFlow=dowJonesQ[,"Operating Cash Flow"]
PCF=price/operatingCashFlow;
dowJonesQ$PCF=PCF
dowJonesQ[which(operatingCashFlow==0),"PCF"]=0


#create more profitability ratios
sales=dowJonesQ$"Revenue"
operatingCashFlow=dowJonesQ$"Operating Cash Flow"
capital=dowJonesQ$"Working Capital"
netIncome=dowJonesQ$"Net Income"
grossIncome=dowJonesQ$"Gross Profit"
fixedAssets=dowJonesQ[,"Property, Plant & Equipment Net"]
netAssets=fixedAssets+capital
longTermDebt=dowJonesQ$"Long-term debt"+1
#add 1 to avoid / by 0
research=dowJonesQ$"Research and Development (R&D) Expenses"+1
researchPrev=c(1,research[-length(research)])
dowJonesQ$"Return on Capital Employed"=dowJonesQ$EBIT/capital
dowJonesQ[which(is.na(dowJonesQ$"Return on Capital Employed")),"Return on Capital Employed"]=0
dowJonesQ$"Operating Profit Margin"=dowJonesQ$"Operating Income"/sales
dowJonesQ$"Return on Operating Cash Flow"=operatingCashFlow/sales
dowJonesQ[which(is.na(dowJonesQ$"Return on Operating Cash Flow")),"Return on Operating Cash Flow"]=0
dowJonesQ$"Return on Retained Earnings"=netIncome/dowJonesQ$"Accumulated Retained Earnings (Deficit)"
dowJonesQ[which(is.na(dowJonesQ$"Return on Retained Earnings")),"Return on Retained Earnings"]=0
dowJonesQ$"Cash Flow Return on Investment"=operatingCashFlow/capital
dowJonesQ[which(is.na(dowJonesQ$"Cash Flow Return on Investment")),"Cash Flow Return on Investment"]=0
dowJonesQ$"Return on Debt"=netIncome/longTermDebt
dowJonesQ[which(is.na(dowJonesQ$"Return on Debt")),"Return on Debt"]=0
dowJonesQ$"Return on Average Equity"=netIncome/dowJonesQ$"Average Equity"
dowJonesQ$"Cash Return on Assets"=operatingCashFlow/dowJonesQ$"Average Assets"
dowJonesQ$"Return on Average Assets"=netIncome/dowJonesQ$"Average Assets"
dowJonesQ$"Return on Research Capital"=grossIncome/researchPrev
dowJonesQ$"Cash Return On Capital Invested"=dowJonesQ$"EBITDA"/dowJonesQ$"Invested Capital"
dowJonesQ$"Return on Net Assets"=netIncome/netAssets
dowJonesQ[which(is.na(dowJonesQ$"Return on Net Assets")),"Return on Net Assets"]=0


#create more valuation ratios
dowJonesQ$"Cash EPS"=operatingCashFlow/dowJonesQ$"Weighted Average Shares Diluted"
dowJonesQ[which(is.na(dowJonesQ$"Cash EPS")),"Cash EPS"]=0
dowJonesQ$"Net Asset Value per Share"=capital/dowJonesQ$"Weighted Average Shares"
dowJonesQ[which(is.na(dowJonesQ$"Net Asset Value per Share")),"Net Asset Value per Share"]=0
dowJonesQ$"Times Preferred Dividends Earned"=dowJonesQ$"Dividends per Common Share"/dowJonesQ$"Price"
dowJonesQ$"PEGY"=dowJonesQ$"PE"/(dowJonesQ$"EPS Growth"+1)*100
dowJonesQ$"Dividend Payout Ratio"=dowJonesQ$"Dividends per Common Share"/dowJonesQ$"EPS"


#create more activity ratios
cash=dowJonesQ[-1,"Cash and cash equivalents"]
cash2=dowJonesQ[-length(dowJonesQ),"Cash and cash equivalents"]
averageCash=c(1,(cash+cash2)/2)
dowJonesQ$"Average Cash"=averageCash

capital=dowJonesQ[-1,"Working Capital"]
capital2=dowJonesQ[-length(dowJonesQ),"Working Capital"]
averageCapital=c(1,(capital+capital2)/2)
dowJonesQ$"Average Working Capital"=averageCapital

dowJonesQ$"Cash Turnover"=sales/(averageCash+1)
dowJonesQ$"Days Sales Outstanding"=dowJonesQ$"Average Receivables"/sales*dowJonesQ$"Average Days of Receivables"
dowJonesQ$"Days Payable Outstanding"=dowJonesQ$"Average Payables"/sales*dowJonesQ$"Average Days of Payables"
dowJonesQ$"Days Inventory Outstanding"=dowJonesQ$"Average Inventory"/sales*dowJonesQ$"Days of Inventory on Hand"
dowJonesQ$"Operating Cash Flow to Sales"=operatingCashFlow/sales
dowJonesQ$"Days Working Capital"=averageCapital*365/sales
dowJonesQ[which(is.na(dowJonesQ$"Days Working Capital")),"Days Working Capital"]=0
dowJonesQ$"Days Cash on Hand"=dowJonesQ$"Cash and cash equivalents"/(dowJonesQ$"Operating Expenses"+dowJonesQ$"Depreciation & Amortization")*365
dowJonesQ$"Sales to Administrative Expenses"=sales/(dowJonesQ$"Selling, General and Administrative (SG&A) Expenses"+1)
dowJonesQ$"Investment Turnover"=sales/(dowJonesQ$"Shareholders Equity"+dowJonesQ$"Total Debt")
dowJonesQ$"Sales to Equity"=sales/dowJonesQ$"Average Equity"
dowJonesQ$"Inventory to Sales"=dowJonesQ$"Average Inventory"/sales
dowJonesQ$"Sales to Operating Income"=sales/dowJonesQ$"Operating Income"
dowJonesQ$"Goodwill to Assets"=dowJonesQ$"Goodwill and Intangible Assets"/dowJonesQ$"Total Assets"
dowJonesQ$"Free Cash Flow to Sales"=dowJonesQ$"Free Cash Flow"/sales



#More liquidity ratios
currentLiabilities=dowJonesQ[,"Current Liabilities"]+1
currentAssets=dowJonesQ[,"Current Assets"]+1
cash=dowJonesQ[,"Cash and cash equivalents"]
workingCapital=dowJonesQ[,"Average Receivables"]+dowJonesQ[,"Inventory"]-dowJonesQ[,"Average Payables"]+1
dowJonesQ[,"Cash Ratio"]=cash/(currentLiabilities)
dowJonesQ[,"Quick Ratio"]=(dowJonesQ[,"Current Assets"]-dowJonesQ[,"Inventory"])/currentLiabilities
dowJonesQ[which(is.na(dowJonesQ$"Quick Ratio")),"Quick Ratio"]=0
dowJonesQ[,"Cash to Working Capital"]=cash/workingCapital
dowJonesQ[,"Inventory to Working Capital"]=dowJonesQ[,"Inventory"]/workingCapital
dowJonesQ[,"Sales to Current Assets"]=dowJonesQ[,"Revenue"]/currentAssets
dowJonesQ[,"Sales to Working Capital"]=dowJonesQ[,"Revenue"]/workingCapital
dowJonesQ[,"Net Working Capital Ratio"]=dowJonesQ[,"Working Capital"]/dowJonesQ[,"Total Assets"]
dowJonesQ[which(is.na(dowJonesQ$"Net Working Capital Ratio")),"Net Working Capital Ratio"]=0
dowJonesQ[,"Acid Test"]=(dowJonesQ[,"Average Receivables"]+dowJonesQ[,"Cash and short-term investments"])/currentLiabilities
dowJonesQ[,"Cash to Current Assets"]=(dowJonesQ[,"Cash and cash equivalents"]+dowJonesQ[,"Investments Current"])/currentAssets
dowJonesQ[,"Cash Flow Coverage"]=dowJonesQ[,"Operating Cash Flow"]/(dowJonesQ[,"Total Debt"]+1)


#More Solvency Ratios
currentLiabilities=dowJonesQ[-length(row.names(dowJonesQ)),"Current Liabilities"]
futureCurrentLiabilities=dowJonesQ[-1,"Current Liabilities"]
currentLiabilitiesAverage=(futureCurrentLiabilities+currentLiabilities)/2
currentLiabilitiesAverage=c(1,currentLiabilitiesAverage)
dowJonesQ$"Current Liabilities Average"=currentLiabilitiesAverage

totalDebt=dowJonesQ[,"Total Debt"]+1
totalAssets=dowJonesQ[,"Total Assets"]
totalLiabilities=dowJonesQ[,"Total Liabilities"]
longTermDebt=dowJonesQ[,"Long-term debt"]

dowJonesQ[,"Cash Flow to Debt"]=dowJonesQ[,"Operating Cash Flow"]/totalLiabilities
dowJonesQ[,"Debt Ratio"]=totalLiabilities/totalAssets
dowJonesQ[,"Equity Ratio"]=(totalAssets-totalLiabilities)/totalAssets
dowJonesQ[,"Working Capital to Debt"]=dowJonesQ[,"Working Capital"]/totalDebt
dowJonesQ[which(is.na(dowJonesQ$"Working Capital to Debt")),"Working Capital to Debt"]=0
dowJonesQ[,"Current Cash Debt Coverage"]=dowJonesQ[,"Operating Cash Flow"]/(currentLiabilitiesAverage+1)
dowJonesQ[,"Interest Coverage"]=dowJonesQ[,"EBIT"]/(dowJonesQ[,"Interest Expense"]+1)
dowJonesQ[,"Asset Coverage"]=(totalAssets-dowJonesQ[,"Goodwill and Intangible Assets"])-(dowJonesQ[,"Current Liabilities"]-dowJonesQ[,"Short-term debt"])/totalDebt
dowJonesQ[which(is.na(dowJonesQ$"Asset Coverage")),"Asset Coverage"]=0
dowJonesQ[,"Interest Expense to Debt"]=dowJonesQ[,"Interest Expense"]/totalDebt
dowJonesQ[,"Capitalization Ratio"]=longTermDebt/(longTermDebt+dowJonesQ[,"Shareholders Equity"])
dowJonesQ[,"Debt to EBITDA"]=totalDebt/dowJonesQ[,"EBITDA"]
dowJonesQ[,"Long-term debt ratio"]=longTermDebt/totalAssets
dowJonesQ[,"Net Debt to EBITDA"]=(longTermDebt+dowJonesQ[,"Short-term debt"]-dowJonesQ[,"Cash and cash equivalents"])/dowJonesQ[,"EBITDA"]
dowJonesQ[which(is.na(dowJonesQ$"Net Debt to EBITDA")),"Net Debt to EBITDA"]=0
dowJonesQ[,"Cash Flow Coverage"]=(dowJonesQ[,"Capital Expenditure"]+dowJonesQ[,"Payment of Dividends & Other Cash Distributions   "]+dowJonesQ[,"Issuance (Repayment) of Debt Securities "])/dowJonesQ[,"Operating Cash Flow"]
dowJonesQ[,"Financial Leverage Index"]=dowJonesQ[,"ROE"]/(dowJonesQ[,"ROA"]+.01)
dowJonesQ[which(is.infinite(dowJonesQ$"Financial Leverage Index")),"Financial Leverage Index"]=0
dowJonesQ[,"Non-Current Asset to Net Worth"]=dowJonesQ[,"Assets Non-Current"]/(totalAssets-totalLiabilities)
dowJonesQ[,"Long-term debt to Equity Ratio"]=longTermDebt/dowJonesQ[,"Shareholders Equity"]
dowJonesQ[,"Long-term debt to Total Assets"]=longTermDebt/totalAssets
dowJonesQ[,"Fixed-Assets to Net Worth Ratio"]=(dowJonesQ[,"Property, Plant & Equipment Net"]-dowJonesQ[,"Depreciation & Amortization "])/(totalAssets-totalLiabilities-dowJonesQ[,"Goodwill and Intangible Assets"])

#c("Total Asset Turnover Ratio","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio")

#Merchandise Inventory Ratio
costOfGoodsSold=dowJonesQ[,"Cost of Revenue"]
inventory=dowJonesQ[,"Average Inventory"]
merchandiseTurnoverRatio=costOfGoodsSold/inventory
dowJonesQ[,"Merchandise Inventory Ratio"]=merchandiseTurnoverRatio
dowJonesQ[which(inventory==0),"Merchandise Inventory Ratio"]=0

#create average working capital
workingCapital=dowJonesQ[-length(row.names(dowJonesQ)),"Working Capital"]
futureWorkingCapital=dowJonesQ[-1,"Working Capital"]
workingCapitalAverage=(futureWorkingCapital+workingCapital)/2
workingCapitalAverage=c(1,workingCapitalAverage)
dowJonesQ$"Working Capital Average"=workingCapitalAverage
#Working Capital Turnover Ratio
workingCapitalTurnoverRatio=sales/workingCapitalAverage
dowJonesQ[,"Working Capital Turnover Ratio"]=workingCapitalTurnoverRatio
dowJonesQ[which(!is.finite(workingCapitalTurnoverRatio)|is.na(workingCapitalTurnoverRatio)),"Working Capital Turnover Ratio"]=0

#Fixed Asset Turnover Ratio
fixedAssets=dowJonesQ[,"Property, Plant & Equipment Net"]
fixedAssetTurnoverRatio=sales/fixedAssets
dowJonesQ[,"Fixed Asset Turnover Ratio"]=fixedAssetTurnoverRatio
dowJonesQ[which(fixedAssets==0),"Fixed Asset Turnover Ratio"]=0


#c(Degree of Financial Leverage,Equity Multiplier)
#Degree of Financial Leverage-Change in EPS / % Change in EBIT
EPSGrowth=dowJonesQ[,"EPS Growth"]
EBITGrowth=dowJonesQ[,"EBIT Growth"]
EBITGrowth[which(EBITGrowth==0)]=1
degreeOfFinancialLeverage=EPSGrowth/EBITGrowth
dowJonesQ[,"Degree of Financial Leverage"]=degreeOfFinancialLeverage

#Equity Multiplier
totalAssets=dowJonesQ[,"Total Assets"]
netEquity=dowJonesQ[,"Current Assets"]-dowJonesQ[,"Inventory"]-dowJonesQ[,"Total Debt"]
dowJonesQ[,"Equity Multiplier"]=totalAssets/netEquity






#Market Cap - Market Price per Share * total shares outstanding
outstandingShares=dowJonesQ[,"Weighted Average Shares"]
marketCap=price * outstandingShares; dowJonesQ$"Market Capital"=marketCap


#market Cap Type
#small cap < 1.4 billion
dowJonesQ$"Market Cap Type"="Small Cap"
#mid cap > 1.4 billion & mid cap<4.4 billion
dowJonesQ[marketCap>1400000000 & marketCap<4400000000,"Market Cap Type"]="Mid Cap"
#large cap > 4.4 billion
dowJonesQ[marketCap>4400000000,"Market Cap Type"]="large Cap"

#O-Metrix Score - 5*(Dividend Yield + Earning Growth) / PE Ratio : 10+ is ideal
dividendYield=dowJonesQ$"Dividends per Common Share"/price
oMetrix=5*(dividendYield+EPSGrowth)/PE; dowJonesQ$"O-Metrix"=oMetrix

#Altman Z-Score = 1.2A + 1.4B + 3.3C + 0.6D + 1.0E  :  Predicts bankruptcy, 80% of buinesses that went bankrupt in a set period were predicted to do so by this score
#Where:
#A = working capital / total assets
#B = retained earnings / total assets
#C = earnings before interest and tax / total assets
#D = market value of equity / total liabilities
#E = sales / total assets

totalAssets=dowJonesQ$"Total Assets"
totalLiabilities=dowJonesQ$"Total Liabilities"

#A = working capital / total assets, working capital=Current assets - Current liabilities
workingCapital=dowJonesQ$"Current Assets" - dowJonesQ$"Current Liabilities"
A=workingCapital/totalAssets

#B = retained earnings / total assets
retainedEarnings= dowJonesQ$"Accumulated Retained Earnings (Deficit)"
B=retainedEarnings/totalAssets

#C = earnings before interest and tax (EBIT or Operating Income) / total assets
EBIT=dowJonesQ$"EBIT"
C=EBIT/totalAssets

#D = market value of equity (Market Capitalization) / total liabilities, marketCap was initalized as a variable above
D=marketCap/totalLiabilities

#E = sales / total assets
sales=dowJonesQ$"Revenue"
E=sales/totalAssets

altmanZScore = 1.2*A + 1.4*B + 3.3*C + 0.6*D + 1.0*E
dowJonesQ$"Altman Z-Score"=altmanZScore

#Asset Turnover Ratio - revenue/assets : shows how effective one dollar of assets is, this is used in the Altman Z-Score as letter E
dowJonesQ$"Asset Turnover Ratio"=E

dataLength=length(rownames(dowJonesQ))
calculateGrowth=function(statName){
  #addition by .000000001 so there is no NAN and INF due to num/0 or 0/0
  stat=dowJonesQ[-dataLength,statName]+.00000000001
  futureStat=dowJonesQ[-1,statName]+.00000000001
  statChange=futureStat-stat
  statChangePercent=(statChange/stat)*100
  statChangePercent=c(0,statChangePercent)
  dowJonesQ[,paste(statName,"Growth",sep=" ")]=statChangePercent
  
  #calculate stat growth  4 year average
  stat=dowJonesQ[c(-(1:4)),paste(statName,"Growth",sep=" ")]
  stat2=dowJonesQ[c(-(1:3),-dataLength),paste(statName,"Growth",sep=" ")]
  stat3=dowJonesQ[c(-(1:2),-(seq(from=dataLength, to=dataLength-1, by=-1))),paste(statName,"Growth",sep=" ")]
  stat4=dowJonesQ[c(-1,-(seq(from=dataLength, to=dataLength-2, by=-1))),paste(statName,"Growth",sep=" ")]
  #add zeros to make up for the first three years inwhich we are unable to get a four year average
  fourYearAvg=c(0,0,0,0,(stat+stat2+stat3+stat4)/4)
  dowJonesQ[,paste("Four Year Average", statName,"Growth",sep=" ")]=fourYearAvg
  
  futureStat=dowJonesQ[,statName]*(1+(dowJonesQ[,paste("Four Year Average", statName,"Growth",sep=" ")]/100))**20
  futureStat[which(is.infinite(futureStat))]=.000001
  futureStat[which(is.na(futureStat))]=0
  dowJonesQ[,paste("Future 20 Year",statName,sep=" ")]=futureStat
  
  return(dowJonesQ)
}
statList=c("Weighted Average Shares","Long-term debt","Total Assets","Revenue","Net Income","Cost of Revenue","Gross Profit","Research and Development (R&D) Expenses","Selling, General and Administrative (SG&A) Expenses","Operating Expenses","EBITDA","EBIT","Cash and cash equivalents","Investments Current","Cash and short-term investments","Inventory","Current Assets","Goodwill and Intangible Assets","Assets Non-Current","Current Liabilities","Liabilities Non-Current","Total Debt","Total Liabilities","Shareholders Equity","Financing Cash Flow","Investing Cash Flow","Operating Cash Flow","Average Receivables","Average Payables","Average Days of Receivables","Average Days of Payables","Account Payables Turnover","Account Receivables Turnover","Inventory Turnover","Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Cash Ratio","Quick Ratio","Equity Multiplier","Degree of Financial Leverage","ROA","PCF","PE","PB","PS","PEG","FCF per Share","Book Value per Share","ROE","EPS","Gross Margin","EBITDA Margin","EBIT Margin","Profit Margin","Free Cash Flow Margin","Cash per Share","Debt to Equity Ratio","Total Debt To Total Assets","Current Ratio","Income Quality","Payout Ratio","Selling, General and Administrative Expense of Revenue","Research and Development Expense of Revenue","Return on Sales","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Cash Return on Assets","Return on Research Capital","Cash Return On Capital Invested","Return on Net Assets","Cash EPS","Net Asset Value per Share","PEGY","Dividend Payout Ratio","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales","Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash to Current Assets","Cash Flow Coverage","Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Long-term debt to Total Assets","Fixed-Assets to Net Worth Ratio")
for(i in statList){
  dowJonesQ=calculateGrowth(i)
}


#dowJonesQ=read.csv(file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Q.csv", check.names=FALSE)


columns=unlist(lapply(dowJonesQ, is.numeric))  

dowJonesQ=dowJonesQ[,columns]


dowJonesQTitles=c("Index",names(dowJonesQ))


dowJonesQAvg=data.frame(matrix(ncol=length(dowJonesQTitles), nrow=0))
dowJonesQMed=data.frame(matrix(ncol=length(dowJonesQTitles), nrow=0))
for(j in 1:33){
  dowJonesQIndex=dowJonesQ[which(dowJonesQ$Index==j),]
  dowJonesQMedian=c()
  dowJonesQMean=c()
  for(i in 1:length(dowJonesQIndex)){
    indexes=which(!is.na(dowJonesQIndex[,i]))
    dowJonesQMean=c(dowJonesQMean,mean(dowJonesQIndex[indexes,i]))
    dowJonesQMedian=c(dowJonesQMedian,median(dowJonesQIndex[indexes,i]))
  }
  dowJonesQMean=c(j,dowJonesQMean)
  dowJonesQAvg=rbind(dowJonesQAvg,dowJonesQMean)
  
  dowJonesQMedian=c(j,dowJonesQMedian)
  dowJonesQMed=rbind(dowJonesQMed,dowJonesQMedian)
  
}
rownames(dowJonesQAvg)=NULL
names(dowJonesQAvg)=dowJonesQTitles
rownames(dowJonesQMed)=NULL
names(dowJonesQMed)=dowJonesQTitles
head(dowJonesQAvg)

write.csv(dowJonesQ,file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Q.csv",row.names=FALSE)
write.csv(dowJonesQAvg,file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Averages Q.csv",row.names=FALSE)
write.csv(dowJonesQMed,file="C:/Users/Kyle Peters/Desktop/R-Workspace/Dataframes/Dow Jones Median Q.csv",row.names=FALSE)
