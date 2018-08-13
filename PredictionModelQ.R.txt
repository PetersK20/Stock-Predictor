library(xlsx)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(xgboost)
library(infotheo)
library(snowfall)
library(gbm)
#for multiple discriminate analysis
library(MASS)
library(DiscriMiner)
#MARS Model
library(mda)



#vars that are used throughout
growthTrainingSetQ[,"Beat Market"]=as.factor(growthTrainingSetQ[,"Beat Market"])
growthTestSetQ[,"Beat Market"]=as.factor(growthTestSetQ[,"Beat Market"])
beatMarketGrowth=as.factor(growthTrainingSetQ$"Beat Market")



#creates a 3 fold cross validation repeated 10 times
beatMarketGrowth=as.factor(growthTrainingSetQ$"Beat Market")
set.seed(1235)
cv3fold = createMultiFolds(beatMarketGrowth, k = 3, times = 5)
#creates the train control for Caret
trainControlGrowth = trainControl(method = "repeatedcv", number = 3, repeats = 5, index=cv3fold)

growthMyDataQ=rbind(growthTrainingSetQ,growthTestSetQ)
growthTrainrows=1:length(row.names(growthTrainingSetQ))
growthTestrows=(length(row.names(growthTrainingSetQ))+1):length(row.names(growthMyDataQ))

theTarget="Beat Market"

calc_accuracy=function(act,pred){
  rightIndexes=which(act==pred)
  accuracy=(length(rightIndexes)/length(act))*100
  return(accuracy)
}
calc_error <- function(act,pred){
  
  aact <- as.matrix(act)
  
  ppred <- as.matrix(pred)
  
  return (sqrt(colSums(((ppred) - (aact)) ^ 2) / nrow(aact)))
  
}

#factors need to have factor names that are appropriate var names in R
beatMarketCaret=ifelse(beatMarketGrowth==0,"beat0","beat1")
beatMarketCaretFull=ifelse(growthMyDataQ$"Beat Market"==0,"beat0","beat1") 


beatMarketIndexes=which(beatMarketGrowth==1)
numFolds <- trainControl(method = 'cv', number = 10, verboseIter = F, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))


#find out which statistics are most different by using a ttest and using the lowest p values

#ttest if all regular statistics
statList=c("Return on Sales","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings",
           "Cash Flow Return on Investment","Return on Debt","Cash Return on Assets","Return on Research Capital","Cash Return On Capital Invested",
           "Return on Net Assets","Cash EPS","Net Asset Value per Share","PEGY","Dividend Payout Ratio","Cash Turnover","Days Sales Outstanding",
           "Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand",
           "Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income",
           "Goodwill to Assets","Free Cash Flow to Sales","Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets",
           "Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash to Current Assets","Cash Flow Coverage","Cash Flow to Debt","Debt Ratio","Equity Ratio",
           "Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio",
           "Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth",
           "Long-term debt to Equity Ratio","Long-term debt to Total Assets","Fixed-Assets to Net Worth Ratio","Average Days of Receivables",
           "Average Days of Payables","Account Payables Turnover","Account Receivables Turnover","Inventory Turnover","Asset Turnover",
           "Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Cash Ratio","Quick Ratio","Equity Multiplier",
           "Degree of Financial Leverage","ROA","PCF","PE","PB","PS","PEG","FCF per Share","Book Value per Share","ROE","EPS","Gross Margin","EBITDA Margin",
           "EBIT Margin","Profit Margin","Free Cash Flow Margin","Cash per Share","Debt to Equity Ratio","Total Debt To Total Assets","Current Ratio",
           "Income Quality","Payout Ratio","Selling, General and Administrative Expense of Revenue","Research and Development Expense of Revenue")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[beatMarketIndexes,statList[i]],growthTrainingSetQ[-beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:30]]
pvalsLowestCols=statList[order(pvals)[1:15]]
pvalsLowestColsReg=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionRegGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionRegGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionRegGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)

svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionRegSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionRegSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionRegSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionRegMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionRegMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionRegMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionRegGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionRegGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionRegGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionRegNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionRegNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionRegNeural[growthTestrows]))
#69.23% accurate



finaltTestRegPrediction=as.numeric(tTestPredictionRegGLM)+as.numeric(tTestPredictionRegSVM)+
  as.numeric(tTestPredictionRegMARS)+as.numeric(tTestPredictionRegGBM)+as.numeric(tTestPredictionRegNeural)
beatIndexes=which(finaltTestRegPrediction>7.5)
finaltTestRegPrediction[beatIndexes]=2
finaltTestRegPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestRegPrediction[growthTestrows])


#growth ttest
statList=c("Four Year Average Return on Sales Growth","Four Year Average Return on Capital Employed Growth","Four Year Average Operating Profit Margin Growth","Four Year Average Return on Operating Cash Flow Growth","Four Year Average Return on Retained Earnings Growth",
           "Four Year Average Cash Flow Return on Investment Growth","Four Year Average Return on Debt Growth","Four Year Average Cash Return on Assets Growth","Four Year Average Return on Research Capital Growth","Four Year Average Cash Return On Capital Invested Growth",
           "Four Year Average Return on Net Assets Growth","Four Year Average Cash EPS Growth","Four Year Average Net Asset Value per Share Growth","Four Year Average PEGY Growth","Four Year Average Dividend Payout Ratio Growth","Four Year Average Cash Turnover Growth","Four Year Average Days Sales Outstanding Growth",
           "Four Year Average Days Payable Outstanding Growth","Four Year Average Days Inventory Outstanding Growth","Four Year Average Operating Cash Flow to Sales Growth","Four Year Average Days Working Capital Growth","Four Year Average Days Cash on Hand Growth",
           "Four Year Average Sales to Administrative Expenses Growth","Four Year Average Investment Turnover Growth","Four Year Average Sales to Equity Growth","Four Year Average Inventory to Sales Growth","Four Year Average Sales to Operating Income Growth",
           "Four Year Average Goodwill to Assets Growth","Four Year Average Free Cash Flow to Sales Growth","Four Year Average Cash Ratio Growth","Four Year Average Quick Ratio Growth","Four Year Average Cash to Working Capital Growth","Four Year Average Inventory to Working Capital Growth","Four Year Average Sales to Current Assets Growth",
           "Four Year Average Sales to Working Capital Growth","Four Year Average Net Working Capital Ratio Growth","Four Year Average Acid Test Growth","Four Year Average Cash to Current Assets Growth","Four Year Average Cash Flow Coverage Growth","Four Year Average Cash Flow to Debt Growth","Four Year Average Debt Ratio Growth","Four Year Average Equity Ratio Growth",
           "Four Year Average Working Capital to Debt Growth","Four Year Average Current Cash Debt Coverage Growth","Four Year Average Interest Coverage Growth","Four Year Average Asset Coverage Growth","Four Year Average Interest Expense to Debt Growth","Four Year Average Capitalization Ratio Growth",
           "Four Year Average Debt to EBITDA Growth","Four Year Average Long-term debt ratio Growth","Four Year Average Net Debt to EBITDA Growth","Four Year Average Cash Flow Coverage Growth","Four Year Average Financial Leverage Index Growth","Four Year Average Non-Current Asset to Net Worth Growth",
           "Four Year Average Long-term debt to Equity Ratio Growth","Four Year Average Long-term debt to Total Assets Growth","Four Year Average Fixed-Assets to Net Worth Ratio Growth","Four Year Average Average Days of Receivables Growth","Four Year Average Average Days of Payables Growth","Four Year Average Account Payables Turnover Growth",
           "Four Year Average Account Receivables Turnover Growth","Four Year Average Inventory Turnover Growth","Four Year Average Asset Turnover Growth","Four Year Average Merchandise Inventory Ratio Growth","Four Year Average Working Capital Turnover Ratio Growth","Four Year Average Fixed Asset Turnover Ratio Growth","Four Year Average Cash Ratio Growth",
           "Four Year Average Quick Ratio Growth","Four Year Average Equity Multiplier Growth","Four Year Average Degree of Financial Leverage Growth","Four Year Average ROA Growth","Four Year Average PCF Growth","Four Year Average PE Growth","Four Year Average PB Growth","Four Year Average PS Growth","Four Year Average PEG Growth","Four Year Average FCF per Share Growth",
           "Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average EBITDA Margin Growth","Four Year Average EBIT Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth",
           "Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Research and Development Expense of Revenue Growth",
           "Four Year Average Weighted Average Shares Growth","Four Year Average Long-term debt Growth","Four Year Average Total Assets Growth","Four Year Average Revenue Growth","Four Year Average Net Income Growth","Four Year Average Cost of Revenue Growth","Four Year Average Gross Profit Growth","Four Year Average Research and Development (R&D) Expenses Growth","Four Year Average Selling, General and Administrative (SG&A) Expenses Growth",
           "Four Year Average Operating Expenses Growth","Four Year Average EBITDA Growth","Four Year Average EBIT Growth","Four Year Average Cash and cash equivalents Growth","Four Year Average Investments Current Growth","Four Year Average Cash and short-term investments Growth","Four Year Average Inventory Growth","Four Year Average Current Assets Growth","Four Year Average Goodwill and Intangible Assets Growth",
           "Four Year Average Assets Non-Current Growth","Four Year Average Current Liabilities Growth","Four Year Average Liabilities Non-Current Growth","Four Year Average Total Debt Growth","Four Year Average Total Liabilities Growth","Four Year Average Shareholders Equity Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Average Receivables Growth","Average Payables Growth")


pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[beatMarketIndexes,statList[i]],growthTrainingSetQ[-beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:30]]
pvalsLowestCols=statList[order(pvals)[1:15]]
pvalsLowestColsGrowth=pvalsLowestCols

tTestTrain=growthTrainingSetQ[,pvalsLowestCols]

#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionGrowthGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionGrowthGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionGrowthGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionGrowthSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionGrowthSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionGrowthSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionGrowthMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionGrowthMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionGrowthMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionGrowthGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionGrowthGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionGrowthGBM[growthTestrows]))
#58.97436% predictive


#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionGrowthNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionGrowthNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionGrowthNeural[growthTestrows]))
#69.23% accurate


finaltTestGrowthPrediction=as.numeric(tTestPredictionGrowthGLM)+as.numeric(tTestPredictionGrowthSVM)+
  as.numeric(tTestPredictionGrowthMARS)+as.numeric(tTestPredictionGrowthGBM)+as.numeric(tTestPredictionGrowthNeural)

beatIndexes=which(finaltTestGrowthPrediction>7.5)
finaltTestGrowthPrediction[beatIndexes]=2
finaltTestGrowthPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestGrowthPrediction[growthTestrows])










#PE ttest
statList=c("Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Return on Retained Earnings to PE",
           "Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Cash Return On Capital Invested to PE",
           "Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","PEGY to PE","Dividend Payout Ratio to PE","Cash Turnover to PE","Days Sales Outstanding to PE",
           "Days Payable Outstanding to PE","Days Inventory Outstanding to PE","Operating Cash Flow to Sales to PE","Days Working Capital to PE","Days Cash on Hand to PE",
           "Sales to Administrative Expenses to PE","Investment Turnover to PE","Sales to Equity to PE","Inventory to Sales to PE","Sales to Operating Income to PE",
           "Goodwill to Assets to PE","Free Cash Flow to Sales to PE","Cash Ratio to PE","Quick Ratio to PE","Cash to Working Capital to PE","Inventory to Working Capital to PE","Sales to Current Assets to PE",
           "Sales to Working Capital to PE","Net Working Capital Ratio to PE","Acid Test to PE","Cash to Current Assets to PE","Cash Flow Coverage to PE","Cash Flow to Debt to PE","Debt Ratio to PE","Equity Ratio to PE",
           "Working Capital to Debt to PE","Current Cash Debt Coverage to PE","Interest Coverage to PE","Asset Coverage to PE","Interest Expense to Debt to PE","Capitalization Ratio to PE",
           "Debt to EBITDA to PE","Long-term debt ratio to PE","Net Debt to EBITDA to PE","Cash Flow Coverage to PE","Financial Leverage Index to PE","Non-Current Asset to Net Worth to PE",
           "Long-term debt to Equity Ratio to PE","Long-term debt to Total Assets to PE","Fixed-Assets to Net Worth Ratio to PE","Average Days of Receivables to PE","Average Days of Payables to PE","Account Payables Turnover to PE",
           "Account Receivables Turnover to PE","Inventory Turnover to PE","Asset Turnover to PE","Merchandise Inventory Ratio to PE","Working Capital Turnover Ratio to PE","Fixed Asset Turnover Ratio to PE",
           "Cash Ratio to PE","Quick Ratio to PE","Equity Multiplier to PE","Degree of Financial Leverage to PE","ROA to PE","PCF to PE","PE to PE","PB to PE","PS to PE","PEG to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE",
           "Gross Margin to PE","EBITDA Margin to PE","EBIT Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE",
           "Current Ratio to PE","Income Quality to PE","Payout Ratio to PE","Selling, General and Administrative Expense of Revenue to PE","Research and Development Expense of Revenue to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:30]]
pvalsLowestCols=statList[order(pvals)[1:20]]
pvalsLowestColsPE=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]

#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionPEGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionPEGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionPEGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionPESVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionPESVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionPESVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionPEMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionPEMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionPEMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionPEGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionPEGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionPEGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionPENeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionPENeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionPENeural[growthTestrows]))
#69.23% accurate


finaltTestPEPrediction=as.numeric(tTestPredictionPEGLM)+as.numeric(tTestPredictionPESVM)+
  as.numeric(tTestPredictionPEMARS)+as.numeric(tTestPredictionPEGBM)+as.numeric(tTestPredictionPENeural)
beatIndexes=which(finaltTestPEPrediction>7.5)
finaltTestPEPrediction[beatIndexes]=2
finaltTestPEPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestPEPrediction[growthTestrows])



#future 20 year ttest
statList=c("Future 20 Year Return on Sales","Future 20 Year Return on Capital Employed","Future 20 Year Operating Profit Margin","Future 20 Year Return on Operating Cash Flow","Future 20 Year Return on Retained Earnings",
           "Future 20 Year Cash Flow Return on Investment","Future 20 Year Return on Debt","Future 20 Year Cash Return on Assets","Future 20 Year Return on Research Capital","Future 20 Year Cash Return On Capital Invested",
           "Future 20 Year Return on Net Assets","Future 20 Year Cash EPS","Future 20 Year Net Asset Value per Share","Future 20 Year PEGY","Future 20 Year Dividend Payout Ratio","Future 20 Year Cash Turnover","Future 20 Year Days Sales Outstanding",
           "Future 20 Year Days Payable Outstanding","Future 20 Year Days Inventory Outstanding","Future 20 Year Operating Cash Flow to Sales","Future 20 Year Days Working Capital","Future 20 Year Days Cash on Hand",
           "Future 20 Year Sales to Administrative Expenses","Future 20 Year Investment Turnover","Future 20 Year Sales to Equity","Future 20 Year Inventory to Sales","Future 20 Year Sales to Operating Income",
           "Future 20 Year Goodwill to Assets","Future 20 Year Free Cash Flow to Sales","Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Cash to Working Capital","Future 20 Year Inventory to Working Capital","Future 20 Year Sales to Current Assets",
           "Future 20 Year Sales to Working Capital","Future 20 Year Net Working Capital Ratio","Future 20 Year Acid Test","Future 20 Year Cash to Current Assets","Future 20 Year Cash Flow Coverage","Future 20 Year Cash Flow to Debt","Future 20 Year Debt Ratio","Future 20 Year Equity Ratio",
           "Future 20 Year Working Capital to Debt","Future 20 Year Current Cash Debt Coverage","Future 20 Year Interest Coverage","Future 20 Year Asset Coverage","Future 20 Year Interest Expense to Debt","Future 20 Year Capitalization Ratio",
           "Future 20 Year Debt to EBITDA","Future 20 Year Long-term debt ratio","Future 20 Year Net Debt to EBITDA","Future 20 Year Cash Flow Coverage","Future 20 Year Financial Leverage Index","Future 20 Year Non-Current Asset to Net Worth",
           "Future 20 Year Long-term debt to Equity Ratio","Future 20 Year Long-term debt to Total Assets","Future 20 Year Fixed-Assets to Net Worth Ratio","Future 20 Year Average Days of Receivables",
           "Future 20 Year Average Days of Payables","Future 20 Year Account Payables Turnover","Future 20 Year Account Receivables Turnover","Future 20 Year Inventory Turnover","Future 20 Year Asset Turnover",
           "Future 20 Year Merchandise Inventory Ratio","Future 20 Year Working Capital Turnover Ratio","Future 20 Year Fixed Asset Turnover Ratio","Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Equity Multiplier",
           "Future 20 Year Degree of Financial Leverage","Future 20 Year ROA","Future 20 Year PCF","Future 20 Year PE","Future 20 Year PB","Future 20 Year PS","Future 20 Year PEG","Future 20 Year FCF per Share","Future 20 Year Book Value per Share","Future 20 Year ROE","Future 20 Year EPS","Future 20 Year Gross Margin","Future 20 Year EBITDA Margin",
           "Future 20 Year EBIT Margin","Future 20 Year Profit Margin","Future 20 Year Free Cash Flow Margin","Future 20 Year Cash per Share","Future 20 Year Debt to Equity Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Current Ratio",
           "Future 20 Year Income Quality","Future 20 Year Payout Ratio","Future 20 Year Selling, General and Administrative Expense of Revenue","Future 20 Year Research and Development Expense of Revenue")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[beatMarketIndexes,statList[i]],growthTrainingSetQ[-beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:93]]
pvalsLowestCols=statList[order(pvals)[1:25]]
pvalsLowestColsFuture=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionFutureGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionFutureGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionFutureGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionFutureSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionFutureSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionFutureSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionFutureMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionFutureMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionFutureMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionFutureGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionFutureGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionFutureGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionFutureNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionFutureNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionFutureNeural[growthTestrows]))
#69.23% accurate



finaltTestFuturePrediction=as.numeric(tTestPredictionFutureGLM)+as.numeric(tTestPredictionFutureSVM)+
  as.numeric(tTestPredictionFutureMARS)+as.numeric(tTestPredictionFutureGBM)+as.numeric(tTestPredictionFutureNeural)
beatIndexes=which(finaltTestFuturePrediction>7.5)
finaltTestFuturePrediction[beatIndexes]=2
finaltTestFuturePrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestFuturePrediction[growthTestrows])



#market average ttest
statList=c("Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Return on Retained Earnings to Market Average",
           "Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Cash Return On Capital Invested to Market Average",
           "Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","PEGY to Market Average","Dividend Payout Ratio to Market Average","Cash Turnover to Market Average","Days Sales Outstanding to Market Average",
           "Days Payable Outstanding to Market Average","Days Inventory Outstanding to Market Average","Operating Cash Flow to Sales to Market Average","Days Working Capital to Market Average","Days Cash on Hand to Market Average",
           "Sales to Administrative Expenses to Market Average","Investment Turnover to Market Average","Sales to Equity to Market Average","Inventory to Sales to Market Average","Sales to Operating Income to Market Average",
           "Goodwill to Assets to Market Average","Free Cash Flow to Sales to Market Average","Cash Ratio to Market Average","Quick Ratio to Market Average","Cash to Working Capital to Market Average","Inventory to Working Capital to Market Average","Sales to Current Assets to Market Average",
           "Sales to Working Capital to Market Average","Net Working Capital Ratio to Market Average","Acid Test to Market Average","Cash to Current Assets to Market Average","Cash Flow Coverage to Market Average","Cash Flow to Debt to Market Average","Debt Ratio to Market Average","Equity Ratio to Market Average",
           "Working Capital to Debt to Market Average","Current Cash Debt Coverage to Market Average","Interest Coverage to Market Average","Asset Coverage to Market Average","Interest Expense to Debt to Market Average","Capitalization Ratio to Market Average",
           "Debt to EBITDA to Market Average","Long-term debt ratio to Market Average","Net Debt to EBITDA to Market Average","Cash Flow Coverage to Market Average","Financial Leverage Index to Market Average","Non-Current Asset to Net Worth to Market Average",
           "Long-term debt to Equity Ratio to Market Average","Long-term debt to Total Assets to Market Average","Fixed-Assets to Net Worth Ratio to Market Average","Average Days of Receivables to Market Average","Average Days of Payables to Market Average","Account Payables Turnover to Market Average",
           "Account Receivables Turnover to Market Average","Inventory Turnover to Market Average","Asset Turnover to Market Average","Merchandise Inventory Ratio to Market Average","Working Capital Turnover Ratio to Market Average","Fixed Asset Turnover Ratio to Market Average",
           "Cash Ratio to Market Average","Quick Ratio to Market Average","Equity Multiplier to Market Average","Degree of Financial Leverage to Market Average","ROA to Market Average","PCF to Market Average","PE to Market Average","PB to Market Average","PS to Market Average","PEG to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average",
           "Gross Margin to Market Average","EBITDA Margin to Market Average","EBIT Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average",
           "Current Ratio to Market Average","Income Quality to Market Average","Payout Ratio to Market Average","Selling, General and Administrative Expense of Revenue to Market Average","Research and Development Expense of Revenue to Market Average")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:30]]
pvalsLowestCols=statList[order(pvals)[1:15]]
pvalsLowestColsMarket=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionMarketGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionMarketGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionMarketGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionMarketSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionMarketSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionMarketSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionMarketMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionMarketMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionMarketMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionMarketGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionMarketGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionMarketGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionMarketNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionMarketNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionMarketNeural[growthTestrows]))
#69.23% accurate

finaltTestMarketPrediction=as.numeric(tTestPredictionMarketGLM)+as.numeric(tTestPredictionMarketSVM)+
  as.numeric(tTestPredictionMarketMARS)+as.numeric(tTestPredictionMarketGBM)+as.numeric(tTestPredictionMarketNeural)
beatIndexes=which(finaltTestMarketPrediction>7.5)
finaltTestMarketPrediction[beatIndexes]=2
finaltTestMarketPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestMarketPrediction[growthTestrows])





#category ttest
statList=c("Category Return on Sales","Category Return on Capital Employed","Category Operating Profit Margin","Category Return on Operating Cash Flow","Category Return on Retained Earnings",
           "Category Cash Flow Return on Investment","Category Return on Debt","Category Cash Return on Assets","Category Return on Research Capital","Category Cash Return On Capital Invested",
           "Category Return on Net Assets","Category Cash EPS","Category Net Asset Value per Share","Category PEGY","Category Dividend Payout Ratio","Category Cash Turnover","Category Days Sales Outstanding",
           "Category Days Payable Outstanding","Category Days Inventory Outstanding","Category Operating Cash Flow to Sales","Category Days Working Capital","Category Days Cash on Hand",
           "Category Sales to Administrative Expenses","Category Investment Turnover","Category Sales to Equity","Category Inventory to Sales","Category Sales to Operating Income",
           "Category Goodwill to Assets","Category Free Cash Flow to Sales","Category Cash Ratio","Category Quick Ratio","Category Cash to Working Capital","Category Inventory to Working Capital","Category Sales to Current Assets",
           "Category Sales to Working Capital","Category Net Working Capital Ratio","Category Acid Test","Category Cash to Current Assets","Category Cash Flow Coverage","Category Cash Flow to Debt","Category Debt Ratio","Category Equity Ratio",
           "Category Working Capital to Debt","Category Current Cash Debt Coverage","Category Interest Coverage","Category Asset Coverage","Category Interest Expense to Debt","Category Capitalization Ratio",
           "Category Debt to EBITDA","Category Long-term debt ratio","Category Net Debt to EBITDA","Category Cash Flow Coverage","Category Financial Leverage Index","Category Non-Current Asset to Net Worth",
           "Category Long-term debt to Equity Ratio","Category Long-term debt to Total Assets","Category Fixed-Assets to Net Worth Ratio","Category Average Days of Receivables",
           "Category Average Days of Payables","Category Account Payables Turnover","Category Account Receivables Turnover","Category Inventory Turnover","Category Asset Turnover",
           "Category Merchandise Inventory Ratio","Category Working Capital Turnover Ratio","Category Fixed Asset Turnover Ratio","Category Cash Ratio","Category Quick Ratio","Category Equity Multiplier",
           "Category Degree of Financial Leverage","Category ROA","Category PCF","Category PE","Category PB","Category PS","Category PEG","Category FCF per Share","Category Book Value per Share","Category ROE","Category EPS","Category Gross Margin","Category EBITDA Margin",
           "Category EBIT Margin","Category Profit Margin","Category Free Cash Flow Margin","Category Cash per Share","Category Debt to Equity Ratio","Category Total Debt To Total Assets","Category Current Ratio",
           "Category Income Quality","Category Payout Ratio","Category Selling, General and Administrative Expense of Revenue","Category Research and Development Expense of Revenue")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:30]]
pvalsLowestCols=statList[order(pvals)[1:30]]
pvalsLowestColsCategory=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionCategoryGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionCategoryGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionCategoryGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionCategorySVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionCategorySVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionCategorySVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionCategoryMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionCategoryMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionCategoryMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionCategoryGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionCategoryGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionCategoryGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionCategoryNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionCategoryNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionCategoryNeural[growthTestrows]))
#69.23% accurate

finaltTestCategoryPrediction=as.numeric(tTestPredictionCategoryGLM)+as.numeric(tTestPredictionCategorySVM)+
  as.numeric(tTestPredictionCategoryMARS)+as.numeric(tTestPredictionCategoryGBM)+as.numeric(tTestPredictionCategoryNeural)
beatIndexes=which(finaltTestCategoryPrediction>7.5)
finaltTestCategoryPrediction[beatIndexes]=2
finaltTestCategoryPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestCategoryPrediction[growthTestrows])





#Profit ttest
statList=c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Cash Return on Assets","Return on Research Capital","Cash Return On Capital Invested")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:6]]
pvalsLowestCols=statList[order(pvals)[1:4]]

statList=c("Future 20 Year Gross Margin","Future 20 Year ROA","Future 20 Year ROE","Future 20 Year Profit Margin","Future 20 Year Free Cash Flow Margin","Future 20 Year Return on Capital Employed","Future 20 Year Operating Profit Margin","Future 20 Year Return on Operating Cash Flow","Future 20 Year Return on Retained Earnings","Future 20 Year Cash Flow Return on Investment","Future 20 Year Return on Debt","Future 20 Year Cash Return on Assets","Future 20 Year Return on Research Capital","Future 20 Year Cash Return On Capital Invested")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])



statList=c("Gross Margin to PE","ROA to PE","ROE to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Return on Retained Earnings to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Cash Return On Capital Invested to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])



statList=c("Four Year Average Gross Margin Growth","Four Year Average ROA Growth","Four Year Average ROE Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Return on Capital Employed Growth","Four Year Average Operating Profit Margin Growth","Four Year Average Return on Operating Cash Flow Growth","Four Year Average Return on Retained Earnings Growth","Four Year Average Cash Flow Return on Investment Growth","Four Year Average Return on Debt Growth","Four Year Average Cash Return on Assets Growth","Four Year Average Return on Research Capital Growth","Four Year Average Cash Return On Capital Invested Growth")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
pvalsLowestColsProfit=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionProfitGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionProfitGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionProfitGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionProfitSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionProfitSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionProfitSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionProfitMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionProfitMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionProfitMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionProfitGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionProfitGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionProfitGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionProfitNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionProfitNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionProfitNeural[growthTestrows]))
#69.23% accurate

finaltTestProfitPrediction=as.numeric(tTestPredictionProfitGLM)+as.numeric(tTestPredictionProfitSVM)+
  as.numeric(tTestPredictionProfitMARS)+as.numeric(tTestPredictionProfitGBM)+as.numeric(tTestPredictionProfitNeural)
beatIndexes=which(finaltTestProfitPrediction>7.5)
finaltTestProfitPrediction[beatIndexes]=2
finaltTestProfitPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestProfitPrediction[growthTestrows])



#Value ttest
statList=c("PCF","PE","PS","PB","PEG","PEGY","EPS","Cash EPS","Net Asset Value per Share","Dividend Payout Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:6]]
pvalsLowestCols=statList[order(pvals)[1:4]]

statList=c("Future 20 Year PCF","Future 20 Year PE","Future 20 Year PS","Future 20 Year PB","Future 20 Year PEG","Future 20 Year PEGY","Future 20 Year EPS","Future 20 Year Cash EPS","Future 20 Year Net Asset Value per Share","Future 20 Year Dividend Payout Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])

statList=c("PCF to PE","PS to PE","PB to PE","PEG to PE","PEGY to PE","EPS to PE","Cash EPS to PE","Net Asset Value per Share to PE","Dividend Payout Ratio to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])


statList=c("Four Year Average PCF Growth","Four Year Average PS Growth","Four Year Average PB Growth","Four Year Average PEG Growth","Four Year Average PEGY Growth","Four Year Average EPS Growth","Four Year Average Cash EPS Growth","Four Year Average Net Asset Value per Share Growth","Four Year Average Dividend Payout Ratio Growth")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
pvalsLowestColsValue=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestColsValue]



#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionValueGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionValueGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionValueGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionValueSVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionValueSVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionValueSVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionValueMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionValueMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionValueMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionValueGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionValueGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionValueGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionValueNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionValueNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionValueNeural[growthTestrows]))
#69.23% accurate


finaltTestValuePrediction=as.numeric(tTestPredictionValueGLM)+as.numeric(tTestPredictionValueSVM)+
  as.numeric(tTestPredictionValueMARS)+as.numeric(tTestPredictionValueGBM)+as.numeric(tTestPredictionValueNeural)
beatIndexes=which(finaltTestValuePrediction>7.5)
finaltTestValuePrediction[beatIndexes]=2
finaltTestValuePrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestValuePrediction[growthTestrows])




#Solvency ttest
statList=c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:6]]
pvalsLowestCols=statList[order(pvals)[1:4]]

statList=c("Future 20 Year Cash Flow to Debt","Future 20 Year Debt Ratio","Future 20 Year Equity Ratio","Future 20 Year Working Capital to Debt","Future 20 Year Current Cash Debt Coverage","Future 20 Year Interest Coverage","Future 20 Year Asset Coverage","Future 20 Year Interest Expense to Debt","Future 20 Year Capitalization Ratio","Future 20 Year Debt to EBITDA","Future 20 Year Long-term debt ratio","Future 20 Year Net Debt to EBITDA","Future 20 Year Cash Flow Coverage","Future 20 Year Financial Leverage Index","Future 20 Year Non-Current Asset to Net Worth","Future 20 Year Long-term debt to Equity Ratio","Future 20 Year Fixed-Assets to Net Worth Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Debt to Equity Ratio","Future 20 Year Current Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])


statList=c("Cash Flow to Debt to PE","Debt Ratio to PE","Equity Ratio to PE","Working Capital to Debt to PE","Current Cash Debt Coverage to PE","Interest Coverage to PE","Asset Coverage to PE","Interest Expense to Debt to PE","Capitalization Ratio to PE","Debt to EBITDA to PE","Long-term debt ratio to PE","Net Debt to EBITDA to PE","Cash Flow Coverage to PE","Financial Leverage Index to PE","Non-Current Asset to Net Worth to PE","Long-term debt to Equity Ratio to PE","Fixed-Assets to Net Worth Ratio to PE","Total Debt To Total Assets to PE","Debt to Equity Ratio to PE","Current Ratio to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])



statList=c("Four Year Average Cash Flow to Debt Growth","Four Year Average Debt Ratio Growth","Four Year Average Equity Ratio Growth","Four Year Average Working Capital to Debt Growth","Four Year Average Current Cash Debt Coverage Growth","Four Year Average Interest Coverage Growth","Four Year Average Asset Coverage Growth","Four Year Average Interest Expense to Debt Growth","Four Year Average Capitalization Ratio Growth","Four Year Average Debt to EBITDA Growth","Four Year Average Long-term debt ratio Growth","Four Year Average Net Debt to EBITDA Growth","Four Year Average Cash Flow Coverage Growth","Four Year Average Financial Leverage Index Growth","Four Year Average Non-Current Asset to Net Worth Growth","Four Year Average Long-term debt to Equity Ratio Growth","Four Year Average Fixed-Assets to Net Worth Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Current Ratio Growth")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
pvalsLowestColsSolvency=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestColsSolvency]




#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionSolvencyGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionSolvencyGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionSolvencyGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionSolvencySVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionSolvencySVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionSolvencySVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionSolvencyMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionSolvencyMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionSolvencyMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionSolvencyGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionSolvencyGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionSolvencyGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionSolvencyNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionSolvencyNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionSolvencyNeural[growthTestrows]))
#69.23% accurate


finaltTestSolvencyPrediction=as.numeric(tTestPredictionSolvencyGLM)+as.numeric(tTestPredictionSolvencySVM)+
  as.numeric(tTestPredictionSolvencyMARS)+as.numeric(tTestPredictionSolvencyGBM)+as.numeric(tTestPredictionSolvencyNeural)
beatIndexes=which(finaltTestSolvencyPrediction>7.5)
finaltTestSolvencyPrediction[beatIndexes]=2
finaltTestSolvencyPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestSolvencyPrediction[growthTestrows])





#Liquidity ttest
statList=c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:6]]
pvalsLowestCols=statList[order(pvals)[1:4]]

statList=c("Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Cash to Working Capital","Future 20 Year Inventory to Working Capital","Future 20 Year Sales to Current Assets","Future 20 Year Sales to Working Capital","Future 20 Year Net Working Capital Ratio","Future 20 Year Acid Test","Future 20 Year Cash Flow Coverage","Future 20 Year Current Ratio")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


statList=c("Cash Ratio to PE","Quick Ratio to PE","Cash to Working Capital to PE","Inventory to Working Capital to PE","Sales to Current Assets to PE","Sales to Working Capital to PE","Net Working Capital Ratio to PE","Acid Test to PE","Cash Flow Coverage to PE","Current Ratio to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


statList=c("Four Year Average Cash Ratio Growth","Four Year Average Quick Ratio Growth","Four Year Average Cash to Working Capital Growth","Four Year Average Inventory to Working Capital Growth","Four Year Average Sales to Current Assets Growth","Four Year Average Sales to Working Capital Growth","Four Year Average Net Working Capital Ratio Growth","Four Year Average Acid Test Growth","Four Year Average Cash Flow Coverage Growth","Four Year Average Current Ratio Growth")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
pvalsLowestColsLiquidity=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]




#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionLiquidityGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionLiquidityGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionLiquidityGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionLiquiditySVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionLiquiditySVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionLiquiditySVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionLiquidityMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionLiquidityMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionLiquidityMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionLiquidityGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionLiquidityGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionLiquidityGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionLiquidityNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionLiquidityNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionLiquidityNeural[growthTestrows]))
#69.23% accurate


finaltTestLiquidityPrediction=as.numeric(tTestPredictionLiquidityGLM)+as.numeric(tTestPredictionLiquiditySVM)+
  as.numeric(tTestPredictionLiquidityMARS)+as.numeric(tTestPredictionLiquidityGBM)+as.numeric(tTestPredictionLiquidityNeural)
beatIndexes=which(finaltTestLiquidityPrediction>7.5)
finaltTestLiquidityPrediction[beatIndexes]=2
finaltTestLiquidityPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestLiquidityPrediction[growthTestrows])


#Activity ttest
statList=c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}
pvals[order(pvals)[1:6]]
pvalsLowestCols=statList[order(pvals)[1:4]]

statList=c("Future 20 Year Asset Turnover","Future 20 Year Merchandise Inventory Ratio","Future 20 Year Working Capital Turnover Ratio","Future 20 Year Fixed Asset Turnover Ratio","Future 20 Year Account Receivables Turnover","Future 20 Year Account Payables Turnover","Future 20 Year Inventory Turnover","Future 20 Year Average Days of Payables","Future 20 Year Average Days of Receivables","Future 20 Year Inventory Turnover","Future 20 Year Cash Turnover","Future 20 Year Days Sales Outstanding","Future 20 Year Days Payable Outstanding","Future 20 Year Days Inventory Outstanding","Future 20 Year Operating Cash Flow to Sales","Future 20 Year Days Working Capital","Future 20 Year Days Cash on Hand","Future 20 Year Sales to Administrative Expenses","Future 20 Year Investment Turnover","Future 20 Year Sales to Equity","Future 20 Year Inventory to Sales","Future 20 Year Sales to Operating Income","Future 20 Year Goodwill to Assets","Future 20 Year Free Cash Flow to Sales")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


statList=c("Asset Turnover to PE","Merchandise Inventory Ratio to PE","Working Capital Turnover Ratio to PE","Fixed Asset Turnover Ratio to PE","Account Receivables Turnover to PE","Account Payables Turnover to PE","Inventory Turnover to PE","Average Days of Payables to PE","Average Days of Receivables to PE","Inventory Turnover to PE","Cash Turnover to PE","Days Sales Outstanding to PE","Days Payable Outstanding to PE","Days Inventory Outstanding to PE","Operating Cash Flow to Sales to PE","Days Working Capital to PE","Days Cash on Hand to PE","Sales to Administrative Expenses to PE","Investment Turnover to PE","Sales to Equity to PE","Inventory to Sales to PE","Sales to Operating Income to PE","Goodwill to Assets to PE","Free Cash Flow to Sales to PE")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]


statList=c("Four Year Average Asset Turnover Growth","Four Year Average Merchandise Inventory Ratio Growth","Four Year Average Working Capital Turnover Ratio Growth","Four Year Average Fixed Asset Turnover Ratio Growth","Four Year Average Account Receivables Turnover Growth","Four Year Average Account Payables Turnover Growth","Four Year Average Inventory Turnover Growth","Four Year Average Average Days of Payables Growth","Four Year Average Average Days of Receivables Growth","Four Year Average Inventory Turnover Growth","Four Year Average Cash Turnover Growth","Four Year Average Days Sales Outstanding Growth","Four Year Average Days Payable Outstanding Growth","Four Year Average Days Inventory Outstanding Growth","Four Year Average Operating Cash Flow to Sales Growth","Four Year Average Days Working Capital Growth","Four Year Average Days Cash on Hand Growth","Four Year Average Sales to Administrative Expenses Growth","Four Year Average Investment Turnover Growth","Four Year Average Sales to Equity Growth","Four Year Average Inventory to Sales Growth","Four Year Average Sales to Operating Income Growth","Four Year Average Goodwill to Assets Growth","Four Year Average Free Cash Flow to Sales Growth")
pvals=c()
for(i in 1:length(statList)){
  t.test=t.test(growthTrainingSetQ[-beatMarketIndexes,statList[i]],growthTrainingSetQ[beatMarketIndexes,statList[i]])
  pval=t.test$p.value
  pvals=c(pvals,pval)
}

pvals[order(pvals)[1:6]]
pvalsLowestCols=c(pvalsLowestCols,statList[order(pvals)[1:4]])
pvalsLowestColsActivity=pvalsLowestCols
tTestTrain=growthTrainingSetQ[,pvalsLowestCols]




#glm model
fit<-train(x=tTestTrain,y=beatMarketCaret,method = "glm")
tTestTrainGlm=growthMyDataQ[,pvalsLowestCols]
tTestPredictionActivityGLM=predict(fit,tTestTrainGlm)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionActivityGLM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionActivityGLM[growthTestrows]))
#61.53846% accurate

#svm model
set.seed(1235)
svmLinearModel <- train(x=tTestTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,pvalsLowestCols]
tTestPredictionActivitySVM=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionActivitySVM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionActivitySVM[growthTestrows]))
#74.35897% accurate

#mars model
mars.fit <- mars(x=tTestTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,pvalsLowestCols]
marsPrediction=predict(mars.fit,marsTrain)
tTestPredictionActivityMARS=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionActivityMARS[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionActivityMARS[growthTestrows]))
#43.58974% accuracy

#gbm model
set.seed(1235)
GBDecisionTree = train(x = tTestTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionActivityGBM<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(tTestPredictionActivityGBM[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(tTestPredictionActivityGBM[growthTestrows]))
#58.97436% predictive

#neural network
set.seed(1235)
neuralNetworkModel<- train(x=tTestTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTrain = growthMyDataQ[,pvalsLowestCols]
tTestPredictionActivityNeural <- predict(neuralNetworkModel, newdata=neuralNetTrain)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(tTestPredictionActivityNeural[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(tTestPredictionActivityNeural[growthTestrows]))
#69.23% accurate


finaltTestActivityPrediction=as.numeric(tTestPredictionActivityGLM)+as.numeric(tTestPredictionActivitySVM)+
  as.numeric(tTestPredictionActivityMARS)+as.numeric(tTestPredictionActivityGBM)+as.numeric(tTestPredictionActivityNeural)
beatIndexes=which(finaltTestActivityPrediction>7.5)
finaltTestActivityPrediction[beatIndexes]=2
finaltTestActivityPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finaltTestActivityPrediction[growthTestrows])









#stat Future 20 year
#stepwise regression
stepwiseData = growthTrainingSetQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')

stepwiseData=growthMyDataQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
stepWisePrediction=predict(step,stepwiseData)

stepWisePredictionFuture=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionFuture[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionFuture[growthTestrows]))
#74.35897% accurate

#svm model
svmTrain=growthTrainingSetQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
set.seed(1235)
svmLinearModel <- train(x=svmTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
svmPredictionFuture=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionFuture[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionFuture[growthTestrows]))
#74.35897% accurate

#mars model
marsTrain=growthTrainingSetQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
mars.fit <- mars(x=marsTrain,y=beatMarketGrowth, degree = 1, prune = TRUE, forward.step = TRUE)
marsTrain=growthMyDataQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
marsPrediction=predict(mars.fit,marsTrain)
marsPredictionFuture=ifelse(marsPrediction > .5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(marsPredictionFuture[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(marsPredictionFuture[growthTestrows]))
#43.58974% accuracy


#gbm model
gbmTrain = growthTrainingSetQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables","Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables","Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
set.seed(1235)
GBDecisionTree = train(x = gbmTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth,verbose=F)
GBDecisionTree
gbmTrain = growthMyDataQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")]
gbmPredictionFuture<- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmPredictionFuture[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmPredictionFuture[growthTestrows]))
#58.97436% predictive




#stat to market average
#stepwise regression
stepwiseData = growthTrainingSetQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionMarketAverage=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionMarketAverage[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionMarketAverage[growthTestrows]))
#74.35897% accurate



#svm model
svmTrain=growthTrainingSetQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
set.seed(1235)
svmLinearModel <- train(x=svmTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
svmPredictionMarketAverage=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionMarketAverage[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionMarketAverage[growthTestrows]))
#71.79487% accurate 



#gbm model
gbmTrain = growthTrainingSetQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
set.seed(1235)
GBDecisionTree = train(x = gbmTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree
gbmTrain = growthMyDataQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","PE to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
gbmPredictionMarketAverage <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmPredictionMarketAverage[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmPredictionMarketAverage[growthTestrows]))
#58.97436% predictive



#use feature of stat to pe
stepwiseData = growthTrainingSetQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionPE=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionPE[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionPE[growthTestrows]))
#71.79487% accurate but test set has 57.5% accurate


#svm model
svmTrain=growthTrainingSetQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
set.seed(1235)
svmLinearModel <- train(x=svmTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=growthMyDataQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
svmPredictionPE=predict(svmLinearModel,svmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionPE[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionPE[growthTestrows]))
#74.35897% accurate

#gbm model
gbmTrain = growthTrainingSetQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
set.seed(1235)
GBDecisionTree = train(x = gbmTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree
gbmTrain = growthMyDataQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
gbmPredictionPE <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmPredictionPE[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmPredictionPE[growthTestrows]))






#let's create a models based on the five financial categories - profit,value,liquidity,solvency,growth

#profitability regression
stepwiseData = growthTrainingSetQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionProfit=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionProfit[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionProfit[growthTestrows]))



#valuability regression
stepwiseData = growthTrainingSetQ[,c("PCF","PE","PS","PB","PEG","EPS","Cash EPS","Net Asset Value per Share","Times Preferred Dividends Earned","Dividend Payout Ratio")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("PCF","PE","PS","PB","PEG","EPS","Cash EPS","Net Asset Value per Share","Times Preferred Dividends Earned","Dividend Payout Ratio")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionValue=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionValue[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionValue[growthTestrows]))


#activity regression
stepwiseData = growthTrainingSetQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionActivity=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionActivity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionActivity[growthTestrows]))



#Solvency regression
stepwiseData = growthTrainingSetQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionSolvency=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionSolvency[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionSolvency[growthTestrows]))


#Growth Regression
stepwiseData = growthTrainingSetQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionGrowth=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionGrowth[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionGrowth[growthTestrows]))


#Liquidity Regression
stepwiseData = growthTrainingSetQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]
glm=glm(as.numeric(beatMarketGrowth)~.,data=stepwiseData)
step=step(glm,direction='both')
stepwiseData=growthMyDataQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]

stepWisePrediction=predict(step,stepwiseData)
stepWisePredictionLiquidity=ifelse(stepWisePrediction>1.5,2,1)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(stepWisePredictionLiquidity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(stepWisePredictionLiquidity[growthTestrows]))













#load these vars before the execution of the algorithms for pca
#profit pca
profitTrain=growthTrainingSetQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]
pcaProfit <- prcomp(profitTrain)
sd <- pcaProfit$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
profitVars=data.frame(pcaProfit$x)[,1:11]

#value pca
valueTrain=growthTrainingSetQ[,c("PCF","PE","PS","PB","EPS","Cash EPS","PEGY","Net Asset Value per Share","Times Preferred Dividends Earned","Dividend Payout Ratio")]
pcaValue <- prcomp(valueTrain)
sd <- pcaValue$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
valueVars=data.frame(pcaValue$x)[,1:10]

#activity pca
activityTrain=growthTrainingSetQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]
pcaActivity <- prcomp(activityTrain)
sd <- pcaActivity$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
activityVars=data.frame(pcaActivity$x)[,1:20]

#growth pca

growthTrain=growthTrainingSetQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]
pcaGrowth<- prcomp(growthTrain)
sd <- pcaGrowth$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
growthVars=data.frame(pcaGrowth$x)[,1:25]

#liquidity pca
liquidityTrain=growthTrainingSetQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]
pcaLiquidity<- prcomp(liquidityTrain)
sd <- pcaLiquidity$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
liquidityVars=data.frame(pcaLiquidity$x)[,1:6]

#solvency pca
solvencyTrain=growthTrainingSetQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]
pcaSolvency<- prcomp(solvencyTrain)
sd <- pcaSolvency$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
solvencyVars=data.frame(pcaSolvency$x)[,1:13]


#SVM Models

#train a svm linear model,profitability with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=profitVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]
svmTrain=predict(pcaProfit, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:11]

svmPredictionProfit=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionProfit[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionProfit[growthTestrows]))



#train a svm linear model,valuability with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=valueVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("PCF","PE","PS","PB","EPS","Cash EPS","PEGY","Net Asset Value per Share","Times Preferred Dividends Earned","Dividend Payout Ratio")]
svmTrain=predict(pcaValue, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:10]

svmPredictionValue=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionValue[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionValue[growthTestrows]))



#train a svm linear model,activity with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=activityVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]
svmTrain=predict(pcaActivity, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:20]

svmPredictionActivity=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionActivity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionActivity[growthTestrows]))



#Train a svm model,growth with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=growthVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]
svmTrain=predict(pcaGrowth, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:25]

svmPredictionGrowth=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionGrowth[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionGrowth[growthTestrows]))



#Train a svm linear model,liquidity with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=liquidityVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]
svmTrain=predict(pcaLiquidity, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:6]

svmPredictionLiquidity=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionLiquidity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionLiquidity[growthTestrows]))


#Train a svm model,solvency with all growth stocks
set.seed(1235)
svmLinearModel <- train(x=solvencyVars,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)

svmTrain=growthMyDataQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]
svmTrain=predict(pcaSolvency, newdata = svmTrain)
svmTrain=as.data.frame(svmTrain)[1:13]

svmPredictionSolvency=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmPredictionSolvency[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmPredictionSolvency[growthTestrows]))







#Profit GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = profitVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]
gbmTrain=predict(pcaProfit, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:11]

gbmProfitPrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmProfitPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmProfitPrediction[growthTestrows]))


#Value GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = valueVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("PCF","PE","PS","PB","EPS","Cash EPS","PEGY","Net Asset Value per Share","Times Preferred Dividends Earned","Dividend Payout Ratio")]
gbmTrain=predict(pcaValue, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:10]

gbmValuePrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmValuePrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmValuePrediction[growthTestrows]))


#Growth GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = growthVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]
gbmTrain=predict(pcaGrowth, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:25]

gbmGrowthPrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmGrowthPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmGrowthPrediction[growthTestrows]))


#Activity GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = activityVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]
gbmTrain=predict(pcaActivity, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:15]

gbmActivityPrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmActivityPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmActivityPrediction[growthTestrows]))


#Liquidity GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = liquidityVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]
gbmTrain=predict(pcaLiquidity, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:6]

gbmLiquidityPrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmLiquidityPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmLiquidityPrediction[growthTestrows]))


#Solvency GBM with just growth stocks
set.seed(1235)
GBDecisionTree = train(x = solvencyVars,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree

gbmTrain = growthMyDataQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]
gbmTrain=predict(pcaSolvency, newdata = gbmTrain)
gbmTrain=as.data.frame(gbmTrain)[1:13]

gbmSolvencyPrediction <- predict(GBDecisionTree, gbmTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmSolvencyPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmSolvencyPrediction[growthTestrows]))












#lets try using our multiple discriminate anaylsis scores

#profit lda
profitTrainLDA=growthTrainingSetQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]

#test to see if predictors are good
Manova=manova(as.matrix(profitTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(profitTrainLDA),beatMarketGrowth)

ldaProfit<- lda(beatMarketGrowth~., data=profitTrainLDA)
plot(ldaProfit, dimen = 1, type = "b")
scaling=ldaProfit$scaling

profitScoreTrain=0
for(i in 1:length(profitTrainLDA)){
  profitScoreTrain=profitScoreTrain+scaling[i]*profitTrainLDA[i]
}
profitScoreTrain=profitScoreTrain[[1]]


profitTest=growthMyDataQ[,c("Gross Margin","ROA","ROE","Profit Margin","Free Cash Flow Margin","Return on Capital Employed","Operating Profit Margin","Return on Operating Cash Flow","Return on Retained Earnings","Cash Flow Return on Investment","Return on Debt","Return on Average Equity","Cash Return on Assets","Return on Average Assets","Return on Research Capital","Cash Return On Capital Invested")]
ldaPrediction=predict(ldaProfit, newdata = profitTest)
ldaPredictionProfit=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionProfit[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionProfit[growthTestrows]))
#66.66667
profitScoreFull=0
for(i in 1:length(profitTrainLDA)){
  profitScoreFull=profitScoreFull+scaling[i]*profitTest[i]
}
profitScoreFull=profitScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~profitScoreFull)



#profit lda 2

profitTrainLDA=growthTrainingSetQ[,pvalsLowestColsProfit]
#test to see if predictors are good
Manova=manova(as.matrix(profitTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(profitTrainLDA),beatMarketGrowth)

ldaProfit<- lda(beatMarketGrowth~., data=profitTrainLDA)
plot(ldaProfit, dimen = 1, type = "b")
scaling=ldaProfit$scaling

profitScoreTrain2=0
for(i in 1:length(profitTrainLDA)){
  profitScoreTrain2=profitScoreTrain2+scaling[i]*profitTrainLDA[i]
}
profitScoreTrain2=profitScoreTrain2[[1]]


profitTest=growthMyDataQ[,pvalsLowestColsProfit]
ldaPrediction=predict(ldaProfit, newdata = profitTest)
ldaPredictionProfit2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionProfit2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionProfit2[growthTestrows]))
#66.66667
profitScoreFull2=0
for(i in 1:length(profitTrainLDA)){
  profitScoreFull2=profitScoreFull2+scaling[i]*profitTest[i]
}
profitScoreFull2=profitScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~profitScoreFull2)








#value lda
valueTrainLDA=growthTrainingSetQ[,c("PE","PCF","PS","PB","PEG","PEGY","EPS","Cash EPS","Net Asset Value per Share","Times Preferred Dividends Earned")]

#test to see if predictors are good
Manova=manova(as.matrix(valueTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(valueTrainLDA),beatMarketGrowth)

ldaValue<- lda(beatMarketGrowth~., data=valueTrainLDA)
plot(ldaValue, dimen = 1, type = "b")
scaling=ldaValue$scaling

valueScoreTrain=0
for(i in 1:length(valueTrainLDA)){
  valueScoreTrain=valueScoreTrain+scaling[i]*valueTrainLDA[i]
}
valueScoreTrain=valueScoreTrain[[1]]

valueTest=growthMyDataQ[,c("PE","PCF","PS","PB","PEG","PEGY","EPS","Cash EPS","Net Asset Value per Share","Times Preferred Dividends Earned")]
ldaPrediction=predict(ldaValue, newdata = valueTest)
ldaPredictionValue=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionValue[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionValue[growthTestrows]))
#66.66667
valueScoreFull=0
for(i in 1:length(valueTrainLDA)){
  valueScoreFull=valueScoreFull+scaling[i]*valueTest[i]
}
valueScoreFull=valueScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~valueScoreFull)


#value lda 2
valueTrainLDA=growthTrainingSetQ[,pvalsLowestColsValue]

#test to see if predictors are good
Manova=manova(as.matrix(valueTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(valueTrainLDA),beatMarketGrowth)

ldaValue<- lda(beatMarketGrowth~., data=valueTrainLDA)
plot(ldaValue, dimen = 1, type = "b")
scaling=ldaValue$scaling

valueScoreTrain2=0
for(i in 1:length(valueTrainLDA)){
  valueScoreTrain2=valueScoreTrain2+scaling[i]*valueTrainLDA[i]
}
valueScoreTrain2=valueScoreTrain2[[1]]


valueTest=growthMyDataQ[,pvalsLowestColsValue]
ldaPrediction=predict(ldaValue, newdata = valueTest)
ldaPredictionValue2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionValue2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionValue2[growthTestrows]))
#66.66667
valueScoreFull2=0
for(i in 1:length(valueTrainLDA)){
  valueScoreFull2=valueScoreFull2+scaling[i]*valueTest[i]
}
valueScoreFull2=valueScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~valueScoreFull2)



#growth lda
growthTrainLDA=growthTrainingSetQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]

#test to see if predictors are good
Manova=manova(as.matrix(growthTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(growthTrainLDA),beatMarketGrowth)

ldaGrowth<- lda(beatMarketGrowth~., data=growthTrainLDA)
plot(ldaGrowth, dimen = 1, type = "b")
scaling=ldaGrowth$scaling

growthScoreTrain=0
for(i in 1:length(growthTrainLDA)){
  growthScoreTrain=growthScoreTrain+scaling[i]*growthTrainLDA[i]
}
growthScoreTrain=growthScoreTrain[[1]]


growthTest=growthMyDataQ[,c("Four Year Average ROA Growth","Four Year Average FCF per Share Growth","Four Year Average Book Value per Share Growth","Four Year Average ROE Growth","Four Year Average EPS Growth","Four Year Average Gross Margin Growth","Four Year Average Profit Margin Growth","Four Year Average Free Cash Flow Margin Growth","Four Year Average Cash per Share Growth","Four Year Average Debt to Equity Ratio Growth","Four Year Average Total Debt To Total Assets Growth","Four Year Average Operating Expenses Growth","Four Year Average Current Ratio Growth","Four Year Average Income Quality Growth","Four Year Average Payout Ratio Growth","Four Year Average Selling, General and Administrative Expense of Revenue Growth","Four Year Average Operating Cash Flow Growth","Four Year Average Total Debt Growth","Four Year Average Current Assets Growth","Four Year Average Gross Profit Growth","Four Year Average Inventory Growth","Four Year Average Average Receivables Growth","Four Year Average Average Payables Growth","Four Year Average Revenue Growth","Four Year Average Total Assets Growth","Four Year Average Net Income Growth","Four Year Average Financing Cash Flow Growth","Four Year Average Investing Cash Flow Growth","Four Year Average Weighted Average Shares Growth","Four Year Average Total Liabilities Growth")]
ldaPrediction=predict(ldaGrowth, newdata = growthTest)
ldaPredictionGrowth=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionGrowth[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionGrowth[growthTestrows]))
#66.66667
growthScoreFull=0
for(i in 1:length(growthTrainLDA)){
  growthScoreFull=growthScoreFull+scaling[i]*growthTest[i]
}
growthScoreFull=growthScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~growthScoreFull)


#growth lda 2
growthTrainLDA=growthTrainingSetQ[,pvalsLowestColsGrowth]

#test to see if predictors are good
Manova=manova(as.matrix(growthTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(growthTrainLDA),beatMarketGrowth)

ldaGrowth<- lda(beatMarketGrowth~., data=growthTrainLDA)
plot(ldaGrowth, dimen = 1, type = "b")
scaling=ldaGrowth$scaling

growthScoreTrain2=0
for(i in 1:length(growthTrainLDA)){
  growthScoreTrain2=growthScoreTrain2+scaling[i]*growthTrainLDA[i]
}
growthScoreTrain2=growthScoreTrain2[[1]]


growthTest=growthMyDataQ[,pvalsLowestColsGrowth]
ldaPrediction=predict(ldaGrowth, newdata = growthTest)
ldaPredictionGrowth2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionGrowth2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionGrowth2[growthTestrows]))
#66.66667
growthScoreFull2=0
for(i in 1:length(growthTrainLDA)){
  growthScoreFull2=growthScoreFull2+scaling[i]*growthTest[i]
}
growthScoreFull2=growthScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~growthScoreFull2)








#activity lda
activityTrainLDA=growthTrainingSetQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]

#test to see if predictors are good
Manova=manova(as.matrix(activityTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(activityTrainLDA),beatMarketGrowth)

ldaActivity<- lda(beatMarketGrowth~., data=activityTrainLDA)
plot(ldaActivity, dimen = 1, type = "b")
scaling=ldaActivity$scaling

activityScoreTrain=0
for(i in 1:length(activityTrainLDA)){
  activityScoreTrain=activityScoreTrain+scaling[i]*activityTrainLDA[i]
}
activityScoreTrain=activityScoreTrain[[1]]


activityTest=growthMyDataQ[,c("Asset Turnover","Merchandise Inventory Ratio","Working Capital Turnover Ratio","Fixed Asset Turnover Ratio","Account Receivables Turnover","Account Payables Turnover","Inventory Turnover","Average Days of Payables","Average Days of Receivables","Inventory Turnover","Cash Turnover","Days Sales Outstanding","Days Payable Outstanding","Days Inventory Outstanding","Operating Cash Flow to Sales","Days Working Capital","Days Cash on Hand","Sales to Administrative Expenses","Investment Turnover","Sales to Equity","Inventory to Sales","Sales to Operating Income","Goodwill to Assets","Free Cash Flow to Sales")]
ldaPrediction=predict(ldaActivity, newdata = activityTest)
ldaPredictionActivity=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionActivity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionActivity[growthTestrows]))
#66.66667
activityScoreFull=0
for(i in 1:length(activityTrainLDA)){
  activityScoreFull=activityScoreFull+scaling[i]*activityTest[i]
}
activityScoreFull=activityScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~activityScoreFull)





#activity lda 2
activityTrainLDA=growthTrainingSetQ[,pvalsLowestColsActivity]

#test to see if predictors are good
Manova=manova(as.matrix(activityTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(activityTrainLDA),beatMarketGrowth)

ldaActivity<- lda(beatMarketGrowth~., data=activityTrainLDA)
plot(ldaActivity, dimen = 1, type = "b")
scaling=ldaActivity$scaling

activityScoreTrain2=0
for(i in 1:length(activityTrainLDA)){
  activityScoreTrain2=activityScoreTrain2+scaling[i]*activityTrainLDA[i]
}
activityScoreTrain2=activityScoreTrain2[[1]]


activityTest=growthMyDataQ[,pvalsLowestColsActivity]
ldaPrediction=predict(ldaActivity, newdata = activityTest)
ldaPredictionActivity2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionActivity2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionActivity2[growthTestrows]))
#66.66667
activityScoreFull2=0
for(i in 1:length(activityTrainLDA)){
  activityScoreFull2=activityScoreFull2+scaling[i]*activityTest[i]
}
activityScoreFull2=activityScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~activityScoreFull2)





#solvency lda
solvencyTrainLDA=growthTrainingSetQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]

#test to see if predictors are good
Manova=manova(as.matrix(solvencyTrainLDA)~beatMarketSolvency)
summary(Manova, test = "Wilks")
discPower(as.matrix(solvencyTrainLDA),beatMarketSolvency)

ldaSolvency<- lda(beatMarketGrowth~., data=solvencyTrainLDA)
plot(ldaSolvency, dimen = 1, type = "b")
scaling=ldaSolvency$scaling

solvencyScoreTrain=0
for(i in 1:length(solvencyTrainLDA)){
  solvencyScoreTrain=solvencyScoreTrain+scaling[i]*solvencyTrainLDA[i]
}
solvencyScoreTrain=solvencyScoreTrain[[1]]


solvencyTest=growthMyDataQ[,c("Cash Flow to Debt","Debt Ratio","Equity Ratio","Working Capital to Debt","Current Cash Debt Coverage","Interest Coverage","Asset Coverage","Interest Expense to Debt","Capitalization Ratio","Debt to EBITDA","Long-term debt ratio","Net Debt to EBITDA","Cash Flow Coverage","Financial Leverage Index","Non-Current Asset to Net Worth","Long-term debt to Equity Ratio","Fixed-Assets to Net Worth Ratio","Total Debt To Total Assets","Debt to Equity Ratio","Current Ratio")]
ldaPrediction=predict(ldaSolvency, newdata = solvencyTest)
ldaPredictionSolvency=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionSolvency[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionSolvency[growthTestrows]))
#66.66667
solvencyScoreFull=0
for(i in 1:length(solvencyTrainLDA)){
  solvencyScoreFull=solvencyScoreFull+scaling[i]*solvencyTest[i]
}
solvencyScoreFull=solvencyScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~solvencyScoreFull)




#solvency lda 2
solvencyTrainLDA=growthTrainingSetQ[,pvalsLowestColsSolvency]

#test to see if predictors are good
Manova=manova(as.matrix(solvencyTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(solvencyTrainLDA),beatMarketGrowth)

ldaSolvency<- lda(beatMarketGrowth~., data=solvencyTrainLDA)
plot(ldaSolvency, dimen = 1, type = "b")
scaling=ldaSolvency$scaling

solvencyScoreTrain2=0
for(i in 1:length(solvencyTrainLDA)){
  solvencyScoreTrain2=solvencyScoreTrain2+scaling[i]*solvencyTrainLDA[i]
}
solvencyScoreTrain2=solvencyScoreTrain2[[1]]


solvencyTest=growthMyDataQ[,pvalsLowestColsSolvency]
ldaPrediction=predict(ldaSolvency, newdata = solvencyTest)
ldaPredictionSolvency2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionSolvency2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionSolvency2[growthTestrows]))
#66.66667
solvencyScoreFull2=0
for(i in 1:length(solvencyTrainLDA)){
  solvencyScoreFull2=solvencyScoreFull2+scaling[i]*solvencyTest[i]
}
solvencyScoreFull2=solvencyScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~solvencyScoreFull2)







#liquidity lda
liquidityTrainLDA=growthTrainingSetQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]

#test to see if predictors are good
Manova=manova(as.matrix(liquidityTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(liquidityTrainLDA),beatMarketGrowth)

ldaLiquidity<- lda(beatMarketGrowth~., data=liquidityTrainLDA)
plot(ldaLiquidity, dimen = 1, type = "b")
scaling=ldaLiquidity$scaling

liquidityScoreTrain=0
for(i in 1:length(liquidityTrainLDA)){
  liquidityScoreTrain=liquidityScoreTrain+scaling[i]*liquidityTrainLDA[i]
}
liquidityScoreTrain=liquidityScoreTrain[[1]]


liquidityTest=growthMyDataQ[,c("Cash Ratio","Quick Ratio","Cash to Working Capital","Inventory to Working Capital","Sales to Current Assets","Sales to Working Capital","Net Working Capital Ratio","Acid Test","Cash Flow Coverage","Current Ratio")]
ldaPrediction=predict(ldaLiquidity, newdata = liquidityTest)
ldaPredictionLiquidity=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionLiquidity[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionLiquidity[growthTestrows]))
#66.66667
liquidityScoreFull=0
for(i in 1:length(liquidityTrainLDA)){
  liquidityScoreFull=liquidityScoreFull+scaling[i]*liquidityTest[i]
}
liquidityScoreFull=liquidityScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~liquidityScoreFull)




#liquidity lda
liquidityTrainLDA=growthTrainingSetQ[,pvalsLowestColsLiquidity]

#test to see if predictors are good
Manova=manova(as.matrix(liquidityTrainLDA)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(liquidityTrainLDA),beatMarketGrowth)

ldaLiquidity<- lda(beatMarketGrowth~., data=liquidityTrainLDA)
plot(ldaLiquidity, dimen = 1, type = "b")
scaling=ldaLiquidity$scaling

liquidityScoreTrain2=0
for(i in 1:length(liquidityTrainLDA)){
  liquidityScoreTrain2=liquidityScoreTrain2+scaling[i]*liquidityTrainLDA[i]
}
liquidityScoreTrain2=liquidityScoreTrain2[[1]]


liquidityTest=growthMyDataQ[,pvalsLowestColsLiquidity]
ldaPrediction=predict(ldaLiquidity, newdata = liquidityTest)
ldaPredictionLiquidity2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionLiquidity2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionLiquidity2[growthTestrows]))
#66.66667
liquidityScoreFull2=0
for(i in 1:length(liquidityTrainLDA)){
  liquidityScoreFull2=liquidityScoreFull2+scaling[i]*liquidityTest[i]
}
liquidityScoreFull2=liquidityScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~liquidityScoreFull2)




#create a stat to PE lda and pca
peTrain=growthTrainingSetQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
pcaPE <- prcomp(peTrain)
sd <- pcaPE$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
peVars=data.frame(pcaPE$x)[,1:20]


#test to see if predictors are good
Manova=manova(as.matrix(peVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(peVars),beatMarketGrowth)

ldaPE<- lda(beatMarketGrowth~., data=peVars)
plot(ldaPE, dimen = 1, type = "b")

scaling=ldaPE$scaling
peScoreTrain=0
for(i in 1:20){
  peScoreTrain=peScoreTrain+scaling[i]*peVars[i]
}
peScoreTrain=peScoreTrain[[1]]

peData=growthMyDataQ[,c("Asset Turnover to PE","Cash Ratio to PE","ROA to PE","FCF per Share to PE","Book Value per Share to PE","ROE to PE","EPS to PE","Gross Margin to PE","Profit Margin to PE","Free Cash Flow Margin to PE","Cash per Share to PE","Debt to Equity Ratio to PE","Total Debt To Total Assets to PE","Current Ratio to PE","Return on Sales to PE","Return on Capital Employed to PE","Operating Profit Margin to PE","Return on Operating Cash Flow to PE","Cash Flow Return on Investment to PE","Return on Debt to PE","Cash Return on Assets to PE","Return on Research Capital to PE","Return on Net Assets to PE","Cash EPS to PE","Net Asset Value per Share to PE","Cash Turnover to PE","Operating Cash Flow to Sales to PE")]
peData=predict(pcaPE, newdata = peData)
peData=as.data.frame(peData)[1:20]

ldaPrediction=predict(ldaPE, newdata = peData)
ldaPredictionPE=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionPE[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionPE[growthTestrows]))
#66.66667
peScoreFull=0
for(i in 1:20){
  peScoreFull=peScoreFull+scaling[i]*peData[i]
}
peScoreFull=peScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~peScoreFull)


#PE LDA 2
peVars=growthTrainingSetQ[,pvalsLowestColsPE]
#test to see if predictors are good
Manova=manova(as.matrix(peVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(peVars),beatMarketGrowth)

ldaPE<- lda(beatMarketGrowth~., data=peVars)
plot(ldaPE, dimen = 1, type = "b")

scaling=ldaPE$scaling
peScoreTrain2=0
for(i in 1:length(peVars)){
  peScoreTrain2=peScoreTrain2+scaling[i]*peVars[i]
}
peScoreTrain2=peScoreTrain2[[1]]

peData=growthMyDataQ[,pvalsLowestColsPE]

ldaPrediction=predict(ldaPE, newdata = peData)
ldaPredictionPE2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionPE2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionPE2[growthTestrows]))
#66.66667
peScoreFull2=0
for(i in 1:length(peVars)){
  peScoreFull2=peScoreFull2+scaling[i]*peData[i]
}
peScoreFull2=peScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~peScoreFull2)






#future data has such a big sd that pca doesn't work because one of the calculations turns to infinity, so let's scale the data
futureData=c("Future 20 Year Return on Sales","Future 20 Year Return on Capital Employed","Future 20 Year Operating Profit Margin","Future 20 Year Return on Operating Cash Flow","Future 20 Year Return on Retained Earnings","Future 20 Year Cash Flow Return on Investment","Future 20 Year Return on Debt","Future 20 Year Cash Return on Assets","Future 20 Year Return on Research Capital","Future 20 Year Cash Return On Capital Invested","Future 20 Year Return on Net Assets","Future 20 Year Cash EPS","Future 20 Year Net Asset Value per Share","Future 20 Year PEGY","Future 20 Year Dividend Payout Ratio","Future 20 Year Cash Turnover","Future 20 Year Days Sales Outstanding","Future 20 Year Days Payable Outstanding","Future 20 Year Days Inventory Outstanding","Future 20 Year Operating Cash Flow to Sales","Future 20 Year Days Working Capital","Future 20 Year Days Cash on Hand","Future 20 Year Sales to Administrative Expenses","Future 20 Year Investment Turnover","Future 20 Year Sales to Equity","Future 20 Year Inventory to Sales","Future 20 Year Sales to Operating Income","Future 20 Year Goodwill to Assets","Future 20 Year Free Cash Flow to Sales","Future 20 Year Cash to Working Capital","Future 20 Year Inventory to Working Capital","Future 20 Year Sales to Current Assets","Future 20 Year Sales to Working Capital","Future 20 Year Net Working Capital Ratio","Future 20 Year Acid Test","Future 20 Year Cash to Current Assets","Future 20 Year Cash Flow to Debt","Future 20 Year Debt Ratio","Future 20 Year Equity Ratio","Future 20 Year Working Capital to Debt","Future 20 Year Current Cash Debt Coverage","Future 20 Year Interest Coverage","Future 20 Year Asset Coverage","Future 20 Year Interest Expense to Debt","Future 20 Year Capitalization Ratio","Future 20 Year Debt to EBITDA","Future 20 Year Long-term debt ratio","Future 20 Year Net Debt to EBITDA","Future 20 Year Cash Flow Coverage","Future 20 Year Financial Leverage Index","Future 20 Year Non-Current Asset to Net Worth",
             "Future 20 Year Long-term debt to Equity Ratio","Future 20 Year Long-term debt to Total Assets","Future 20 Year Fixed-Assets to Net Worth Ratio","Future 20 Year Average Days of Receivables","Future 20 Year Average Days of Payables","Future 20 Year Account Payables Turnover","Future 20 Year Account Receivables Turnover","Future 20 Year Inventory Turnover","Future 20 Year Asset Turnover","Future 20 Year Merchandise Inventory Ratio","Future 20 Year Working Capital Turnover Ratio","Future 20 Year Fixed Asset Turnover Ratio","Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Equity Multiplier","Future 20 Year Degree of Financial Leverage","Future 20 Year ROA","Future 20 Year PCF","Future 20 Year PE","Future 20 Year PB","Future 20 Year PS","Future 20 Year PEG","Future 20 Year FCF per Share","Future 20 Year Book Value per Share","Future 20 Year ROE","Future 20 Year EPS","Future 20 Year Gross Margin","Future 20 Year EBITDA Margin","Future 20 Year EBIT Margin","Future 20 Year Profit Margin","Future 20 Year Free Cash Flow Margin","Future 20 Year Cash per Share","Future 20 Year Debt to Equity Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Current Ratio","Future 20 Year Income Quality","Future 20 Year Payout Ratio","Future 20 Year Selling, General and Administrative Expense of Revenue","Future 20 Year Research and Development Expense of Revenue","Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables")

growthTrainingSetQ[,futureData]=scale(growthTrainingSetQ[,futureData],center = F, scale = T)

growthTestSetQ[,futureData]=scale(growthTestSetQ[,futureData], center = F, scale = T)

growthMyDataQ=rbind(growthTrainingSetQ,growthTestSetQ)


#create a future 20 year stat with lda and pca
futureTrain=growthTrainingSetQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables","Future 20 Year Asset Turnover","Future 20 Year Cash Ratio","Future 20 Year ROA","Future 20 Year FCF per Share","Future 20 Year Book Value per Share","Future 20 Year ROE","Future 20 Year EPS","Future 20 Year Gross Margin","Future 20 Year Profit Margin","Future 20 Year Free Cash Flow Margin","Future 20 Year Cash per Share","Future 20 Year Debt to Equity Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Current Ratio","Future 20 Year Return on Sales","Future 20 Year Return on Capital Employed","Future 20 Year Operating Profit Margin","Future 20 Year Return on Operating Cash Flow","Future 20 Year Cash Flow Return on Investment","Future 20 Year Return on Debt","Future 20 Year Cash Return on Assets","Future 20 Year Return on Research Capital","Future 20 Year Return on Net Assets","Future 20 Year Cash EPS","Future 20 Year Net Asset Value per Share","Future 20 Year Cash Turnover","Future 20 Year Operating Cash Flow to Sales")]
pcaFuture <- prcomp(futureTrain)
sd <- sapply(futureTrain)
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
futureVars=data.frame(pcaFuture$x)[,1:30]


#test to see if predictors are good
Manova=manova(as.matrix(futureVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(futureVars),beatMarketGrowth)

ldaFuture<- lda(beatMarketGrowth~., data=futureVars)
plot(ldaFuture, dimen = 1, type = "b")

scaling=ldaFuture$scaling
futureScoreTrain=0
for(i in 1:30){
  futureScoreTrain=futureScoreTrain+scaling[i]*futureVars[i]
}
futureScoreTrain=futureScoreTrain[[1]]

futureData=growthMyDataQ[,c("Future 20 Year Total Assets","Future 20 Year Revenue","Future 20 Year Net Income","Future 20 Year Cost of Revenue","Future 20 Year Gross Profit","Future 20 Year Research and Development (R&D) Expenses","Future 20 Year Selling, General and Administrative (SG&A) Expenses","Future 20 Year Operating Expenses","Future 20 Year EBITDA","Future 20 Year EBIT","Future 20 Year Cash and cash equivalents","Future 20 Year Investments Current","Future 20 Year Cash and short-term investments","Future 20 Year Inventory","Future 20 Year Current Assets","Future 20 Year Goodwill and Intangible Assets","Future 20 Year Assets Non-Current","Future 20 Year Current Liabilities","Future 20 Year Liabilities Non-Current","Future 20 Year Total Debt","Future 20 Year Total Liabilities","Future 20 Year Shareholders Equity","Future 20 Year Financing Cash Flow","Future 20 Year Investing Cash Flow","Future 20 Year Operating Cash Flow","Future 20 Year Average Receivables","Future 20 Year Average Payables","Future 20 Year Asset Turnover","Future 20 Year Cash Ratio","Future 20 Year ROA","Future 20 Year FCF per Share","Future 20 Year Book Value per Share","Future 20 Year ROE","Future 20 Year EPS","Future 20 Year Gross Margin","Future 20 Year Profit Margin","Future 20 Year Free Cash Flow Margin","Future 20 Year Cash per Share","Future 20 Year Debt to Equity Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Current Ratio","Future 20 Year Return on Sales","Future 20 Year Return on Capital Employed","Future 20 Year Operating Profit Margin","Future 20 Year Return on Operating Cash Flow","Future 20 Year Cash Flow Return on Investment","Future 20 Year Return on Debt","Future 20 Year Cash Return on Assets","Future 20 Year Return on Research Capital","Future 20 Year Return on Net Assets","Future 20 Year Cash EPS","Future 20 Year Net Asset Value per Share","Future 20 Year Cash Turnover","Future 20 Year Operating Cash Flow to Sales")]
futureData=predict(pcaFuture, newdata = futureData)
futureData=as.data.frame(futureData)[1:30]

ldaPrediction=predict(ldaFuture, newdata = futureData)
ldaPredictionFuture=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionFuture[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionFuture[growthTestrows]))
#66.66667
futureScoreFull=0
for(i in 1:30){
  futureScoreFull=futureScoreFull+scaling[i]*futureData[i]
}
futureScoreFull=futureScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~futureScoreFull,xlim=c(-5,5))












#Future 20 year stat lda 2

#test to see if predictors are good
futureVars=growthTrainingSetQ[,pvalsLowestColsFuture]
Manova=manova(as.matrix(futureVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(futureVars),beatMarketGrowth)

ldaFuture<- lda(beatMarketGrowth~., data=futureVars)
plot(ldaFuture, dimen = 1, type = "b")

scaling=ldaFuture$scaling
futureScoreTrain2=0
for(i in 1:length(futureVars)){
  futureScoreTrain2=futureScoreTrain2+scaling[i]*futureVars[i]
}
futureScoreTrain2=futureScoreTrain2[[1]]

futureData=growthMyDataQ[,pvalsLowestColsFuture]

ldaPrediction=predict(ldaFuture, newdata = futureData)
ldaPredictionFuture2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionFuture2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionFuture2[growthTestrows]))
#66.66667
futureScoreFull2=0
for(i in 1:length(futureVars)){
  futureScoreFull2=futureScoreFull2+scaling[i]*futureData[i]
}
futureScoreFull2=futureScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~futureScoreFull2,xlim=c(-5,5))

par(mfrow=c(1,2))
plot(growthMyDataQ[,"Price Change %"]~liquidityScoreFull,main="Liquidity Score to Change in Price",ylab="Price Change in %",xlab="Liquidity Score",xlim=c(-5,5))
plot(growthMyDataQ[,"Price Change %"]~sumFullScore,main="LDA Combined Score to Change in Price",ylab="Price Change in %",xlab="Combined LDA Score",xlim=c(-13,16))




#create a stat to the average with lda and pca
marketAverageTrain=growthTrainingSetQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
pcaMarketAverage <- prcomp(marketAverageTrain)
sd <- pcaMarketAverage$sdev
var <- sd^2
varex <- var/sum(var)
plot(varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
marketAverageVars=data.frame(pcaMarketAverage$x)[,1:23]


#test to see if predictors are good
Manova=manova(as.matrix(marketAverageVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(marketAverageVars),beatMarketGrowth)

ldaMarketAverage<- lda(beatMarketGrowth~., data=marketAverageVars)
plot(ldaMarketAverage, dimen = 1, type = "b")

scaling=ldaMarketAverage$scaling
marketAverageScoreTrain=0
for(i in 1:23){
  marketAverageScoreTrain=marketAverageScoreTrain+scaling[i]*marketAverageVars[i]
}
marketAverageScoreTrain=marketAverageScoreTrain[[1]]

marketAverageData=growthMyDataQ[,c("Asset Turnover to Market Average","Cash Ratio to Market Average","ROA to Market Average","FCF per Share to Market Average","Book Value per Share to Market Average","ROE to Market Average","EPS to Market Average","Gross Margin to Market Average","Profit Margin to Market Average","Free Cash Flow Margin to Market Average","Cash per Share to Market Average","Debt to Equity Ratio to Market Average","Total Debt To Total Assets to Market Average","Current Ratio to Market Average","Return on Sales to Market Average","Return on Capital Employed to Market Average","Operating Profit Margin to Market Average","Return on Operating Cash Flow to Market Average","Cash Flow Return on Investment to Market Average","Return on Debt to Market Average","Cash Return on Assets to Market Average","Return on Research Capital to Market Average","Return on Net Assets to Market Average","Cash EPS to Market Average","Net Asset Value per Share to Market Average","Cash Turnover to Market Average","Operating Cash Flow to Sales to Market Average")]
marketAverageData=predict(pcaMarketAverage,newdata=marketAverageData)
marketAverageData=as.data.frame(marketAverageData)[1:23]

ldaPrediction=predict(ldaMarketAverage,newdata=marketAverageData)
ldaPredictionMarketAverage=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionMarketAverage[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionMarketAverage[growthTestrows]))
#70.21277%
marketAverageScoreFull=0
for(i in 1:23){
  marketAverageScoreFull=marketAverageScoreFull+scaling[i]*marketAverageData[i]
}
marketAverageScoreFull=marketAverageScoreFull[[1]]
plot(growthMyDataQ[,"Price Change %"]~marketAverageScoreFull)




#market average lda 2
#test to see if predictors are good
marketAverageVars=growthTrainingSetQ[,pvalsLowestColsMarket]
Manova=manova(as.matrix(marketAverageVars)~beatMarketGrowth)
summary(Manova, test = "Wilks")
discPower(as.matrix(marketAverageVars),beatMarketGrowth)

ldaMarketAverage<- lda(beatMarketGrowth~., data=marketAverageVars)
plot(ldaMarketAverage, dimen = 1, type = "b")

scaling=ldaMarketAverage$scaling
marketAverageScoreTrain2=0
for(i in 1:length(marketAverageVars)){
  marketAverageScoreTrain2=marketAverageScoreTrain2+scaling[i]*marketAverageVars[i]
}
marketAverageScoreTrain2=marketAverageScoreTrain2[[1]]

marketAverageData=growthMyDataQ[,pvalsLowestColsMarket]

ldaPrediction=predict(ldaMarketAverage,newdata=marketAverageData)
ldaPredictionMarketAverage2=ldaPrediction$class
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(ldaPredictionMarketAverage2[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(ldaPredictionMarketAverage2[growthTestrows]))
#70.21277%
marketAverageScoreFull2=0
for(i in 1:length(marketAverageVars)){
  marketAverageScoreFull2=marketAverageScoreFull2+scaling[i]*marketAverageData[i]
}
marketAverageScoreFull2=marketAverageScoreFull2[[1]]
plot(growthMyDataQ[,"Price Change %"]~marketAverageScoreFull2)







#scale and center the data then run machine learning models on the sum of each lda score
ldaScoresFull=data.frame(profitScoreFull,valueScoreFull,activityScoreFull,solvencyScoreFull,growthScoreFull,liquidityScoreFull,marketAverageScoreFull,peScoreFull,futureScoreFull)
for(i in 1:8){
  ldaScoresFull[,i]=scale(ldaScoresFull[,i],center = T, scale = T)
}
sumFullScore=apply(ldaScoresFull,1,sum)


#scale and center the data then run machine learning models on the sum of each lda 2 score
ldaScoresFull2=data.frame(profitScoreFull2,valueScoreFull2,activityScoreFull2,solvencyScoreFull2,growthScoreFull2,liquidityScoreFull2,marketAverageScoreFull2,peScoreFull2,futureScoreFull2)
for(i in 1:8){
  ldaScoresFull2[,i]=scale(ldaScoresFull[,i],center = T, scale = T)
}
sumFullScore2=apply(ldaScoresFull2,1,sum)






#Train Neural Networks

#combine the five financial catgeory score into 1 discriminant score
neuralNetTrain=data.frame("profit"=profitScoreTrain,"value"=valueScoreTrain,"activity"=activityScoreTrain,"solvency"=solvencyScoreTrain,"growth"=growthScoreTrain,"liquidity"=liquidityScoreTrain)
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
set.seed(1235)
neuralNetworkModel<- train(x=neuralNetTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTest=data.frame("profit"=profitScoreFull,"value"=valueScoreFull,"activity"=activityScoreFull,"solvency"=solvencyScoreFull,"growth"=growthScoreFull,"liquidity"=liquidityScoreFull)
neuralNetworkPredictionDiscriminant <- predict(neuralNetworkModel, newdata=neuralNetTest)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(neuralNetworkPredictionDiscriminant[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(neuralNetworkPredictionDiscriminant[growthTestrows]))
#69.23% accurate


#for growth
neuralNetTrain=growthTrainingSetQ[,c("ROA Growth to MK Growth","FCF per Share Growth to MK Growth","Book Value per Share Growth to MK Growth","ROE Growth to MK Growth","EPS Growth to MK Growth","Gross Margin Growth to MK Growth","Profit Margin Growth to MK Growth","Free Cash Flow Margin Growth to MK Growth","Cash per Share Growth to MK Growth","Debt to Equity Ratio Growth to MK Growth","Total Debt To Total Assets Growth to MK Growth","Operating Expenses Growth to MK Growth","Current Ratio Growth to MK Growth","Income Quality Growth to MK Growth","Payout Ratio Growth to MK Growth","Selling, General and Administrative Expense of Revenue Growth to MK Growth","Operating Cash Flow Growth to MK Growth","Total Debt Growth to MK Growth","Current Assets Growth to MK Growth","Gross Profit Growth to MK Growth","Inventory Growth to MK Growth","Average Receivables Growth to MK Growth","Average Payables Growth to MK Growth","Revenue Growth to MK Growth","Total Assets Growth to MK Growth","Net Income Growth to MK Growth","Financing Cash Flow Growth to MK Growth","Investing Cash Flow Growth to MK Growth","Weighted Average Shares Growth to MK Growth","Total Liabilities Growth to MK Growth")]
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
set.seed(1235)
neuralNetworkModel<- train(x=neuralNetTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTest=growthMyDataQ[,c("ROA Growth to MK Growth","FCF per Share Growth to MK Growth","Book Value per Share Growth to MK Growth","ROE Growth to MK Growth","EPS Growth to MK Growth","Gross Margin Growth to MK Growth","Profit Margin Growth to MK Growth","Free Cash Flow Margin Growth to MK Growth","Cash per Share Growth to MK Growth","Debt to Equity Ratio Growth to MK Growth","Total Debt To Total Assets Growth to MK Growth","Operating Expenses Growth to MK Growth","Current Ratio Growth to MK Growth","Income Quality Growth to MK Growth","Payout Ratio Growth to MK Growth","Selling, General and Administrative Expense of Revenue Growth to MK Growth","Operating Cash Flow Growth to MK Growth","Total Debt Growth to MK Growth","Current Assets Growth to MK Growth","Gross Profit Growth to MK Growth","Inventory Growth to MK Growth","Average Receivables Growth to MK Growth","Average Payables Growth to MK Growth","Revenue Growth to MK Growth","Total Assets Growth to MK Growth","Net Income Growth to MK Growth","Financing Cash Flow Growth to MK Growth","Investing Cash Flow Growth to MK Growth","Weighted Average Shares Growth to MK Growth","Total Liabilities Growth to MK Growth")]
neuralNetworkPredictionGrowth <- predict(neuralNetworkModel, newdata=neuralNetTest)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(neuralNetworkPredictionGrowth[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(neuralNetworkPredictionGrowth[growthTestrows]))
#74.36% accurate

#with neural nets, let's combine several variables including, Growth to MK Growth, stat, future 20 year stat, and stat to market average

#use solvency - future and stat to market average
neuralNetTrain=growthTrainingSetQ[c("Future 20 Year Cash Flow to Debt","Future 20 Year Debt Ratio","Future 20 Year Equity Ratio","Future 20 Year Working Capital to Debt","Future 20 Year Current Cash Debt Coverage","Future 20 Year Interest Coverage","Future 20 Year Asset Coverage","Future 20 Year Interest Expense to Debt","Future 20 Year Capitalization Ratio","Future 20 Year Debt to EBITDA","Future 20 Year Long-term debt ratio","Future 20 Year Net Debt to EBITDA","Future 20 Year Cash Flow Coverage","Future 20 Year Financial Leverage Index","Future 20 Year Non-Current Asset to Net Worth","Future 20 Year Long-term debt to Equity Ratio","Future 20 Year Fixed-Assets to Net Worth Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Debt to Equity Ratio","Future 20 Year Current Ratio","Cash Flow to Debt Growth to MK Growth","Debt Ratio Growth to MK Growth","Equity Ratio Growth to MK Growth","Working Capital to Debt Growth to MK Growth","Current Cash Debt Coverage Growth to MK Growth","Interest Coverage Growth to MK Growth","Asset Coverage Growth to MK Growth","Interest Expense to Debt Growth to MK Growth","Capitalization Ratio Growth to MK Growth","Debt to EBITDA Growth to MK Growth","Long-term debt ratio Growth to MK Growth","Net Debt to EBITDA Growth to MK Growth","Cash Flow Coverage Growth to MK Growth","Financial Leverage Index Growth to MK Growth","Non-Current Asset to Net Worth Growth to MK Growth","Long-term debt to Equity Ratio Growth to MK Growth","Fixed-Assets to Net Worth Ratio Growth to MK Growth","Total Debt To Total Assets Growth to MK Growth","Debt to Equity Ratio Growth to MK Growth","Current Ratio Growth to MK Growth")]
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
set.seed(1235)
neuralNetworkModel<- train(x=neuralNetTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTest=growthMyDataQ[c("Future 20 Year Cash Flow to Debt","Future 20 Year Debt Ratio","Future 20 Year Equity Ratio","Future 20 Year Working Capital to Debt","Future 20 Year Current Cash Debt Coverage","Future 20 Year Interest Coverage","Future 20 Year Asset Coverage","Future 20 Year Interest Expense to Debt","Future 20 Year Capitalization Ratio","Future 20 Year Debt to EBITDA","Future 20 Year Long-term debt ratio","Future 20 Year Net Debt to EBITDA","Future 20 Year Cash Flow Coverage","Future 20 Year Financial Leverage Index","Future 20 Year Non-Current Asset to Net Worth","Future 20 Year Long-term debt to Equity Ratio","Future 20 Year Fixed-Assets to Net Worth Ratio","Future 20 Year Total Debt To Total Assets","Future 20 Year Debt to Equity Ratio","Future 20 Year Current Ratio","Cash Flow to Debt Growth to MK Growth","Debt Ratio Growth to MK Growth","Equity Ratio Growth to MK Growth","Working Capital to Debt Growth to MK Growth","Current Cash Debt Coverage Growth to MK Growth","Interest Coverage Growth to MK Growth","Asset Coverage Growth to MK Growth","Interest Expense to Debt Growth to MK Growth","Capitalization Ratio Growth to MK Growth","Debt to EBITDA Growth to MK Growth","Long-term debt ratio Growth to MK Growth","Net Debt to EBITDA Growth to MK Growth","Cash Flow Coverage Growth to MK Growth","Financial Leverage Index Growth to MK Growth","Non-Current Asset to Net Worth Growth to MK Growth","Long-term debt to Equity Ratio Growth to MK Growth","Fixed-Assets to Net Worth Ratio Growth to MK Growth","Total Debt To Total Assets Growth to MK Growth","Debt to Equity Ratio Growth to MK Growth","Current Ratio Growth to MK Growth")]
neuralNetworkPredictionSolvency <- predict(neuralNetworkModel, newdata=neuralNetTest)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(neuralNetworkPredictionSolvency[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(neuralNetworkPredictionSolvency[growthTestrows]))
#66.67% accurate

#activity - Growth to MK Growth
neuralNetTrain=growthTrainingSetQ[,c("Future 20 Year Asset Turnover","Future 20 Year Working Capital Turnover Ratio","Future 20 Year Account Receivables Turnover","Future 20 Year Account Payables Turnover","Future 20 Year Inventory Turnover","Future 20 Year Cash Turnover","Future 20 Year Operating Cash Flow to Sales","Future 20 Year Days Working Capital","Future 20 Year Sales to Administrative Expenses","Future 20 Year Investment Turnover","Future 20 Year Sales to Equity","Future 20 Year Inventory to Sales","Future 20 Year Sales to Operating Income","Future 20 Year Goodwill to Assets","Future 20 Year Free Cash Flow to Sales","Asset Turnover Growth to MK Growth","Working Capital Turnover Ratio Growth to MK Growth","Account Receivables Turnover Growth to MK Growth","Account Payables Turnover Growth to MK Growth","Inventory Turnover Growth to MK Growth","Cash Turnover Growth to MK Growth","Operating Cash Flow to Sales Growth to MK Growth","Days Working Capital Growth to MK Growth","Sales to Administrative Expenses Growth to MK Growth","Investment Turnover Growth to MK Growth","Sales to Equity Growth to MK Growth","Inventory to Sales Growth to MK Growth","Sales to Operating Income Growth to MK Growth","Goodwill to Assets Growth to MK Growth","Free Cash Flow to Sales Growth to MK Growth")]
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
set.seed(1235)
neuralNetworkModel<- train(x=neuralNetTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTest=growthMyDataQ[,c("Future 20 Year Asset Turnover","Future 20 Year Working Capital Turnover Ratio","Future 20 Year Account Receivables Turnover","Future 20 Year Account Payables Turnover","Future 20 Year Inventory Turnover","Future 20 Year Cash Turnover","Future 20 Year Operating Cash Flow to Sales","Future 20 Year Days Working Capital","Future 20 Year Sales to Administrative Expenses","Future 20 Year Investment Turnover","Future 20 Year Sales to Equity","Future 20 Year Inventory to Sales","Future 20 Year Sales to Operating Income","Future 20 Year Goodwill to Assets","Future 20 Year Free Cash Flow to Sales","Asset Turnover Growth to MK Growth","Working Capital Turnover Ratio Growth to MK Growth","Account Receivables Turnover Growth to MK Growth","Account Payables Turnover Growth to MK Growth","Inventory Turnover Growth to MK Growth","Cash Turnover Growth to MK Growth","Operating Cash Flow to Sales Growth to MK Growth","Days Working Capital Growth to MK Growth","Sales to Administrative Expenses Growth to MK Growth","Investment Turnover Growth to MK Growth","Sales to Equity Growth to MK Growth","Inventory to Sales Growth to MK Growth","Sales to Operating Income Growth to MK Growth","Goodwill to Assets Growth to MK Growth","Free Cash Flow to Sales Growth to MK Growth")]
neuralNetworkPredictionActivity <- predict(neuralNetworkModel, newdata=neuralNetTest)

confusionMatrix(as.factor(beatMarketCaretFull[growthTrainrows]),as.factor(neuralNetworkPredictionActivity[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(neuralNetworkPredictionActivity[growthTestrows]))
#74.36% accurate


#liquidity - Growth to MK Growth and stat to average
neuralNetTrain=growthTrainingSetQ[,c("Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Cash to Working Capital","Future 20 Year Inventory to Working Capital","Future 20 Year Sales to Current Assets","Future 20 Year Sales to Working Capital","Future 20 Year Net Working Capital Ratio","Future 20 Year Acid Test","Future 20 Year Cash Flow Coverage","Future 20 Year Current Ratio","Cash Ratio Growth to MK Growth","Quick Ratio Growth to MK Growth","Cash to Working Capital Growth to MK Growth","Inventory to Working Capital Growth to MK Growth","Sales to Current Assets Growth to MK Growth","Sales to Working Capital Growth to MK Growth","Net Working Capital Ratio Growth to MK Growth","Acid Test Growth to MK Growth","Cash Flow Coverage Growth to MK Growth","Current Ratio Growth to MK Growth")]
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))
set.seed(1235)
neuralNetworkModel<- train(x=neuralNetTrain,y=beatMarketCaret, method = 'nnet', trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
neuralNetTest=growthMyDataQ[,c("Future 20 Year Cash Ratio","Future 20 Year Quick Ratio","Future 20 Year Cash to Working Capital","Future 20 Year Inventory to Working Capital","Future 20 Year Sales to Current Assets","Future 20 Year Sales to Working Capital","Future 20 Year Net Working Capital Ratio","Future 20 Year Acid Test","Future 20 Year Cash Flow Coverage","Future 20 Year Current Ratio","Cash Ratio Growth to MK Growth","Quick Ratio Growth to MK Growth","Cash to Working Capital Growth to MK Growth","Inventory to Working Capital Growth to MK Growth","Sales to Current Assets Growth to MK Growth","Sales to Working Capital Growth to MK Growth","Net Working Capital Ratio Growth to MK Growth","Acid Test Growth to MK Growth","Cash Flow Coverage Growth to MK Growth","Current Ratio Growth to MK Growth","Cash Ratio to Average","Quick Ratio to Average","Cash to Working Capital to Average","Inventory to Working Capital to Average","Sales to Current Assets to Average","Sales to Working Capital to Average","Net Working Capital Ratio to Average","Acid Test to Average","Cash Flow Coverage to Average","Current Ratio to Average")]
neuralNetworkPredictionLiquidity <- predict(neuralNetworkModel, newdata=neuralNetTest)

confusionMatrix(as.factor(as.factor(beatMarketCaretFull[growthTrainrows]),neuralNetworkPredictionLiquidity[growthTrainrows]))
confusionMatrix(as.factor(beatMarketCaretFull[growthTestrows]),as.factor(neuralNetworkPredictionLiquidity[growthTestrows]))
#74.36% accurate

#put all the discriminant scores into a glm
trainData = data.frame(peScoreTrain,futureScoreTrain,profitScoreTrain,valueScoreTrain,growthScoreTrain,solvencyScoreTrain,activityScoreTrain,liquidityScoreTrain)
glmDiscriminantModel<-train(x=trainData,y=beatMarketCaret,method = "glm")
testData=data.frame(peScoreTrain=peScoreFull,futureScoreTrain=futureScoreFull,profitScoreTrain=profitScoreFull,valueScoreTrain=valueScoreFull,growthScoreTrain=growthScoreFull,solvencyScoreTrain=solvencyScoreFull,activityScoreTrain=activityScoreFull,liquidityScoreTrain=liquidityScoreFull)
glmDiscriminantPrediction=predict(glmDiscriminantModel,testData)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(glmDiscriminantPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(glmDiscriminantPrediction[growthTestrows]))
#65.95745

#put all the discriminant scores into a gbm
gbmTrain = data.frame(peScoreTrain,futureScoreTrain,profitScoreTrain,valueScoreTrain,growthScoreTrain,solvencyScoreTrain,activityScoreTrain,liquidityScoreTrain)
set.seed(1235)
GBDecisionTree = train(x = gbmTrain,y=beatMarketCaret, method = "gbm", trControl=trainControlGrowth, verbose=FALSE)
GBDecisionTree
testTrain=data.frame(peScoreTrain=peScoreFull,futureScoreTrain=futureScoreFull,profitScoreTrain=profitScoreFull,valueScoreTrain=valueScoreFull,growthScoreTrain=growthScoreFull,solvencyScoreTrain=solvencyScoreFull,activityScoreTrain=activityScoreFull,liquidityScoreTrain=liquidityScoreFull)
gbmDiscriminantPrediction <- predict(GBDecisionTree, testTrain)

full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(gbmDiscriminantPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(gbmDiscriminantPrediction[growthTestrows]))
#70.21277% predictive



#put all the discriminant scores into a SVM
svmTrain=data.frame(peScoreTrain,futureScoreTrain,profitScoreTrain,valueScoreTrain,growthScoreTrain,solvencyScoreTrain,activityScoreTrain,liquidityScoreTrain)
set.seed(1235)
svmLinearModel <- train(x=svmTrain,y=beatMarketCaret,method = "svmLinear",trControl=trainControlGrowth,tuneLength = 10)
svmTrain=data.frame(peScoreTrain=peScoreFull,futureScoreTrain=futureScoreFull,profitScoreTrain=profitScoreFull,valueScoreTrain=valueScoreFull,growthScoreTrain=growthScoreFull,solvencyScoreTrain=solvencyScoreFull,activityScoreTrain=activityScoreFull,liquidityScoreTrain=liquidityScoreFull)
svmDiscriminantPrediction=predict(svmLinearModel,svmTrain)
full_Train_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTrainrows,theTarget]),as.numeric(svmDiscriminantPrediction[growthTrainrows]))
full_Test_Accuracy <- calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(svmDiscriminantPrediction[growthTestrows]))
#74.35897% accurate 









#combines profit models
#61.53% accuracy - 2017 Q4
finalProfitPrediction=as.numeric(ldaPredictionProfit)+as.numeric(svmPredictionProfit)+
  as.numeric(gbmProfitPrediction)+as.numeric(stepWisePredictionProfit)
beatIndexes=which(finalProfitPrediction>6)
finalProfitPrediction[beatIndexes]=2
finalProfitPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalProfitPrediction[growthTestrows])

#84.61% accuracy - 2017 Q4
finalValuePrediction=as.numeric(ldaPredictionValue)+as.numeric(svmPredictionValue)+
  as.numeric(gbmValuePrediction)+as.numeric(stepWisePredictionValue)
beatIndexes=which(finalValuePrediction>6)
finalValuePrediction[beatIndexes]=2
finalValuePrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalValuePrediction[growthTestrows])



#76.92% accuracy - 2017 Q4
finalSolvencyPrediction=as.numeric(ldaPredictionSolvency)+as.numeric(svmPredictionSolvency)+
  as.numeric(stepWisePredictionSolvency)+as.numeric(gbmSolvencyPrediction)
beatIndexes=which(finalSolvencyPrediction>6)
finalSolvencyPrediction[beatIndexes]=2
finalSolvencyPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalSolvencyPrediction[growthTestrows])



#50% accuracy - 2017 Q4
finalLiquidityPrediction=as.numeric(ldaPredictionLiquidity)+as.numeric(svmPredictionLiquidity)+
  as.numeric(stepWisePredictionLiquidity)+as.numeric(gbmLiquidityPrediction)
beatIndexes=which(finalLiquidityPrediction>6)
finalLiquidityPrediction[beatIndexes]=2
finalLiquidityPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalLiquidityPrediction[growthTestrows])

#57.69% accuracy - 2017 Q4
finalGrowthPrediction=as.numeric(ldaPredictionGrowth)+as.numeric(svmPredictionGrowth)+
  as.numeric(stepWisePredictionGrowth)+as.numeric(gbmGrowthPrediction)
beatIndexes=which(finalGrowthPrediction>6)
finalGrowthPrediction[beatIndexes]=2
finalGrowthPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalGrowthPrediction[growthTestrows])


#57.69 accuracy - 2017 Q4
finalActivityPrediction=as.numeric(ldaPredictionActivity)+as.numeric(svmPredictionActivity)+
  as.numeric(stepWisePredictionActivity)+as.numeric(gbmActivityPrediction)
beatIndexes=which(finalActivityPrediction>6)
finalActivityPrediction[beatIndexes]=2
finalActivityPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),finalActivityPrediction[growthTestrows])



#76.92% accuracy - 2017 Q4
finalFuturePrediction=as.numeric(stepWisePredictionFuture)+as.numeric(svmPredictionFuture)+
  as.numeric(ldaPredictionFuture)+as.numeric(gbmPredictionFuture)
beatIndexes=which(finalFuturePrediction>6)
finalFuturePrediction[beatIndexes]=2
finalFuturePrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(finalFuturePrediction[growthTestrows]))

#73.07% accuracy - 2017 Q4
finalPEPrediction=as.numeric(stepWisePredictionPE)+as.numeric(svmPredictionPE)+
  as.numeric(ldaPredictionPE)+as.numeric(gbmPredictionPE)
beatIndexes=which(finalPEPrediction>6)
finalPEPrediction[beatIndexes]=2
finalPEPrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(finalPEPrediction[growthTestrows]))



#73.07 accuracy - 2017 Q4
finalMarketAveragePrediction=as.numeric(stepWisePredictionMarketAverage)+as.numeric(svmPredictionMarketAverage)+
  as.numeric(ldaPredictionMarketAverage)+as.numeric(gbmPredictionMarketAverage)
beatIndexes=which(finalMarketAveragePrediction>6)
finalMarketAveragePrediction[beatIndexes]=2
finalMarketAveragePrediction[-beatIndexes]=1
calc_accuracy(as.numeric(growthMyDataQ[growthTestrows,theTarget]),as.numeric(finalMarketAveragePrediction[growthTestrows]))





#final prediction of all models
sumFullScore=scale(sumFullScore, center = T, scale = F)
sumFullScore2=scale(sumFullScore2, center = T, scale = F)


finalPrediction=
  as.numeric(neuralNetworkPredictionSolvency)+as.numeric(neuralNetworkPredictionGrowth)+
  as.numeric(neuralNetworkPredictionActivity)+as.numeric(neuralNetworkPredictionLiquidity)+

  as.numeric(stepWisePredictionMarketAverage)+as.numeric(svmPredictionMarketAverage)+
  as.numeric(gbmPredictionMarketAverage)+as.numeric(ldaPredictionMarketAverage)+
  as.numeric(stepWisePredictionPE)+as.numeric(svmPredictionPE)+
  as.numeric(gbmPredictionPE)+as.numeric(ldaPredictionPE)+
  as.numeric(stepWisePredictionFuture)+as.numeric(svmPredictionFuture)+
  as.numeric(gbmPredictionFuture)+as.numeric(ldaPredictionFuture)+
  as.numeric(ldaPredictionActivity)+as.numeric(svmPredictionActivity)+
  as.numeric(stepWisePredictionActivity)+as.numeric(gbmActivityPrediction)+
  as.numeric(ldaPredictionLiquidity)+as.numeric(svmPredictionLiquidity)+
  as.numeric(stepWisePredictionLiquidity)+as.numeric(gbmLiquidityPrediction)+
  as.numeric(ldaPredictionGrowth)+as.numeric(svmPredictionGrowth)+
  as.numeric(stepWisePredictionGrowth)+as.numeric(gbmGrowthPrediction)+
  as.numeric(ldaPredictionSolvency)+as.numeric(svmPredictionSolvency)+
  as.numeric(stepWisePredictionSolvency)+as.numeric(gbmSolvencyPrediction)+
  as.numeric(ldaPredictionValue)+as.numeric(svmPredictionValue)+
  as.numeric(gbmValuePrediction)+as.numeric(stepWisePredictionValue)+
  as.numeric(svmPredictionProfit)+as.numeric(ldaPredictionProfit)+
  as.numeric(gbmProfitPrediction)+as.numeric(stepWisePredictionProfit)+

  as.numeric(tTestPredictionRegGLM)+as.numeric(tTestPredictionRegSVM)+
  as.numeric(tTestPredictionRegGBM)+
  as.numeric(tTestPredictionGrowthGLM)+as.numeric(tTestPredictionGrowthSVM)+
  as.numeric(tTestPredictionGrowthGBM)+
  as.numeric(tTestPredictionPEGLM)+as.numeric(tTestPredictionPESVM)+
  as.numeric(tTestPredictionPEGBM)+
  as.numeric(tTestPredictionFutureGLM)+as.numeric(tTestPredictionFutureSVM)+
  as.numeric(tTestPredictionFutureGBM)+
  as.numeric(tTestPredictionMarketGLM)+as.numeric(tTestPredictionMarketSVM)+
  as.numeric(tTestPredictionMarketGBM)+
  as.numeric(tTestPredictionCategoryGLM)+as.numeric(tTestPredictionCategorySVM)+
  as.numeric(tTestPredictionCategoryGBM)+
  as.numeric(tTestPredictionSolvencyGLM)+as.numeric(tTestPredictionSolvencySVM)+
  as.numeric(tTestPredictionSolvencyGBM)+
  as.numeric(tTestPredictionActivityGLM)+as.numeric(tTestPredictionActivitySVM)+
  as.numeric(tTestPredictionActivityGBM)+
  as.numeric(tTestPredictionLiquidityGLM)+as.numeric(tTestPredictionLiquiditySVM)+
  as.numeric(tTestPredictionLiquidityGBM)+
  as.numeric(tTestPredictionProfitGLM)+as.numeric(tTestPredictionProfitSVM)+
  as.numeric(tTestPredictionProfitGBM)+
  as.numeric(tTestPredictionValueGLM)+as.numeric(tTestPredictionValueSVM)+
  as.numeric(tTestPredictionValueGBM)+
  
  as.numeric(ldaPredictionProfit2)+as.numeric(ldaPredictionValue2)+
  as.numeric(ldaPredictionGrowth2)+as.numeric(ldaPredictionActivity2)+
  as.numeric(ldaPredictionSolvency2)+as.numeric(ldaPredictionLiquidity2)+
  as.numeric(ldaPredictionPE2)+as.numeric(ldaPredictionFuture2)+
  as.numeric(ldaPredictionMarketAverage2)+

  as.numeric(neuralNetworkPredictionDiscriminant)+as.numeric(svmDiscriminantPrediction)+
  as.numeric(gbmDiscriminantPrediction)+as.numeric(glmDiscriminantPrediction)+
  
  finalProfitPrediction+finalValuePrediction+
  finalSolvencyPrediction+finalLiquidityPrediction+
  finalActivityPrediction+finalGrowthPrediction+
  finalFuturePrediction+finalPEPrediction+
  finalMarketAveragePrediction+
  
  as.numeric(finaltTestRegPrediction)+as.numeric(finaltTestMarketPrediction)+
  as.numeric(finaltTestFuturePrediction)+as.numeric(finaltTestPEPrediction)+
  as.numeric(finaltTestGrowthPrediction)+as.numeric(finaltTestCategoryPrediction)+
  as.numeric(finaltTestProfitPrediction)+as.numeric(finaltTestValuePrediction)+
  as.numeric(finaltTestSolvencyPrediction)+as.numeric(finaltTestLiquidityPrediction)+
  as.numeric(finaltTestActivityPrediction)+
  sumFullScore+sumFullScore2


quantile(finalPrediction)
#100% specifity - 2017 Q4 with finalPrediction>=47
beatIndexes=which(finalPrediction>=219)
finalPrediction2=finalPrediction
finalPrediction2[beatIndexes]=1
finalPrediction2[-beatIndexes]=0
finalPrediction2=as.factor(finalPrediction2)


confusionMatrix(table(growthMyDataQ[growthTestrows,theTarget],finalPrediction2[growthTestrows]))









median=median(stockDataframeQTemp$"Annual Price Change %")


finalPredictionTest=finalPrediction[growthTestrows]
predictionOrder=order(finalPredictionTest,decreasing = TRUE)

predictionOrderTop=predictionOrder[1:15]


growthTestSetQ[predictionOrderTop,c("Price Change %","Stock","Beat Market")]
summary(growthTestSetQ[predictionOrderTop,"Price Change %"])
summary(testSetQ$"Price Change %")

