# Stock-Predictor
  This project used R and R Studio to create machine learning algorithms to predict whether a stock would go up or not.  
  
  The file Stock Market Research.docx is an explanation of my research in the format of a research paper, and the file Stock Market Results.xlsx is the results of my research if my model was used to make investment decisions from the last quarter of 2011 to the first quarter of 2018.  This model achieved a 384.89% return during this period which was just under three times better than how the S&P500 performed.  
  
  The code for this project is in two file formats: R and txt, so the code can be analyzed with and without RStudio.  The StockDataCreatorQ file creates the data frame of 326 stocks from the last quarter of 2011 to the first quarter of 2018.  The PredictionModelQ file runs the machine learning algorithms on the data frame and picks the top 15 stocks that are most likely to beat the market.  The DowJonesCreatorQ file creates the data frame of every stock in the Dow Jones Industrial Average, so that the features of the other data frame can be compared to the Dow Jones to create new features in the other data frame.
