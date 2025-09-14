###Shiny App functions:
-The app takes a company stock symbol/name, a date (ranging from Jan 1st 2000 to current day), and timeframe (intra-day, month, year, or decades) as input and generates:
	1) A summary table stating the open-high-low-close (OHLC) prices of the company's stock over the given timeframe.
	2) A stock close price plot for the company over the given timeframe.
	3) A stock OHLC candlestick plot over the given timeframe
-The app uses the Alpha Vantage family of stock APIs to achives these functions.
-The API is accessed twice per use of the action button:
	1) To find the closest match stock symbol based on the text input. 
	2) To obtain the OHLC dataset over the specified timeframe. 
-The dataframe and plots generated are interactive

Example:

The following screenshots use the IMB stock data:
![alt text](https://github.com/sreeman-reddy/stocks-ohlc-shiny-app/blob/main/example1.png "Main Search")

![alt text](https://github.com/sreeman-reddy/stocks-ohlc-shiny-app/blob/main/example2.png "Stock Table")

![alt text](https://github.com/sreeman-reddy/stocks-ohlc-shiny-app/blob/main/example3.png "Data Graph")

![alt text](https://github.com/sreeman-reddy/stocks-ohlc-shiny-app/blob/main/example4.png "OHLC Plot")



New API key limit reached issue-
-The API has a 25 calls limit per day.
-New API keys generated give a limit reached error.
-I have tried running the code with 7 new keys but I still get a "limit reached" message.
-All instances of GET() function in the project code work properly against the example urls provided in the Alpha Vantage documentation when api_key is set to "demo". So I still do not know why a new API key is giving a limit reached message.






