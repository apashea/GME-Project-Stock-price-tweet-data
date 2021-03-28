# GME-Project-Stock-price-tweet-data
This project demonstrates use of textual analysis and visualization. See also the uploaded image files which refer to the outputs in the script.

This R script does the following:
- Imports, subsets, and visualizes stock prices for GME
- Shows how to authenticate and use the Twitter API in R
- Extracts Tweet data containing the #GME hashtag, writes and loads it as a .csv file, and visualizes tweet frequencies using three-hour time interval
- Uses textual analysis packages to:
	- Import stop words, create custom stop word lists, and remove stop words from our tweet word counts
	- Find word counts
	- Visualize wordclouds
	- Output topic models (here a latent dirichlet allocation model which finds 'latent' patterns of particular words used across our documents, i.e. our tweets. We use Gibbs sampling to output four topics).
	- Visualize topic models and appearance proportions per day.
- Finds subsets of tweets most related to individual topics from topic models for deeper inspection
- Plots topic appearance per tweet's timestamp versus the adjusted close stock price, which might be used conceptually to study the relationship between stock price and twitter topics (inspired by economist Thomas Renault's 2017 study of intraday StockTwits sentiment analysis versus stock price changes; citation included in the script)
- Basic sentiment analysis, including top sentiments found and visualization of these topic sentiments and their respective top word frequencies
