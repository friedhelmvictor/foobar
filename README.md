# TELEGRAM PUMP AND DUMP PAPER

## Data
- telegramPnD.sqlite - contains the telegram messages with labeled pump messages
- pumpStats.csv - contains trading information on those labeled pumps

The underlying raw tick data is too large to upload, but the python script in the trade_fetcher folder allows to extract historical trades

## Structure
- quantification.R - quantifies the pumps
- machine-learning.R - ML model to classify pumps
