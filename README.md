# stock_EMA
Single stock exponential moving average buy and sell signals.

Explanation
-----------
This project optimizes a moving average crossover system for a number of risk and return measures for a single stock. Currently the model runs separately for long and short optimization. Tell the model the range of both the fast and slow exponential moving average (EMA). The data file needs time and OHLC data. That's all you need to run it.

The return values created for each combination are instantaneous compound annual growth rate (ICAGR) and ending dollar value, based on a $10,000 starting value. 

Risk measures include drawdown, measured as the greatest decline in equity from the highwater mark, and the Lake ratio, which is the accumulated fraction of every drawdown divided by equity in each period. Whereas the drawdown is a risk measure for one point in time, the greatest percentage loss, the Lake ratio conveys the average or expected drawdown experience over the time of the data. Lower numbers are better for both measures. 

The bliss number combines both risk and return into one measure. Bliss is caluculated as the ICAGR/ drawdown, which means how fast the strategy of the specific EMA crossover pair will recover the loss of the drawdown. Bliss2 is the ICAGR/ Lake ratio, which relates the growth rate of the entire strategy to the expected drawdown. Higher numbers are better for both. The bliss measure was created by Ed Seykota. 

The file main.R sets up the process. The file run.R executes the optimization. The file file_list.R searches csv files for data and shows the start date, end date and time interval. The viz.R script shows 3D renderings of the results of the optimization by ICAGR, bliss, bliss2, drawdown and lake ratio. The viz2.R file shows scatterplots of the top 50 runs  (as measured by bliss2) by different risk and return measures:
  ICAGR vs drawdown
  Lake ratio vs bliss
  Lake ratio vs drawdown
  Ending value vs drawdown
  ICAGR vs Lake ratio
  Bliss vs dardown

The viz3.R file shows the results of individual runs. So in the use case where you complete your runs, evaluate your results based on risk and return, and select a few EMA pairs that look promising, viz3.R will show the actual EMAs and trades, winners and losers, overlaid on top of the price chart. 

Operation
---------
Run main.R first.
Setup the run in lines numbererd in the 20s and 30s.

Set the Long or Short field.

The ticker variable goes in the headers of the graphs but is not necessary for operation. It does not change any numbers but it sure helps when you are awash in output from the model.

The fast/slow and high/low/step varaibles set the envelope for the moving averages.

The data file "BATS_ULTA, 5_ec9c3.csv" is stored on github. Run that one if the file is different. This is an interesting data file as in about 14 months of 5 minute data there was all sorts of interesting price movement, rallies, sell offs, sideways and spikes. Who cold ask for more? The variety of scenarios in the training data give confidence that the resulting EMA pair selected wold be somewhat successful in any of those environments. 

Then move over to the run.R file.

Using keyboard shortcuts helps while running the model.

Option, command B will run the file from the beginning to the cursor.
Option, command E will run from teh cursor to the end of the file.
These are Mac keyboard shortcuts. PC shortcuts are similar.
Option shift K brings up the keyboard shortcut menu for R Studio.

Using the shortcuts above for all the runs, instead of sourcing them, will show graphs while running and after finishing. Depending on the computer, the runs may take a few minutes. 

