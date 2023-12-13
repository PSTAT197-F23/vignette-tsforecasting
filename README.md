# Vignette: Time Series Forecasting

> Vignette on implementing ARIMA and LSTM forecasting using time series data; created as a class project for PSTAT197A in Fall 2023.

**Contributors:** Patrick Moon, Sharon Lee, Matthew Lee

**Vignette abstract:** In this vignette, we attempt to predict weekly gasoline prices in California by applying time series forecasting methods, specifically ARIMA (AutoRegressive Integrated Moving Average) and LSTM (Long Short_Term Memory). Our data contains the average weekly gasoline price from May 2000 to November 2023. For the outcomes, we examined MAE (Mean Absolute Error) metrics to compare the performance of ARIMA and LSTM, and as a result, the LSTM method did a better job of predicting future gasoline prices.

**Repository contents:** In the root repository, you can find `vignette.qmd` which is our primary vignette document that shows time series forecasting methods(LSTM, ARIMA) with step-by-step explanations. Additional respository contents include:

-   `data`: contains `rawdata.csv` and `processed.csv`

-   `scripts`: contains a `drafts` folder, `html` folder, `EDA.Rmd`, `ARIMA Modeling.Rmd`,`LSTM Modelling.Rmd` file
    -   `drafts`: contains draft scripts for the scratchwork
    -   `html`: contains html output from the Rmd files below
    -   `EDA.Rmd`: script containing code for exploratory data analysis
    -   `ARIMA Modeling.Rmd`: script containing code for ARIMA Modeling with line annotations
    -   `LSTM Modelling.Rmd`: script containing code for LSTM Modeling with line annotations

-   `plots` contains .png files of necessary plots we used in the vignette document

**Reference list:** The following are references:

1.  https://keras.io/examples/timeseries/timeseries_weather_forecasting/
2.  https://towardsdatascience.com/an-end-to-end-project-on-time-series-analysis-and-forecasting-with-python-4835e6bf050b
