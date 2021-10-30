# -*- coding: utf-8 -*-

import pandas as pd
import glob as glob
import numpy as np
import matplotlib.pyplot as plt


import os
os.getcwd()
os.chdir("C:\\Users\\Ankit\\Desktop\\study\\project\\stocknews")

### Taking Apple's Stock and dow jones stock data
apl = pd.read_csv('AAPL.csv', parse_dates=['Date'])
djt =   pd.read_csv('DJIA_table.csv', parse_dates=['Date'])

####### Checking whether there is sufficient data to analyze.
apl.count() #1989
djt.count()  #1989

######################### EDA ###############
apl.count()
apl.head()
apl.describe()     ####### Mean for 'Close' is higher than that of median
np.mean(apl.Close) ####### Right skewed
apl.isnull()       ####### No missing values

djt.count()
djt.head()
djt.describe()     ####### Mean for 'Close' is higher than that of median
np.mean(apl.Close) ####### Right skewed
djt.isnull()       ####### No missing values
 



#####################################################
######### Closing price distribution of AAPL######
#####################################################

import matplotlib.dates as mdates
import seaborn as sns


color = sns.color_palette()
df = apl
df.head()
df['Date_mpl'] = df['Date'].apply(lambda x: mdates.date2num(x))


fig, ax = plt.subplots(figsize=(12,8))
sns.tsplot(df.Close.values, time=df.Date_mpl.values, alpha=0.8, color=color[3], ax=ax)
ax.xaxis.set_major_locator(mdates.AutoDateLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y.%m.%d'))
fig.autofmt_xdate()
plt.xlabel('Date', fontsize=12)
plt.ylabel('Price in USD', fontsize=12)
plt.title("Closing price distribution of bitcoin", fontsize=15)
plt.show()


fields = ['Date', 'Close']
series = pd.read_csv('AAPL.csv', parse_dates=['Date'], index_col =0, usecols = fields, squeeze = True)
series.hist()
#######there are many peaks in our data


from statsmodels.tsa.stattools import adfuller
result = adfuller(series)
print('ADF Statistic: %f' % result[0])
print('p-value: %f' % result[1])
print('Critical Values:')
for key, value in result[4].items():
	print('\t%s: %.3f' % (key, value))    
########### Accepting H0 - time series is non-stationary

########################################################
######### Closing price distribution of DOW JONES######
########################################################

import matplotlib.dates as mdates
import seaborn as sns


color = sns.color_palette()
df = djt
df.head()
df['Date_mpl'] = df['Date'].apply(lambda x: mdates.date2num(x))


fig, ax = plt.subplots(figsize=(12,8))
sns.tsplot(df.Close.values, time=df.Date_mpl.values, alpha=0.8, color=color[3], ax=ax)
ax.xaxis.set_major_locator(mdates.AutoDateLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y.%m.%d'))
fig.autofmt_xdate()
plt.xlabel('Date', fontsize=12)
plt.ylabel('Price in USD', fontsize=12)
plt.title("Closing price distribution of bitcoin", fontsize=15)
plt.show()


fields = ['Date', 'Close']
seriesdj = pd.read_csv('DJIA_table.csv', parse_dates=['Date'], index_col =0, usecols = fields, squeeze = True)
series.hist()
#######there are many peaks in our data


from statsmodels.tsa.stattools import adfuller
result = adfuller(seriesdj)
print('ADF Statistic: %f' % result[0])
print('p-value: %f' % result[1])
print('Critical Values:')
for key, value in result[4].items():
	print('\t%s: %.3f' % (key, value))    
########### Accepting H0 - time series is non-stationary






