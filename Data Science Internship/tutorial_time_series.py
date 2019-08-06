
import pandas as pd 
import matplotlib.pyplot as plt 
import seaborn as sns

pd.to_datetime('2018-01-15 3:45pm')
pd.to_datetime('7/8/1952')
opsd_daily = pd.read_csv('/Users/aaronkruchten/Desktop/Data analysis internship/opsd_germany_daily.csv')

print(opsd_daily.shape)
#print(opsd_daily.Date)
#print(opsd_daily.tail(3))


opsd_daily.Date = pd.to_datetime(opsd_daily.Date)

opsd_daily = opsd_daily.set_index('Date')

#print(opsd_daily.dtypes)

print(opsd_daily.head(3))

print(opsd_daily.index)

opsd_daily['Year'] = opsd_daily.index.year
opsd_daily['Month'] = opsd_daily.index.month
opsd_daily['Weekday'] = opsd_daily.index.weekday_name

print(opsd_daily.sample(5,random_state = 0))


print(opsd_daily.loc['2017-08-10'])

print(opsd_daily.loc['2017-08-10':'2017-08-15'])

import matplotlib.pyplot as plt 
import seaborn as sns
sns.set(rc = {'figure.figsize':(11,4)})



cols_plot = ['Consumption','Solar','Wind']
axes = opsd_daily[cols_plot].plot(marker = '.',alpha = 0.5, linestyle = 'None', figsize = (11,9), subplots = True)
for ax in axes:
	ax.set_ylabel('Daily Totals (GWh)')

opsd_daily['Consumption'].plot(linewidth = 0.5);


ax = opsd_daily.loc['2017','Consumption'].plot()
ax.set_ylabel('Daily Consumption (GWh)');


ax = opsd_daily.loc['2017-01':'2017-02','Consumption'].plot(marker = 'o',linestyle = '-')
ax.set_ylabel('Daily Consumption (GWh');


import matplotlib.dates as mdates
fig, ax = plt.subplots()
ax.plot(opsd_daily.loc['2017-01':'2017-02','Consumption'], marker = 'o',linestyle = '-')
ax.set_ylabel('Daily Consumption (GWh)')
ax.set_title('Jan-Feb 2017 Electricity Consumption')
ax.xaxis.set_major_locator(mdates.WeekdayLocator(byweekday = mdates.MONDAY))
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %d'));

fig, axes = plt.subplots(3, 1, figsize=(11, 10), sharex=False)
for name, ax in zip(['Consumption', 'Solar', 'Wind'], axes):
	sns.boxplot(data=opsd_daily, x='Month', y= name, ax= ax)
	ax.set_ylabel('GWh')
	ax.set_title(name)
# Remove the automatic x-axis label from all but the bottom subplot
if ax != axes[-1]:
	ax.set_xlabel('')




date_range = pd.date_range('1998-03-10','1998-03-15', freq = 'H')
#print(date_range)

times_sample = pd.to_datetime(['2013-02-03','2013-02-06','2013-02-08'])
consum_sample = opsd_daily.loc[times_sample,['Consumption']].copy()


consum_freq = consum_sample.asfreq('D')

consum_freq['Consumption - Forward Fill'] = consum_sample.asfreq('D', method = 'ffill')

#print(consum_freq)

data_columns = ['Consumption','Wind','Solar','Wind+Solar']
opsd_weekly_mean = opsd_daily[data_columns].resample('W').mean()
print(opsd_weekly_mean.head(3))


print(opsd_daily.shape[0])
print(opsd_weekly_mean.shape[0])

start = '2017-01'
end = '2017-06'

opsd_monthly_mean = opsd_daily[data_columns].resample('M').mean()

opsd_7d = opsd_daily[data_columns].rolling(7,center = True).mean()

fig,ax = plt.subplots()
ax.plot(opsd_daily.loc[start:end,'Solar'],marker = '.',linestyle = '-', linewidth = 0.5, label = 'Daily')
#ax.plot(opsd_weekly_mean.loc[start:end,'Solar'], marker = 'o', markersize = 8, linestyle = '-', label = 'Weekly Mean Resample')
#ax.plot(opsd_monthly_mean.loc[start:end,'Solar'], marker = 'o', markersize = 8, linestyle = '-', label = "Monthly Mean Resample")
ax.plot(opsd_7d.loc[start:end,'Solar'], marker = 'o', markersize = 3, linestyle = '-', label = "Rolling 7 day average")
ax.set_ylabel('Solar Production (GWh)')
ax.legend();


opsd_monthly = opsd_daily[data_columns].resample('M').sum(min_count = 28)
print(opsd_monthly.head(3))

fig, ax = plt.subplots()
ax.plot(opsd_monthly['Consumption'], color = 'black', label = 'Consumption')
opsd_monthly[['Wind','Solar']].plot.area(ax = ax, linewidth = 0)
ax.xaxis.set_major_locator(mdates.YearLocator())
ax.legend()
ax.set_ylabel('Monthly Total (GWh)')


opsd_annual = opsd_daily[data_columns].resample('A').sum(min_count = 360)
opsd_annual = opsd_annual.set_index(opsd_annual.index.year)
opsd_annual.index.name = 'Year'
opsd_annual['Wind+Solar/Consumption'] = opsd_annual['Wind+Solar'] / opsd_annual['Consumption']
opsd_annual.tail(3)


ax = opsd_annual.loc[2012:,'Wind+Solar/Consumption'].plot.bar(color = 'C0')
ax.set_ylabel('Fraction')
ax.set_ylim(0,0.3)
ax.set_title('Wind + Solar Share of Annual Electricity Consumption')
plt.xticks(rotation = 0)
plt.show()




