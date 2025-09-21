# -*- coding: utf-8 -*-
"""
Created on Fri Feb 17 17:12:02 2017

@author: user
"""
#code written in this file is not efficient
#it should be rewritten if must be shown to someone who knows pandas well
#.at, append, parse_dates

import pandas
import os
import string
#import numpy

from pandas import read_csv
from datetime import datetime
from datetime import date

def ReturnBasicStat(vec):
    return([vec.min()/vec.quantile(0.05), vec.min()/vec.median(), 
     vec.quantile(0.95)/vec.max(), vec.median()/vec.max()])
 #  vec = (vec - vec.mean())/vec.std()
 #  return([vec.min(), vec.mean(), vec.median(), vec.max()])  

def GetTabAlt(storagepath, afile, ispure = False):
    if ispure:
        tab = read_csv(storagepath + '\\' + afile, sep = ';', 
                       dtype = {'TICKER': object,
                                'DELETEME': int,
                                'DATE': object,
                                'TIME': object,
                                'OPEN': float,
                                'HIGH': float,
                                'LOW': float,
                                'CLOSE': float,
                                'VOL': int,
                                'IsCorrect': int,
                                'HasGap': int,
                                'SplitDay': int})        
        tab['dt'] = pandas.to_datetime(tab['DATE'] + ' ' + tab['TIME'], format = '%Y%m%d %H:%M:%S')       
        tab['DATE'] = tab['dt'].dt.date
        tab['HasGap'] = tab['HasGap'].apply(lambda x: bool(x))
        tab['IsCorrect'] = tab['IsCorrect'].apply(lambda x: bool(x))
        tab['SplitDay'] = tab['SplitDay'].apply(lambda x: bool(x))        
        return(tab)
    else:
        tab = read_csv(storagepath + '\\' + afile, sep = ";", header = None,
                       names = ['TICKER', 'DELETEME', 'DATE', 'TIME',
                            'OPEN', 'HIGH', 'LOW', 'CLOSE', 'VOL'],
                       dtype = {'TICKER': object,
                            'DELETEME': int,
                            'DATE': object,
                            'TIME': object,
                            'OPEN': float,
                            'HIGH': float,
                            'LOW': float,
                            'CLOSE': float,
                            'VOL': int},
                       decimal = ',')
        tab['dt'] = pandas.to_datetime(tab['DATE'] + ' ' + tab['TIME'], format = '%d.%m.%Y %H:%M:%S')
        tab['DATE'] = tab['dt'].dt.date
        return(tab.drop_duplicates(keep = 'first'))
 
def GetTab(storagepath, afile, ispure = False):
    if ispure:
        tab = read_csv(storagepath + '\\' + afile, sep = ';', 
                       dtype = {'TICKER': object,
                                'DATE': object,
                                'TIME': object,
                                'OPEN': float,
                                'HIGH': float,
                                'LOW': float,
                                'CLOSE': float,
                                'IsCorrect': int,
                                'HasGap': int,
                                'SplitDay': int}) 
        #tab['dt'] = pandas.to_datetime(tab['dt'], format = "%Y-%m-%d %H:%M:%S")
        tab['dt'] = pandas.to_datetime(tab['DATE'] + ' ' + tab['TIME'], format = '%Y%m%d %H%M%S')       
        tab['DATE'] = tab['dt'].dt.date
        tab['HasGap'] = tab['HasGap'].apply(lambda x: bool(x))
        tab['IsCorrect'] = tab['IsCorrect'].apply(lambda x: bool(x))
        tab['SplitDay'] = tab['SplitDay'].apply(lambda x: bool(x))
    else:
        print(storagepath + '\\' + afile)
        tab = read_csv(storagepath + '\\' + afile, sep = ';', 
                       usecols = [0, 2, 3, 4, 5, 6, 7], 
                       dtype = {'<TICKER>': object, 
                                '<DATE>': object,
                                '<TIME>': object,
                                '<OPEN>': float,
                                '<HIGH>': float,
                                '<LOW>': float,
                                '<CLOSE>':float})
        tab.columns = ['TICKER', 'DATE', 'TIME', 'OPEN', 'HIGH', 'LOW', 'CLOSE']
        tab['dt'] = pandas.to_datetime(tab['DATE'] + ' ' + tab['TIME'], format = '%Y%m%d %H%M%S')
        tab['DATE'] = tab['dt'].dt.date
    return(tab)   

def AnomalPriceSeekerFile(tab, mycols):
    dates = tab.DATE.unique()
    statfordates = pandas.DataFrame(columns = mycols)
    count = 0
    for adate in dates:
        tabday = tab.loc[tab.DATE == adate]
        statopen = ReturnBasicStat(tabday['OPEN'])
        stathigh = ReturnBasicStat(tabday['HIGH'])
        statlow = ReturnBasicStat(tabday['LOW'])
        statclose = ReturnBasicStat(tabday['CLOSE'])
        totstat = statopen + stathigh + statlow + statclose            
        statfordates.loc[count] = [adate] + totstat
        count += 1
    return(statfordates)
    
def AnomalPriceSeeker(storagepath, ispure, getdatfunc):
    cols = string.ascii_letters[:17]
    mycols = list()
    for ind in cols: mycols.append(ind)
    print(mycols)
    largecols = ['ticker'] + ['min' + acol for acol in mycols[1:17]] 
    largecols = largecols + ['med' + acol for acol in mycols[1:17]]
    largecols = largecols + ['max' + acol for acol in mycols[1:17]]
    print(largecols)
    filelist = os.listdir(storagepath)
    comstats = pandas.DataFrame(columns = largecols)
    globcount = 0
    for afile in filelist:
        tab = getdatfunc(storagepath, afile, ispure)
        statfordates = AnomalPriceSeekerFile(tab, mycols)
        astat = [tab['TICKER'][0]]
        print(astat)
        for j in cols[1:]:
            astat = astat + [statfordates[j].min()]
      #  return(astat)
        for j in cols[1:]:
            astat = astat + [statfordates[j].median()]
        for j in cols[1:]:
            astat = astat + [statfordates[j].max()]
        comstats.loc[globcount] = astat
        globcount += 1
    return(comstats)
    
def GapsSeeker(storagepath, ispure, getdatfunc):
    comstats = pandas.DataFrame(columns = ['ticker', 'max', 'q95', 'q75', 'ind'])
    filelist = os.listdir(storagepath)
    globcount = 0
    for afile in filelist:
        tab = getdatfunc(storagepath, afile, ispure)
        vec = (abs(tab['OPEN'].diff(1))/tab['OPEN'])[1:]
        print(afile)
        astat = [tab['TICKER'][0], vec.max(), vec.quantile(0.95), vec.quantile(0.75), tab['dt'][vec.idxmax()]] 
        comstats.loc[globcount] = astat
        globcount += 1
    return(comstats)
    
def FreezeTester(storagepath, getdatfunc):
    comstats = pandas.DataFrame(columns = ['ticker', 'maxdur', 'ind'])
    filelist = os.listdir(storagepath)
    globcount = 0
    for afile in filelist:
        tab = getdatfunc(storagepath, afile)
        maxdur = 0
        priceprev = 0
        ind = 0
        row_iterator = tab.iterrows()
        for i, row in row_iterator:
            price = row['OPEN']
            if abs(price-priceprev)>0.00001:
                priceprev = price
                ind = row['dt']
                maxdur = 0
            else:
                maxdur += 1
        comstats.loc[globcount] = [tab['TICKER'][0], maxdur, ind]
        globcount += 1 
        print(tab['TICKER'][0])
    return(comstats)
    
def MaxTimeGapsSeeker(storagepath, getdatfunc):
    comstats = pandas.DataFrame(columns = ['ticker', 'maxdur', 'ind'])
    filelist = os.listdir(storagepath)
    globcount = 0
    for afile in filelist:
        tab = getdatfunc(storagepath, afile)
        vec = tab['dt'].diff()
        comstats.loc[globcount] = [tab['TICKER'][0], vec.max(), tab['dt'][vec.idxmax()]]
        globcount += 1
        print(tab['TICKER'][0])
    return(comstats)
    
def DayCounter(storagepath, getdatfunc):
    comstats = pandas.DataFrame(columns = ['ticker', 'mincount', 'avcount', 'medcount', 'maxcount'])
    filelist = os.listdir(storagepath)
    globcount = 0
    for afile in filelist:
        tab = getdatfunc(storagepath, afile)
        tabgrouped = (tab.groupby('DATE'))['OPEN'].count()
        print(tab['TICKER'][0])
        astat = [tab['TICKER'][0], tabgrouped.min(), tabgrouped.mean(), tabgrouped.median(), tabgrouped.max()]
        comstats.loc[globcount] = astat
        globcount += 1         
    return(comstats)
    
def FixSplit(tab, datestart, dateend, splitsize):
    tab.loc[tab.DATE <= datestart, ['OPEN', 'CLOSE', 'LOW', 'HIGH']] = tab.loc[tab.DATE <= datestart, ['OPEN', 'CLOSE', 'LOW', 'HIGH']]/splitsize
    tab.loc[tab.DATE.isin([datestart, dateend]), 'SplitDay'] = True
    return(tab)

def CheckDuplicates(storagepath, geddatfunc):
    filelist = os.listdir(storagepath)
    comstats = pandas.DataFrame(columns = ['ticker', 'totnum', 'totunique'])
    globcount = 0
    for afile in filelist:
        tab = geddatfunc(storagepath, afile)
        totnum = len(tab['dt'])
        totdiff = totnum - len(tab['dt'].unique())
        #print(pandas.DataFrame([[tab['TICKER'][0], totnum, totdiff]], columns = ['ticker', 'totnum', 'totunique']))
        #comstats.append(pandas.DataFrame([[tab['TICKER'][0], totnum, totdiff]], columns = ['ticker', 'totnum', 'totunique']), ignore_index = True)
        comstats.loc[globcount] = [tab['TICKER'][0], totnum, totdiff]
        print([tab['TICKER'][0], totnum, totdiff])
        globcount += 1
    return(comstats)
    
def OneStockTestAlt(storagepath, finalpath, gapsize = 0.05, minnumofcandles = 100):
    filelist = os.listdir(storagepath)
    for afile in filelist:
        tab = GetTabAlt(storagepath, afile)
        tab['IsCorrect'] = True
        tab['HasGap'] = False 
        tab['SplitDay'] = False
        print(tab['TICKER'][0])
        #we do not consider some of the stocks
        if tab['TICKER'][0] in ['GWW', 'AGN']:
            continue
        #here we consider very large gaps
        if tab['TICKER'][0] == 'EBAY':    
            tab = FixSplit(tab, date(2015, 7, 17), date(2015, 7, 20), 2.376)
        elif tab['TICKER'][0] == 'BAX':
            tab = FixSplit(tab, date(2015, 6, 30), date(2015, 7, 1), 1.841)
        elif tab['TICKER'][0] == 'DHR':
            tab = FixSplit(tab, date(2016, 7, 1), date(2016, 7, 5), 1.319)
        elif tab['TICKER'][0] == 'CAG':
            tab = FixSplit(tab, date(2016, 11, 9), date(2016, 11, 10), 1.285)
        #here we mark gaps
        tab['mydiff'] = (abs(tab['OPEN'].diff(1))/tab['OPEN'])
        tab['DATENEXT'] = tab['DATE'].shift(1)
        vec = tab['mydiff'][1:]
        if vec.max() >= gapsize:
            dates = tab[tab.mydiff >= gapsize][['DATE', 'DATENEXT']]
            dates = (pandas.concat([dates['DATE'], dates['DATENEXT']])).unique()
            tab.loc[tab.DATE.isin(dates), 'HasGap'] = True
 
        #here we mark days with few data
        tabgrouped = (tab.groupby('DATE')['OPEN']).count()
        dates = (tabgrouped[tabgrouped < minnumofcandles].index).unique()
        tab.loc[tab.DATE.isin(dates), 'IsCorrect'] = False
        tab[['OPEN', 'HIGH', 'LOW', 'CLOSE']] = round(tab[['OPEN', 'HIGH', 'LOW', 'CLOSE']], 2)
        tab['DATE'] = (tab['DATE']).apply(lambda x: x.strftime('%Y%m%d'))
        tab['IsCorrect'] = tab['IsCorrect'].apply(lambda x: int(x))
        tab['HasGap'] = tab['HasGap'].apply(lambda x: int(x))
        tab['SplitDay'] = tab['SplitDay'].apply(lambda x: int(x))
        tab.to_csv(finalpath + '\\' + afile, sep = ';', 
                   columns = ['TICKER', 'DATE', 'TIME', 'OPEN', 'HIGH', 'LOW', 'CLOSE', 'IsCorrect', 'HasGap', 'SplitDay'],
                   index = False)
            
def OneStockTest(storagepath, finalpath, gapsize = 0.05, minnumofcandles = 100):
    filelist = os.listdir(storagepath)
    for afile in filelist:
        tab = GetTab(storagepath, afile)
        tab['IsCorrect'] = True
        tab['HasGap'] = False 
        tab['SplitDay'] = False
        print(tab['TICKER'][0])
        #we do not consider some of the stocks
        if tab['TICKER'][0] in ['TWTR', 'AMZN', 'TSLA', 'GOOG']:
            continue
        #here we consider very large gaps
        if tab['TICKER'][0] == 'MA':
            tab.loc[tab.dt <= datetime(2014, 1, 22, 0, 59), ['OPEN', 'HIGH', 'LOW', 'CLOSE']] = tab.loc[tab.dt <= datetime(2014, 1, 22, 0, 59), ['OPEN', 'HIGH', 'LOW', 'CLOSE']]/10
            tab.loc[tab.DATE == date(2014, 1, 22), 'SplitDay'] = True 
        elif tab['TICKER'][0] == 'CHK':
            tab.loc[tab.DATE == date(2016, 2, 8), 'HasGap'] = True
        elif tab['TICKER'][0] == 'NFLX':
            tab.loc[tab.DATE == date(2013, 1, 24), 'HasGap'] = True
        elif tab['TICKER'][0] == 'FLS':
            tab = FixSplit(tab, date(2013, 6, 21), date(2013, 6, 24), 3)
        elif tab['TICKER'][0] == 'NKE':
            tab = FixSplit(tab, date(2012, 12, 24), date(2012, 12, 26), 2)
            tab = FixSplit(tab, date(2015, 12, 23), date(2015, 12, 24), 2)
        elif tab['TICKER'][0] == 'CELG':
            tab = FixSplit(tab, date(2014, 6, 25), date(2014, 6, 26), 2)
        elif tab['TICKER'][0] == 'DLTR':
            tab = FixSplit(tab, date(2012, 6, 26), date(2012, 6, 27), 2)
        elif tab['TICKER'][0] == 'EOG':
            tab = FixSplit(tab, date(2014, 3, 31), date(2014, 4, 1), 2)
        elif tab['TICKER'][0] == 'UNP':
            tab = FixSplit(tab, date(2014, 6, 6), date(2014, 6, 9), 2)
        elif tab['TICKER'][0] == 'AA':
            tab = FixSplit(tab, date(2016, 10, 5), date(2016, 10, 6), 1/3)
        elif tab['TICKER'][0] == 'AME':
            tab = FixSplit(tab, date(2012, 6, 29), date(2012, 7, 2), 1.5)
        elif tab['TICKER'][0] == 'COP':
            tab = FixSplit(tab, date(2012, 4, 30), date(2012, 5, 1), 1.311791)
        elif tab['TICKER'][0] == 'DOV':
            tab = FixSplit(tab, date(2014, 3, 1), date(2014, 3, 3), 1.205)
        elif tab['TICKER'][0] == 'DHR':
            tab = FixSplit(tab, date(2016, 7, 1), date(2016, 7, 5), 1.319)
        #here we mark gaps
        tab['mydiff'] = (abs(tab['OPEN'].diff(1))/tab['OPEN'])
        tab['DATENEXT'] = tab['DATE'].shift(1)
        vec = tab['mydiff'][1:]
        if vec.max() >= gapsize:
            dates = tab[tab.mydiff >= gapsize][['DATE', 'DATENEXT']]
            dates = (pandas.concat([dates['DATE'], dates['DATENEXT']])).unique()
            tab.loc[tab.DATE.isin(dates), 'HasGap'] = True
 
        #here we mark days with few data
        tabgrouped = (tab.groupby('DATE')['OPEN']).count()
        dates = (tabgrouped[tabgrouped < minnumofcandles].index).unique()
        tab.loc[tab.DATE.isin(dates), 'IsCorrect'] = False
        tab[['OPEN', 'HIGH', 'LOW', 'CLOSE']] = round(tab[['OPEN', 'HIGH', 'LOW', 'CLOSE']], 2)
        tab['DATE'] = (tab['DATE']).apply(lambda x: x.strftime('%Y%m%d'))
        tab['IsCorrect'] = tab['IsCorrect'].apply(lambda x: int(x))
        tab['HasGap'] = tab['HasGap'].apply(lambda x: int(x))
        tab['SplitDay'] = tab['SplitDay'].apply(lambda x: int(x))
        tab.to_csv(finalpath + '\\' + afile, sep = ';', 
                   columns = ['TICKER', 'DATE', 'TIME', 'OPEN', 'HIGH', 'LOW', 'CLOSE', 'IsCorrect', 'HasGap', 'SplitDay'],
                   index = False)
        
def GetIndicesByCounter(counter):
    #currently works for no more than 2 stocks
    if counter == 1: return([7])
    elif counter == 2: return([7, 18])
    
def MultiStockTest(storagepath, files, getdatfunc, minnumofcandles = 100, minnumofdays = 14, daysgap = 30):
    #currently works for no more than 2 stocks    
    files = set(files)
    counter = 1
    for afile in files:
        if counter == 1:
            tabcum = getdatfunc(storagepath, afile, True)
        else:
            tab = getdatfunc(storagepath, afile, True)
            tabcum = pandas.merge(tabcum, tab, on = 'dt')
        counter += 1
    #we start from making global IsCorrect column
    if counter == 2: tabcum['DATE_x'] = tabcum['DATE']
    inds = GetIndicesByCounter(counter-1)
    tabcum['IsCorrect'] = tabcum.iloc[:, inds[0]]
    if len(inds)>1:
        for j in inds[1:]:
            tabcum['IsCorrect'] = tabcum['IsCorrect'] & tabcum.iloc[:,j]
    #we mark the days with not enough candles
    tabgrouped = (tabcum.groupby('DATE_x')['DATE_x']).count()
    dates = (tabgrouped[tabgrouped < minnumofcandles].index).unique()
    tabcum.loc[tabcum.DATE_x.isin(dates), 'IsCorrect'] = False
    tabred = tabcum[tabcum.IsCorrect]
    #now we count the number of days:
    dates = tabred['DATE_x'].unique()
    lendat = len(dates)
    vec = tabred['DATE_x'].diff()
    vmd = vec.max().days
    if (lendat<=minnumofdays) | (vmd>=daysgap): flag = False 
    else: flag = True
    return(flag, lendat, vmd, tabcum)
    
def GlobalMultiStockTest(storagepath, getdatfunc):
    filelist = os.listdir(storagepath)
    resframe = pandas.DataFrame(columns = ['fullname', 'flag', 'len', 'vmax'])
    globcount = 0
    for i in range(len(filelist)):
        for j in range(i, len(filelist)):
            afile = filelist[i]
            bfile = filelist[j]
            print([afile, bfile])
            res = MultiStockTest(storagepath, [afile, bfile], getdatfunc = getdatfunc)
            resframe.loc[globcount] = [[afile, bfile], res[0], res[1], res[2]]
            globcount += 1
    return(resframe)