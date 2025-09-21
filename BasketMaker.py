# -*- coding: utf-8 -*-
"""
Created on Mon Mar  6 12:55:49 2017

@author: user
"""
import pandas
from collections import namedtuple
import imp
import TestsCleanData
imp.reload(TestsCleanData)
from sklearn import linear_model
import rpy2.robjects as robjects
import os
import numpy
import time

cortestint = robjects.r('function(x, y) {cor.test(x, y)$conf.int[1:2]}')

def LoadData(storagepath, pairname):
    df = pandas.read_csv(storagepath + '//' + pairname + '.txt', sep = ';')
    df = df.rename(columns = {'ticker_1': 'ticker1',
                         'ticker_2': 'ticker2',
                         'price_1': 'price1',
                         'price_2': 'price2'})
    df.dt = pandas.to_datetime(df.dt)
    #del df['predictor'], df['inter']
    return(df)
    
def CreateEmptyDf():
    return(pandas.DataFrame(columns = ['ticker1', 'ticker2', 'dt', 'k1', 'k2', 'price1', 'price2']))

def PrepareDublicatesInfo(tab):
    cols = tab.columns
    tickernums = [s for s in cols if 'ticker' in  s]
    tickernames = [tab[s][0] for s in tickernums]
    tickernums = [s[6:] for s in tickernums]
    tickers = pandas.DataFrame.from_dict({'nums': tickernums, 'names': tickernames})
    tickersset = set(tickers['names'])
    dublinfo = []
    for ticker in tickersset:
        temp = tickers.loc[tickers.names == ticker, 'nums']
        dublinfo.append(temp.tolist())
    res = namedtuple('tickersset', 'dublinfo')
    res.tickersset = tickersset
    res.dublinfo = dublinfo
    return(res)
    
def MergeDfList(dflist):
    curdf = dflist[0].rename(columns = {'ticker1': 'ticker1_1',  
                                        'ticker2': 'ticker2_1',
                                        'k1': 'k1_1',
                                        'k2': 'k2_1',
                                        'price1': 'price1_1',
                                        'price2': 'price2_1'})
    for i, frame in enumerate(dflist[1:], 2):
        curdf = curdf.merge(frame, on = 'dt').rename(
                            columns = {'ticker1': 'ticker1_%d' % i,  
                                       'ticker2': 'ticker2_%d' % i,
                                       'k1': 'k1_%d' % i,
                                       'k2': 'k2_%d' % i,
                                       'price1': 'price1_%d' % i,
                                       'price2': 'price2_%d' % i})
    return(curdf)

def MergeDfListSimple(dflist):
    curdf = dflist[0].rename(columns = {'ticker': 'ticker_1', 'price': 'price_1'})
    for i, frame in enumerate(dflist[1:], 2):
        curdf = curdf.merge(frame, on = 'dt').rename(
                            columns = {'ticker': 'ticker_%d' % i, 'price': 'price_%d' % i})
    return(curdf)
    
def ApplyEqualWeights(curdf, numofpairs):
    for i in range(1, numofpairs + 1):
        curdf['k1_%d' % i] = curdf['k1_%d' % i]/numofpairs
        curdf['k2_%d' % i] = curdf['k2_%d' % i]/numofpairs
    return(curdf)
    
def ConsiderDublicates(curdf, res):
    tickersset = res.tickersset
    dublinfo = res.dublinfo
    for j in range(len(tickersset)):
      if len(dublinfo[j])>1:
          mark0 = dublinfo[j][0]
          for mark1 in dublinfo[j][1:]:
              for phrase in 'k', 'price':
                  curdf[phrase + mark0] += curdf[phrase + mark1]
                  del curdf[phrase + mark1]
    return(curdf)
              
def CalculateSyntheticPrices(curdf, numofpairs):
    minval = 100000
    cols = curdf.columns
    for s in cols: 
        if s[0] == 'k':
            minval = min(minval, min(curdf[s]))
    curdf['synthetic_price'] = 0
    for s in cols:
        if s[0] == 'k':
            curdf[s] /= minval
            ending = s[1:]
            curdf['synthetic_price'] += curdf['k' + ending]*curdf['price' + ending]    
    return(curdf)

def SimpleBasketMaker(dflist):
    numofpairs = len(dflist)
    curdf = MergeDfList(dflist)
    res = PrepareDublicatesInfo(curdf)
    curdf = ApplyEqualWeights(curdf, numofpairs)
    curdf = ConsiderDublicates(curdf, res)
    return(CalculateSyntheticPrices(curdf, numofpairs))
    
def RegrBasketMaker(tickerpairs, storagepath, winlen, shiftlen):
    tickers = []
    for pair in tickerpairs:
        tickers.append(pair[0])
        tickers.append(pair[1])
    #if (keyasset == None) | (keyasset not in tickers):
    keyasset = tickers[0]
    print(keyasset)
    tickers = set(tickers)
    dflist = []
    for ticker in tickers:
        print(ticker)
        dtab = (TestsCleanData.GetTabAlt(storagepath, ticker + '.txt', True))
        dtab = dtab.rename(columns = {'TICKER': 'ticker', 'OPEN': 'price'})
        dflist.append(dtab.loc[dtab.IsCorrect, ['ticker', 'price', 'dt']])
    tab = MergeDfListSimple(dflist)
    tab['predictor'] = pandas.np.nan
    regr = linear_model.LinearRegression(fit_intercept = False)
    count = 0
    keyasset = 'price_1'
    nonkeyassets =  ['price_' + str(j) for j in range(2, len(tickers)+1)]
    tab['inter'] = 0
    for i in range(len(tickers)):
        tab['k' + str(i+1)] = 0. 
    for j in range(winlen, (len(tab))):
        if count % shiftlen == 0:
            regr.fit(tab[nonkeyassets][(j-winlen):(j+1)], tab[keyasset][(j-winlen):(j+1)])
        kefs = regr.coef_
        count += 1
        inter = regr.intercept_
        tab.set_value(j, 'predictor', regr.predict(tab[nonkeyassets][j:(j+1)])[0])
        tab.set_value(j, 'k1', 1)
        #tab.set_value(j, 'inter', inter)
        for i in range(1, len(tickers)):
            tab.set_value(j, 'k' + str(i+1), kefs[i-1])
    tab = tab[pandas.isnull(tab['predictor']) == False]
    x = robjects.vectors.FloatVector(tab['predictor']-tab['inter'])
    y = robjects.vectors.FloatVector(tab['price_1'])
    z = cortestint(x, y)
    res = namedtuple('confint', 'tab')
    res.confint = [z[0], z[1], (abs(tab['predictor'] - tab['price_1'])).quantile(0.95)]
    res.tab = tab
    return(res)

def RegrBasketMakerOptim(tickerpairs, storagepath, winlen, shiftlen):
    start = time.time()
    tickers = []
    for pair in tickerpairs:
        tickers.append(pair[0])
        tickers.append(pair[1])
    #if (keyasset == None) | (keyasset not in tickers):
    keyasset = tickers[0]
    print(keyasset)
    tickers = set(tickers)
    dflist = []
    for ticker in tickers:
        print(ticker)
        dtab = (TestsCleanData.GetTabAlt(storagepath, ticker + '.txt', True))
        dtab = dtab.rename(columns = {'TICKER': 'ticker', 'OPEN': 'price'})
        dflist.append(dtab.loc[dtab.IsCorrect, ['ticker', 'price', 'dt']])
    tab = MergeDfListSimple(dflist)
    tab['predictor'] = 0
    regr = linear_model.LinearRegression(fit_intercept = False)
    keyasset = 'price_1'
    nonkeyassets =  ['price_' + str(j) for j in range(2, len(tickers)+1)]
    kstr = []
    for i in range(len(tickers)):
        kstr.append('k' + str(i+1))
        tab[kstr[i]] = 0.
    tab['k1'] = 1.
    for j in numpy.arange(winlen, len(tab), shiftlen):
        #regr.fit(tab[nonkeyassets][(j-winlen):(j)], tab[keyasset][(j-winlen):(j)])
        regr.fit(tab.loc[(j-winlen):(j-1), nonkeyassets], tab.loc[(j-winlen):(j-1), keyasset])
        kefs = regr.coef_
       # x = tab.loc[(j-winlen):(j-1), nonkeyassets]
       # y = tab.loc[(j-winlen):(j-1), keyasset]
       # kefs = (x*y).sum()/(x**2).sum()
        #tab.loc[j:(j+shiftlen-1), 'predictor'] = regr.predict(tab[nonkeyassets][j:(j+shiftlen)])
        #print(j)
        for i in range(1, len(tickers)):
            tab.loc[j:(j+shiftlen-1), 'k' + str(i+1)] = kefs[i-1]
            tab.loc[j:(j+shiftlen-1), 'predictor'] += kefs[i-1]*tab.loc[j:(j+shiftlen-1), nonkeyassets[i-1]]
    tab = tab[tab.predictor > 0]
  #  print(tab.head(15))
  #  end = time.time()
  #  print(end - start)
  #  return(tab)
    x = robjects.vectors.FloatVector(tab['predictor'])
    y = robjects.vectors.FloatVector(tab['price_1'])
    z = cortestint(x, y)
    res = namedtuple('confint', 'tab')
    res.confint = [z[0], z[1], (abs(tab['predictor'] - tab['price_1'])).quantile(0.95)]
   # res.confint = [(abs(tab['predictor'] - tab['price_1'])).quantile(0.95)]
    res.tab = tab
    end = time.time()
    print(end - start)
    return(res)

    
def RegrBasketTester(storagepath, finalpath, winlen, shiftlen):
    filelist = os.listdir(storagepath)
    counter = 1
    for i in range(len(filelist)-1):
        afile = filelist[i]
        afile = afile.rsplit('.', 1)[0]
       # print(afile)
        for j in range(i+1, len(filelist)):
            bfile = filelist[j]
            bfile = bfile.rsplit('.', 1)[0]
        #    print(bfile)
            if counter > 18553:
                res = RegrBasketMakerOptim([[afile, bfile]], storagepath, winlen, shiftlen)
                print(res.confint)
                res.tab.to_csv(finalpath + afile + '-' + bfile + '.txt', sep = ';', index = False)
                target = open(finalpath + 'res.txt', 'a')
                target.write(afile + '-' + bfile + ';' + str(res.confint[0]) + ';' + str(res.confint[1]) + ';' + str(res.confint[2]))
            #target.write(afile + '-' + bfile + ';' + str(res.confint[0]))
                target.write('\n')
                target.close()
            counter += 1