# -*- coding: utf-8 -*-
"""
Created on Tue Mar 14 20:15:06 2017

@author: Семья
"""

import pandas

def AmericanStrat(tab, numofassets, winlen, shiftlen, avlen):
    #we start from smoothing the portfolio weights
    for i in range(1, numofassets+1):
        #tab['k' + str(i)] = pandas.ewma(tab['k' + str(i)], span = avlen, adjust = False)
        tab['k' + str(i)] = tab['k' + str(i)].ewm(span = avlen, min_periods = 0, adjust = False, ignore_na = False).mean()
    tab['Spread'] = tab['price1'] - tab['predictor']
    tab['sd'] = tab['Spread'].rolling(window = winlen, min_periods = winlen, center = False).std()
    tab = tab[pandas.isnull(tab.sd) == False]
    tab.loc[:, 'sd'] = tab['sd'].ewm(span = avlen, min_periods = 0, adjust = False, ignore_na = False).mean()
    return(tab)