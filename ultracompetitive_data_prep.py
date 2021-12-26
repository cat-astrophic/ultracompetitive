# This script does the data prep for the us data in the ultra competition gender gap paper

# Importing required modules

import pandas as pd
import numpy as np

# Declaring the filepath where the data was stored by the scraper

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultracompetitive/'

# Reading in the data

print('Reading in the data.......')

data = pd.read_csv(filepath + 'raw_results_data.csv')

# Relevant parameters

print('Creating parameters.......')

years = list(data.RACE_Year.unique())
genders = list(data.Gender.unique())
events = list(data.RACE_Distance.unique())

# See which events are worth studying

xxx = []
yyy = []

for e in events:
    
    tmp = data[data.RACE_Distance == e]
    xxx.append(e)
    yyy.append(len(tmp))
    
ddd = pd.concat([pd.Series(xxx, name = 'Event'), pd.Series(yyy, name = 'Result')], axis = 1)
ddd = ddd.sort_values('Result', ascending = False)
ddd = ddd.reset_index(drop = True)

# Take any events with at least 10k observations over the time period

events = [ddd.Event[i] for i in range(len(ddd)) if ddd.Result[i] >= 10000]

# Helper function for converting time strings to seconds

def str_to_time(inp):
    
    try:
        
        l = len(inp)
        
        if l == 7:
            
            out = 3600*int(inp[0]) + 60*int(inp[2:4]) + int(inp[5:])
            
        elif l == 8:
            
            out = 3600*int(inp[0:2]) + 60*int(inp[3:5]) + int(inp[6:])
            
        elif l == 9:
            
            out = 3600*int(inp[0:3]) + 60*int(inp[4:6]) + int(inp[7:])
            
        else:
            
            out = np.nan
            
    except:
        
        out = np.nan
        
    return out

# Create data set for creating PD data

print('Creating data.............')

df = pd.DataFrame()

for g in genders:
    
    vals = []
    races = []
    ranks = []
    ys = []
    gs = []
    
    datag = data[data.Gender == g]
    
    for y in years:
        
        datagy = datag[datag.RACE_Year == y]
        
        for e in events:
            
            print('                          Gender = ' + g + ' ....... Year = ' + str(y) + ' ....... Event = ' + e)
            
            tmp = datagy[datagy.RACE_Distance == e].reset_index(drop = True)
            
            if e in ['24 Hours', '12 Hours']:
                
                tmp = tmp.dropna(subset = ['Distance'])
                tmp = tmp.sort_values('Distance', ascending = False).reset_index(drop = True)
                vals = vals + list(tmp.Distance)
                races = races + [e]*len(tmp)
                ranks = ranks + [i+1 for i in range(len(tmp))]
                ys = ys + [y]*len(tmp)
                gs = gs + [g]*len(tmp)
                
            else:
                
                seconds = [str_to_time(tmp.Time[i]) for i in range(len(tmp))]
                tmp = pd.concat([tmp, pd.Series(seconds, name = 'Seconds')], axis = 1)
                tmp = tmp.dropna(subset = ['Seconds'])
                tmp = tmp.sort_values('Seconds', ascending = True).reset_index(drop = True)
                vals = vals + list(tmp.Seconds)
                races = races + [e]*len(tmp)
                ranks = ranks + [i+1 for i in range(len(tmp))]
                ys = ys + [y]*len(tmp)
                gs = gs + [g]*len(tmp)
                
    tmpdf = pd.concat([pd.Series(gs, name = 'Gender'), pd.Series(races, name = 'Event'), pd.Series(ys, name = 'Year'),
                    pd.Series(ranks, name = 'Rank'), pd.Series(vals, name = 'Result')], axis = 1)
    df = pd.concat([df, tmpdf], axis = 0)
    
# Create pd data

print('Creating PD data.......')

df = df[df.Year < 2021].reset_index(drop = True)
women = df[df.Gender == 'F'].reset_index(drop = True)
men = df[df.Gender == 'M'].reset_index(drop = True)
years = [y for y in years if y <= 2020]
pddf = pd.DataFrame()

for y in years:
    
    wy = women[women.Year == y]
    my = men[men.Year == y]
    
    for e in events[0:4]:
        
        wye = wy[wy.Event == e]
        mye = my[my.Event == e]
        
        max_rank = min(max(wye.Rank), max(mye.Rank))
        wye = wye[wye.Rank <= max_rank].reset_index(drop = True)
        mye = mye[mye.Rank <= max_rank].reset_index(drop = True)
        
        tmp_pd = [100*(wye.Result[i] - mye.Result[i])/mye.Result[i] for i in range(len(wye))]
        tmp_df = pd.concat([wye.Year, wye.Rank, wye.Event, pd.Series(tmp_pd, name = 'PD')], axis = 1)
        pddf = pd.concat([pddf, tmp_df], axis = 0)
        
    for e in events[4:]:
        
        wye = wy[wy.Event == e]
        mye = my[my.Event == e]
        
        max_rank = min(max(wye.Rank), max(mye.Rank))
        wye = wye[wye.Rank <= max_rank].reset_index(drop = True)
        mye = mye[mye.Rank <= max_rank].reset_index(drop = True)
        
        tmp_pd = [100*(mye.Result[i] - wye.Result[i])/mye.Result[i] for i in range(len(wye))]
        tmp_df = pd.concat([wye.Year, wye.Rank, wye.Event, pd.Series(tmp_pd, name = 'PD')], axis = 1)
        pddf = pd.concat([pddf, tmp_df], axis = 0)
        
# Write pddf to csv

pddf.to_csv(filepath + 'gap_results/pd_data.csv', index = False)

# Next create binned data

print('Creating binned data......')

bins = []

for g in genders:
    
    dfg = df[df.Gender == g]
    
    for y in years:
        
        pcut = 0
        dfgy = dfg[dfg.Year == y]
        
        for e in events:
            
            print('                          Gender = ' + g + ' ....... Year = ' + str(y) + ' ....... Event = ' + e)
            
            tmp = dfgy[dfgy.Event == e].reset_index(drop = True)
            pcut = 0
            morebins = []
            
            for i in range(100):
                
                cutoff = int(np.ceil(((i+1)*len(tmp)/100)))
                morebins = morebins + [i+1]*(cutoff-pcut)
                pcut = cutoff
                
            bins = bins + morebins
            
bins = pd.Series(bins, name = 'Bin')
df = pd.concat([df, bins], axis = 1)

# Create pd data for bins

print('Creating bin PDs.......')

women = df[df.Gender == 'F'].reset_index(drop = True)
men = df[df.Gender == 'M'].reset_index(drop = True)

e_list = []
y_list = []
b_list = []
pd_list = []

for y in years:
    
    wy = women[women.Year == y]
    my = men[men.Year == y]
    
    for e in events[0:4]:
        
        print('                          Year = ' + str(y) + ' ....... Event = ' + e)
        
        wye = wy[wy.Event == e]
        mye = my[my.Event == e]
        
        for i in range(1,101):
            
            wtmp = wye[wye.Bin == i]
            mtmp = mye[mye.Bin == i]
            
            wval = wtmp.Result.mean()
            mval = mtmp.Result.mean()
            pdval = 100*(mval-wval)/mval
            
            e_list.append(e)
            y_list.append(y)
            b_list.append(i)
            pd_list.append(pdval)
            
    for e in events[4:]:
        
        print('                          Year = ' + str(y) + ' ....... Event = ' + e)
        
        wye = wy[wy.Event == e]
        mye = my[my.Event == e]
        
        for i in range(1,101):
            
            wtmp = wye[wye.Bin == i]
            mtmp = mye[mye.Bin == i]
            
            wval = wtmp.Result.mean()
            mval = mtmp.Result.mean()
            pdval = 100*(wval-mval)/mval
            
            e_list.append(e)
            y_list.append(y)
            b_list.append(i)
            pd_list.append(pdval)
            
el = pd.Series(e_list, name = 'Event')
yl = pd.Series(y_list, name = 'Year')
bl = pd.Series(b_list, name = 'Bin')
pdl = pd.Series(pd_list, name = 'PD')
bpddf = pd.concat([el, yl, bl, pdl], axis = 1)

# Write bpddf to csv

bpddf.to_csv(filepath + 'gap_results/binned_data.csv', index = False)

