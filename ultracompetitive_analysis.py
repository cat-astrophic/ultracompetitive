# This script runs analyses for gender + competition

# Importing required modules

import pandas as pd
from matplotlib import pyplot as plt

# Declaring the filepath where the data was stored by the scraper

filepath = 'D:/ultracompetitive/international_data/'

# Relevant parameters

years = [y for y in range(2005,2021)]
distances = ['24h', '48h', '6d', '12h', '6h', '100mi', '100km', '50mi', '50km']
sexes = ['W', 'M']

# Reading in the data

w50k = pd.read_csv(filepath + 'raw_data/50km_W.csv')
m50k = pd.read_csv(filepath + 'raw_data/50km_M.csv')
w50m = pd.read_csv(filepath + 'raw_data/50mi_W.csv')
m50m = pd.read_csv(filepath + 'raw_data/50mi_M.csv')
w100k = pd.read_csv(filepath + 'raw_data/100km_W.csv')
m100k = pd.read_csv(filepath + 'raw_data/100km_M.csv')
w100m = pd.read_csv(filepath + 'raw_data/100mi_W.csv')
m100m = pd.read_csv(filepath + 'raw_data/100mi_M.csv')
w6h = pd.read_csv(filepath + 'raw_data/6h_W.csv')
m6h = pd.read_csv(filepath + 'raw_data/6h_M.csv')
w12h = pd.read_csv(filepath + 'raw_data/12h_W.csv')
m12h = pd.read_csv(filepath + 'raw_data/12h_M.csv')
w24h = pd.read_csv(filepath + 'raw_data/24h_W.csv')
m24h = pd.read_csv(filepath + 'raw_data/24h_M.csv')
w48h = pd.read_csv(filepath + 'raw_data/48h_W.csv')
m48h = pd.read_csv(filepath + 'raw_data/48h_M.csv')
w6d = pd.read_csv(filepath + 'raw_data/6d_W.csv')
m6d = pd.read_csv(filepath + 'raw_data/6d_M.csv')

# Creating a function to compute race times for distance based races

def str_to_time(inp):
    
    l = len(inp)
    
    if l == 9:
        
        out = 3600*int(inp[0]) + 60*int(inp[2:4]) + int(inp[5:7])
        
    elif l == 10:
        
        out = 3600*int(inp[0:2]) + 60*int(inp[3:5]) + int(inp[6:8])
        
    elif l == 11:
        
        out = 3600*int(inp[0:3]) + 60*int(inp[4:6]) + int(inp[7:9])
        
    return out

# Creating a function to compute race times for distance based races

def str_to_distance(inp):
    
    out = int(inp[:-3].replace('.', ''))
    
    return out

# Calculating times/distances for each file

tw50k = pd.Series([str_to_time(x) for x in w50k.Performance], name = 'Seconds')
tm50k = pd.Series([str_to_time(x) for x in m50k.Performance], name = 'Seconds')
tw50m = pd.Series([str_to_time(x) for x in w50m.Performance], name = 'Seconds')
tm50m = pd.Series([str_to_time(x) for x in m50m.Performance], name = 'Seconds')
tw100k = pd.Series([str_to_time(x) for x in w100k.Performance], name = 'Seconds')
tm100k = pd.Series([str_to_time(x) for x in m100k.Performance], name = 'Seconds')
tw100m = pd.Series([str_to_time(x) for x in w100m.Performance], name = 'Seconds')
tm100m = pd.Series([str_to_time(x) for x in m100m.Performance], name = 'Seconds')

tw6h = pd.Series([str_to_distance(x) for x in w6h.Performance], name = 'Kilometers')
tm6h = pd.Series([str_to_distance(x) for x in m6h.Performance], name = 'Kilometers')
tw12h = pd.Series([str_to_distance(x) for x in w12h.Performance], name = 'Kilometers')
tm12h = pd.Series([str_to_distance(x) for x in m12h.Performance], name = 'Kilometers')
tw24h = pd.Series([str_to_distance(x) for x in w24h.Performance], name = 'Kilometers')
tm24h = pd.Series([str_to_distance(x) for x in m24h.Performance], name = 'Kilometers')
tw48h = pd.Series([str_to_distance(x) for x in w48h.Performance], name = 'Kilometers')
tm48h = pd.Series([str_to_distance(x) for x in m48h.Performance], name = 'Kilometers')
tw6d = pd.Series([str_to_distance(x) for x in w6d.Performance], name = 'Kilometers')
tm6d = pd.Series([str_to_distance(x) for x in m6d.Performance], name = 'Kilometers')

# Adding these data to the dataframes

w50k = pd.concat([w50k, tw50k], axis = 1)
m50k = pd.concat([m50k, tm50k], axis = 1)
w50m = pd.concat([w50m, tw50m], axis = 1)
m50m = pd.concat([m50m, tm50m], axis = 1)
w100k = pd.concat([w100k, tw100k], axis = 1)
m100k = pd.concat([m100k, tm100k], axis = 1)
w100m = pd.concat([w100m, tw100m], axis = 1)
m100m = pd.concat([m100m, tm100m], axis = 1)

w6h = pd.concat([w6h, tw6h], axis = 1)
m6h = pd.concat([m6h, tm6h], axis = 1)
w12h = pd.concat([w12h, tw12h], axis = 1)
m12h = pd.concat([m12h, tm12h], axis = 1)
w24h = pd.concat([w24h, tw24h], axis = 1)
m24h = pd.concat([m24h, tm24h], axis = 1)
w48h = pd.concat([w48h, tw48h], axis = 1)
m48h = pd.concat([m48h, tm48h], axis = 1)
w6d = pd.concat([w6d, tw6d], axis = 1)
m6d = pd.concat([m6d, tm6d], axis = 1)

# Main loop for computing the percent differences for the ordered cross sections

df50k = pd.DataFrame()
df50m = pd.DataFrame()
df100k = pd.DataFrame()
df100m = pd.DataFrame()
df6h = pd.DataFrame()
df12h = pd.DataFrame()
df24h = pd.DataFrame()
df48h = pd.DataFrame()
df6d = pd.DataFrame()

for year in years:
    
    a = w50k[w50k.Year == year].reset_index(drop = True)
    b = m50k[m50k.Year == year].reset_index(drop = True)
    c = w50m[w50m.Year == year].reset_index(drop = True)
    d = m50m[m50m.Year == year].reset_index(drop = True)
    e = w100k[w100k.Year == year].reset_index(drop = True)
    f = m100k[m100k.Year == year].reset_index(drop = True)
    g = w100m[w100m.Year == year].reset_index(drop = True)
    h = m100m[m100m.Year == year].reset_index(drop = True)
    x = w6h[w6h.Year == year].reset_index(drop = True)
    j = m6h[m6h.Year == year].reset_index(drop = True)
    k = w12h[w12h.Year == year].reset_index(drop = True)
    l = m12h[m12h.Year == year].reset_index(drop = True)
    m = w24h[w24h.Year == year].reset_index(drop = True)
    n = m24h[m24h.Year == year].reset_index(drop = True)
    o = w48h[w48h.Year == year].reset_index(drop = True)
    p = m48h[m48h.Year == year].reset_index(drop = True)
    q = w6d[w6d.Year == year].reset_index(drop = True)
    r = m6d[m6d.Year == year].reset_index(drop = True)
    
    l1 = min(len(a),len(b),1000)
    l2 = min(len(c),len(d),1000)
    l3 = min(len(e),len(f),1000)
    l4 = min(len(g),len(h),1000)
    l5 = min(len(x),len(j),1000)
    l6 = min(len(k),len(l),1000)
    l7 = min(len(m),len(n),1000)
    l8 = min(len(o),len(p),1000)
    l9 = min(len(q),len(r),1000)
    
    y1 = pd.Series([year]*l1, name = 'Year')
    y2 = pd.Series([year]*l2, name = 'Year')
    y3 = pd.Series([year]*l3, name = 'Year')
    y4 = pd.Series([year]*l4, name = 'Year')
    y5 = pd.Series([year]*l5, name = 'Year')
    y6 = pd.Series([year]*l6, name = 'Year')
    y7 = pd.Series([year]*l7, name = 'Year')
    y8 = pd.Series([year]*l8, name = 'Year')
    y9 = pd.Series([year]*l9, name = 'Year')
    
    r1 = pd.Series([i+1 for i in range(l1)], name = 'Rank')
    r2 = pd.Series([i+1 for i in range(l2)], name = 'Rank')
    r3 = pd.Series([i+1 for i in range(l3)], name = 'Rank')
    r4 = pd.Series([i+1 for i in range(l4)], name = 'Rank')
    r5 = pd.Series([i+1 for i in range(l5)], name = 'Rank')
    r6 = pd.Series([i+1 for i in range(l6)], name = 'Rank')
    r7 = pd.Series([i+1 for i in range(l7)], name = 'Rank')
    r8 = pd.Series([i+1 for i in range(l8)], name = 'Rank')
    r9 = pd.Series([i+1 for i in range(l9)], name = 'Rank')
    
    pd50k = pd.Series([100*((a.Seconds[i] - b.Seconds[i])/b.Seconds[i]) for i in range(l1)], name = 'PD')
    pd50m = pd.Series([100*((c.Seconds[i] - d.Seconds[i])/d.Seconds[i]) for i in range(l2)], name = 'PD')
    pd100k = pd.Series([100*((e.Seconds[i] - f.Seconds[i])/f.Seconds[i]) for i in range(l3)], name = 'PD')
    pd100m = pd.Series([100*((g.Seconds[i] - h.Seconds[i])/h.Seconds[i]) for i in range(l4)], name = 'PD')
    pd6h = pd.Series([100*((x.Kilometers[i] - j.Kilometers[i])/j.Kilometers[i]) for i in range(l5)], name = 'PD')
    pd12h = pd.Series([100*((k.Kilometers[i] - l.Kilometers[i])/l.Kilometers[i]) for i in range(l6)], name = 'PD')
    pd24h = pd.Series([100*((m.Kilometers[i] - n.Kilometers[i])/n.Kilometers[i]) for i in range(l7)], name = 'PD')
    pd48h = pd.Series([100*((o.Kilometers[i] - p.Kilometers[i])/p.Kilometers[i]) for i in range(l8)], name = 'PD')
    pd6d = pd.Series([100*((q.Kilometers[i] - r.Kilometers[i])/r.Kilometers[i]) for i in range(l9)], name = 'PD')
    
    df1 = pd.concat([r1,y1,pd50k], axis = 1)
    df2 = pd.concat([r2,y2,pd50m], axis = 1)
    df3 = pd.concat([r3,y3,pd100k], axis = 1)
    df4 = pd.concat([r4,y4,pd100m], axis = 1)
    df5 = pd.concat([r5,y5,pd6h], axis = 1)
    df6 = pd.concat([r6,y6,pd12h], axis = 1)
    df7 = pd.concat([r7,y7,pd24h], axis = 1)
    df8 = pd.concat([r8,y8,pd48h], axis = 1)
    df9 = pd.concat([r9,y9,pd6d], axis = 1)
    
    df50k = pd.concat([df50k,df1], axis = 0).reset_index(drop = True)
    df50m = pd.concat([df50m,df2], axis = 0).reset_index(drop = True)
    df100k = pd.concat([df100k,df3], axis = 0).reset_index(drop = True)
    df100m = pd.concat([df100m,df4], axis = 0).reset_index(drop = True)
    df6h = pd.concat([df6h,df5], axis = 0).reset_index(drop = True)
    df12h = pd.concat([df12h,df6], axis = 0).reset_index(drop = True)
    df24h = pd.concat([df24h,df7], axis = 0).reset_index(drop = True)
    df48h = pd.concat([df48h,df8], axis = 0).reset_index(drop = True)
    df6d = pd.concat([df6d,df9], axis = 0).reset_index(drop = True)
    
# Writing these dfs to file

df50k.to_csv(filepath + 'pd_data/pd_50k.csv', index = False)
df50m.to_csv(filepath + 'pd_data/pd_50m.csv', index = False)
df100k.to_csv(filepath + 'pd_data/pd_100k.csv', index = False)
df100m.to_csv(filepath + 'pd_data/pd_100m.csv', index = False)
df6h.to_csv(filepath + 'pd_data/pd_6h.csv', index = False)
df12h.to_csv(filepath + 'pd_data/pd_12h.csv', index = False)
df24h.to_csv(filepath + 'pd_data/pd_24h.csv', index = False)
df48h.to_csv(filepath + 'pd_data/pd_48h.csv', index = False)
df6d.to_csv(filepath + 'pd_data/pd_6d.csv', index = False)

# Creating figures to show the evolution over time of the PD values

blah = [2005,2009,2013,2017]

# 50km

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df50k[df50k.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 50km #2

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in [2017]:
    
    tmp = df50k[df50k.Year == year]['PD']
    plt.plot(tmp)
    plt.legend([2017])

# 100km

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df100k[df100k.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 100km #2

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in [2017]:
    
    tmp = df100k[df100k.Year == year]['PD']
    plt.plot(tmp)
    plt.legend([2017])

# 50mi

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df50m[df50m.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 100mi

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df100m[df100m.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 6hr

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df6h[df6h.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 12hr

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df12h[df12h.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 24hr

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df24h[df24h.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 48hr

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df48h[df48h.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# 6d

fig = plt.figure(figsize = (10, 6), dpi = 300)

for year in blah:
    
    tmp = df6d[df6d.Year == year]['PD']
    plt.plot(tmp)
    plt.legend(blah)

# Plotting the change in gender sample sizes over time

w50k_sizes = []
m50k_sizes = []
w50m_sizes = []
m50m_sizes = []
w100k_sizes = []
m100k_sizes = []
w100m_sizes = []
m100m_sizes = []

w6h_sizes = []
m6h_sizes = []
w12h_sizes = []
m12h_sizes = []
w24h_sizes = []
m24h_sizes = []
w48h_sizes = []
m48h_sizes = []
w6d_sizes = []
m6d_sizes = []

for y in range(2005,2021):
    
    w50k_sizes.append(len(w50k[w50k.Year == y]))
    m50k_sizes.append(len(m50k[m50k.Year == y]))
    w50m_sizes.append(len(w50m[w50m.Year == y]))
    m50m_sizes.append(len(m50m[m50m.Year == y]))
    w100k_sizes.append(len(w100k[w100k.Year == y]))
    m100k_sizes.append(len(m100k[m100k.Year == y]))
    w100m_sizes.append(len(w100m[w100m.Year == y]))
    m100m_sizes.append(len(m100m[m100m.Year == y]))

    w6h_sizes.append(len(w6h[w6h.Year == y]))
    m6h_sizes.append(len(m6h[m6h.Year == y]))
    w12h_sizes.append(len(w12h[w12h.Year == y]))
    m12h_sizes.append(len(m12h[m12h.Year == y]))
    w24h_sizes.append(len(w24h[w24h.Year == y]))
    m24h_sizes.append(len(m24h[m24h.Year == y]))
    w48h_sizes.append(len(w48h[w48h.Year == y]))
    m48h_sizes.append(len(m48h[m48h.Year == y]))
    w6d_sizes.append(len(w6d[w6d.Year == y]))
    m6d_sizes.append(len(m6d[m6d.Year == y]))

rat_50k = [m50k_sizes[i] / w50k_sizes[i] for i in range(len(years))]
rat_50m = [m50m_sizes[i] / w50m_sizes[i] for i in range(len(years))]
rat_100k = [m100k_sizes[i] / w100k_sizes[i] for i in range(len(years))]
rat_100m = [m100m_sizes[i] / w100m_sizes[i] for i in range(len(years))]

rat_6h = [m6h_sizes[i] / w6h_sizes[i] for i in range(len(years))]
rat_12h = [m12h_sizes[i] / w12h_sizes[i] for i in range(len(years))]
rat_24h = [m24h_sizes[i] / w24h_sizes[i] for i in range(len(years))]
rat_48h = [m48h_sizes[i] / w48h_sizes[i] for i in range(len(years))]
rat_6d = [m6d_sizes[i] / w6d_sizes[i] for i in range(len(years))]

fig = plt.figure(figsize = (10, 6), dpi = 300)
plt.plot(years, rat_50k)
plt.plot(years, rat_50m)
plt.plot(years, rat_100k)
plt.plot(years, rat_100m)
plt.legend(['50 km', '50 mi', '100 km', '100 mi'], title = 'Event', loc = 'center left', bbox_to_anchor = (1, 0.5))
plt.title('Gender Ratios (M/F) for Time-Based Events')
plt.ylabel('Year')
plt.xlabel('Ratio')
plt.ylim(0,7)
plt.xticks(years, years, rotation = 45)

fig = plt.figure(figsize = (10, 6), dpi = 300)
plt.plot(years, rat_6h)
plt.plot(years, rat_12h)
plt.plot(years, rat_24h)
plt.plot(years, rat_48h)
plt.plot(years, rat_6d)
plt.legend(['6 hr', '12 hr', '24 hr', '48 hr', '6 days'], title = 'Event', loc = 'center left', bbox_to_anchor = (1, 0.5))
plt.title('Gender Ratios (M/F) for Distance-Based Events')
plt.ylabel('Year')
plt.xlabel('Ratio')
plt.ylim(0,7)
plt.xticks(years, years, rotation = 45)

### Then redo this using the US data set -- use the Brazos Bend events + the 12:21 time as an example (+WS!)
### of the incompleteness of the DUV data set, and that if women are less likely to attend the mega-events
### (in Frick he states that ~55% of top times come from 4 events) then this would bias the results
### The US data set should avoid this issue because it is significantly more complete ...
### ... create a summary statistic showing how many performances in the US data set are better than the
### say 500th or 1000th time in the DUV data set, then see how many of these are included
### What about other countries?! Definitely missing critical observations -- what is the breakdwon by
### gender for this summary statistic??? THIS MOTIVATES THE BETTER DATA SET!!!
### But still use the DUV data set to replicate Frick and show that the gap is closing over time, even
### for the poorer data set!

