# This script creates additional data set from the ultramarthon data for the project on gender and competitiveness

# Importing required modules

import pandas as pd
import numpy as np

# Project directory

direc = 'D:/ultracompetitive/'

# Reading in the data set

data = pd.read_csv(direc + 'output.csv')

# Creating a relative place variable for each observation

rp = []

for i in range(len(data)):
    
    print('Computing relative place for runner ' + str(i+1) + ' of ' + str(len(data)) + '.......')
    
    tmp = data[data.RACE_ID == data.RACE_ID[i]]
    tmp = tmp[tmp.Gender == data.Gender[i]]
    
    rp.append(data.Gender_Place[i] / len(tmp))

data = pd.concat([data, pd.Series(rp, name = 'Relative_Place')], axis = 1)

# Remove the instances where rp > 1

data = data[data.Relative_Place <= 1].reset_index(drop = True)

# The first new data set will look at relative place volatility

genders = []
counts = []
volatility = []
mean_rp = []

for r in data.Runner_ID.unique():
    
    print('Computing relative place for runner ' + str(list(data.Runner_ID.unique()).index(r)+1) + ' of ' + str(len(data.Runner_ID.unique())) + '.......')
    
    tmp = data[data.Runner_ID == r].reset_index(drop = True)
    
    genders.append(tmp.Gender[0])
    counts.append(len(tmp))
    volatility.append(np.std(tmp.Relative_Place))
    mean_rp.append(np.mean(tmp.Relative_Place))

rp_data = pd.concat([pd.Series(data.Runner_ID.unique(), name = 'Runner'), pd.Series(genders, name = 'Gender'), pd.Series(counts, name = 'Races'), pd.Series(volatility, name = 'Volatility'), pd.Series(mean_rp, name = 'Ability')], axis = 1)

# Save rp_data

rp_data.to_csv(direc + 'volatility_data.csv', index = False)

# Removing people who do not race frequently enough to consider in this analysis

rpx = rp_data[rp_data.Races >= 10]

# Removing people who are not in the top 25% of runners, typically

rpx = rp_data[rp_data.Ability <= 0.25].reset_index(drop = True)

# The second new data set will look at runners who typically finish in the top 25% of the sample and who have an outlyingly bad performance

pre = [] # Mean relative place prior to first bad race
post = [] # Mean relative place after the first bad race
mas = [] # Indiator for having another bad race after the first
cuantos = [] # Count of bad races after the first
witch = [] # Index of first bad race

for i in range(len(rpx)):
    
    print('Weird analysis thingy for runner ' + str(i+1) + ' of ' + str(len(rpx)) + '.......')
    
    tmp = data[data.Runner_ID == rpx.Runner[i]].reset_index(drop = True)
    
    if max(tmp.Relative_Place) >= 0.75:
        
        baddies = tmp[tmp.Relative_Place >= 0.75].index.tolist()
        
        pre.append(np.mean(tmp.Relative_Place[:min(baddies)]))
        post.append(np.mean(tmp.Relative_Place[min(baddies)+1:]))
        mas.append(int(len(baddies) > 1))
        cuantos.append(len(baddies) - 1)
        witch.append(min(baddies))
        
    else:
        
        pre.append(None)
        post.append(None)
        mas.append(None)
        cuantos.append(None)
        witch.append(None)

diffs = [pre[i] - post[i] if pre[i] != None else None for i in range(len(pre))]
witches = [witch[i] / rpx.Races[i] if witch[i] != None else None for i in range(len(witch))]

rpx = pd.concat([rpx, pd.Series(pre, name = 'Pre_Mean'), pd.Series(post, name = 'Post_Mean'), pd.Series(diffs, name = 'Difference'), pd.Series(mas, name = 'More_Badness'), pd.Series(cuantos, name = 'Amount_of_More_Badness'), pd.Series(witch, name = 'First_Bad_Race'), pd.Series(witches, name = 'First_Bad_Race_Pct')], axis = 1)

# Save this data and be done

rpx.to_csv(direc + 'rpx.csv', index = False)

