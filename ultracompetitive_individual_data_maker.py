# This script does the data prep for the gender + competition + ultramarathons project

# Importing required modules

import pandas as pd
from datetime import datetime
from geopy.distance import geodesic

# Specifying your username and the directory in which the raw data is located

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultracompetitive/'

# Reading in the data

compdata = pd.read_csv(filepath + 'raw_data.csv') # Main dataframe :: comes from the ultraCOVID project
ccmap = pd.read_csv(filepath + 'ccmap.csv', sep = '|') # City to county map
latlong = pd.read_csv(filepath + 'latlong.csv') # GPS coords for each county

# Normalizing latling county data to lowercase

latlong.Admin2 = latlong.Admin2.str.lower()

# Beginning the city -> county assignment

# Convert county and state data to lowercase for matching

ccmap['City'] = ccmap['City'].str.lower()
compdata['City'] = compdata['City'].str.lower()
compdata['RACE_City'] = compdata['RACE_City'].str.lower()

# Create a city + state variable for matching and add to the dfs

cs_df = [ccmap.City[i] + ccmap['State short'][i] for i in range(len(ccmap))]
cs_map = [compdata.City[i] + compdata.State[i] for i in range(len(compdata))]
cs_map2 = [compdata.RACE_City[i] + compdata.RACE_State[i] for i in range(len(compdata))]

ccmap = pd.concat([ccmap, pd.Series(cs_df, name = 'MERGE_ID')], axis = 1)
compdata = pd.concat([compdata, pd.Series(cs_map, name = 'MERGE_ID_RNR')], axis = 1)
compdata = pd.concat([compdata, pd.Series(cs_map2, name = 'MERGE_ID_RACE')], axis = 1)

# Assigning counties to runners in compdata and add to compdata :: written this way to check progress

rnr_county = []
race_county = []

for i in range(len(compdata)):
    
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visuzlize progress
    
    if compdata.MERGE_ID_RNR[i] in list(ccmap.MERGE_ID):
        
        rnr_county.append(ccmap.County[list(ccmap.MERGE_ID).index(compdata.MERGE_ID_RNR[i])])
        
    else:
        
        rnr_county.append(None)
        
    if compdata.MERGE_ID_RACE[i] in list(ccmap.MERGE_ID):
        
        race_county.append(ccmap.County[list(ccmap.MERGE_ID).index(compdata.MERGE_ID_RACE[i])])
        
    else:
        
        race_county.append(None)

compdata = pd.concat([compdata, pd.Series(rnr_county, name = 'Runner_County')], axis = 1)
compdata = pd.concat([compdata, pd.Series(race_county, name = 'Race_County')], axis = 1)

# Remove MERGE_ID columns from compdata

compdata = compdata.drop(columns = ['MERGE_ID_RNR', 'MERGE_ID_RACE'])

# Creating new variables in compdata for identifying events

idvar = [compdata.RACE_Name[i] + compdata.RACE_Distance[i] for i in range(len(compdata))] # An ID for race + distane w/o year
compdata = pd.concat([compdata, pd.Series(idvar, name = 'idvar')], axis = 1) # Add this variable to the df

# Initializing lists for storing new data

racedpy = [] # Did they race py
timepy = [] # Time at event py if distnace based event
distpy = [] # Distance at event py if time based event
sameco = [] # Number of runners in same county in current event
ncomps = [] # Number of competitors in current event
gcomps = [] # Number of gender competitors in current event
gpct = [] # Percentage of competitors in your gender in current event
timebt = [] # Time since last event
expyrs = [] # Number years to date with ultramarathon experience
expraces = [] # Number of races ran prior to current event

# Loop for py data

for i in range(len(compdata)):
    
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualizing progress
    
    runid = compdata.Runner_ID[i] # Get next runner id
    raceid = compdata.idvar[i] # Get next race id
    yr = compdata.RACE_Year[i] # Get year of event
    
    temp = compdata[compdata.Runner_ID == runid] # Subset for runner id
    temp = temp[temp.idvar == raceid].reset_index(drop = True) # Subset for race id
    
    if yr-1 in list(temp.RACE_Year): # If they ran the race during py get data
        
        temppy = temp[temp.RACE_Year == yr-1].reset_index(drop = True) # Subset temp for runner + py event data
        temp = temp[temp.RACE_Year == yr].reset_index(drop = True) # Subset temp for runner + event data
        temp2 = compdata[compdata.idvar == raceid] # Subset compdata for race data
        temp4 = temp2[temp2.RACE_Year == yr] # Sorry, this was added later, but it counts runners in same county
        temp4 = temp4[temp4.Runner_County == compdata.Runner_County[i]] # Sorry, this was added later, but it counts runners in same county
        temp2 = temp2[temp2.Gender == compdata.Gender[i]] # Subset temp2 again for same gender data
        temp3 = compdata[compdata.Runner_ID == runid].reset_index(drop = True) # Subset compdata for runn id again
        racedpy.append(1)
        timepy.append(temppy.Time[0])
        distpy.append(temppy.Distance[0])
        sameco.append(len(temp4))
        ncomps.append(max(temp2.Overall))
        gcomps.append(max(temp2.Gender_Place))
        gpct.append(100*max(temp2.Gender_Place) / max(temp2.Overall))
        timebt.append(int((datetime.strptime(temp3.Clean_Race_Date[list(temp3.Clean_Race_Date).index(compdata.Clean_Race_Date[i])], '%m/%d/%Y') - datetime.strptime(temp3.Clean_Race_Date[list(temp3.Clean_Race_Date).index(compdata.Clean_Race_Date[i])+1], '%m/%d/%Y')).days))
        expyrs.append(int(compdata.RACE_Year[i]) - int(list(temp3.RACE_Year)[-1]))
        expraces.append(len(temp3) - 1 - int(list(temp3.RACE_ID).index(compdata.RACE_ID[i])))
        
    else: # Else who cares
        
        racedpy.append(0)
        timepy.append(None)
        distpy.append(None)
        sameco.append(None)
        ncomps.append(None)
        gcomps.append(None)
        gpct.append(None)
        timebt.append(None)
        expyrs.append(None)
        expraces.append(None)

# Make a dataframe and merge with compdata

racedpy = pd.Series(racedpy, name = 'Raced_PY')
timepy = pd.Series(timepy, name = 'Time_PY')
distpy = pd.Series(distpy, name = 'Distance_PY')
ncomps = pd.Series(ncomps, name = 'Competitors')
gcomps = pd.Series(gcomps, name = 'Gender_Competitors')
gpct = pd.Series(gpct, name = 'Gender_Pct')
sameco = pd.Series(sameco, name = 'Same_County_Competitors')
timebt = pd.Series(timebt, name = 'Days_Since_Last_Race')
expyrs = pd.Series(expyrs, name = 'Experience_Years')
expraces = pd.Series(expraces, name = 'Experience_Races')

newdf = pd.concat([racedpy, timepy, distpy, ncomps, gcomps, gpct, sameco, timebt, expyrs, expraces], axis = 1)
compdata = pd.concat([compdata, newdf], axis = 1)

# Before getting travel distances, we subset for american runners and races only

compdata = compdata[compdata.RACE_Nation == 'US']
compdata = compdata[compdata.Country == 'USA'].reset_index(drop = True)

# The functions for mapping city to county for runners

def coords_runner(inp):
    
    try:
        
        city = inp.City.lower()
        state = inp.State.upper().strip('"')
        sx = list(ccmap['State short']).index(state)
        st = ccmap['State full'][sx]
    
    except:
        
        city = 'hell'
        state = 'also hell'
        st = 'still hell'
    
    try:
        
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County']
        
    except:
        
        county = 'NOPE'
    
    if county != 'NOPE':
        
        if (county[0:5] == 'saint') and (county != 'saint marys'):
            
            back = county[5:]
            county = 'st.' + back
        
        elif county == 'virginia beach city':
            
            county = 'virginia beach'
            
        elif county ==  'alexandria city':
            
            county = 'alexandria'
            
        elif county == 'norfolk city':
            
            county = 'norfolk'
            
        elif county == 'fredericksburg city':
            
            county = 'fredericksburg'
            
        elif county == 'chesapeake city':
            
            county = 'chesapeake'
        
        elif county == 'lexington city':
            
            county = 'lexington'
            
        elif county == 'falls church city':
            
            county = 'falls church'
            
        elif county == 'staunton city':
            
            county = 'staunton'
            
        elif county == 'la porte':
            
            county = 'laporte'
            
        elif county == 'suffolk city':
            
            county = 'suffolk'
            
        elif county == 'newport news city':
            
            county = 'newport news'
            
        elif county == 'hampton city':
            
            county = 'hampton'
        
        elif county == 'manassas city':
            
            county = 'manassas'
            
        elif county == 'harrisonburg city':
            
            county = 'harrisonburg'
            
        elif county == 'prince georges':
            
            county = "prince george's"
            
        elif county == 'la salle':
            
            county = 'lasalle'
            
        elif county == 'saint marys':
            
            county = "st. mary's"
            
        elif county == 'lynchburg city':
            
            county = 'lynchburg'
            
        elif county == 'portsmouth city':
            
            county = 'portsmouth'
            
        elif county == 'poquoson city':
            
            county = 'poquoson'
            
        elif county == 'queen annes':
            
            county = "queen anne's"
            
        elif county == 'matanuska susitna':
            
            county = 'matanuska-susitna'
            
        elif county == 'st joseph':
            
            county = 'st. joseph'
            
        elif county == 'de kalb':
            
            county = 'dekalb'
            
        elif county == 'waynesboro city':
            
            county = 'waynesboro'
            
        elif county == 'winchester city':
            
            county = 'winchester'
            
        elif county == 'martinsville city':
            
            county = 'martinsville'
            
        elif county == 'danville city':
            
            county = 'danville'
            
        elif county == 'bristol city':
            
            county = 'bristol'
            
        elif county == 'de witt':
            
            county = 'dewitt'
            
        elif county == 'galax city':
            
            county = 'galax'
            
        elif county == 'colonial heights city':
            
            county = 'colonial heights'
            
    tmp = latlong[latlong.Province_State == st]
    tmp = tmp[tmp.Admin2 == county.lower()]
    
    if len(tmp) > 0:
        
        try:
            
            lat = tmp.iloc[0]['Lat']
            long = tmp.iloc[0]['Long_']
            coord = [lat,long]
            
        except:
            
            coord = [None,None]
        
    else:
        
        coord = [None,None]
    
    return coord

# The functions for mapping city to county for runners

def coords_race(inp):
    
    try:
        
        city = inp.RACE_City.lower()
        state = inp.RACE_State.upper().strip('"')
        sx = list(ccmap['State short']).index(state)
        st = ccmap['State full'][sx]
    
    except:
        
        st = 'hell'
        state = 'also hell'
        st = 'still hell'
    
    try:
        
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County']
        
    except:
        
        county = 'NOPE'
    
    if county != 'NOPE':
        
        if (county[0:5] == 'saint') and (county != 'saint marys'):
            
            back = county[5:]
            county = 'st.' + back
        
        elif county == 'virginia beach city':
            
            county = 'virginia beach'
            
        elif county ==  'alexandria city':
            
            county = 'alexandria'
            
        elif county == 'norfolk city':
            
            county = 'norfolk'
            
        elif county == 'fredericksburg city':
            
            county = 'fredericksburg'
            
        elif county == 'chesapeake city':
            
            county = 'chesapeake'
        
        elif county == 'lexington city':
            
            county = 'lexington'
            
        elif county == 'falls church city':
            
            county = 'falls church'
            
        elif county == 'staunton city':
            
            county = 'staunton'
            
        elif county == 'la porte':
            
            county = 'laporte'
            
        elif county == 'suffolk city':
            
            county = 'suffolk'
            
        elif county == 'newport news city':
            
            county = 'newport news'
            
        elif county == 'hampton city':
            
            county = 'hampton'
        
        elif county == 'manassas city':
            
            county = 'manassas'
            
        elif county == 'harrisonburg city':
            
            county = 'harrisonburg'
            
        elif county == 'prince georges':
            
            county = "prince george's"
            
        elif county == 'la salle':
            
            county = 'lasalle'
            
        elif county == 'saint marys':
            
            county = "st. mary's"
            
        elif county == 'lynchburg city':
            
            county = 'lynchburg'
            
        elif county == 'portsmouth city':
            
            county = 'portsmouth'
            
        elif county == 'poquoson city':
            
            county = 'poquoson'
            
        elif county == 'queen annes':
            
            county = "queen anne's"
            
        elif county == 'matanuska susitna':
            
            county = 'matanuska-susitna'
            
        elif county == 'st joseph':
            
            county = 'st. joseph'
            
        elif county == 'de kalb':
            
            county = 'dekalb'
            
        elif county == 'waynesboro city':
            
            county = 'waynesboro'
            
        elif county == 'winchester city':
            
            county = 'winchester'
            
        elif county == 'martinsville city':
            
            county = 'martinsville'
            
        elif county == 'danville city':
            
            county = 'danville'
            
        elif county == 'bristol city':
            
            county = 'bristol'
            
        elif county == 'de witt':
            
            county = 'dewitt'
            
        elif county == 'galax city':
            
            county = 'galax'
            
        elif county == 'colonial heights city':
            
            county = 'colonial heights'
            
    tmp = latlong[latlong.Province_State == st]
    tmp = tmp[tmp.Admin2 == county.lower()]
    
    if len(tmp) > 0:
        
        try:
            
            lat = tmp.iloc[0]['Lat']
            long = tmp.iloc[0]['Long_']
            coord = [lat,long]
            
        except:
            
            coord = [None,None]
        
    else:
        
        coord = [None,None]
    
    return coord

# Use the functions to get the coordinates for runner and race locations

rnr_coords = []
race_coords = []

for i in range(len(compdata)): # A loop is used here just because I'm impatient and want to see progress
    
    print('Getting coordinates ' + str(i+1) + ' of ' + str(len(compdata)) + '.......')
    
    rnr_coords.append(coords_runner(compdata.iloc[i]))
    race_coords.append(coords_race(compdata.iloc[i]))

# Use geopy.distances.geodesic to compute distances between runner and race for all observations

distances = []

for i in range(len(rnr_coords)):
    
    print('Computing distance ' + str(i+1) + ' of ' + str(len(rnr_coords)) + '.......') # Visualize progress
    
    if (rnr_coords[i] != [None,None]) and (race_coords[i] != [None,None]):
        
        distances.append(geodesic(rnr_coords[i], race_coords[i]).mi)
        
    else:
        
        distances.append(None)

# Adding distances to the dataframe

distances = pd.Series(distances, name = 'Travel_Distance')
compdata = pd.concat([compdata, distances], axis = 1).reset_index(drop = True)

# Convert time into seconds and add to dataframe

def time_to_sec(inp):
    
    try:
        
        a = inp.find(':')
        b = inp[a+1:].find(':')
        seconds = 1*int(inp[a+b+2:]) + 60*int(inp[a+1:a+b+1]) + 3600*int(inp[:a])
        
    except:
        
        seconds = None
    
    return seconds

seconds = [time_to_sec(str(x)) for x in compdata.Time]
seconds_py = [time_to_sec(str(x)) for x in compdata.Time_PY]

seconds = pd.Series(seconds, name = 'Seconds')
seconds_py = pd.Series(seconds_py, name = 'Seconds_PY')

compdata = pd.concat([compdata, seconds, seconds_py], axis = 1)

# Calculate the percent change in performance when Raced_PY == 1 and add to dataframe

delta_time = [100*(compdata.Seconds[i] - compdata.Seconds_PY[i]) / compdata.Seconds_PY[i] for i in range(len(compdata))]
delta_distance = [100*(compdata.Distance[i] - compdata.Distance_PY[i]) / compdata.Distance_PY[i] for i in range(len(compdata))]

delta_time = pd.Series(delta_time, name = 'Y_time')
delta_distance = pd.Series(delta_distance, name = 'Y_distance')
compdata = pd.concat([compdata, delta_time, delta_distance], axis = 1)

# Create a time since COVID variable to control for the impact of COVID on training and add to dataframe

compact = [max(0,int((datetime.strptime(compdata.Clean_Race_Date[i], '%m/%d/%Y') - datetime.strptime('04/01/2020', '%m/%d/%Y')).days)) for i in range(len(compdata))]
compact = pd.Series(compact, name = 'Days_Since_COVID')
compdata = pd.concat([compdata, compact], axis = 1)

# Creating an in-state variable

instate = [1 if compdata.State[i] == compdata.RACE_State[i] else 0 for i in range(len(compdata))]
instate = pd.Series(instate, name = 'In_State')
compdata = pd.concat([compdata, instate], axis = 1)

# Write the complete dataframe to file

compdata.to_csv(filepath + 'output.csv', index = False)

