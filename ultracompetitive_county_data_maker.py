# This script adds in the county level socioeconomic and altitude data

# Importing required modules

import pandas as pd

# Specifying the path to the data -- update this accordingly!

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultracompetitive/'

# Reading in the data

compdata = pd.read_csv(filepath + 'output.csv')
edu = pd.read_csv(filepath + 'Education.csv', engine = 'python')
labor = pd.read_csv(filepath + 'Unemployment.csv', engine = 'python')
inc = pd.read_csv(filepath + 'Income.csv', engine = 'python')
ccmap = pd.read_csv(filepath + 'ccmap.csv', sep = '|')
cases = pd.read_csv(filepath + 'time_series_covid19_confirmed_US.csv')

# Standardizing ccmap for DC

ccmap = ccmap.replace(to_replace = 'Washington, D.C.', value = 'District of Columbia')
ccmap.City = ccmap.City.str.lower()

# Making county names in cases.csv all lower case

lows = [str(c).lower() for c in cases.Admin2]
lows = pd.Series(lows, name = 'County')
cases = pd.concat([cases, lows], axis = 1)

# Defining a helper function for assigning FIPS via ccmap and cases

def flipsadelphia(inp):
   
    city = inp.City
    state = inp.State.upper().strip('"')
    sx = list(ccmap['State short']).index(state)
    st = ccmap['State full'][sx]
   
    try:
       
        cc = ccmap[ccmap['City'] == city]
        cc = cc[cc['State short'] == state]
        county = cc.iloc[0]['County'].lower()
       
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
       
        try:
           
            tmp = cases[cases['County'] == county]
            tmp = tmp[tmp['Province_State'] == st]
            flips = int(tmp.iloc[0]['FIPS'])
       
        except:
           
            flips = None
       
    else:
       
        flips = None
       
    return flips

# Using the function to get FIPS codes

fips = []

for i in range(len(compdata)):
   
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
   
    fips.append(flipsadelphia(compdata.iloc[i]))

# Adding FIPS codes to the main dataframe

fips = pd.Series(fips, name = 'FIPS')
compdata = pd.concat([compdata, fips], axis = 1)

# Use FIPS codes to get county level census/ERS data

# Education data

ed_cols = ['percentsomehs_', 'percenthsgrad_', 'percentsomecollege_', 'percentassociates_', 'percentbachelors_', 'percentgrad_degree_'] # base of column headings

somehs = []
hs = []
someuni = []
ass = []
bach = []
grad = []

for i in range(len(compdata)):
    
    print('Education :: ' +  str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
    
    try:
        
        fip = int(compdata.iloc[i]['FIPS']) # Get the location of the event
        yr = compdata.iloc[i]['RACE_Year'] - 1 # Get the year of the event
        
        tmp = edu[edu['ï»¿fips'] == fip].reset_index(drop = True) # Subset for location
        
        somehs.append(tmp[ed_cols[0] + str(yr)][0])
        hs.append(tmp[ed_cols[1] + str(yr)][0])
        someuni.append(tmp[ed_cols[2] + str(yr)][0])
        ass.append(tmp[ed_cols[3] + str(yr)][0])
        bach.append(tmp[ed_cols[4] + str(yr)][0])
        grad.append(tmp[ed_cols[5] + str(yr)][0])
        
    except:
        
        somehs.append(None)
        hs.append(None)
        someuni.append(None)
        ass.append(None)
        bach.append(None)
        grad.append(None)

somehs = pd.Series(somehs, name = 'Some_HS')
hs = pd.Series(hs, name = 'HS')
someuni = pd.Series(someuni, name = 'Some_Uni')
ass = pd.Series(ass, name = 'Associate')
bach = pd.Series(bach, name = 'Bachelor')
grad = pd.Series(grad, name = 'Graduate')

compdata = pd.concat([compdata, somehs, hs, someuni, ass, bach, grad], axis = 1)

# Unemployment data

unemp = []

for i in range(len(compdata)):
   
    print('Unemployment :: ' + str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
   
    try:
       
        fip = compdata.iloc[i]['FIPS'] # Get the location of the event
        yr = compdata.iloc[i]['RACE_Year'] - 1 # Get the year of the event
       
        tmp = labor[labor['ï»¿fips'] == fip] # Subset for location
        tmp = tmp[tmp.year == yr].reset_index(drop = True) # Subset for year
       
        unemp.append(tmp.unemploymentrate[0])
       
    except:
       
        unemp.append(None)

unemp = pd.Series(unemp, name = 'Unemployment_Rate')
compdata = pd.concat([compdata, unemp], axis = 1)

# Income data

hhinc = []

for i in range(len(compdata)):
   
    print('Income :: ' + str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
   
    try:
       
        fip = compdata.iloc[i]['FIPS'] # Get the location of the event
        yr = compdata.iloc[i]['RACE_Year'] - 1 # Get the year of the event
       
        tmp = inc[inc['countyid'] == fip] # Subset for location
        tmp = tmp[tmp['ï»¿year'] == yr].reset_index(drop = True) # Subset for year
       
        hhinc.append(tmp.medianhouseholdincome[0])
       
    except:
       
        hhinc.append(None)

hhinc = pd.Series(hhinc, name = 'Median_Household_Income')
compdata = pd.concat([compdata, hhinc], axis = 1)

# Write dataframe to file

compdata.to_csv(filepath + 'output.csv', index = False)

