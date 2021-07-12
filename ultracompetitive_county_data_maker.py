# This script adds in the county level socioeconomic and altitude data

# Importing required modules

import pandas as pd

# Specifying the path to the data -- update this accordingly!

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultracompetitive/'

# Reading in the data

compdata = pd.read_csv(filepath + 'output.csv')
edu = pd.read_csv(filepath + 'Education.csv', engine = 'python')
pop = pd.read_csv(filepath + 'PopulationEstimates.csv', engine = 'python')
unemp = pd.read_csv(filepath + 'Unemployment.csv', engine = 'python')
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

edu_cc = []
edu_bs = []

for i in range(len(compdata)):
    
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
    
    try:
        
        fip = compdata.iloc[i]['FIPS']
        tmp = edu[edu['FIPS Code'] == fip]
        edu_cc.append(tmp.iloc[0]["Percent of adults completing some college or associate's degree, 2015-19"])
        edu_bs.append(tmp.iloc[0]["Percent of adults with a bachelor's degree or higher, 2015-19"])
        
    except:
        
        edu_cc.append(None)
        edu_bs.append(None)

edu_cc = pd.Series(edu_cc, name = 'Some_College_Ass')
edu_bs = pd.Series(edu_bs, name = 'College_Degree')
compdata = pd.concat([compdata, edu_cc, edu_bs], axis = 1)

# Population data

popu = []

for i in range(len(compdata)):
    
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
    
    try:
        
        fip = compdata.iloc[i]['FIPS']
        tmp = pop[pop['FIPStxt'] == fip]
        popu.append(tmp.iloc[0]['POP_ESTIMATE_2019'])
                
    except:
        
        popu.append(None)

popu = pd.Series(popu, name = 'County_Population')
compdata = pd.concat([compdata, popu], axis = 1)

# Unemployment and Income data

nojo = []
hhinc = []

for i in range(len(compdata)):
    
    print(str(i+1) + ' of ' + str(len(compdata)) + '.......') # Visualize progress
    
    try:
        
        fip = compdata.iloc[i]['FIPS']
        tmp = unemp[unemp['fips_txt'] == fip]
        tmpu = tmp[tmp['Attribute'] == 'Unemployment_rate_2019']
        tmpi = tmp[tmp['Attribute'] == 'Median_Household_Income_2019']
        nojo.append(tmpu.iloc[0]['Value'])
        hhinc.append(tmpi.iloc[0]['Value'])
        
    except:
        
        nojo.append(None)
        hhinc.append(None)

nojo = pd.Series(nojo, name = 'Unemployment_Rate')
hhinc = pd.Series(hhinc, name = 'Median_Household_Income')
compdata = pd.concat([compdata, nojo, hhinc], axis = 1)

# Write dataframe to file

compdata.to_csv(filepath + 'output.csv', index = False)

