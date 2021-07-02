# This script scrapes the DUV site for ultramarathon race results data

# Importing required modules

import pandas as pd
import urllib
from bs4 import BeautifulSoup as bs

# Specifying the directory for writing data files -- update this as appropriate

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/ultracompetitive/international_data/'

# Defining the components of the url

url0 = 'https://statistik.d-u-v.org/getintbestlist.php?year='
url1 = '&dist='
url2 = '&gender='
url3 = '&cat=all&nat=all&label=&hili=none&tt=netto&Submit'#.x=24&Submit.y=9'

# Defining the paramater sets for the queries

years = [str(y) for y in range(2005,2021)]
distances = ['24h', '48h', '6d', '12h', '6h', '100mi', '100km', '50mi', '50km']
sexes = ['W', 'M']

# Scraper loop

for sex in sexes:
    
    for distance in distances:
        
        annum = []
        rank = []
        performance = []
        
        for year in years:
            
            url = url0 + year + url1 + distance + url2 + sex + url3 # Define the url
            page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'})  # Go to the url and get some data 
            response = urllib.request.urlopen(page) # Go to the url and get some data
            soup = bs(response, 'html.parser') # Go to the url and get some data
            data = soup.find_all('tr') # Get the correct type of data
            data = data[11:len(data)-2] # Pare down to the runner data
            
            for d in data:
                
                d = str(d) # Make it a string
                id1 = d.find('<b>') # Identifying the potential rank
                id2 = d[id1:].find('</b>') # Identifying the potential rank
                tmp = d[id1+3:id1+3+id2-3] # Identifying the potential rank
                
                if len(tmp) < 6:
                    
                    rank.append(tmp) # Rank for that year/gender
                    d = d[id1+3+id2:] # Removing used text
                    id1 = d.find('nowrap="nowrap">') # Identifying the performance
                    id2 = d[39+16:].find('</td>') # Identifying the performance
                    performance.append(d[id1+16:id1+16+id2]) # Time/distance
                    annum.append(year) # Race year
                    print(annum[-1])
                    print(rank[-1])
                    print(performance[-1])
                
        annum = pd.Series(annum, name = 'Year')
        rank = pd.Series(rank, name = 'Rank')
        performance = pd.Series(performance, name = 'Performance')
        df = pd.concat([annum, rank, performance], axis = 1)
        df.to_csv(filepath + distance + '_' + sex + '.csv', index = False)

