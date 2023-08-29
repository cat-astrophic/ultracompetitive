# This script scrapes data from worldathletics

# Importing required modules

import urllib
import pandas as pd
from bs4 import BeautifulSoup as bs

# Filepath for to save data to

filepath = 'D:/ultracompetitive/international_data/iaaf/iaaf.csv'

# Parameters

event_list = ['800-metres', '1500-metres', '5000-metres', '10000-metres', 'marathon']
gender_list = ['women', 'men']

# Distance events loop

ranks = []
marks = []
names = []
nats = []
years = []
genders = []
events = []

for event in event_list:
    
    for gender in gender_list:
        
        for year in range(2001,2023):
            
            for p in range(1,21):
                
                print(event + ' :: ' + gender + ' :: ' + str(year) + ' :: ' + str(p))
                
                try:
                    
                    url = 'https://worldathletics.org/records/toplists/middle-long/' + event + '/outdoor/' + gender + '/senior/' + str(year) + '?regionType=world&timing=all&page=' + str(p) + '&bestResultsOnly=false'
                    page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'})
                    response = urllib.request.urlopen(page)
                    soup = bs(response, 'html.parser')
                    data = soup.find_all('td')
                    
                    for i in range(100):
                        
                        ranks.append(str(data[(10*i)])[20:-5].replace(' ', ''))
                        marks.append(str(data[(10*i) + 1])[20:].replace(' ', '').replace('\n</td>', ''))
                        names.append(str(data[(10*i) + 2])[53:str(data[(10*i) + 2])[53:].find('\n')+51])
                        nats.append(str(data[(10*i) + 4])[29:32])
                        years.append(year)
                        genders.append(gender)
                        events.append(event)
                        
                except:
                    
                    continue

# Make a dataframe

ranks = pd.Series(ranks, name = 'Rank')
marks = pd.Series(marks, name = 'Mark')
names = pd.Series(names, name = 'Runner')
nats = pd.Series(nats, name = 'Nation')
years = pd.Series(years, name = 'Year')
genders = pd.Series(genders, name = 'Gender')
events = pd.Series(events, name = 'Event')

df = pd.concat([years, genders, events, ranks, marks, names, nats], axis = 1)

# Save the dataframe

df.to_csv(filepath, index = False)

