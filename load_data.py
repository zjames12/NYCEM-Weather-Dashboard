# -*- coding: utf-8 -*-
"""
Created on Wed Jun 21 09:14:58 2023

@author: Zach
"""

import urllib.request, json 
import pandas as pd
import time

t0 = time.time()
locs = pd.read_csv("./nyc_grid_locs.csv")

query_base = "https://api.weather.gov/gridpoints/OKX/"

for i in range(len(locs["X"])):
    query = query_base + str(locs["X"][i]) + "," + str(locs["Y"][i])
    try:
        with urllib.request.urlopen(query) as url:
            data = json.load(url)
            #print(data)
            f = open("data/" + str(locs["X"][i]) + "," + str(locs["Y"][i]) +".geojson", "w")
            f.write(str(data))
            f.close()
    except:
        print(query)
        
t1 = time.time()

total = t1-t0
print(total)