#this file convert .pck file into .csv file to be later parsed into R using python
import pickle
import csv

f = open("field2run3-s_area-articleIDs.pck", 'rU')
fOut = open('area.csv', 'wb')
p = pickle.load(f)
lol = []
for key, val in p.iteritems():
    lol.append(val)
top11 = sorted(lol, key = len, reverse = True)[:11]
writer = csv.writer(fOut, delimiter = ',')
for line in top11:
    writer.writerow(line)
    
