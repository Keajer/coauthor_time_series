# this file is to get the core authors in a field
import glob
from bs4 import BeautifulSoup
import pickle

fileNames = glob.glob('*.xml')

auid_set = set()

for f in fileNames:
    xml = open(f, 'rU').read()
    soup = BeautifulSoup(xml)
    for ar in soup.findAll('article_rec'):
        if ar.find('au').find('person_id'):
            auid_set.add(ar.find('au').find('person_id').text)

with open('auid.pck', 'wb') as auid:
    pickle.dump(auid_set, auid)
