# this script is to get the reference information from xml files in both proceeding and periodical folders
# i will run in proceeding folder first and then change out the commented lines to run again in periodical
import glob
import cPickle as pickle
from bs4 import BeautifulSoup

fileNames = glob.glob('*.xml')
proceeding = open('proceeding.pck', 'rU')
refInfo = pickle.loads(proceeding.read())
proceeding.close()

# refInfo = dict() # this line is for the first run

for f in fileNames:
    xml = open(f, 'rU').read()
    soup = BeautifulSoup(xml)
    for ref in soup.findAll('ref'):
        if ref.find('ref_obj_id'):
            if ref.find('ref_obj_id').text not in refInfo:
                refInfo[ref.find('ref_obj_id').text] = ref.find('ref_text').text

# below two lines are for the first run
# with open('proceeding.pck', 'wb') as proceeding: 
#     pickle.dump(refInfo, proceeding)
# proceeding.close()

# # below is for second run in periodical folder
with open('ref-info.json', mode = 'wb') as refInfoJson:
    json.dump(refInfo, refInfoJson, indent = 2)
refInfoJson.close()