import glob
from bs4 import BeautifulSoup

fileNames = glob.glob('*.xml')
summary2 = open('summary2.txt', 'wb')

refid_counter = 0
pub_counter = 0

for f in fileNames:
    xml = open(f, 'rU').read()
    soup = BeautifulSoup(xml)
    for ar in soup.findAll('article_rec'):
        if ar.find('au').find('person_id'):
            year = ar.find('article_publication_date').text[-4:]
            if year != '':
                if 1991 <= int(year) <= 2010:
                    pub_counter += 1
                    if ar.find('ref_obj_id'):
                        refid_counter += 1

summary2.write('Percantage of Reference with Id: ' + str(refid_counter / float(pub_counter) * 100) + '%' + '\n')
summary2.close()
