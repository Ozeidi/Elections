# -*- coding: utf-8 -*-
import requests
import json
import codecs
import time
import os
def consolidate():
    Consolidated= codecs.open('{}\output\consolidated1.txt'.format(os.getcwd()),'wb','utf-8')
    for w_id in range(1,62,1):
        path='{0}\output\{1}.txt'.format(os.getcwd(),w_id)
        w_file=codecs.open(path,'rb','utf-8')
        if w_id>1: w_file.readline()
        data=w_file.readlines()
        Consolidated.writelines(data)
        print 'File {0} written'.format(w_id)
        w_file.close()
    Consolidated.close()

def main():

    for w_id in range(1,3,1):
        records_per_page=2000
        root_url="https://election.gov.om/election-service/api/Voter/GetRegisteredVoterFinalList/{0}".format(w_id)
        path='{0}\output\{1}.txt'.format(os.getcwd(),w_id)
        Output=codecs.open(path,'wb','utf-8')
        
        Output.write('row_no;wilaya_id;name')
        Output.write('\n')
        r=requests.get(root_url+'/1/'+str(records_per_page),'utf8')
        j=json.loads(r.content)
        print j
        totalPages=j['totalPages']+1
        for x in range(1,totalPages,1):
            page_url=root_url+'/{0}/{1}'.format(x,records_per_page)
            r_completed=False
            while r_completed==False:
                try:
                    r=requests.get(page_url,'utf8')
                    r_completed=True
                except:time.sleep(60)

            j=json.loads(r.content)
            for v in j['voters']:
                l=u'{0};{1};{2}'.format(v['row_no'],v['wilaya_id'],v['voter_name'])
                print l
                Output.write(l)
                Output.write('\n')
        Output.close()
    print "finished"
    consolidate()
if __name__ == "__main__":
    main()
