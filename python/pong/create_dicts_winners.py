

import psycopg2 as pg
import psycopg2.extras as ex
import numpy as np
import _pickle as pickle
import time
import os
import os.path
from pathlib import Path


############################################################

def create_cached_dicts(marketid, conn):

  filename_win_dict = "pickles/win_dict_" + marketid + ".pickle"
  filename_plc_dict = "pickles/plc_dict_" + marketid + ".pickle"

  win_dict={}
  plc_dict={}
  cur = conn.cursor()
  cur.execute("""
    select R.SELECTIONID from ARUNNERS R
    where R.MARKETID = %s
    and R.STATUS = %s """,(marketid,'WINNER'))
  idx=0
  datarows = cur.fetchall()
  for datarow in datarows:
    selid = datarow['selectionid']
    win_dict[idx] = selid
    idx = idx +1
  cur.close()
  
    
  cur = conn.cursor()
  cur.execute("""
     select MP.* from AMARKETS MW, AMARKETS MP
     where MW.EVENTID = MP.EVENTID
     and MW.STARTTS = MP.STARTTS
     and MW.MARKETID = %s
     and MP.MARKETTYPE = 'PLACE'
     and MP.NUMWINNERS = %s
     and MW.MARKETTYPE = 'WIN' """,(marketid,3))

  datarows = cur.fetchall()
  found=False
  for datarow in datarows:
    plcid = datarow['marketid']
    found=True

  cur.close()

  if Found :
    cur = conn.cursor()
    cur.execute("""
      select R.SELECTIONID from ARUNNERS R
      where R.MARKETID = %s
      and R.STATUS = %s """,(marketid,'WINNER'))
    idx=0
    datarows = cur.fetchall()
    for datarow in datarows:
      selid = datarow['selectionid']
      plc_dict[idx] = selid
      idx = idx +1
    cur.close()
    pickle.dump(plc_dict, open(filename_plc_dict, 'wb'))


  
  pickle.dump(win_dict, open(filename_win_dict, 'wb'))

  print('create_cached_dicts','done')

############################################################



########################################################


def do_cache():
  try:
    conn = pg.connect(
      host="127.0.0.1",
      #host="192.168.1.202",
      database="bnl",
      user="bnl",
      password="bnl",
      cursor_factory=ex.DictCursor)


    marketlist = []
    filename = "pickles/marketlist" + ".pickle"

    if cache_exists(filename):
      marketlist = pickle.load(open(filename, 'rb'))


# market is a dict:
    for market in marketlist:
      start_time = time.time()
      print('marketid', market['marketid'])
      create_cached_dicts(market['marketid'], conn)
      #per market
      elapsed_time = time.time() - start_time
      print('elapsed_time', market['marketid'], elapsed_time)
#      break
  finally :
    conn.close();

if __name__ == '__main__':
  do_cache()



