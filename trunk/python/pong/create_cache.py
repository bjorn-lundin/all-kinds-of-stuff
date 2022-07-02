

import psycopg2 as pg
import psycopg2.extras as ex
import numpy as np
import _pickle as pickle
import time
import os
import os.path
from pathlib import Path

BACKPRICE  = 0
LAYPRICE   = 1
BACKREWARD = 2
LAYREWARD  = 3


def cache_exists(filenname):
      my_file_back = Path(filenname)
      return my_file_back.is_file()


############################################################


def create_cached_dicts(marketid, conn):

  filename_selid_idx = "pickles/selid_idx_" + marketid + ".pickle"
  filename_idx_selid = "pickles/idx_selid_" + marketid + ".pickle"

  if cache_exists(filename_selid_idx):
    return pickle.load(open(filename_selid_idx, 'rb'))
  else:
    dict_selid_idx={}
    dict_idx_selid={}
    cur = conn.cursor()
    cur.execute("select SELECTIONID from ARUNNERS where MARKETID = %s and STATUS <> %s order by SORTPRIO",(marketid,'REMOVED'))
    idx=0
    datarows = cur.fetchall()
    for datarow in datarows:
      selid = datarow['selectionid']
      dict_selid_idx[selid] = idx
      dict_idx_selid[idx] = selid
      idx = idx +1
    cur.close()

    pickle.dump(dict_idx_selid, open(filename_idx_selid, 'wb'))
    pickle.dump(dict_selid_idx, open(filename_selid_idx, 'wb'))

    print('dict_selid_Idx',dict_selid_idx)
    return dict_selid_idx

############################################################
def create_empty_matrix(marketid, conn):
    num_rows = 0
    num_cols = 16
    cur = conn.cursor()
    cur.execute("select max(cnt) mx from (select SELECTIONID, count('a') cnt from APRICESHISTORY where MARKETID = %s group by SELECTIONID) tmp",(marketid,))
    datarow = cur.fetchone()
    if datarow is not None:
#          print(row)
      print('datarow[mx]',datarow['mx'])
      num_rows = datarow['mx']
    cur.close()
    return np.zeros( (num_rows, num_cols, 4) )
############################################################


def create_cache(marketid, conn):
  filename = "pickles/cache_" + marketid + ".pickle"

  if cache_exists(filename):
    cache_matrix = pickle.load(open(filename, 'rb'))
    dict_selid_idx = create_cached_dicts(marketid, conn)

#    for a in range(cache_matrix.shape[0]):
#        for b in range(cache_matrix.shape[1]):
#            for c in range(cache_matrix.shape[2]):
#                print ('cache_matrix2', a,b,c ,cache_matrix [a][b][c])

  else:
    dict_selid_idx = create_cached_dicts(marketid, conn)
    cache_matrix = create_empty_matrix(marketid, conn)
    print('dict_selid_Idx',dict_selid_idx)

    for selid, col in dict_selid_idx.items():
      print('selid',selid,'col',col)
      cur = conn.cursor()
      cur.execute("""
        select LR.PROFIT layprofit, BR.PROFIT backprofit, PH.*
        from APRICESHISTORY PH, AREWARDS LR, AREWARDS BR
        where true
        and PH.MARKETID = LR.MARKETID
        and PH.SELECTIONID = LR.SELECTIONID
        and PH.PRICETS = LR.PRICETS
        and LR.SIDE = 'LAY'
        and PH.MARKETID = BR.MARKETID
        and PH.SELECTIONID = BR.SELECTIONID
        and PH.PRICETS = BR.PRICETS
        and BR.SIDE = 'BACK'
        and PH.MARKETID = %s
        and PH.SELECTIONID = %s
        order by PH.PRICETS """,(marketid,selid))
      row = 0
      datarows = cur.fetchall()
      for datarow in datarows:
        #print('cache_matrix1',row, col)
        #print('cache_matrix1',datarow)
        #print('cache_matrix1',cache_matrix)
        #else:
          cache_matrix  [row] [col] [LAYREWARD]  = datarow['layprofit']
          cache_matrix  [row] [col] [BACKREWARD] = datarow['backprofit']
          cache_matrix  [row] [col] [LAYPRICE]   = datarow['layprice']
          cache_matrix  [row] [col] [BACKPRICE]  = datarow['backprice']
            
          #for c in range(cache_matrix.shape[2]):
          #  print('cache_matrix1',cache_matrix [row] [col] [c], row, col, c)

          row = row +1

      cur.close()

    pickle.dump(cache_matrix, open(filename, 'wb'))



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
    else:
      #list of markets to use
      cur = conn.cursor()
      cur.execute("""
        select M.* from OKMARKETS OK, AMARKETS M
        where true
        and OK.MARKETID = M.MARKETID
        and OK.MARKETTYPE = %s
        and M.STARTTS >= %s
        order by M.STARTTS""",
        ("WIN",'2016-04-01 00:00:00.000'))
      rows = cur.fetchall()
      for row in rows:
      #  marketidlist.append(row['marketid'])
        marketlist.append(row)
      cur.close()
      pickle.dump(marketlist, open(filename, 'wb'))

#    for market in marketlist:
#      print(market)
#      print(market['marketid'])
#      print(market['markettype'])
#      print(market['startts'])
#      break
#get_observation idx 0 ['1.124156004', 7789090, datetime.datetime(2016, 4, 10, 15, 4, 33, 607000), 'ACTIVE', Decimal('13854.00'), Decimal('360.00'),

# market is a dict:
    for market in marketlist:
      start_time = time.time()
      print('marketid', market['marketid'])
      create_cache(market['marketid'],conn)
      #per market
      elapsed_time = time.time() - start_time
      print('elapsed_time', market['marketid'], elapsed_time)
#      break
  finally :
    conn.close();

if __name__ == '__main__':
  do_cache()



