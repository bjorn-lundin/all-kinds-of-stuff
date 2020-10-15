

import psycopg2 as pg
import psycopg2.extras as ex
import numpy as np

def test():
  try:
    conn = pg.connect(
      host="192.168.1.136",
      database="bnl",
      user="bnl",
      password="bnl",
      cursor_factory=ex.DictCursor)

    marketid = '1.146187859'
    selectionid = 19068149
    timestamp = '2018-08-01 15:14:06.826'
    side='LAY'

    cur = conn.cursor()
    cur.execute("select * from AREWARDS where MARKETID = %s and selectionid = %s and PRICETS = %s and SIDE = %s", (marketid, selectionid, timestamp, side))
    row = cur.fetchone()
    if row is not None:
      print(row)
      print(row['profit'])
    cur.close()


    cur = conn.cursor()
    cur.execute("select * from OKMARKETS OK, AMARKETS M where OK.MARKETID = M.MARKETID " +
                "and OK.MARKETTYPE='WIN' order by M.STARTTS")
    rows = cur.fetchall()
    for row in rows:
      print(row['marketid'])
    cur.close()


    print("START")


#get_observation idx 0 ['1.124156004', 7789090, datetime.datetime(2016, 4, 10, 15, 4, 33, 607000), 'ACTIVE', Decimal('13854.00'), Decimal('360.00'),


    num_rows = 0
    num_cols = 16
    cur = conn.cursor()
    cur.execute("select max(cnt) mx from (select selectionid, count('a') cnt from apriceshistory where marketid = '1.124156004' group by selectionid) tmp")
    row = cur.fetchone()
    if row is not None:
      print(row)
      print(row['mx'])
      num_rows = row['mx']
    cur.close()
    odds_matrix = np.zeros( (num_rows, num_cols) )

    dictSrt_Selid={}
    dictSelid_Srt={}
    cur = conn.cursor()
    cur.execute("select selectionid, sortprio from arunners where marketid = '1.124156004' ")
    rows = cur.fetchall()
    for row in rows:
      print(row)
      srt=row['sortprio']
      selid=row['selectionid']
      dictSrt_Selid[srt]=selid
      dictSelid_Srt[selid]=srt
    cur.close()
    print('dictSrt_Selid',dictSrt_Selid)
    print('dictSelid_Srt',dictSelid_Srt)

    dictTs_Idx={}
    for selid in dictSrt_Selid.values():
      idx=0
      print('selid',selid)
      cur = conn.cursor()
      cur.execute("select backprice,pricets from apriceshistory where marketid = %s and selectionid = %s order by pricets",('1.124156004',selid))
      rows = cur.fetchall()
      for row in rows:
        #print(row)
        dictTs_Idx[idx]=row['pricets']
        backprice=row['backprice']
        odds_matrix[idx][dictSelid_Srt[selid]]=backprice
        print(odds_matrix[idx][dictSelid_Srt[selid]],idx,dictSelid_Srt[selid],selid)
        idx=idx+1
      cur.close()

    print('---------')
    print(odds_matrix)
    r=500
    c=3
    print( odds_matrix[r][c],r,c)

#    for r in range(num_rows):
#      for c in range(num_cols):
#        if odds_matrix[r][c] > 0.0 :
#          print(r,c,odds_matrix[r][c])


  finally :
    conn.close();

if __name__ == '__main__':
  test()



