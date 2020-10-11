

import psycopg2 as pg
import psycopg2.extras as ex

def test():

  try:
    conn = pg.connect(
      host="localhost",
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
  except (Exception, pg.DatabaseError) as error:
    print(error)
  finally:
    if conn is not None:
      conn.close()

if __name__ == '__main__':
    test()



