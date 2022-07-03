

import matplotlib.pyplot as plt
import numpy as np

import _pickle as pickle
import time
import os
import os.path
from pathlib import Path

#########################################################

def marketname_ok(marketname):
      print('"' + marketname + '"')
      if marketname == "5f Hcap"    : return True
      if marketname == "6f Hcap"    : return True
      if marketname == "7f Hcap"    : return True
      if marketname == "1m Hcap"    : return True
      if marketname == "1m1f Hcap"  : return True
      if marketname == "1m2f Hcap"  : return True
      if marketname == "1m3f Hcap"  : return True
      if marketname == "1m4f Hcap"  : return True
      if marketname == "1m5f Hcap"  : return True
      if marketname == "1m6f Hcap"  : return True
      if marketname == "1m7f Hcap"  : return True
      if marketname == "2m Hcap"    : return True
      if marketname == "2m1f Hcap"  : return True
      if marketname == "2m2f Hcap"  : return True
      if marketname == "2m3f Hcap"  : return True
      if marketname == "2m4f Hcap"  : return True
      if marketname == "2m5f Hcap"  : return True
      if marketname == "2m6f Hcap"  : return True
      if marketname == "2m7f Hcap"  : return True
      if marketname == "3m Hcap"    : return True
      if marketname == "3m1f Hcap"  : return True
      if marketname == "3m2f Hcap"  : return True
      if marketname == "3m3f Hcap"  : return True
      if marketname == "3m4f Hcap"  : return True
      return False
#######################################################

def cache_exists(filenname):
      my_file_back = Path(filenname)
      return my_file_back.is_file()
  
##################################

def get_cached_dict(marketid, dict_type):
  print(dict_type, marketid)

  filename = 'pickles/' + dict_type + '_' + marketid + '.pickle'

  if cache_exists(filename):
    return pickle.load(open(filename, 'rb'))
  else:
    print(dict_type, '- no dict', marketid, filename)
    a=1/0

##################################

def get_color(selectionid,placedict,windict):
  
  for item in windict.items():
 #   print('win',item[1],selectionid)
    if item[1] == selectionid :
  #    print('winner')
      return "green"
      
  for item in placedict.items():
   # print('plc',item[1],selectionid)
    if item[1] == selectionid :
    #  print('place')
      return "blue"
  

  #print('loser')
  return "red"
    

########################################################


def create_plot(marketid,marketname):
#marketid='1.125301065'
  matrix_4d = get_cached_dict(marketid,'cache')
  #yes 4 dimensions
  BACKPRICE  = 0
  LAYPRICE   = 1
  BACKREWARD = 2
  LAYREWARD  = 3

  # get backprice dimension
  matrix_1d=matrix_4d[... ,BACKPRICE]

  dict_selid_idx = get_cached_dict(marketid,'selid_idx')
  dict_idx_selid = get_cached_dict(marketid,'idx_selid')

  plc_dict = get_cached_dict(marketid,'plc_dict')
  #print('plc_dict', plc_dict)

  win_dict = get_cached_dict(marketid,'win_dict')
  #print('win_dict', win_dict)

  s=[]
  l=[]
  t=range(matrix_1d.shape[0])
  #print('len(t)',len(t))

  for selid, col in dict_selid_idx.items():
    s.append(col)
    l.append(selid)

  plt.rcParams["figure.figsize"] = (20,10) #x,y in INCHES!!
  plt.margins(0.01) #  in INCHES!!

  for i in s :
    row = []
    linecolor = get_color(dict_idx_selid[i], plc_dict, win_dict)
    for r in range(matrix_1d.shape[0]):
      row.append(matrix_1d[r][i])
    
    plt.plot(t, row, color=linecolor, label=str(dict_idx_selid[i]))
  

  ax = plt.gca()
  #ax.legend(l, loc='upper center',ncol=len(s))
  ax.legend(l)
  ax.set_title(marketid)
  ax.set_xlabel('timesteps')
  ax.set_ylabel('odds')
  #ax.set_xlim([xmin, xmax])

  yticks = np.arange(0, 21, 1)
  #print(yticks)
  ax.set_yticks(yticks)
  ax.set_ylim([yticks[0], yticks[-1]])
  #print([yticks[0], yticks[-1]])
  ax.grid(True)
  #print('plt.margins()',plt.margins())
  tmpdirectory ='images/' + str(yticks[-1]) + '/' + marketname
  directory= tmpdirectory.replace(' ', '_')
  if not os.path.exists(directory):
    os.makedirs(directory)
    print('created dir:',directory)
  plt.savefig(directory + '/' +  marketid + '.png')
  #plt.show()
  plt.close()
  plt.clf()
  plt.cla()

#########################################################
def do_create_plot():
  last_market_list=[]
  cnt = 100
  last_market='0.128656761'
  last_market='1.128656761'
  fname_last_market='pickles/last_market.pickle'
  if os.path.exists(fname_last_market):
    last_market_list=pickle.load(open(fname_last_market, 'rb'))
    last_market=last_market_list[0]

  filename='pickles/marketlist.pickle'
  marketlist = pickle.load(open(filename, 'rb'))
#  print(marketlist)
  for mrow in marketlist:
    try:
      m = mrow['marketid']
      if marketname_ok(mrow['marketname']) :
        if m > last_market :
          create_plot(m,mrow['marketname'])
          cnt = cnt -1
          print('cnt',cnt)
          if cnt == 0 :
            last_market_list.clear()
            last_market_list.append(m)
            pickle.dump(last_market_list, open(fname_last_market, 'wb'))
            break
    except ZeroDivisionError:
      print(m,'problem get at least 1 dict - plc perhaps')
        
#########################################################

if __name__ == '__main__':
  do_create_plot()



