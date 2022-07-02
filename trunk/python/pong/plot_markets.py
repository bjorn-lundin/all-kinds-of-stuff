import matplotlib.pyplot as plt
import numpy as np

import _pickle as pickle
import time
import os
import os.path
from pathlib import Path


def cache_exists(filenname):
      my_file_back = Path(filenname)
      return my_file_back.is_file()
  
##################################

def get_cached_dict(marketid, dict_type):

  filename = "pickles/" + dict_type + '_' + marketid + ".pickle"

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

marketid='1.125301065'
matrix_4d = get_cached_dict(marketid,'cache')
#yes 4 dimensions
BACKPRICE  = 0
LAYPRICE   = 1
BACKREWARD = 2
LAYREWARD  = 3

# get backprice dimension
matrix_1d=matrix_4d[... ,BACKPRICE]


#print('type matrix_1d',type(matrix_1d), str(matrix_1d.shape))
#print(matrix_1d)
#print('--------')

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


matrix_1d_T = matrix_1d.transpose()

#print('type matrix_1d_T',type(matrix_1d_T), str(matrix_1d_T.shape))
#print(matrix_1d_T)
#print('--------')
#for c in range(matrix_1d.shape[1]):
#  for r in range(matrix_1d.shape[0]):
#    print (r,c,matrix_1d[r][c])
 
#print('s',s)

for i in s :
  row = []
  linecolor = get_color(dict_idx_selid[i], plc_dict, win_dict)
#  print('series',i,'with legend', dict_idx_selid[i], linecolor)
  for r in range(matrix_1d.shape[0]):
    row.append(matrix_1d[r][i])
  plt.plot(t, row, color=linecolor, label=str(dict_idx_selid[i]))
  
plt.legend(l)
plt.title(marketid)
plt.xlabel('timesteps')
plt.ylabel('odds')

ax = plt.gca()
#ax.set_xlim([xmin, xmax])
ax.set_ylim([0.0, 20.0])

plt.show()