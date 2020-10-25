

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

PRICE  = 0
REWARD = 1



def cache_exists(filenname):
      my_file_back = Path(filenname)
      return my_file_back.is_file()

############################################################


def create_cached_dicts(marketid):

  filename_selid_idx = "pickles/selid_idx_" + marketid + ".pickle"
  filename_idx_selid = "pickles/idx_selid_" + marketid + ".pickle"

  if cache_exists(filename_selid_idx):
    return pickle.load(open(filename_selid_idx, 'rb'))
    print('dict_selid_Idx',dict_selid_idx)
    return dict_selid_idx

############################################################
def create_empty_matrix(marketid):
    num_rows = 0
    num_cols = 16
    cur.close()
    return np.zeros( (num_rows, num_cols, 4) )
############################################################


def row_is_all_zero(matrix, row, dimension):
  all_zero = True  
  for i in range(16):
    if matrix [row] [i] [dimension] > 1.0:
      all_zero = False
      break
  return all_zero




def row_is_same_as_previous(matrix, row, dimension):
  all_equal = True  
  for i in range(16):
    if abs(matrix [row] [i] [dimension] -  matrix [row-1] [i] [dimension]) > 0.0000000001  :
      all_equal = False
      break
  if all_equal:
      print ('row, dimension',row, dimension)
  return all_equal



#          cache_matrix  [row] [col] [LAYREWARD]  = datarow['layprofit']
#          cache_matrix  [row] [col] [BACKREWARD] = datarow['backprofit']
#          cache_matrixk [row] [col] [LAYPRICE]   = datarow['layprice']
#          cache_matrixk [row] [col] [BACKPRICE]  = datarow['backprice']



def fix_cache(marketid,side):
  filename_newer = "pickles/new_cache_" + marketid + ".pickle"
  filename = "pickles/cache_" + marketid + ".pickle"
  orig_matrix_changed = False
  filename2=''
  PRC=-1
  RWD=-1
  
  print('marketid',marketid)
  print('filename_newer',filename_newer)
  print('filename',filename)
  
  
  if side == 'BACK' : 
    filename2 = "pickles/no_zero_back_cache_" + marketid + ".pickle"
    PRC=BACKPRICE
    RWD=BACKREWARD
  elif side == 'LAY' : 
    filename2 = "pickles/no_zero_lay_cache_" + marketid + ".pickle"
    PRC=LAYPRICE
    RWD=LAYREWARD
  else : 
    a=1/0
    
    
    
  if cache_exists(filename_newer):
    print('cache_exists',filename_newer)
    cache_matrix = pickle.load(open(filename_newer, 'rb'))
    dict_selid_idx = create_cached_dicts(marketid)
    
  elif cache_exists(filename):
    print('cache_exists',filename)
    cache_matrix = pickle.load(open(filename, 'rb'))
    dict_selid_idx = create_cached_dicts(marketid)

  all_equal = False   
  num_non_zero = 0
#if this row = prev row --> make all price values 0
# back first
  for i in range(cache_matrix.shape[0]):
    if i == 0 :
      pass
    else:
      all_equal = row_is_same_as_previous(cache_matrix, i, PRC)

      #make 0      
      if all_equal :
        #print('back','all_equal',all_equal,'i',i)
        for selid, col in dict_selid_idx.items():
          cache_matrix [i] [col] [PRC] = 0.0
          orig_matrix_changed = True
          print('orig_matrix_changed',orig_matrix_changed,'marketid',marketid)

      if not row_is_all_zero(cache_matrix, i, PRC):
        num_non_zero = num_non_zero +1
        
  #print(side,'num_non_zero',num_non_zero,'cache_matrix.shape[0]',cache_matrix.shape[0])

  #process row   
  if num_non_zero < cache_matrix.shape[0]:
    matrix2 = np.zeros( (num_non_zero, 16, 2) )
    r = 0
    for i in range(cache_matrix.shape[0]):
      if not row_is_all_zero(cache_matrix, i, PRC):
        if r >= num_non_zero :
            #print (r,num_non_zero)  
            pass
        else:    
          for selid, col in dict_selid_idx.items():
            matrix2 [r] [col] [PRICE]  = cache_matrix [i] [col] [PRC] 
            matrix2 [r] [col] [REWARD] = cache_matrix [i] [col] [RWD] 
          r = r+1
    
    pickle.dump(matrix2, open(filename2, 'wb'))
    if orig_matrix_changed :    
      pickle.dump(cache_matrix, open(filename_newer, 'wb'))
      print('new matrix dumped',filename_newer)
      
        
        
#    for a in range(cache_matrix.shape[0]):
#        for b in range(cache_matrix.shape[1]):
#            for c in range(cache_matrix.shape[2]):
#                print ('cache_matrix', a,b,c ,cache_matrix [a][b][c])




########################################################


def do_fix_cache(side):

    marketlist = []
    #filename = "pickles/marketlist" + ".pickle"

    #if cache_exists(filename):
    #  marketlist = pickle.load(open(filename, 'rb'))
    
    
    myFile= open( "markets.txt", "r" )
    for x in myFile:
      y = x.strip()
      marketlist.append(y)
    myFile.close()

# market is a dict:
    for marketid in marketlist:
      print('market',  "'" + marketid + "'")
      start_time = time.time()
#      print('do_fix_cache','marketid',side, market)
      fix_cache(marketid, side)
#      print('do_fix_cache','marketid', market['marketid'],side)
#      fix_cache(market['marketid'], side)
      #per market
      elapsed_time = time.time() - start_time
      #print('elapsed_time', market['marketid'], elapsed_time)

if __name__ == '__main__':
  do_fix_cache('BACK')
  do_fix_cache('LAY')



