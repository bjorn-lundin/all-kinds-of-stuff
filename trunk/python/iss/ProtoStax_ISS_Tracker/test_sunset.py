


import datetime
import time


sun={}
now2 = datetime.datetime.now()

def is_sun_up(t) :
  now = datetime.datetime.now()
  doy = now.timetuple().tm_yday
  num_in_file = 0
  times=None
  
  if len(sun) == 0 :
  
    with open('sune_rise_and_set.dat') as file:
      for line in file:
        if line.rstrip()[0] == '#' : continue  
        num_in_file = num_in_file+1
        if doy == 366 : 
          doy=365
        times = line.rstrip().split()
        rise = times[0].split(':')
        sset = times[1].split(':')
        sunrise = now.replace(hour=int(rise[0]), minute=int(rise[1]),second=0)
        sunset = now.replace(hour=int(sset[0]), minute=int(sset[1]),second=0)
        sun[num_in_file] = [sunrise,sunset]  
  
  
  print('2-dict',sun[doy])
  print('doy',now.timetuple().tm_yday)

  sun_is_up=False
  if sun[doy][0] < t and t < sun[doy][1]  : sun_is_up=True;
  print('sun_is_up',sun_is_up)


if time.daylight :
    print('DST defined')
else:
    print('DST not defined')

is_sun_up(now2)
is_sun_up(now2)

datetime_obj2 = datetime.datetime.fromtimestamp(1656321888)
print("Converted time stamp from datetime class",datetime_obj2)

d = datetime.datetime.fromtimestamp(1656321888)
print('d',d)

