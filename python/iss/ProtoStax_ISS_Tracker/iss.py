# ***************************************************
#   International Space Station Tracker.
#   using Raspberry Pi B+, Waveshare ePaper Display and ProtoStax enclosure
#   --> https://www.waveshare.com/product/modules/oleds-lcds/e-paper/2.7inch-e-paper-hat-b.htm
#   --> https://www.protostax.com/products/protostax-for-raspberry-pi-b
#
#   It displays the current location of the ISS and also its tracked trajectory. The
#   current location is shown by the ISS icon, and the trajectory by small circles.
#   15 minute markers are shown as small rectangles.
#
#   ISS Current Location is obtained using Open Notify ISS Current Location API
#   http://open-notify.org/Open-Notify-API/ISS-Location-Now/
#
#   Written by Sridhar Rajagopal for ProtoStax.
#
#   Contributions by:
#   jplegat
#   MatKier
#   MiketheChap/melcasipit-Mike Davis paid coder melcasipit on Fiverr to write ring/circular buffer+exception handling
#
#   BSD license. All text above must be included in any redistribution


from exclusiveprocess import Lock, CannotAcquireLock

import sys
sys.path.append(r'lib')

if sys.version_info[0] < 3:
    raise Exception("Must be using Python 3")

from enum import Enum
import signal
#import epd2in7b
import epd1in54b
import epdconfig
import time

from PIL import Image,  ImageDraw,  ImageFont, ImageOps
from datetime import datetime
#from time import time, sleep

import requests

import datetime
#dict for keeping rise/set of sun


def read_sundata_into_dict():
  sun={}
  num_in_file = 0
  with open('sune_rise_and_set.dat') as file:
    for line in file:
      if line.rstrip()[0] == '#' : 
        continue
      num_in_file = num_in_file+1
      if num_in_file == 366 :
        num_in_file=365
      times = line.rstrip().split()
      rise = times[0].split(':')
      sset = times[1].split(':')
      d2 = datetime.datetime.fromisoformat('2021-12-31')
      #print('d2',d2)
      d3 = d2 + datetime.timedelta(days=num_in_file)
      sunrise = d3.replace(hour=int(rise[0]), minute=int(rise[1]),second=0)
      sunset = d3.replace(hour=int(sset[0]), minute=int(sset[1]),second=0)
      sun[num_in_file] = [sunrise,sunset]
  print('whole dict', sun)
  return sun


# function to see if sun is up
def is_sun_up(sundict,t) :
  doy = t.timetuple().tm_yday
  print('/////is_sun_up start///////')
  print('doy',t.timetuple().tm_yday)

  print('dict-sunrise and sunset',sundict[doy])
  print('doy',t.timetuple().tm_yday)

  print('arg', t)

  sun_is_up=False
  if sundict[doy][0] < t and t < sundict[doy][1]  : sun_is_up=True;
  print('sunrise',sundict[doy][0] ,'sunset',sundict[doy][1])
  print('sun_is_up',sun_is_up,t)

  if sundict[doy][0] < t :
    print ('after sunrise')
  else:
    print('before sunrise')

  if t < sundict[doy][1] :
    print ('before sunset')
  else:
    print('after sunset')

  print ('return', sun_is_up)
  return sun_is_up
#-----------------------


# Update Interval for fetching positions
DATA_INTERVAL = 30 #seconds
# Update interval for the display
DISPLAY_REFRESH_INTERVAL = 2 # Number of DATA_INTERVAL between successive display updates (e.g. 2 => update display every second deta fetch)

# Limit the number of data entries
# Entries older than this will be aged out - ie. circular buffer
# Since we update the position every 30 seconds (DATA_INTERVAL),
# that would be 24*60*2 = 2880 data points. The ISS does about
# 16 orbits per day. Setting the DATA_LIMIT to 1440 would give us about
# 8 orbits worth of data
# You can adjust this down to the period of interest for you with the
# above math
#DATA_LIMIT = 1440 # positions limit
DATA_LIMIT = (int) (1440/8) # positions limit

# Note:
# The dimensions of the 2.7 in ePaper display are
# 264 x 176
# The dimensions of the 1.54 in ePaper display are
# 200 x 200

class Display(object):
    def __init__(self, imageWidth, imageHeight):
        self.imageWidth = imageWidth
        self.imageHeight = imageHeight

    # Draws the ISS current location and trajectory from array of positions
    def drawISS(self, positions, passages):
        imageBlack = Image.new('1', (self.imageWidth, self.imageHeight), 255) # 1: clear the frame
        imageMap = Image.open('world_map_b.bmp').convert('L')
        imageBlack.paste(imageMap, (0,0))

        imageRed = Image.new('1', (self.imageWidth, self.imageHeight), 255) # 1: clear the frame
        issLogo = Image.open('iss_b.bmp').convert('L')
        logobox = issLogo.getbbox()
        #print('info-logo',issLogo.info)
        #print('info-imageMap',imageMap.info)

        drawred = ImageDraw.Draw(imageRed)

        for i,t in enumerate(positions):
            (lat,lon) = t

            # Map the lat, lon to our x/y coordinate system
            (x,y) = self.mapLatLongToXY(lat, lon)

            # last position in the positions array is the latest location
            # Every 15 minutes, we add a rectangular marker
            # and a small red circle to mark other locations

            if (i == len(positions) - 1):
                s = 10
                ldx = int(logobox[2]/2)
                ldy = int(logobox[3]/2)
                dx = 11
                dy = 11
                # drawred.rectangle((x-s,y-s,x+s,y+s), fill=0)
                #imageRed.paste(issLogo, ((int)(x-s), (int)(y-s)))
                #c is circle center
                drawred.ellipse((x-dx,y-dy,x+dx,y+dy), fill=127, outline=128) # draw circle around it as well
                drawred.ellipse((x-4 ,y-4 ,x+4 ,y+4), outline=128) # draw circle around it as well
                drawred.ellipse((x-1 ,y-1 ,x+1 ,y+1), outline=128) # draw circle around it as well
#                imageRed.paste(issLogo, (x-ldx,y-ldy))
            elif (((i+1) % (15 * 60 / DATA_INTERVAL)) == 0): # every 15 minutes (so 15 * 60s / DATA_INTERVAL = number of readings within 15 minutes)
                s = 2
                drawred.rectangle((x-s,y-s,x+s,y+s), fill=0)
            else:
                s = 1
                drawred.ellipse((x-s,y-s,x+s,y+s), outline=0)
                # drawred.point((x,y), fill=0)

        #next 2 passes
        font = ImageFont.truetype('/opt/vc/src/hello_pi/hello_font/Vera.ttf', 12)
        time_image = Image.new('1', (200, 200-137), 255)
        time_draw = ImageDraw.Draw(time_image)
        y = 10
        for p,d in passages:
          print('drawISS','duration','risetime',p,d)
          time_draw.text((10, y), datetime.datetime.fromtimestamp(p).strftime('%Y-%m-%d %H:%M:%S') + " " + str(d) +"s", font = font, fill = 0)
          y = y+12

        imageBlack.paste(time_image,(0,138))

        # Rotate image 180 degrees - Remove the # comments of the lines below to rotate the image and allow for alternate positioning/mounting of the Raspberry Pi 
        imageRed = imageRed.transpose(Image.ROTATE_180)
        imageBlack = imageBlack.transpose(Image.ROTATE_180)

        # return the rendered Red and Black images
        return imageBlack, imageRed

    # Maps lat, long to x,y coordinates in 264x181 (the size of the world map)
    # (90 to -90 lat and -180 to 180 lon) map to 0-181 (y) and 0-264 (x) respectively
    # Simple algebra gives us the equations below
    # Recalculate as appropriate for map size and coordinates
#    def mapLatLongToXY(self, lat, lon):
#        x = (int)(0.733 * lon + 132)   #264/360=0.733, 264/2=132
#        y = (int)(-1.006 * lat + 90.5) #181/180=1.006, 181/2=90.5
#        return x, y

#    200x200 (1.54"), but map is 200x137
    def mapLatLongToXY(self, lat, lon):
        x  = (int) (200/360 * lon + 100)   #200/360=0.5556, 200/2=100
        y  = (int) (-(lat - 90)*137/180)
        #print('x','y',x,y)
        return x, y

# The main function
def main():

    Lock(die=True,name="iss-tracker").forever()
    sun = read_sundata_into_dict()

    # API to get ISS Current Location
    n=100 # num_passages
    URL = 'http://api.open-notify.org/iss-now.json'
    URL2 = 'http://api.open-notify.org/iss-pass.json?lat=55.805131913818066&lon=13.10102441653741&alt=20&n=' + str(n)

#    # Initialize and clear the 2in7b (tri-color) display
#    epd = epd2in7b.EPD()
#    display = Display(epd2in7b.EPD_HEIGHT, epd2in7b.EPD_WIDTH)

#    # Initialize and clear the 1in54b (two-color) display
    epd = epd1in54b.EPD()
    display = Display(epd1in54b.EPD_HEIGHT, epd1in54b.EPD_WIDTH)

    # Store positions in list
    positions = []

    while(True):
        t0 = time.time()
        try:
            r = requests.get(url = URL)
            r2 = requests.get(url = URL2)
            # extracting data in json format
            data = r.json()
            data2 = r2.json()
            print(data)
            print(data2)
        except:
            print("error getting data.... might be a temporary hiccup so continuing")
            continue

        lat = float(data['iss_position']['latitude'])
        lon = float(data['iss_position']['longitude'])
        if len(positions) > (DATA_LIMIT - 1):
            del positions[0]
        positions.append((lat, lon))
        print(positions)

        passages = []
        for i in range(n):
          print('i',i,len(data2['response']))
          if len(data2['response']) > i :
            duration = int(data2['response'][i]['duration'])
            risetime = int(data2['response'][i]['risetime'])
            risetime = risetime + 3600 # compensate UTC
            if time.daylight :  risetime + 3600 # compensate daylight savings time
            print('main','duration','risetime',duration, risetime)
            if len(passages) < 4 :
              dt = datetime.datetime.fromtimestamp(risetime)
              print('call is_sun_up with', dt)
              if not is_sun_up(sun, dt) :
                passages.append((risetime,duration))

        # Refresh the display on the first fetch and then on every DISPLAY_REFRESH_INTERVAL fetch
        if ((len(positions) >= 1) and ((len(positions)-1) % DISPLAY_REFRESH_INTERVAL)):
#            epd.init()
            epd.init(epd.lut_full_update)
            (imageBlack, imageRed) = display.drawISS(positions, passages)
            # We're drawing the map in black and the ISS location and trajectory in red
            # Swap it around if you'd like the inverse color scheme

            imwidth, imheight = imageRed.size
            pixelsRed = imageRed.load()
            pixelsBlack = imageBlack.load()
            for y in range(imheight):
              for x in range(imwidth):
#                if x == 199 : print('')
                if pixelsRed[x, y] == 0 :
#                  print('B',end='')
                #whereever we draw black in the red pic - negate the map
                  if pixelsBlack[x, y] == 0 :
                    imageBlack.putpixel((x, y), 255)
                  else:
                    imageBlack.putpixel((x, y), 0)
#                else:
#                  print('W',end='')
                elif pixelsRed[x, y] == 127 :
                  imageBlack.putpixel((x, y), 255)
                elif pixelsRed[x, y] == 128 :
                  imageBlack.putpixel((x, y), 0)

            print('Display world!')
            epd.display(epd.getbuffer(imageBlack))
            epd.sleep()
            time.sleep(10)

        t1 = time.time()
        sleepTime = max(DATA_INTERVAL - (t1 - t0), 0)
        time.sleep(sleepTime) # sleep for 30 seconds minus duration of get request and display refresh



# gracefully exit without a big exception message if possible
def ctrl_c_handler(signal, frame):
    print('Goodbye!')
    # XXX : TODO
    #
    # To preserve the life of the ePaper display, it is best not to keep it powered up -
    # instead putting it to sleep when done displaying, or cutting off power to it altogether.
    #
    # epdconfig.module_exit() shuts off power to the module and calls GPIO.cleanup()
    # The latest epd library chooses to shut off power (call module_exit) even when calling epd.sleep()
    # epd.sleep() calls epdconfig.module_exit(), which in turns calls cleanup().
    # We can therefore end up in a situation calling GPIO.cleanup twice
    #
    # Need to cleanup Waveshare epd code to call GPIO.cleanup() only once
    # for now, calling epdconfig.module_init() to set up GPIO before calling module_exit to make sure
    # power to the ePaper display is cut off on exit
    # I have also modified epdconfig.py to initialize SPI handle in module_init() (vs. at the global scope)
    # because slepe/module_exit closes the SPI handle, which wasn't getting initialized in module_init
    epdconfig.module_init()
    epdconfig.module_exit()
    print("Remeber to clear the display using cleardisplay.py if you plan to power down your Pi and store it, to prevent burn-in!")
    epd = epd1in54b.EPD()
    epd.Clear(0xFF)
    exit(0)

signal.signal(signal.SIGINT, ctrl_c_handler)


if __name__ == '__main__':
    main()
