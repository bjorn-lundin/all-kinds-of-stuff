#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import traceback
import datetime
#to find opencv as root....
sys.path.append('/usr/local/lib/python2.7/site-packages')

import cv2
import time
import numpy as np
from picamera.array import PiRGBArray
from picamera import PiCamera


class Robocam(object): 


    def __init__(self,USB=False):
        self.board_dict = {}  
        self.font = cv2.FONT_HERSHEY_SIMPLEX
        self.USB = USB
        if self.USB:
            self.camera = cv2.VideoCapture(0)
        else:
            self.camera = PiCamera()
            self.camera.resolution = (640, 480)
            #self.camera.resolution = (800, 608)


    def get_x_as_percent(self, x, tl, br):
        xpct = 100.0 * (float(x) - tl[0]) / (br[0] - tl[0])
        #print 'get_x_index', x, '->', int(xpct)
        return int(xpct)

    def get_y_as_percent(self, y, tl, br):
        ypct = 100.0 * (float(y) - tl[1]) / (br[1] - tl[1])
        #print 'get_y_index', y, '->', int(ypct)
        return int(ypct)


    def get_x_index(self, x, tl, br):
        xpct = self.get_x_as_percent(x, tl, br)
        if xpct < 10:
            ret = -1
        elif xpct < 16 + 15:
            ret = 0
        elif xpct < 45 + 10:
            ret = 1
        elif xpct < 65 + 10:
            ret = 2
        elif xpct < 85 + 10:
            ret = 3
        else:
            ret = 4
    
        #print 'get_x_index', x, '->', int(xpct), '->', ret
        return ret


    def get_y_index(self, y, tl, br):
        ypct = self.get_y_as_percent(y, tl, br)
        if ypct < 20:
            ret = 1
        elif ypct < 27 + 20:
            ret = 2
        elif ypct < 60 + 20:
            ret = 3
        else:
            ret = 4
        #print 'get_y_index', y, '->', int(ypct), '->', ret
        return ret

    
    def get_true_index(self, x, y):
        #Turn to RC style
        if x == -1:
            if y == 1:
              return (5, 1) ; # top left
            else:
                raise Exception('x=-1, y !=1 -> y={}'.format(y))
           
        elif x == 0:
            if y == 1:
              return (0, 1) ; # white home 1
            elif y == 2:
              return (0, 2) ; # white home 2
            elif y == 3:
              return (0, 3) ; # white home 3
            else:
                raise Exception('x=-1, y not in 1..3 -> y={}'.format(y))
        elif x == 1:
            if y == 1:
              return (y, x) ; # board
            elif y == 2:
              return (y, x) ; # board
            elif y == 3:
              return (y, x) ; # board
            elif y == 4:
              return (4, 1) ; # black home 1
            else:
                raise Exception('x=1 y not in 1..4 -> y={}'.format(y))
        elif x == 2:
            if y == 1:
              return (y, x) ; # board
            elif y == 2:
              return (y, x) ; # board
            elif y == 3:
              return (y, x) ; # board
            elif y == 4:
              return (4, 2) ; # black home 2
            else:
                raise Exception('x=2 y not in 1..4 -> y={}'.format(y))
        elif x == 3:
            if y == 1:
              return (y, x) ; # board
            elif y == 2:
              return (y, x) ; # board
            elif y == 3:
              return (y, x) ; # board
            elif y == 4:
              return (4, 3) ; # black home 3
            else:
                raise Exception('x=3 y not in 1..4 -> y={}'.format(y))
        elif x == 4:
            if y == 4:
              return (5, 2) ; # bottom_right
            else:
                raise Exception('x=4 y not 4 -> y={}'.format(y))
    

    def get_board_snapshop(self):
        try:
    
            print '-----START-------', datetime.datetime.now(), \
                '----------------'
            grey = None
            blur = None
            frame = None
            
            if self.USB:
                (ret, frame) = self.camera.read()
            else:
                self.rawCapture = PiRGBArray(camera)
            
             # allow the camera to warmup
            
                time.sleep(0.1)
            
             # grab an image from the camera
            
                self.camera.capture(self.rawCapture, format='bgr')
                frame = rawCapture.array
            
            # frame = cv2.imread('/dev/shm/1.png')
            
            (height, width, channels) = frame.shape
            print 'height, width, channels', height, width, channels
            
            grey = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
            cv2.imwrite('/dev/shm/2.png', grey)
            blur = cv2.blur(grey, (5, 5))
            cv2.imwrite('/dev/shm/3.png', blur)
            
            c_list = [] 
            circles = cv2.HoughCircles(
                blur,
                method=cv2.HOUGH_GRADIENT,
                dp=1,
                minDist=50,
                param1=50,
                param2=30,
                minRadius=25,
                maxRadius=50,)
                
            if circles is not None:        
                for i in circles[0, :]:
                    cmin = 99999
                    cmax = 0
                    cmean = 0
                    cnt = 0
                    # circle is (x,y) based, frams is heigh-width based (y,x) that is .. so switch them
                    for y in range(int(i[0] - 5), int(i[0] + 5)):
                        for x in range(int(i[1] - 5), int(i[1] + 5)):
                            b = blur[x, y]
                            cnt +=1
                            cmean += b 
                            if b > cmax:
                                cmax = b
                            if b < cmin:
                                cmin = b
                    cmean /= cnt  
                    print 'circle', i[0], i[1], i[2], cmin, cmax, cmean
                    c_list.append(( i[0], i[1], i[2], cmin, cmax, cmean))
                    cv2.circle(frame, (i[0], i[1]), i[2], (0, 255, 0), 2)
                    cv2.imwrite('/dev/shm/9.png', frame)
            else:
                raise Exception('8 circles should have been found. But there was None')
            
            # find top_left and bottom_right
            top_left = (6000, 6000, 10, 100000, 0, 0)
            bottom_right = (0, 0, 10, 0, 0, 0)
            if len(c_list) != 8:
                raise Exception('8 circles should have been found. But there was {}'.format(len(c_list)))
            # find the ones with biggest resp smallest x    
            for c in c_list:
                if c[0] < top_left[0]:
                    top_left = c
                elif c[0] > bottom_right[0]:
                    bottom_right = c
            
            print "top_left", top_left             
            print "bottom_right", bottom_right             
            
            def sort_by_mean(e):
              return e[5]
            
            c_list.sort(key=sort_by_mean)
            for c in c_list:
                print c
            
            #max black values is in 4th element, 5th item      
            hb = c_list[3][4]
            
            self.board_dict = {}
            
            if c_list is not None:
                for i in c_list:
            
                    cv2.putText(
                        grey,
                        str(int(i[0])),
                        (int(i[0] - 8), int(i[1] - 5)),
                        self.font,
                        0.2,
                        (255, 255, 255),
                        1,
                        cv2.LINE_AA)
                    cv2.putText(
                        grey,
                        str(int(i[1])),
                        (int(i[0] - 8), int(i[1] + 5)),
                        self.font,
                        0.2,
                        (255, 255, 255),
                        1,
                        cv2.LINE_AA)
            
                    cl = 'S'
                    if i[4] > hb:
                        cl = 'V'
                        
                    cv2.putText(
                        grey,
                        cl,
                        (int(i[0] - 5), int(i[1]+15)),
                        self.font,
                        0.2,
                        (255, 255, 255),
                        1,
                        cv2.LINE_AA)
                        
                    cv2.circle(frame, (i[0], i[1]), i[2], (0, 255, 0), 2)
            
                    xpct = self.get_x_as_percent(i[0], top_left, bottom_right)
                    ypct = self.get_y_as_percent(i[1], top_left, bottom_right)
                    cv2.putText(
                        grey,
                        str(xpct) + "," + str(ypct),
                        (int(i[0] - 5), int(i[1]-20)),
                        self.font,
                        0.2,
                        (255, 255, 255),
                        1,
                        cv2.LINE_AA)
                    
                    x = self.get_x_index(i[0], top_left, bottom_right)
                    y = self.get_y_index(i[1], top_left, bottom_right)
                    x, y = self.get_true_index(x, y)
            
                    cv2.putText(
                        grey,
                        str(x) + "," + str(y),
                        (int(i[0] - 5), int(i[1]+25)),
                        self.font,
                        0.2,
                        (255, 255, 255),
                        1,
                        cv2.LINE_AA)
                        
                    print "board x,y", x, y
                    # use RC notation
                    self.board_dict.setdefault(str(x)+","+str(y),cl)
                    cv2.imwrite('/dev/shm/4.jpg', frame)
                    cv2.imwrite('/dev/shm/5.jpg', blur)
                    cv2.imwrite('/dev/shm/6.png', grey)
                    
        except Exception as err:
            print '-'*60
            print "exception caught", err.message()
            traceback.print_stack()
            print repr(traceback.extract_stack())
            print repr(traceback.format_stack())
            #traceback.print_exc(file=sys.stdout)
            print '-'*60        
        finally :         
            cv2.destroyAllWindows()
            if self.USB:
                self.camera.release()
            

        
        

#while True:
#    r = Robocam()
#    try:
#        r.get_board_snapshop()
#        time.sleep(5.0)
#    except Exception as ex:
#        print ex.message
