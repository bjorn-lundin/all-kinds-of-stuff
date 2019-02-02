#!/usr/bin/python
# -*- coding: latin-1 -*-

# Copyright (c) 2014 Adafruit Industries
# Author: Tony DiCola
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

import datetime
import time
import json
import Adafruit_Nokia_LCD as LCD

import Adafruit_GPIO.SPI as SPI
#import ast
import json


from PIL import Image
from PIL import ImageDraw
from PIL import ImageFont

class Board(object): 

#board x,y 1 5
#[['-', '-', '-', '-', '-', '-'], ['-', 'S', '-', '-', 'S', 'V'], ['-', '-', '-', 'S', '-', 'S'], ['-', 'V', 'V', 'V', '-', '-']]
#-------
#|S|-|V|
#-------
#|-|-|V|
#-------
#|-|S|V|
#-------
#-white home--
#|-|-|-|
#-black home--
#|S|-|-|
#-------
#--Markers-----
#|V|S|-|
#-------


    def __init__(self):
        # Raspberry Pi hardware SPI config:
        self.DC = 23
        self.RST = 24
        self.SPI_PORT = 0
        self.SPI_DEVICE = 0
        self.WHITE = 255
        self.BLACK = 0

        # Hardware SPI usage:
        self.disp = LCD.PCD8544(self.DC, self.RST, spi=SPI.SpiDev(self.SPI_PORT, self.SPI_DEVICE, max_speed_hz=4000000))
        
        # Initialize library.
        self.disp.begin(contrast=50)

        # Clear display.
        self.disp.clear()
        self.disp.display()

        # Create blank image for drawing.
        # Make sure to create image with mode '1' for 1-bit color.
        self.image = Image.new('1', (LCD.LCDWIDTH, LCD.LCDHEIGHT))
        
        # Get drawing object to draw on image.
        self.draw = ImageDraw.Draw(self.image)
        
        # Load default font.
        self.font = ImageFont.load_default()
        
        # Alternatively load a TTF font.
        # Some nice fonts to try: http://www.dafont.com/bitmap.php
        # font = ImageFont.truetype('Minecraftia.ttf', 8)
        
        self.position_dict = {}
        
    def clear(self):
        # Draw a white filled box to clear the image.
        self.draw.rectangle((0,0,LCD.LCDWIDTH,LCD.LCDHEIGHT), outline=255, fill=255)

    def draw_grid(self):
        # Draw the board grid.
        
        self.draw.rectangle((4, 4,14,14), outline=0, fill=255) # White marker
        self.draw.rectangle((4,24,14,34), outline=0, fill=255) # Black marker
        
        #white start locations
        self.draw.rectangle((19, 4,29,14), outline=0, fill=255)
        self.draw.rectangle((19,14,29,24), outline=0, fill=255)
        self.draw.rectangle((19,24,29,34), outline=0, fill=255)
        
        #black start locations
        self.draw.rectangle((69, 4,79,14), outline=0, fill=255)
        self.draw.rectangle((69,14,79,24), outline=0, fill=255)
        self.draw.rectangle((69,24,79,34), outline=0, fill=255)
        
        #board itself -col 1
        self.draw.rectangle((34, 4,44,14), outline=0, fill=255)
        self.draw.rectangle((34,14,44,24), outline=0, fill=255)
        self.draw.rectangle((34,24,44,34), outline=0, fill=255)
        #board itself -col 2
        self.draw.rectangle((44, 4,54,14), outline=0, fill=255)
        self.draw.rectangle((44,14,54,24), outline=0, fill=255)
        self.draw.rectangle((44,24,54,34), outline=0, fill=255)
        #board itself -col 2
        self.draw.rectangle((54, 4,64,14), outline=0, fill=255)
        self.draw.rectangle((54,14,64,24), outline=0, fill=255)
        self.draw.rectangle((54,24,64,34), outline=0, fill=255)
        
    def draw_marker(self,x,y,c):
        #print "draw_marker", x, y, c
        if c == 'V':
            self.draw.ellipse((x-3, y-3,x+3,y+3), outline=0, fill=self.WHITE)
        elif c == 'S':
            self.draw.ellipse((x-3, y-3,x+3,y+3), outline=0, fill=self.BLACK)
                
    def draw_markers(self):
        #self.draw_marker(9,9,self.WHITE);
        #self.draw_marker(9,29,self.BLACK);
        #for item in self.d:
        #   print item, self.position_dict[item]
        #print "-+-+-+-"             
        for item in self.position_dict.keys():
           if self.position_dict[item] != '-':
               if item == "0,1":
                  self.draw_marker(24, 9,self.position_dict[item]);
               if item == "0,2":        
                  self.draw_marker(24,19,self.position_dict[item]);
               if item == "0,3":        
                  self.draw_marker(24,29,self.position_dict[item]);
               if item == "1,1":        
                  self.draw_marker(39, 9,self.position_dict[item]);
               if item == "1,2":        
                  self.draw_marker(49, 9,self.position_dict[item]);
               if item == "1,3":        
                  self.draw_marker(59, 9,self.position_dict[item]);
               if item == "2,1":        
                  self.draw_marker(39,19,self.position_dict[item]);
               if item == "2,2":        
                  self.draw_marker(49,19,self.position_dict[item]);
               if item == "2,3":        
                  self.draw_marker(59,19,self.position_dict[item]);
               if item == "3,1":        
                  self.draw_marker(39,29,self.position_dict[item]);
               if item == "3,2":        
                  self.draw_marker(49,29,self.position_dict[item]);
               if item == "3,3":        
                  self.draw_marker(59,29,self.position_dict[item]);
               if item == "4,1":        
                  self.draw_marker(74, 9,self.position_dict[item]);
               if item == "4,2":        
                  self.draw_marker(74,19,self.position_dict[item]);
               if item == "4,3":        
                  self.draw_marker(74,29,self.position_dict[item]);
               if item == "5,1":        
                  self.draw_marker( 9, 9,self.position_dict[item]);
               if item == "5,2":        
                  self.draw_marker( 9,29,self.position_dict[item]);
        

    def text(self,txt):        
        # Display image.
        self.draw.text((24,39), txt, font=self.font)

    def set_pieces(self,position_dict):
        #convert stringified dict to dict
        #{'0,3': 'S', '0,2': 'S', '5,2': 'S', '5,1': 'V', '1,2': 'S', '3,1': 'V', '2,1': 'V', '2,2': 'V'}
        #{"0,3": "S", "0,2": "S", "5,2": "S", "5,1": "V", "1,2": "S", "3,1": "V", "2,1": "V", "2,2": "V"}
        # all ' must be replaced with "
        
        #fixed_dict_string = position_dict.replace("'", '"') 
        #print "set_pieces-old", position_dict
        #print "set_pieces-new", fixed_dict_string
        #self.position_dict = json.loads(fixed_dict_string)
        self.position_dict = position_dict

    def update(self):        
        # Display image.
        self.disp.image(self.image)
        self.disp.display()


#dict_of_positions={"4,1": "S", "5,1": "V", "2,1": "-", "3,1": "-", "0,1": "-", "1,1": "S", "5,2": "S", "4,2": "-", "1,2": "-", "0,2": "-", "3,2": "S", "2,2": "-", "4,3": "-", "0,3": "-", "1,3": "V", "2,3": "V", "3,3": "V"}

#board = Board(dict_of_positions)       
#board.clear()
#board.draw_grid()
#board.draw_markers()
#board.update()


#print('Press Ctrl-C to quit.')
#while True:
#
#    print '-----START clear----------', datetime.datetime.now(), \
#    board.clear()
#    print '-----START-draw_grid------', datetime.datetime.now(), \
#    board.draw_grid()
#    print '-----START-draw_markers---', datetime.datetime.now(), \
#    board.draw_markers()
#    print '-----START-update---------', datetime.datetime.now(), \
#    board.update()
#    print '-----stop-------', datetime.datetime.now(), \
#    time.sleep(1.0)




#print('Press Ctrl-C to quit.')
#while True:
#    time.sleep(1.0)
