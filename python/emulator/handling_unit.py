#!/usr/bin/python
import pygame
import sys
import time
from time import strftime
import os
import json
import socket
import errno
import signal
from pygame.locals import *
 
#Set the framebuffer device to be the TFT
#os.environ["SDL_FBDEV"] = "/dev/fb1"


def signal_handler(signal, frame):
        pygame.event.post(pygame.event.Event(pygame.QUIT))
        print '\nYou pressed Ctrl+C, will post QUIT)\n'
        

def displayText(text, size, line, color, clearScreen):
 
    """Used to display text to the screen. displayText is only configured to display
    two lines on the TFT. Only clear screen when writing the first line"""
    if clearScreen:
        screen.fill((0, 0, 0))
 
    font = pygame.font.Font(None, size)
    text = font.render(text, 0, color)
    textpos = text.get_rect()
    textpos.centerx = 160  
    if line == 1:
         textpos.centery = 120
         screen.blit(text,textpos)
    elif line == 2:
        textpos.centery = 80
        screen.blit(text,textpos)
 
def main():
    global screen

    #set signal handler for ctrl-C
    signal.signal(signal.SIGINT, signal_handler)


    disp_no = os.getenv("DISPLAY")
    if disp_no:
            print "I'm running under X display = {0}".format(disp_no)
            size = (340, 240)
            print "Framebuffer size: %d x %d" % (size[0], size[1])
            screen = pygame.display.set_mode(size, 0)

    else:    
            print "I'm NOT running under X display "
            # Check which frame buffer drivers are available
            # Start with fbcon since directfb hangs with composite output
            drivers = ['fbcon', 'directfb', 'svgalib']
            found = False
            for driver in drivers:
                # Make sure that SDL_VIDEODRIVER is set
                if not os.getenv('SDL_VIDEODRIVER'):
                    os.putenv('SDL_VIDEODRIVER', driver)
                try:
                    pygame.display.init()
                except pygame.error:
                    print 'Driver: {0} failed.'.format(driver)
                    continue
                found = True
                break
    
            if not found:
                raise Exception('No suitable video driver found!')
                
            size = (pygame.display.Info().current_w, pygame.display.Info().current_h)
            print "Framebuffer size: %d x %d" % (size[0], size[1])
            screen = pygame.display.set_mode(size, pygame.FULLSCREEN)


    pygame.init()
    # Clear the screen to start
    screen.fill((0, 0, 0))
    # Initialise font support
    pygame.font.init()
    # Render the screen
    pygame.display.update()

    pygame.mouse.set_visible(1)
       
    serversocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    serversocket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    serversocket.bind((socket.gethostname(), 8080))
    serversocket.listen(5)
    try :
      conn, addr = serversocket.accept()
    except socket.error, e:
      err = e.args[0]
      if err == errno.EINTR:
        serversocket.close()
        return 0 

    conn.settimeout(1.0)
    conn.setblocking(0)
    sheet = pygame.image.load("sonic2.png").convert() ; # load only once

    while True:

        event = pygame.event.poll()
        if event.type == QUIT or (event.type == KEYUP and event.key == K_ESCAPE):
          conn.close()
          serversocket.close()
          break

        try :
          # recv can throw socket.timeout
          msg = conn.recv(1024)
          msg = msg.rstrip('\n\r ') ;# trim trailing cr/lf and spaces from msg
        except socket.error, e:
          err = e.args[0]
          if err == errno.EAGAIN or err == errno.EWOULDBLOCK:
            msg = "None"
            time.sleep(0.05)
          else:
            # a "real" error occurred
            print e
            sys.exit(1)            
 
        #displayText(msg, 30, 1, (200,200,1), True )
        #pygame.display.flip()

        if msg == 'q' : 
          conn.close()
          serversocket.close()
          break

        elif msg == 'LEFT':
          #graph = pygame.transform.rotate(graph, 270)
          ##graphrect = graph.get_rect(x=50,y=50,w=50,h=107)
          #screen.blit(graph, graphrect)
          #pygame.display.flip()

          sheet.set_clip(pygame.Rect(120, 120, 120, 120))    ;#Locate the sprite you want
          draw_me = sheet.subsurface(sheet.get_clip())     ;#Extract the sprite you want
          backdrop = pygame.Rect(0, 0, 320, 240) ;#Create the whole screen so you can draw on it

          screen.blit(draw_me,backdrop) ; #'Blit' on the backdrop
          pygame.display.flip() ; #Draw the sprite on the screen
        elif msg == 'RIGHT':
          #graph = pygame.transform.rotate(graph, 270)
          graphrect = graph.get_rect(x=103,y=0,w=102,h=107)
          screen.blit(graph, graphrect)
          pygame.display.flip()
        else :
          pass




 
if __name__ == '__main__':
    main()

