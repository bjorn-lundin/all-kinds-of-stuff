"""
 Pygame base template for opening a window

 Sample Python/Pygame Programs
 Simpson College Computer Science
 http://programarcadegames.com/
 http://simpson.edu/computer-science/

 Explanation video: http://youtu.be/vRB_983kUMc
"""

import sys
import time
from time import strftime
import os
import urllib3
import json
import requests
import pygame
import logging
urllib3.disable_warnings()

# Define some colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
GREEN = (0, 255, 0)
RED = (255, 0, 0)

URL='https://lundin.duckdns.org'

def puts(what,size,x,y):
    logging.debug(what + " size=" + str(size) + " x=" + str(x) + " y=" +str(y))
    font = pygame.font.SysFont("freserif", size)
    text = font.render(what, True, (255,128, 0))
    screen.blit(text, (x - text.get_width() // 2, y - text.get_height() // 2))
#    time.sleep(1)


def get_data(sess, context):
   payload = {'context': context, 'dummy': str(time.time())}
   r = sess.get(URL, params=payload, verify=False)
   data = json.loads(r.text)
   logging.debug("today %s"  % data['total'] )
   return data


logging.basicConfig(filename='botresult.log',level=logging.DEBUG, format='%(asctime)s %(levelname)s:%(message)s')
s = None
os.environ["SDL_FBDEV"] = "/dev/fb1"


pygame.init()
# Set the width and height of the screen [width, height]
size = (320, 240)
screen = pygame.display.set_mode(size)
pygame.mouse.set_visible(0)

done = False
clock = pygame.time.Clock()
cnt = 100
# -------- Main Program Loop -----------
while not done:
  try:
    # --- Main event loop

    # --- Limit to 1 frames per second
    cnt = cnt + 1

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            done = True
        elif event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
            done = True

    if cnt > 100 :
      cnt = 0
      # --- Game logic should go here

      # --- Screen-clearing code goes here

      # Here, we clear the screen to white. Don't put other drawing commands
      # above this, or they will be erased with this command.

      # If you want a background image, replace this clear with blit'ing the
      # background image.
      screen.fill(WHITE)

      #get a seession  if not already in one
      payload = {'context': 'check_logged_in', 'dummy': str(time.time())}
      if s == None :
        s = requests.session()

      r = s.get(URL, params=payload, verify=False)

      if r.status_code == 200:
        pass
      elif r.status_code == 401:
      # login needed
        payload = {'username': 'bnl', 'context': 'login'}
        r = s.post(URL, params=payload, verify=False)

      today = get_data(s, 'todays_bets')
      this_week = get_data(s, 'thisweeks_bets')
      last_week = get_data(s, 'lastweeks_bets')
      this_month = get_data(s, 'thismonths_bets')
      last_month = get_data(s, 'lastmonths_bets')

      # --- Drawing code should go here
      puts(str(today['total']),      60,  80, 120)
      puts(str(this_week['total']),  40, 240,  20)
      puts(str(last_week['total']),  40, 240,  86)
      puts(str(this_month['total']), 40, 240, 152)
      puts(str(last_month['total']), 40, 240, 220)

      # --- Go ahead and update the screen with what we've drawn.
      pygame.display.flip()
    else:
      logging.debug("sleeping %s"  % str(cnt) )
      clock.tick(1)

  except ValueError :
    logging.exception("ValueError exception - server down? wait 1 min")
    time.sleep(60)

# Close the window and quit.
pygame.quit()
exit()

except Exception :
  logging.exception("generic exception")
