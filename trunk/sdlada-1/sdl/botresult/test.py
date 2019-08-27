
import sys
import time
from time import strftime
import os
import urllib3

#import urllib
import json
import requests
#import pygame



urllib3.disable_warnings()
#import logging
#logging.captureWarnings(True)

done = False
s = None
while not done:
    # --- Main event loop
  #for event in pygame.event.get():
  #  if event.type == pygame.QUIT:
  #    done = True
  #  elif event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
  #    done = True

    time.sleep(10)
    payload = {'context': 'check_logged_in', 'dummy': str(time.time())}

    #get a seession  if not already in one
    if s == None :
      s = requests.session()

    r = s.get('https://lundin.duckdns.org', params=payload, verify=False)
    print "r1->", r
    if r.status_code == 200:
      pass
    elif r.status_code == 401:
    # login needed
      payload = {'username': 'bnl', 'context': 'login'}
      r = s.post('https://lundin.duckdns.org', params=payload, verify=False)
      print "r2->", r

