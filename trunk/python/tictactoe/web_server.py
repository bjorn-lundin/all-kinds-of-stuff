#!/usr/bin/python
# -*- coding: latin-1 -*-
from BaseHTTPServer import BaseHTTPRequestHandler,HTTPServer
from os import curdir, sep
import cgi
import time

from board import Board 
from robocam import Robocam

PORT_NUMBER = 8080

#This class will handles any incoming request from
#the browser 
class WebHandler(BaseHTTPRequestHandler):
    
    #Handler for the GET requests
    def do_GET(self):
#        if self.path=="/":
#            self.path="/index_example3.html"

        try:
            #Check the file extension required and
            #set the right mime type

#            if self.path.endswith(".html"):
#                mimetype='text/html'

#                #Open the static file requested and send it
#                f = open(curdir + sep + self.path) 
#                self.send_response(200)
#                self.send_header('Content-type',mimetype)
#                self.end_headers()
#                self.wfile.write(f.read())
#                f.close()
#            return
            self.send_error(404,'Get is not supported')

        except IOError:
            self.send_error(404,'File Not Found')

    #Handler for the POST requests
    def do_POST(self):
        print "----- got a POST ----"
        print "path",  self.path
        print "headers",  self.headers
        if self.path=="/board":
            form = cgi.FieldStorage(fp=self.rfile, headers=self.headers, environ={'REQUEST_METHOD':'POST', 'CONTENT_TYPE':self.headers['Content-Type'],})
            #dict_of_positions = form["board"].value
            #print "The dict is: %s" % dict_of_positions

            board = Board()
            pic = Robocam(True)
            while True:
                #get a new picture until ok parsed
                try:
                    pic.get_board_snapshop()
                    break
                except Exception as ex:
                    print ex.message
                    board.clear()
                    board.text('Letar O...')
                    board.update()                    
                    time.sleep(1.0)            
            print "pic.board_dict",pic.board_dict
            board.set_pieces(pic.board_dict)
            board.clear()
            board.draw_grid()
            board.draw_markers()
            board.text('Din tur!')
            board.update()
            self.send_response(200)
            self.end_headers()
#            self.wfile.write("")
            return
        elif self.path=="/cam":
            pic = Robocam()
            while True:
                #get a new picture until ok parsed
                #try:
                    pic.get_board_snapshop()
                    break
                #except Exception as ex:
                #    print ex.message
                #    time.sleep(1.0)            
                    
            print "pic.board_dict", pic.board_dict
            self.send_response(200)
            self.end_headers()
#            self.wfile.write("")
            return
            
#while True:
#    r = Robocam(True)
#    try:
#        r.get_board_snapshop()
#        time.sleep(5.0)
#    except Exception as ex:
#        print ex.message
            
            

try:
    #Create a web server and define the handler to manage the
    #incoming request
    server = HTTPServer(('', PORT_NUMBER), WebHandler)
    print 'Started httpserver on port ' , PORT_NUMBER
    
    #Wait forever for incoming htto requests
    server.serve_forever()

except KeyboardInterrupt:
    print '^C received, shutting down the web server'
    server.socket.close()