
import sys
import numpy

from io import BytesIO

# neural network class definition
from pyclasses import hnn

from http.server import BaseHTTPRequestHandler, HTTPServer
import time

import logging
import json


hostName = "localhost"
serverPort = 8080



class MyServer(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes("<html><head><title>https://pythonbasics.org</title></head>", "utf-8"))
        self.wfile.write(bytes("<p>Request: %s</p>" % self.path, "utf-8"))
        self.wfile.write(bytes("<body>", "utf-8"))
        self.wfile.write(bytes("<p>This is an example web server.</p>", "utf-8"))
        self.wfile.write(bytes("</body></html>", "utf-8"))

    def do_POST(self):
        content_length = int(self.headers['Content-Length']) # <--- Gets the size of data
        post_data = self.rfile.read(content_length) # <--- Gets the data itself
        logging.info("POST request,\nPath: %s\nHeaders:\n%s\n\nBody:\n%s\n",
                str(self.path), str(self.headers), post_data.decode('utf-8'))



        self.send_response(200)
        self.end_headers()
        response = BytesIO()
        response.write(b'This is POST request. ')
        response.write(b'Received: ')
        response.write(post_data)


        parsed_json = json.loads(post_data)
        print(json.dumps(parsed_json, indent=4, sort_keys=True))
        loaded_json = parsed_json
        for x in loaded_json:
	        print("x=", x, "ljx=", loaded_json[x])
        output = n.query(loaded_json[x])
        print ("output=", output)
        print ("winner=",numpy.argmax(output) +1)

        for x in output:
            response.write (bytes(str(x), "utf-8"))
        response.write (bytes("winner=" + str(numpy.argmax(output) +1), "utf-8"))

        self.wfile.write(response.getvalue())






#
# number of input, hidden and output nodes
input_nodes = 16
hidden_nodes = 200
output_nodes = 16

# learning rate
learning_rate = 0.01

# train the neural network
# epochs is the number of times the training data set is used for training
epochs = 10

if sys.argv[1] != "" : learning_rate = float(sys.argv[1])
if sys.argv[2] != "" : hidden_nodes = int(sys.argv[2])
if sys.argv[3] != "" : epochs = int(sys.argv[3])


# create instance of neural network
n = hnn.neuralNetwork(input_nodes,hidden_nodes,output_nodes, learning_rate, epochs)
n.load()

inp = [3, 5.70000E-03, 6.60000E-03, 7.40000E-03, 9.00000E-03, 1.10000E-02, 1.10000E-02, 1.00000E-02, 1.90000E-02, 1.95000E-02, 1.95000E-02,0.0,0.0,0.0,0.0,0.0,0.0]
output = n.query(inp[1:])
print (output)
winner=numpy.argmax(output) +1
print ("return    runner with sortprio", winner)
print ("winner is runner with sortprio", inp[0])

webServer = HTTPServer((hostName, serverPort), MyServer)
print("Server started http://%s:%s" % (hostName, serverPort))

logging.basicConfig(level=logging.DEBUG)

try:
   webServer.serve_forever()
except KeyboardInterrupt:
   pass

webServer.server_close()
print("Server stopped.")

