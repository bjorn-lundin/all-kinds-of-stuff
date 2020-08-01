""" Trains an agent with (stochastic) Policy Gradients on Pong. Uses OpenAI Gym. """
import numpy as np
import _pickle as pickle
from io import BytesIO
from http.server import BaseHTTPRequestHandler, HTTPServer
import time
import logging
import json

#hostName = "localhost"
hostName = ""
serverPort = 8080

class AI_Model(object):
  """lay leader? send in a diff between 2 samples"""

  def __init__(self):
    #print('init-AI_Model')
    #set up data structure
    # model initialization
#    self.D = 16 * 1 # input dimensionality: 16x1 grid
    self.D = 32 * 1 # input dimensionality: 16x1 grid
    self.filename = "./horses_2_actions_2nd_with_real_reward_lay.p"
    self.model = pickle.load(open(self.filename, 'rb'))

  def sigmoid(self,x):
    return 1.0 / (1.0 + np.exp(-x)) # sigmoid "squashing" function to interval [0,1]

  def policy_forward(self,x):
    h = np.dot(self.model['W1'], x)
    h[h<0] = 0 # ReLU nonlinearity
    logp = np.dot(self.model['W2'], h)
    p = self.sigmoid(logp)
    return p, h # return probability of taking action 2, and hidden state

  def query(self,diff,curr):
    x = np.zeros(self.D)
    cnt = 0
    #first look at the diff
    for diffed_odds in diff:
        x[cnt] = float(diffed_odds)
        cnt = cnt + 1
    #now add the current odds
    for odds in curr:
        x[cnt] = float(odds)
        cnt = cnt + 1

    # forward the policy network and sample an action from the returned probability
    aprob, h = self.policy_forward(x)

    #action = 2 => bet
    print("aprob",aprob) 
    if aprob > 0.999 :  #let model decide
        action = 2 # bet
    else :
        action = 3 # no bet

    return action == 2

#########################################################################


class MyServer(BaseHTTPRequestHandler):


    def __init__(self, *args, **kwargs):
         self.ai_model = AI_Model()
         super().__init__(*args, **kwargs)

    def do_POST(self):
        content_length = int(self.headers['Content-Length'].strip()) # <--- Gets the size of data
        post_data = self.rfile.read(content_length) # <--- Gets the data itself
        #logging.info("POST request,\nPath: %s\nHeaders:\n%s\n\nBody:\n%s\n",
        #        str(self.path), str(self.headers), post_data.decode('utf-8'))

        parsed_json = json.loads(post_data)
        #print(json.dumps(parsed_json, indent=4, sort_keys=True))

        self.do_bet = self.ai_model.query(parsed_json['diff'],parsed_json['curr'])

        self.send_response(200)
        self.end_headers()
        response = BytesIO()
        if self.do_bet :
            response.write(bytes(str("1"), "utf-8"))
        else:
            response.write(bytes(str("0"), "utf-8"))

        self.wfile.write(response.getvalue())
##############################################################


webServer = HTTPServer((hostName, serverPort), MyServer)
print("Server started http://%s:%s" % (hostName, serverPort))

logging.basicConfig(level=logging.DEBUG)


try:
   webServer.serve_forever()
except KeyboardInterrupt:
   pass

webServer.server_close()
print("Server stopped.")

