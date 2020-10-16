
""" Trains an agent with (stochastic) Policy Gradients on Pong. Uses OpenAI Gym. """
import numpy as np
import _pickle as pickle
#import gym
#bnl
import os
import os.path
from pathlib import Path
import copy
import getopt, sys

import psycopg2 as pg
import psycopg2.extras as ex



# Get full command-line arguments
full_cmd_arguments = sys.argv

# Keep all but the first
argument_list = full_cmd_arguments[1:]

long_options = ["mode="]
short_options = ["m:"]

try:
    arguments, values = getopt.getopt(argument_list, short_options, long_options)
except getopt.error as err:
    # Output error, and return with an error code
    print (str(err))
    sys.exit(2)

#defaults
mode='train'

MAKE_BET = 2
MAKE_NOTHING = 3

learning_rate=1e-4
position='1'
side='lay'

for current_argument, current_value in arguments:
    if current_argument in ("-m", "--mode"):
        mode = current_value
        print ("Enabling mode", mode)
    elif current_argument in ("-h", "--help"):
        print ("Displaying help")


class FakeHorseDb(object):

###################################
  def __init__(self, mode='train', side='lay'):
    """a list of files, keep track of them.
    and in each file, keep track on what line we are on"""
    print('init')
    #set up data structure

    self.conn = pg.connect(
      host="192.168.1.136",
      database="bnl",
      user="bnl",
      password="bnl",
      cursor_factory=ex.DictCursor)

    self.commision = 0.02
    self.current_marketid = ''
    self.side = side.upper()
    self.marketid = None
    self.selectionid = None
    self.timestamp = None
    self.cursor = None
    self.selectionidlist = []
    self.bestof = np.zeros(16)
    self.marketidlist = []

    #list of marketids to use
    cur = self.conn.cursor()
    cur.execute("""
      select OK.MARKETID from OKMARKETS OK, AMARKETS M
        where true
        and OK.MARKETID = M.MARKETID
        and OK.MARKETTYPE = %s
        and M.STARTTS >= %s
        order by M.STARTTS""",
        ("WIN",'2016-04-01 00:00:00.000'))
    rows = cur.fetchall()
    for row in rows:
#      print('init',row['marketid'])
      self.marketidlist.append(row['marketid'])

    cur.close()


###################################
  def reward(self):
    print("reward",self.marketid, self.selectionid, self.timestamp, self.side)
    rew = -15.0
    cur = self.conn.cursor()
    cur.execute("""
      select * from AREWARDS
        where true
        and MARKETID = %s
        and SELECTIONID = %s
        and PRICETS = %s
        and SIDE = %s """,
        (self.marketid, self.selectionid, self.timestamp, self.side))
    row = cur.fetchone()
    if row is not None:
      rew = float(row['profit'])
      print('reward','found',row['profit'],'row',row)
    else:
      print('reward','not found for',self.marketid, self.selectionid, self.timestamp, self.side )

    cur.close()

    return rew/100.0

################################


  def step(self,action):
    """ will do action and step one step further"""
    ob = self.get_observation()
    done = False
    if ob is None:
        done = True
    rew = 0.0
    info = "no_info"
    #decide to bet or not
    if action == MAKE_BET and not done :
        #do bet on first runner found with lowest odds
        idx = -1
        idx_with_lowest_odds = 0
        lowest_odds = 10000000.0
#        print ("step",ob)
        for o in ob:
          idx = idx +1
#          print ("step",o)
          if o > 1.0 :
            if o < lowest_odds :
              lowest_odds = o
              idx_with_lowest_odds = idx

        selid_with_lowest_odds = int(self.bestof[idx_with_lowest_odds])
#        print ("lowest_odds",lowest_odds)
#        print ("lowest_odds idx",idx_with_lowest_odds)
#        print ("lowest_odds selid",selid_with_lowest_odds)
        print('step',self.selectionid," -> ",selid_with_lowest_odds)
        self.selectionid = selid_with_lowest_odds
        rew = self.reward()
        print('step','reward',rew)
    else:
        pass

    return (ob,rew,done,info)

  ######################################

  def render(self):
      print("render", "marketid", self.marketid,"selid", self.selectionid, "timestamp",self.timestamp)

  ######################################

  def get_observation(self):
    idx = 0
    sql = """
      select PH.* from APRICESHISTORY PH
        where true
        and PH.MARKETID = %s
        and PH.SELECTIONID = %s
        and PH.PRICETS > %s
        order by PH.PRICETS"""

    if self.timestamp is None :
      print('get_observation','incoming timestamp was None')
      self.timestamp = '1900-01-01 00:00:00.000'

    ob = np.zeros(16)
    self.bestof = np.zeros(16)
    found = False
    for selectionid in self.selectionidlist:
      self.cursor = self.conn.cursor()
      self.cursor.execute(sql, (self.marketid, selectionid, self.timestamp))
      row = self.cursor.fetchone()
      if row is not None :
        print('get_observation','idx',idx,row)
        found = True
        if idx < 16 :
          self.bestof[idx] = row['selectionid']
          if self.side == 'BACK' :
            ob[idx] = row['backprice']
          elif self.side == 'LAY' :
            ob[idx] = row['layprice']
          else:
            a = 1/0

        idx = idx +1
        self.timestamp = row['pricets']

    print('get_observation','found',found)
    print('get_observation',ob)
    if not found :
      #signal that this race is done, no rows found for any selid
      self.timestamp = None
      self.selectionid = None
      self.marketid = None
      return None

    return ob

  ##########################################


  def reset(self):
    """ will pickup and read next market"""
    #get one from top of list and delete the listitem
    try :
      self.marketid = self.marketidlist.pop(0)
      print('reset','new marketid is', self.marketid)
      #get a list if runners in sortorder
      cur = self.conn.cursor()
      cur.execute(
        """
        select SELECTIONID from ARUNNERS
          where true
          and MARKETID = %s
          order by SORTPRIO
        """,
          (self.marketid,))

      rows = cur.fetchall()
      for row in rows:
        print('reset',row['selectionid'])
        self.selectionidlist.append(row['selectionid'])

      cur.close()
      self.timestamp = None

      return self.get_observation()
    except IndexError:
      return None

  ##########################################


# hyperparameters
H = 200 # number of hidden layer neurons
batch_size = 10 # every how many episodes to do a param update?
#learning_rate = 1e-4
#cmdline param - default =1e-4
gamma = 0.99 # discount factor for reward
decay_rate = 0.99 # decay factor for RMSProp leaky sum of grad^2

#bnl
filename = "./horses_" + str(position) + "_" + side + "_real_rewards.p"
my_file = Path(filename)
resume = my_file.is_file()

render = False
#render = True

#for horses always 16x1
#each row in file corresponds to a frame
#could actually feed it row-diff, just like frame-diffs

# model initialization
#D = 80 * 80 # input dimensionality: 80x80 grid
D = 16 * 1 # input dimensionality: 16x1 grid
#D = 32 * 1 # input dimensionality: 16x1 grid
if resume:
  model = pickle.load(open(filename, 'rb'))
else:
  model = {}
  model['W1'] = np.random.randn(H,D) / np.sqrt(D) # "Xavier" initialization
  model['W2'] = np.random.randn(H) / np.sqrt(H)

grad_buffer = { k : np.zeros_like(v) for k,v in model.items() } # update buffers that add up gradients over a batch
rmsprop_cache = { k : np.zeros_like(v) for k,v in model.items() } # rmsprop memory

def sigmoid(x):
  return 1.0 / (1.0 + np.exp(-x)) # sigmoid "squashing" function to interval [0,1]

def prepro(I):
  """ prepro 16x1  (16x1) 1D float vector """
  return I ; # ger numera numpy vector
  J = np.zeros(16)
  cnt=0
#  print(prepro,I)
#  for odds in I[DATA_OFFSET:DATA_OFFSET+16]:
  for odds in I:
      J[cnt] = float(odds)
      cnt=cnt+1
      if cnt == 16 : break
#  print(prepro,J)
  return J

#  """ prepro 32x1  (32x1) 1D float vector """
#  J = np.zeros(32)
#  cnt=0
#  print(I)
#  for odds in I[DATA_OFFSET:DATA_OFFSET+32]:
#      J[cnt] = float(odds)
#      cnt=cnt+1
#      if cnt == 32 : break
#  return J

def discount_rewards(r):
  """ take 1D float array of rewards and compute discounted reward """
  discounted_r = np.zeros_like(r)
  running_add = 0
  for t in reversed(range(0, r.size)):
    #if r[t] != 0: running_add = 0 # reset the sum, since this was a game boundary (pong specific!)
    running_add = running_add * gamma + r[t]
    discounted_r[t] = running_add
  return discounted_r

def policy_forward(x):
  h = np.dot(model['W1'], x)
  h[h<0] = 0 # ReLU nonlinearity
  logp = np.dot(model['W2'], h)
  p = sigmoid(logp)
  return p, h # return probability of taking action 2=MAKE_BET, and hidden state

def policy_backward(eph, epdlogp):
  """ backward pass. (eph is array of intermediate hidden states) """
  dW2 = np.dot(eph.T, epdlogp).ravel()
  dh = np.outer(epdlogp, model['W2'])
  dh[eph <= 0] = 0 # backpro prelu
  dW1 = np.dot(dh.T, epx)
  return {'W1':dW1, 'W2':dW2}

#env = gym.make("Pong-v0")
print("mode",mode,"side",side)
env = FakeHorseDb(mode, side)
observation = env.reset()
prev_x = None # used in computing the difference frame
xs,hs,dlogps,drs = [],[],[],[]
running_reward = None
reward_sum = 0
episode_number = 0
render = True

#print("test",env.reward( '1.160934105', 19450871, '06:55:41.572'))
#a = 1.0 / 0

while True:
    try:
        if render: env.render()
        print("")
        print("-------------------------------")

        # preprocess the observation, set input to network to be difference image
        cur_x = prepro(observation)
        reread = False
        if prev_x is None :
            x = np.zeros(D)
            reread = True
        else:
            x = cur_x - prev_x

        prev_x = cur_x
        if reread : # get another sample so we get a diff. do NOT bet now (MAKE_NOTHING)
            observation, reward, done, info = env.step(MAKE_NOTHING)
            cur_x = prepro(observation)
            x = cur_x - prev_x
            prev_x = cur_x
        else:

            # forward the policy network and sample an action from the returned probability
            aprob, h = policy_forward(x)

            if np.random.uniform() < aprob :   # roll the dice! [ 0  1  3  4 11 12]
                action = MAKE_BET # lay favorite
            else :
                action = MAKE_NOTHING # no bet

            # record various intermediates (needed later for backprop)
            xs.append(x) # observation
            hs.append(h) # hidden state
            if action == MAKE_BET : # a "fake label"
                y = 1
            else :
                y = 0

            dlogps.append(y - aprob) # grad that encourages the action that was taken to be taken (see http://cs231n.github.io/neural-networks-2/#losses if confused)

            # step the environment and get new measurements
            observation, reward, done, info = env.step(action)
            reward_sum += reward

            print ("action",action,"reward",reward,"reward_sum",reward_sum,"aprob",aprob)

            drs.append(reward) # record reward (has to be done after we call step() to get reward for previous action)

            if done: # an episode/race finished
                episode_number += 1

                # stack together all inputs, hidden states, action gradients, and rewards for this episode
                epx = np.vstack(xs)
                eph = np.vstack(hs)
                epdlogp = np.vstack(dlogps)
                epr = np.vstack(drs)
                xs,hs,dlogps,drs = [],[],[],[] # reset array memory

                # compute the discounted reward backwards through time
                discounted_epr = discount_rewards(epr)
                # standardize the rewards to be unit normal (helps control the gradient estimator variance)
                discounted_epr -= np.mean(discounted_epr)
                discounted_epr /= np.std(discounted_epr)

                epdlogp *= discounted_epr # modulate the gradient with advantage (PG magic happens right here.)
                grad = policy_backward(eph, epdlogp)
                for k in model:
                    grad_buffer[k] += grad[k] # accumulate grad over batch

                # perform rmsprop parameter update every batch_size episodes
                if episode_number % batch_size == 0:
                    for k,v in model.items():
                        g = grad_buffer[k] # gradient
                        rmsprop_cache[k] = decay_rate * rmsprop_cache[k] + (1 - decay_rate) * g**2
                        model[k] += learning_rate * g / (np.sqrt(rmsprop_cache[k]) + 1e-5)
                        grad_buffer[k] = np.zeros_like(v) # reset batch gradient buffer

                # boring book-keeping
                if running_reward is None:
                    running_reward = reward_sum
                else:
                    running_reward = running_reward * 0.99 + reward_sum * 0.01

                print ('resetting env. episode reward total was %f. running mean: %f' % (reward_sum, running_reward))
                if episode_number % 100 == 0:
                    pickle.dump(model, open(filename, 'wb'))
                reward_sum = 0
                observation = env.reset() # get new observation for next turn
                if observation is None :
                    #try again
                    observation, reward, done, info = env.step(MAKE_NOTHING) # get new observation for next turn
                    if observation is None : # give up
                       raise KeyboardInterrupt
                prev_x = None # don't compare last input of race n with first input of race n+1


    except KeyboardInterrupt:
     print('saving model')
     pickle.dump(model, open(filename, 'wb'))
     print('done')
     break
