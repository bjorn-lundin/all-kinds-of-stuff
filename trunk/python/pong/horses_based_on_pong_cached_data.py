
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

BACKPRICE  = 0
LAYPRICE   = 1
BACKREWARD = 2
LAYREWARD  = 3


learning_rate=1e-4
#learning_rate=1e-5
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

    self.commision = 0.02
    self.current_marketid = ''
    self.side = side.upper()
    self.marketid = None
    self.selectionid = None
    self.currentrow = None
    self.numrows = None
    self.cursor = None
    self.selectionidlist = []
    self.dict_selid_idx = {}
    self.dict_idx_selid = {}
    self.cache_matrix = None
    self.bestof = np.zeros(16)
    filename = "pickles/marketlist" + ".pickle"
    self.marketlist = pickle.load(open(filename, 'rb'))
    print('init','num markets', len(self.marketlist))


###################################
  def reward(self):
    print("reward",self.market['marketid'], self.selectionid, self.currentrow, self.side)
    rew = -15.0
    col = self.dict_selid_idx[self.selectionid]

    if self.side == 'BACK' :
      rew = float(self.cache_matrix [self.currentrow] [col] [BACKREWARD])
    elif self.side == 'LAY' :
      rew = float(self.cache_matrix [self.currentrow] [col] [LAYREWARD])
    else:
      a = 2/0

    print('reward','for',self.market['marketid'], self.selectionid, self.currentrow, self.side, rew )

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
#          print ("step",o,idx)
          if o > 1.0 :
            if o < lowest_odds :
#              print ("step",'lowest',o,idx)
              lowest_odds = o
              idx_with_lowest_odds = idx

#        print ('step',"self.dict_idx_selid",self.dict_idx_selid,'idx',idx)
        selid_with_lowest_odds = int(self.dict_idx_selid[idx_with_lowest_odds])
#        print ("lowest_odds idx",idx_with_lowest_odds)
#        print ("lowest_odds selid",selid_with_lowest_odds)
#        print('step',self.selectionid," -> ",selid_with_lowest_odds)
        self.selectionid = selid_with_lowest_odds
        rew = self.reward()
        print('step','reward',rew)
    else:
        pass

    return (ob,rew,done,info)

  ######################################

  def render(self):
      print("render", "marketid", self.market['marketid'], "currentrow",self.currentrow)

  ######################################

  def get_observation(self):

    if self.currentrow is None :
      print('get_observation','incoming currentrow was None, first in race')
      self.currentrow = 0
    else:
      self.currentrow = self.currentrow +1

    if self.currentrow >= self.numrows  :
      #signal that this race is done, no rows found for any selid
      return None

    ob = np.zeros(16)
    for selid, col in self.dict_selid_idx.items():
      if self.side == 'BACK' :
        ob[col] = self.cache_matrix [self.currentrow] [col] [BACKPRICE]/1000.0
      elif self.side == 'LAY' :
        ob[col] = self.cache_matrix [self.currentrow] [col] [LAYPRICE]/1000.0
      else:
        a = 1/0

    print('get_observation',ob)
    ok = False
    for i in ob :
      if i > 0.0:
        ok = True
        break

    if not ok:
      return None

    return ob

  ##########################################


  def reset(self):
    """ will pickup and read next market"""
    #get one from top of list and delete the listitem
    try :
      self.market = self.marketlist.pop(0)
      print('reset','num markets left', len(self.marketlist))

      filename_selid_idx = "pickles/selid_idx_" + self.market['marketid'] + ".pickle"
      filename_idx_selid = "pickles/idx_selid_" + self.market['marketid'] + ".pickle"

      self.dict_selid_idx = pickle.load(open(filename_selid_idx, 'rb'))
      self.dict_idx_selid = pickle.load(open(filename_idx_selid, 'rb'))

      filename = "pickles/cache_" + self.market['marketid'] + ".pickle"
      self.cache_matrix = pickle.load(open(filename, 'rb'))
      self.numrows = self.cache_matrix.shape[0]
      self.currentrow = None
      print('reset','new marketid is', self.market['marketid'],'numrows',self.numrows)
      return self.get_observation()
    except IndexError:
      return None

  ##########################################

  def do_print(self):
    ob = np.zeros(16)
    row = 0
    while row < self.numrows -1 :
      for selid, col in self.dict_selid_idx.items():
        ob[col] = self.cache_matrix [row] [col] [BACKPRICE]
      ok = False
      for i in ob :
        if i > 0.0:
          ok = True
          break

      if not ok:
        break
      else:
        print(ob)
        row = row+1
   ##################################





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
  print('h',h)
  h[h<0] = 0 # ReLU nonlinearity
  logp = np.dot(model['W2'], h)
  print('logp',logp)
  p = sigmoid(logp)
  print('p',p)
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
env.do_print()
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
        print("-------------------------------")
        print("new turn in loop")
        if render: 
            env.render()

        # preprocess the observation, set input to network to be difference image
        cur_x = prepro(observation)
        print('main','cur_x',cur_x)
        if prev_x is None :
            prev_x = np.zeros(D)
            print('main','prev_x was None')

        x = cur_x - prev_x
        prev_x = cur_x
        
        if True:
            # forward the policy network and sample an action from the returned probability
            aprob, h = policy_forward(x)
            rnd = np.random.uniform()
            do_bet = rnd < aprob 
            print('main','rnd',rnd,'aprob',aprob,'do_bet',do_bet)
            if do_bet :   # roll the dice! [ 0  1  3  4 11 12]
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
                print ('main', 'done this race')
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
                env.do_print()


    except KeyboardInterrupt:
     print('saving model')
     pickle.dump(model, open(filename, 'wb'))
     print('done')
     break
