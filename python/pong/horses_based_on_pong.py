""" Trains an agent with (stochastic) Policy Gradients on Pong. Uses OpenAI Gym. """
import numpy as np
#import cPickle as pickle
import _pickle as pickle
#import gym
#bnl
from pathlib import Path
import os
import os.path
from pathlib import Path
import copy

import getopt, sys

# Get full command-line arguments
full_cmd_arguments = sys.argv

# Keep all but the first
argument_list = full_cmd_arguments[1:]

long_options = ["mode=", "bettype="]
short_options = ["m:","b:"]

try:
    arguments, values = getopt.getopt(argument_list, short_options, long_options)
except getopt.error as err:
    # Output error, and return with an error code
    print (str(err))
    sys.exit(2)

#defaults
mode='train'
bettype='back'

for current_argument, current_value in arguments:
    if current_argument in ("-m", "--mode"):
        mode = current_value
        print ("Enabling mode", mode)
    elif current_argument in ("-h", "--help"):
        print ("Displaying help")
    elif current_argument in ("-b", "--bettype"):
        bettype = current_value
        print ("Enabling bettype", bettype)



class FakeHorse(object):

  def __init__(self, mode='train', bettype='back'):
    """a list of files, keep track of them.
    and in each file, keep track on what line we are on"""
    print('init')
    #set up data structure
    self.racefile_list_idx = -1
    self.racefile_list = []
    self.racefile_name = []
    self.racefile_idx = 0
    self.mode = mode
    self.bettype = bettype
    self.dirname = os.environ.get('BOT_HISTORY') + '/data/ai/pong/' + self.bettype + '/win/' + self.mode
    self.size = 1.0
    self.commision = 0.05

    #print(self.dirname)
    filelist = os.listdir(self.dirname)
    #print(filelist)
    for filename in filelist:
        if os.path.isfile(self.dirname + '/' + filename):
            if filename == '.DS_Store' :
                pass
            else :
                self.racefile_list.append(self.dirname + '/' + filename)
                #print(self.dirname + '/' + filename)
        else:
            pass


  def step(self,action):
    """ will do action and step one step further"""
    #print('step')
    #print('action ' + str(action))
    #move one step into array
    self.racefile_idx = self.racefile_idx +1
    ob = self.get_observation()
    done = False
    rew = 0.0
    info = "no_info"

    #decide to bet or not
    if action == 2 :
        #do bet on first runner found with lowest odds
        lowest = 1.1
        idx = 1
        sortidx = 0

       # x=0
       # for odds in ob:
       #     print('x',x,'ob[x]',ob[x])
       #     x=x+1

       # x=5
       # for odds in ob[5:]:
       #     print('x',x,'ob[x]',ob[x])
       #     x=x+1

        #odds runner zero is in ob[5]
        for odds in ob[5:]:
            if 0.0 <= float(odds) and float(odds) < lowest :
                lowest = float(odds)
                sortidx = idx
            idx = idx +1

        if lowest > 1.0 :
            print('did not find a valid odds')
            print(ob)
            print("lowest",lowest)
            print("sortidx",sortidx-1)

            print ("file",self.racefile_list[self.racefile_list_idx],"line",[self.racefile_idx])
            a=1/0

        sortidx = sortidx -1 # back one for 0-based


        #print(ob)
        #print("sortidx",sortidx,"lowest odds",lowest)
        #print("ob[0]",ob[0],"sortidxq",sortidx)
        oddsidx = sortidx +5 # offset metadata
        #print("ob[0]",ob[0],"sortidxq",sortidx,"ob[oddsidx]",ob[oddsidx],oddsidx)

        #used size = 1.0
        #commisiom = 5%
        if self.bettype == 'back' :
            #check for winner - or looser
            if sortidx == int(ob[0])   :
                rew = self.size * (1.0 - self.commision) * (float(ob[oddsidx]) - 0.001)
            elif sortidx == int(ob[1]) :
                rew = self.size * (1.0 - self.commision) * (float(ob[oddsidx]) - 0.001)
            elif sortidx == int(ob[2]) :
                rew = self.size * (1.0 - self.commision) * (float(ob[oddsidx]) - 0.001)
            else :
                rew = -self.size
        elif self.bettype == 'lay':  #lay bets - we do not want to be winners
            if sortidx == int(ob[0])   :
                rew = -self.size * (float(ob[oddsidx]) - 0.001)
            elif sortidx == int(ob[1]) :
                rew = -self.size * (float(ob[oddsidx]) - 0.001)
            elif sortidx == int(ob[2]) :
                rew = -self.size * (float(ob[oddsidx]) - 0.001)
            else :
                rew = self.size * (1.0 - self.commision)
        else:
            raise Exception("bad bettype '" + self.bettype + "'" )

        #if betting , then quit - only 1 bet/race
#        done = True
    else:
        pass

    #try only to end of file
    if not done :
        done = self.racefile_idx == len(self.racefile_name) -1
        #print ("done",done,"self.racefile_idx" ,self.racefile_idx ,"len(self.racefile_name) -1", len(self.racefile_name) -1)
    return (ob,rew,done,info)


  def render():
      print("render not implemented")


  def get_observation(self):
   # print('get_observation')
   # print('get_observation.racefile_idx ', self.racefile_idx)
   # print('get_observation rewardfile ' + self.reward_file )
   # print('get_observation racefile_idx ' + str(self.racefile_idx) )

    #wtf IS this shitlanguage
    try:
        tmp = copy.deepcopy(self.racefile_name[self.racefile_idx])
    except IndexError:
        print('get_observation.IndexError ')
        print('get_observation.racefile_idx ', self.racefile_idx)
        print('get_observation rewardfile ' + self.reward_file )
        print('get_observation racefile_name ' + str(self.racefile_name) )
        print('get_observation racefile_list_idx ' + str(self.racefile_list_idx) )
        print('get_observation racefile_list[idx] ' + self.racefile_list[self.racefile_list_idx] )

    #print(tmp)
    return tmp
  ##########################################


  def reset(self):
    """ will pickup and read next file"""
    print('reset ',self.racefile_list_idx, '/',len(self.racefile_list))
    self.racefile_name = []
    self.racefile_idx = 0

    self.racefile_list_idx = self.racefile_list_idx +1

    try:
        with open(self.racefile_list[self.racefile_list_idx]) as rf:
            for line in rf:
               self.racefile_name.append(line.split(','))
        return self.get_observation()
    except IndexError:
        print('reset.IndexError ')
    return None


  ##########################################




# hyperparameters
H = 200 # number of hidden layer neurons
batch_size = 10 # every how many episodes to do a param update?
learning_rate = 1e-4
gamma = 0.99 # discount factor for reward
decay_rate = 0.99 # decay factor for RMSProp leaky sum of grad^2


#bnl
filename = "./horses_2_actions_" + bettype +".p"

my_file = Path(filename)
resume = my_file.is_file()
#resume = False # resume from previous checkpoint?

render = False
#render = True

#for horses always 16x1
#each row in file corresponds to a frame
#could actauly feed it row-diff, just like frame-diffs

# model initialization
#D = 80 * 80 # input dimensionality: 80x80 grid
D = 16 * 1 # input dimensionality: 16x1 grid
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
  J = np.zeros(16)
  cnt=0
  for odds in I[5:]:
      J[cnt] = float(odds)
      cnt=cnt+1
  return J

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
  return p, h # return probability of taking action 2, and hidden state

def policy_backward(eph, epdlogp):
  """ backward pass. (eph is array of intermediate hidden states) """
  dW2 = np.dot(eph.T, epdlogp).ravel()
  dh = np.outer(epdlogp, model['W2'])
  dh[eph <= 0] = 0 # backpro prelu
  dW1 = np.dot(dh.T, epx)
  return {'W1':dW1, 'W2':dW2}

#env = gym.make("Pong-v0")
env = FakeHorse(mode, bettype)
observation = env.reset()
prev_x = None # used in computing the difference frame
xs,hs,dlogps,drs = [],[],[],[]
running_reward = None
reward_sum = 0
episode_number = 0
render = False

while True:
 try:
  if render: env.render()

  # preprocess the observation, set input to network to be difference image
  cur_x = prepro(observation)
  x = cur_x - prev_x if prev_x is not None else np.zeros(D)
  prev_x = cur_x

  # forward the policy network and sample an action from the returned probability
  aprob, h = policy_forward(x)
  #action = 2 if np.random.uniform() < aprob else 3 # roll the dice!
  if np.random.uniform() < aprob :   # roll the dice! [ 0  1  3  4 11 12]
      action = 2 # up
  else :
      action = 3 # down


  # record various intermediates (needed later for backprop)
  xs.append(x) # observation
  hs.append(h) # hidden state
  #y = 1 if action == 2 else 0 # a "fake label"
  if action == 2 : # a "fake label"
      y = 1
  else :
      y = 0


  dlogps.append(y - aprob) # grad that encourages the action that was taken to be taken (see http://cs231n.github.io/neural-networks-2/#losses if confused)

  # step the environment and get new measurements
  observation, reward, done, info = env.step(action)
  reward_sum += reward

  #print ("action",action,"reward",reward,"reward_sum",reward_sum)


  drs.append(reward) # record reward (has to be done after we call step() to get reward for previous action)

  if done: # an episode finished
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
    for k in model: grad_buffer[k] += grad[k] # accumulate grad over batch

    # perform rmsprop parameter update every batch_size episodes
    if episode_number % batch_size == 0:
      for k,v in model.items():
        g = grad_buffer[k] # gradient
        rmsprop_cache[k] = decay_rate * rmsprop_cache[k] + (1 - decay_rate) * g**2
        model[k] += learning_rate * g / (np.sqrt(rmsprop_cache[k]) + 1e-5)
        grad_buffer[k] = np.zeros_like(v) # reset batch gradient buffer

    # boring book-keeping
    #running_reward = reward_sum if running_reward is None else running_reward * 0.99 + reward_sum * 0.01
    if running_reward is None:
        running_reward = reward_sum
    else:
        running_reward = running_reward * 0.99 + reward_sum * 0.01

    print ('resetting env. episode reward total was %f. running mean: %f' % (reward_sum, running_reward))
    if episode_number % 100 == 0: pickle.dump(model, open(filename, 'wb'))
    reward_sum = 0
    observation = env.reset() # reset env
    if observation is None : raise KeyboardInterrupt
    prev_x = None

    #last frame - ball is fiished - does not apply to horses
    #if reward > 0 :
    #  print ('ep %d: race finished, reward: %f :-)' % (episode_number, reward))
    #elif reward < 0 :
    #  print ('ep %d: race finished, reward: %f :-(' % (episode_number, reward))
    #else:
    #  print ('ep %d: race finished, reward: %f :-|' % (episode_number, reward))

 except KeyboardInterrupt:
  print('saving model')
  pickle.dump(model, open(filename, 'wb'))
  print('done')
  break
