""" Trains an agent with (stochastic) Policy Gradients on Pong. Uses OpenAI Gym. """
import numpy as np
#import cPickle as pickle
import _pickle as pickle
#import gym
#bnl
from pathlib import Path
import os
from pathlib import Path
import copy

RACEFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/win/races'
REWARDFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/plc/rewards'
WIN_PLACE_CONNECTION = os.environ.get('BOT_HISTORY') + '/data/ai/win_place_connection.dat'

#TRAINING_FILE="/Users/bnl/svn2/trunk/python/neuralnetwork/horse_runner_prices_train.csv"
#SAMPLE_FILE="/Users/bnl/svn2/trunk/python/neuralnetwork/horse_runner_prices_sample.csv"

#CURRENT_FILE=TRAINING_FILE

class FakeHorse(object):

  def __init__(self):
    print('init')
    self.total_count = 0
    #set up data structure
    self.racefile_list_idx = -1
    self.racefile_list = []
    self.racefile = []
    self.racefile_idx = 0
    self.win_place = {}

    dir_list = os.listdir(RACEFILE_DIRECTORY)
    for dirname in dir_list:
        mydir = Path(RACEFILE_DIRECTORY + '/' + dirname)
        #print(dirname + ' ' + str(mydir.is_dir()))
        if mydir.is_dir():
            file_list = os.listdir(RACEFILE_DIRECTORY + '/' + dirname)
            for filename in file_list:
                if filename == '.DS_Store' :
                    pass
                else :
                    self.racefile_list.append(dirname + '/' + filename)
                    #print(dirname + '/' + filename)

    self.reward_file=""
    self.reward_list=[]

    # treat as dict
    with open(WIN_PLACE_CONNECTION) as wpc:
        self.win_place = eval(wpc.read())



  def reset(self):
      """ will pickup and read next file"""

  def step(self,action):
      """ will do action and step one step further"""


  def render():
      print("render not implemented")






# hyperparameters
H = 200 # number of hidden layer neurons
batch_size = 10 # every how many episodes to do a param update?
learning_rate = 1e-4
gamma = 0.99 # discount factor for reward
decay_rate = 0.99 # decay factor for RMSProp leaky sum of grad^2

#bnl
filename = "./save_horses_2_actions.p"

my_file = Path(filename)
resume = my_file.is_file()
#resume = False # resume from previous checkpoint?

render = False
#render = True

#for horses always 16x350
#each row in file corresponds to a frame
#could actauly feed it row-diff, just like frame-diffs

# model initialization
#D = 80 * 80 # input dimensionality: 80x80 grid
D = 16 * 350 # input dimensionality: 16x350 grid
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

def prepro_unused_old(I):
  """ prepro 210x160x3 uint8 frame into 6400 (80x80) 1D float vector """
  I = I[35:195] # crop
  I = I[::2,::2,0] # downsample by factor of 2
  I[I == 144] = 0 # erase background (background type 1)
  I[I == 109] = 0 # erase background (background type 2)
  I[I != 0] = 1 # everything else (paddles, ball) just set to 1
  return I.astype(np.float).ravel()

def prepro(I):
  """ prepro 16x350x1 uint8 frame into 5600 (16x350) 1D float vector """
  I[I != 0] = 1 # everything else (oddmarkers) just set to 1
  return I.astype(np.float).ravel()

def discount_rewards(r):
  """ take 1D float array of rewards and compute discounted reward """
  discounted_r = np.zeros_like(r)
  running_add = 0
  for t in reversed(range(0, r.size)):
    if r[t] != 0: running_add = 0 # reset the sum, since this was a game boundary (pong specific!)
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
env = FakeHorse()
observation = env.reset()
prev_x = None # used in computing the difference frame
xs,hs,dlogps,drs = [],[],[],[]
running_reward = None
reward_sum = 0
episode_number = 0
while True:
 try:
#  if render: env.render()

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

#  print ("action",action)
#  print(env.ale.getMinimalActionSet())

  dlogps.append(y - aprob) # grad that encourages the action that was taken to be taken (see http://cs231n.github.io/neural-networks-2/#losses if confused)

  # step the environment and get new measurements
  observation, reward, done, info = env.step(action)
  reward_sum += reward

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
    prev_x = None

  if reward != 0: # Pong has either +1 or -1 reward exactly when game ends.
    if reward > 0 :
      print ('ep %d: ball finished, reward: %f :-)' % (episode_number, reward))
    else:
      print ('ep %d: ball finished, reward: %f' % (episode_number, reward))



 except KeyboardInterrupt:
  print('saving model')
  pickle.dump(model, open(filename, 'wb'))
  print('done')
  break
