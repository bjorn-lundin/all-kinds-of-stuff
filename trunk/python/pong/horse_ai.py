""" Trains an agent with (stochastic) Policy Gradients on Horseodds. Uses OpenAI Gym. """
import numpy as np
#import cPickle as pickle
import _pickle as pickle
import gym
import gym_bnlbot
#bnl
from pathlib import Path

# hyperparameters
H = 200 # number of hidden layer neurons
batch_size = 10 # every how many episodes to do a param update?
learning_rate = 1e-4
gamma = 0.99 # discount factor for reward
decay_rate = 0.99 # decay factor for RMSProp leaky sum of grad^2

#bnl
filename = "./one_bet_plc_horse_ai.p"
my_file = Path(filename)
resume = my_file.is_file()
#resume = False # resume from previous checkpoint?

render = False
#render = True

#for horses always 1x16
#each row in file corresponds to a frame
#could actually feed it row-diff, just like frame-diffs

# model initialization
D = 1 * 16 # input dimensionality: 1x16 grid
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
  return np.array(I)

#  """ prepro 210x160x3 uint8 frame into 6400 (80x80) 1D float vector """
#  I = I[35:195] # crop
#  I = I[::2,::2,0] # downsample by factor of 2
#  I[I == 144] = 0 # erase background (background type 1)
#  I[I == 109] = 0 # erase background (background type 2)
#  I[I != 0] = 1 # everything else (paddles, ball) just set to 1
#  return I.astype(np.float).ravel()

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
  print("policy_forward.model['W1']", model['W1'])
  h = np.dot(model['W1'], x)
  print('policy_forward.h-1', h)
  h[h<0] = 0 # ReLU nonlinearity
  print('policy_forward.h-2', h)
  logp = np.dot(model['W2'], h)
  print('policy_forward.logp',logp)
  p = sigmoid(logp)
  return p, h # return probability of taking action 2, and hidden state. !!! what does this really mean??

def policy_backward(eph, epdlogp):
  """ backward pass. (eph is array of intermediate hidden states) """
  print('policy_backward.eph', eph)
  print('policy_backward.epdlogp', epdlogp)
  dW2 = np.dot(eph.T, epdlogp).ravel()
  print('policy_backward.dW2', dW2)
  dh = np.outer(epdlogp, model['W2'])
  print('policy_backward.dh-1', dh)
  dh[eph <= 0] = 0 # backpro prelu
  print('policy_backward.dh-2', dh)


  dW1 = np.dot(dh.T, epx)
  return {'W1':dW1, 'W2':dW2}

env = gym.make("bnlbot-v0")
observation = env.reset()
prev_x = None # used in computing the difference frame
xs,hs,dlogps,drs = [],[],[],[]
running_reward = None
reward_sum = 0
episode_number = 1
while True:
 try:
  if render: env.render()

# preprocess the observation, set input to network to be difference image
  print('observation',observation)
  cur_x = prepro(observation)
  print('cur_x',cur_x)
  if prev_x is not None:
    x = cur_x - prev_x
  else:
    x = np.zeros(D)

  print('x',x)

  prev_x = cur_x

  # forward the policy network and sample an action from the returned probability
  aprob, h = policy_forward(x)
  # action is to back the leader or do nothing
  # 2 = back, 3 = pass

  rnd = np.random.uniform()
  print('random, aprob ', rnd, aprob)
  if rnd < aprob:
    action = 2 # back leader
  else:
    action = 3 # do nothing

  # record various intermediates (needed later for backprop)
  xs.append(x) # observation
  hs.append(h) # hidden state
  if action == 2:
    y = 1
  else:
    y = 0 # a "fake label"

  dlogps.append(y - aprob) # grad that encourages the action that was taken to be taken (see http://cs231n.github.io/neural-networks-2/#losses if confused)

  # step the environment and get new measurements
  observation, reward, done, info = env.step(action)
  print('done after step with action ', action, done)
  print('reward was ', reward)

  reward_sum += reward

  drs.append(reward) # record reward (has to be done after we call step() to get reward for previous action)

  if done: # an episode finished
    print('done with episode', episode_number)

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
    print('discounted_epr',discounted_epr)
    discounted_epr /= np.std(discounted_epr)

    epdlogp *= discounted_epr # modulate the gradient with advantage (PG magic happens right here.)
    grad = policy_backward(eph, epdlogp)
    print('grad',grad)
    print('grad_buffer1',grad_buffer)
    for k in model:
      grad_buffer[k] += grad[k] # accumulate grad over batch
      print('k,grad_buffer[k],grad[k]',k,grad_buffer[k],grad[k])

    print('grad_buffer2',grad_buffer)


    # perform rmsprop parameter update every batch_size episodes
    if episode_number % batch_size == 0:
      for k,v in model.items():
        g = grad_buffer[k] # gradient
        rmsprop_cache[k] = decay_rate * rmsprop_cache[k] + (1 - decay_rate) * g**2

        print('rmsprop_cache[k]',rmsprop_cache[k])
        sq = np.sqrt(rmsprop_cache[k])
        print('sq',sq)
        model[k] += learning_rate * g / (sq + 1e-5)
        #model[k] += learning_rate * g / (np.sqrt(rmsprop_cache[k]) + 1e-5)
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
    observation = env.reset() # reset env
    prev_x = None
    episode_number += 1
  else:
    pass

  if reward < 0:
      print ('race %d: turn finished, reward: %f :-(' % (episode_number, reward))
  elif reward == 0:
      print ('race %d: turn finished, reward: %f :-|' % (episode_number, reward))
  else:
      print ('race %d: turn finished, reward: %f :-)' % (episode_number, reward))

#  if done:
#      reward = 0
  if episode_number >= 92 :
      raise KeyboardInterrupt

 except KeyboardInterrupt:
    print('saving model, KeyboardInterrupt')
    pickle.dump(model, open(filename, 'wb'))
    print('done')
    break

# except IndexError:
#    print('saving model, IndexError')
#    pickle.dump(model, open(filename, 'wb'))
#    print('done')
#    break

