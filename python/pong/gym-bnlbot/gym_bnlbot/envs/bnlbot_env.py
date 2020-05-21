import gym
from gym import error, spaces, utils
from gym.utils import seeding
import os
from pathlib import Path
import copy

RACEFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/win/races'
REWARDFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/plc/rewards'
WIN_PLACE_CONNECTION = os.environ.get('BOT_HISTORY') + '/data/ai/win_place_connection.dat'


  ##########################################


class BnlbotEnv(gym.Env):
  metadata = {'render.modes': ['human']}

  ##########################################
  def __init__(self):
    print('init')
    self.total_count = 0
    #set up data structure
    self.racefile_list_idx = -1
    self.racefile_list = []
    self.racefile_name = []
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


  ##########################################

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
    tmp2=tmp
    for i in range(1,len(tmp)):
        tmp2[i-1] = float(tmp[i])/10000.0
    del tmp2[-1]
    #print(tmp2)
    return tmp2
  ##########################################
  def get_reward(self,ts,idx, sel):
     # print ( "get_reward " + ts + ',' + str(idx) + ',' + str(sel))
      passed_headline = False
      for line in self.reward_list:
          if line[0] >= ts and passed_headline:
              if self.reward_list[0][idx] == sel:
                  r = float(line[idx])
                  if r > 0.0 :
                      r = 0.95 * r
                  print ('get_reward.reward', r, line[idx])
                  return r/100000.0
              else:
                  print ('get_reward.wtf?' + str(self.reward_list[0][idx]) + '/=' + str(sel))
                  print ( "get_reward " + ts + ',' + str(idx) + ',' + str(sel))
                  return 0
                  #a=1/0
          passed_headline = True

      print ('get_reward.wtf? no hit in get reward' )
      print ('get_reward.wtf?' + str(self.reward_list[0][idx]) + '/=' + str(sel))
      print ('get_reward ' + ts + ',' + str(idx) + ',' + str(sel))
      print ('get_reward ', self.reward_file)
      return 0
      a=1/0
  ##########################################

  def step(self, action):
    #print('step')
    #print('action ' + str(action))
    #move one step into array
    self.racefile_idx  = self.racefile_idx +1
    ob = self.get_observation()
    done = False
    rew = 0.0
    info = "no_info"

    #decide to bet or not
    if action == 2 :
        #do bet on first runner found with lowest odds
        lowest = 10000.0
        idx = 0
        selidx = 0
        b=[]
        for odds in ob:
            if 0.0 < odds and odds < lowest :
                lowest = odds
                selidx = idx
            idx = idx +1

        if lowest > 1000.0 :
            print('did not find a valid odds')
            a=1/0

        #in observation first col is runner, timestamp is stripped away

        idx_list = selidx +1
        selid = self.racefile_name[0][idx_list]

        #check outcome of bet
        timestamp = self.racefile_name[self.racefile_idx][0]
        #print('timestamp ' + timestamp)

        rew = self.get_reward(timestamp,idx_list,selid)
        #if betting , then quit - only 1 bet/race
#        done = True
    else:
        pass
#        done = True


    #try only to end of file
    if not done :
        done = self.racefile_idx == len(self.racefile_name) -1

    return (ob,rew,done,info)

  ##########################################

  def reset(self):
    self.total_count = self.total_count +1
    print('reset ' + str(self.total_count))

    self.racefile_name = []
    self.racefile_idx = 0

    # read race-file into array
    for i in range(self.racefile_list_idx, len(self.racefile_list)):
        #print("'" + str(i) + "'")
        if i == -1 :
            #print('in -1')
            self.racefile_list_idx = self.racefile_list_idx +1
            pass
        elif self.racefile_list[i] == '.DS_Store' :
            #print('in .DS_Store')
            self.racefile_list_idx = self.racefile_list_idx +1
            pass
        else:
            #print('in else')
            self.racefile_list_idx = self.racefile_list_idx +1
            #print("'" + self.racefile_list[self.racefile_list_idx] + "'")
            break

    placemarket = ''
    while True:
        # read reward-file into array
        # get name from racefile_name
        path = self.racefile_list[self.racefile_list_idx].split('/')
        print('reset.path',path)
        tmp = path[1].split('.')
        winmarket = tmp[0] + '.' + tmp[1]
        print('winmarket', winmarket)
        try :
            placemarket = self.win_place[winmarket]
            print('Found placemarket', placemarket)
            #check for rewardfile
            filename = REWARDFILE_DIRECTORY + '/' + placemarket + '.dat'
            my_file = Path(filename)

            if my_file.is_file() :
                break
            else:
                self.racefile_list_idx = self.racefile_list_idx +1
                print('no rewardfile named', filename)

        except KeyError :
            self.racefile_list_idx = self.racefile_list_idx +1
            print('KeyError, use next, winmarket was ', winmarket)


    if self.racefile_list_idx >= 8000 :
        print('racefile_list size', len(self.racefile_list))
        print('racefile_list_idx', self.racefile_list_idx)
        print('exit since >= 8000')
        raise KeyboardInterrupt

    with open(RACEFILE_DIRECTORY + '/' + self.racefile_list[self.racefile_list_idx]) as rf:
        for line in rf:
            self.racefile_name.append(line.split('|'))


    self.reward_file=""
    self.reward_list=[]

    #self.reward_file = REWARDFILE_DIRECTORY + '/' + path[1]
    self.reward_file = REWARDFILE_DIRECTORY + '/' + placemarket + '.dat'
    #print(self.reward_file)
    with open(self.reward_file) as rf:
        for line in rf:
            self.reward_list.append(line.split('|'))

    #skip header row
    #self.racefile_idx = 1
    self.racefile_idx = int(0.75 * len(self.racefile_name))
    #print(self.racefile_name[self.racefile_idx])

    return self.get_observation()


  ##########################################

  def render(self, mode='human', close=False):
    print('render')

  ##########################################
