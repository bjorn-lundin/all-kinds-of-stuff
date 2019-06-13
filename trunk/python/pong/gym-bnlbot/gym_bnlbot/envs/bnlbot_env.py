import gym
from gym import error, spaces, utils
from gym.utils import seeding
import os
from pathlib import Path

RACEFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/races'
REWARDFILE_DIRECTORY = os.environ.get('BOT_HISTORY') + '/data/ai/rewards'

  ##########################################


class BnlbotEnv(gym.Env):
  metadata = {'render.modes': ['human']}

  ##########################################
  def __init__(self):
    print('init')
    #set up data structure
    self.racefile_list_idx = -1
    self.racefile_list = []
    self.race_list = []
    self.race_list_idx = 0

    dir_list = os.listdir(RACEFILE_DIRECTORY)
    for dirname in dir_list:
        mydir = Path(RACEFILE_DIRECTORY + '/' + dirname)
        print(dirname + ' ' + str(mydir.is_dir()))
        if mydir.is_dir():
            file_list = os.listdir(RACEFILE_DIRECTORY + '/' + dirname)
            for filename in file_list:
                if filename == '.DS_Store' :
                    pass
                else :
                    self.racefile_list.append(dirname + '/' + filename)
                    print(dirname + '/' + filename)

    self.row_maxrow = 0
    self.reward_file=""
    self.reward_list=[]

  ##########################################

  def step(self, action):
    print('step')
    #move one step into array
    #decide to bet or not
    #check outcome of bet

  ##########################################

  def reset(self):
    print('reset')

    # read race-file into array
    for i in range(self.racefile_list_idx,len(self.racefile_list)):
        print("'" + str(i) + "'")
        if i == -1 :
            print('in -1')
            self.racefile_list_idx = self.racefile_list_idx +1
            pass
        elif self.racefile_list[i] == '.DS_Store' :
            print('in .DS_Store')
            self.racefile_list_idx = self.racefile_list_idx +1
            pass
        else:
            print('in else')
            self.racefile_list_idx = self.racefile_list_idx +1
            print("'" + self.racefile_list[self.racefile_list_idx] + "'")
            break

    with open(RACEFILE_DIRECTORY + '/' + self.racefile_list[self.racefile_list_idx]) as rf:
        for line in rf:
            self.race_list.append(line)


    # read reward-file into array
    # get name from racefile
    path = self.racefile_list[self.racefile_list_idx].split('/')
    self.reward_file = REWARDFILE_DIRECTORY + '/' + path[1]
    print(self.reward_file)
    with open(self.reward_file) as rf:
        for line in rf:
            self.reward_list.append(line)
    #skip header row
    self.race_list_idx = 1
    print(self.race_list[self.race_list_idx])

    tmp = self.race_list[self.race_list_idx].split('|')
    print(tmp)
    tmp2=tmp
    for i in range(1,len(tmp)):
        tmp2[i-1] = float(tmp[i])
    del tmp2[-1]
    print(tmp2)
    return tmp2


  ##########################################

  def render(self, mode='human', close=False):
    print('render')

  ##########################################
