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
    self.race_list = []
    self.race_list_idx = 0

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

    with open(WIN_PLACE_CONNECTION) as wpc:
        self.win_place = eval(wpc.read())


  ##########################################

  def get_observation(self):
   # print('get_observation')
   # print('get_observation.race_list_idx ', self.race_list_idx)
   # print('get_observation rewardfile ' + self.reward_file )
   # print('get_observation race_list_idx ' + str(self.race_list_idx) )

    #wtf IS this shitlanguage
    try:
        tmp = copy.deepcopy(self.race_list[self.race_list_idx])
    except IndexError:
        print('get_observation.IndexError ')
        print('get_observation.race_list_idx ', self.race_list_idx)
        print('get_observation rewardfile ' + self.reward_file )
        print('get_observation race_list_idx ' + str(self.race_list_idx) )
          
    #print(tmp)
    tmp2=tmp
    for i in range(1,len(tmp)):
        tmp2[i-1] = float(tmp[i])
    del tmp2[-1]
    #print(tmp2)
    return tmp2
  ##########################################
  def get_reward(self,ts,idx, sel):
     # print ( "get_reward " + ts + ',' + str(idx) + ',' + str(sel))
      for line in self.reward_list:
          if line[0] >= ts:
             # print('get_reward ', self.race_list[0])
             # print('get_reward ', self.race_list[self.race_list_idx])
             # print('get_reward ', line)
              if self.reward_list[0][idx] == sel:
                 # print('get_reward ',idx, line[idx])

                  r = float(line[idx])
                  if r > 0.0 :
                      r = 0.95 * r
                  return r
              else:
                  print ('get_reward.wtf?' + str(self.reward_list[0][idx]) + '/=' + str(sel))
                  print ( "get_reward " + ts + ',' + str(idx) + ',' + str(sel))
                  return 0
                  #a=1/0

      print ('get_reward.wtf? no hit in get reward' )
      a=1/0
  ##########################################

  def step(self, action):
    #print('step')
    #print('action ' + str(action))
    #move one step into array
    self.race_list_idx = self.race_list_idx +1
    ob = self.get_observation()

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
            if 1.0 < odds and odds < lowest :
                lowest = odds
                selidx = idx
            idx = idx +1

        if lowest > 1000.0 :
            print('did not find a valid odds')
            a=1/0

        #print('step:selidx/lowodds ' + str(selidx) + '/' + str(lowest))
        #in observation first col is runner, timestamp is stripped away
 
        idx_list = selidx +1
#        print('step.idx_list',idx_list)
#        print('step.len(race_list)', len(self.race_list))
        selid = self.race_list[0][idx_list]

       # print('step.selidx ' + str(selidx))
       # print('step.selid ' + str(selid))

        #check outcome of bet
        timestamp = self.race_list[self.race_list_idx][0]
        #print('timestamp ' + timestamp)

        rew = self.get_reward(timestamp,idx_list,selid)

    else:
        pass

    done = self.race_list_idx == len(self.race_list) -1
    #print('step.ob.  ' + str(ob))
    #print('step.rew  ' + str(rew))
    #print('step.done ' + str(done))
    return (ob,rew,done,info)


  ##########################################

  def reset(self):
    self.total_count = self.total_count +1
    print('reset ' + str(self.total_count))

    self.race_list = []
    self.race_list_idx = 0

    # read race-file into array
    for i in range(self.racefile_list_idx,len(self.racefile_list)):
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
        # get name from racefile
        path = self.racefile_list[self.racefile_list_idx].split('/')
        print('path',path)
        tmp = path[1].split('.')
        winmarket = tmp[0] + '.' + tmp[1]
        print('winmarket', winmarket)
        try :
            placemarket = self.win_place[winmarket]
            print('Found placemarket', placemarket)
            #check for rewardfile
            filename = REWARDFILE_DIRECTORY + '/' + placemarket + '.dat'
            my_file = Path(filename)
            resume = my_file.is_file()
  
            if resume : 
                break
            else:
                print('no rewardfile named', filename)   

        except KeyError :
            self.racefile_list_idx = self.racefile_list_idx +1 
            print('KeyError, use next')



    with open(RACEFILE_DIRECTORY + '/' + self.racefile_list[self.racefile_list_idx]) as rf:
        for line in rf:
            self.race_list.append(line.split('|'))


    self.reward_file=""
    self.reward_list=[]
    
    #self.reward_file = REWARDFILE_DIRECTORY + '/' + path[1]
    self.reward_file = REWARDFILE_DIRECTORY + '/' + placemarket + '.dat'
    #print(self.reward_file)
    with open(self.reward_file) as rf:
        for line in rf:
            self.reward_list.append(line.split('|'))

    #skip header row
    #self.race_list_idx = 1
    self.race_list_idx = int(0.75 * len(self.race_list))
    #print(self.race_list[self.race_list_idx])

    return self.get_observation()


  ##########################################

  def render(self, mode='human', close=False):
    print('render')

  ##########################################