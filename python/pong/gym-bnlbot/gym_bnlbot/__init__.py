from gym.envs.registration import register

register(
    id='bnlbot-v0',
    entry_point='gym_bnlbot.envs:BnlbotEnv',
)