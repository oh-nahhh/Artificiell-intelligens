#Assignment 5
#Niklas Bergqvist
#Group 29

#Load libraries
import pandas as pd
import gym

import random
import numpy as np

env = gym.make('Taxi-v3').env

#Resets the environment and returns a random initial state
env.reset()

#Renders one frame of the environment
env.render()

#6 actions {0,1,2,3} (move), {4} (pick up), {5} (drop off)
print(env.action_space)
print(env.observation_space)

state = env.encode(3, 1, 2, 0)
print(state)
env.s = state
env.render()

print(env.P[328])

# Q-learning
#A Q-value for a particular state-action combination is representative of the "quality" of
# an action taken from that state

#Initialize q table
q_table = np.zeros([env.observation_space.n, env.action_space.n])

#Hyperparameters
alpha = 0.1
gamma = 0.6
epsilon = 0.1    #Exploration parameter

#1,000,000 episodes
episode_nbr = 1000000

for i in range(1, episode_nbr):
    state = env.reset()
    #Initially all elements are initialized to 0 and are updated gradually.
    dropouts = 0
    reward = 0

    #Explore or Exploit
    while True:
        if random.uniform(0, 1) < epsilon:
            action = env.action_space.sample()
        else:
            action = np.argmax(q_table[state])

        #Step the environment by one step
        next_state, reward, done, _ = env.step(action)

        # Update values
        old_value = q_table[state, action]
        next_max = np.max(q_table[next_state])

        new_value = (1-alpha)*old_value + alpha*(reward + gamma*next_max)
        q_table[state, action] = new_value

        #Reward if your action was beneficial or not
        if reward == -10:
            dropouts += 1

        state = next_state

        #Be done if we have successfully picked up and dropped off a passenger
        if done:
            break

#Save submission as csv-file
index = np.arange(1, 3001)
value = q_table.ravel()
df = pd.DataFrame({"Id": index, "Value": value})
df.to_csv("submission.csv", index=False)
