#######################################
#simulating some data with this Q-learning model paired with a bandit 
#environment we defined in this section of my codes in the github I shared.
#We first need to specify some task parameters, 
#such as the number of arms, the means and standard deviations of rewards,
#number of rounds, and number of trials per round. 
#Then we need to define the model parameters, 
#where we start with some sensible values of α=.9
#, β=1, and Q0=0
#. The code block below describes the entire simulation procedure and plots reward curves over each individual round (colored lines) and aggregated over 10 rounds (black line).
######################################
#Task parameters for bandit with Gaussian rewards
k <- 10 #number of arms
meanVec <- seq(-10,10, length.out=k) #Payoff means
sigmaVec <- rep(1, k) #Payoff stdevs
T <- 25 #total number of trials
nAgents <- 4
rounds <- 10
s
banditGenerator <- function(a) {#a is an integer or a vector of integers, selecting one of the 1:k arms of the bandit
  payoff <- rnorm(n = length(a), mean = meanVec[a], sd = sigmaVec[a])
  return (payoff)
}

#Model parameters
alpha <- .9 #learning rate
beta <- 1 #Softmax inverse temperature
Q0 <- Qvec <-  rep(0,k) #prior initialization of Q-values

#Now simulate data for multiple agents over multiple rounds
simDF <- data.frame()
for (a in 1:nAgents){ #loop through agents
  for (r in 1:rounds){ #loop through rounds
    Qvec <- Q0 #reset Q-values
    for (t in 1:T){ #loop through trials
      p <- softmax(beta, Qvec) #compute softmax policy
      action <- sample(1:k,size = 1, prob=p) #sample action
      reward <- banditGenerator(action) #generate reward
      Qvec[action] <- Qvec[action] + alpha*(reward - Qvec[action]) #update q-values
      chosen <- rep(0, k) #create an index for the chosen option
      chosen[action]<- 1 #1 = chosen, 0 = not
      trialDF <- data.frame(trial = t, agent = a, round = r, Q = Qvec, action = 1:k, chosen = chosen, reward = reward)
      simDF <- rbind(simDF,trialDF)
    }
  }
}

saveRDS(simDF, 'data/simChoicesQlearning.Rds')
#Plot results
ggplot(subset(simDF, chosen==1), aes(x = trial, y = reward, color = interaction(agent,round)))+
  geom_line(size = .5, alpha = 0.5)+
  stat_summary(fun = mean, geom='line', color = 'black', size = 1)+
  theme_classic()+
  xlab('Trial') +
  ylab('Reward')+
  ggtitle('Simulated performance')+
  theme(legend.position='none')