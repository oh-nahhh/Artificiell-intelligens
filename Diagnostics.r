# Assignment 3
# Niklas Bergqvist
# Group 95

#rm(list = ls())
library(Diagnostics)

#Candidate function
candidatefcn = function(prob)
{
  if(prob == 1)
  {
    return (0)
  }
  else
  {
    return (1)
  }
}

#Calculate probabilities
calcProb = function(case, network)
{
  probOld = dnorm(case[[2]], mean = network$Te[2*case[[1]] + 1], sd = network$Te[2*case[[1]] + 2])

  if(case[[1]] == 1)
  {
    probOld = probOld*network$Pn
  }
  if(case[[1]] == 0)
  {
    probOld = probOld*(1 -network$Pn)
  }

  if(case[[3]] == 1)
  {
    probOld = probOld*network$VTB
  }
  if(case[[3]] == 0)
  {
    probOld = probOld*(1 - network$VTB)
  }

  if(case[[4]] == 1)
  {
    probOld = probOld*network$TB[[1 + case[[3]]]]
  }
  if(case[[4]] == 0)
  {
    probOld = probOld*(1 - network$TB[[1 + case[[3]]]])
  }

  if(case[[5]] == 1)
  {
    probOld = probOld*network$Sm
  }
  if(case[[5]] == 0)
  {
    probOld = probOld*(1 - network$Sm)
  }

  if(case[[6]] == 1)
  {
    probOld = probOld*network$LC[[1 + case[[5]]]]
  }
  if(case[[6]] == 0)
  {
    probOld = probOld*(1 - network$LC[[1 + case[[5]]]])
  }

  if(case[[7]] == 1)
  {
    probOld = probOld*network$Br[[1 + case[[5]]]]
  }
  if(case[[7]] == 0)
  {
    probOld = probOld*(1 - network$Br[[1 + case[[5]]]])
  }

  if(case[[8]] == 1)
  {
    probOld = probOld*network$XR[[1 + case[[1]] + 2*case[[4]] + 4*case[[6]]]]
  }
  if(case[[8]] == 0)
  {
    probOld = probOld*(1 - network$XR[[1 + case[[1]] + 2*case[[4]] + 4*case[[6]]]])
  }

  if(case[[9]] == 1)
  {
    probOld = probOld*network$Dy[[1 + case[[6]] + 2*case[[7]]]]
  }
  if(case[[9]] == 0)
  {
    probOld = probOld*(1 - network$Dy[[1 + case[[6]] + 2*case[[7]]]])
  }

  return(probOld)
}

#learn function to be used
learn = function(hist)
{
  network = list()

  #P(Temperature | Pneumonia)
  network$Te = c(mean(hist[which(hist$Pn == 0), 'Te']), sd(hist[which(hist$Pn == 0), 'Te']),
                 mean(hist[which(hist$Pn == 1), 'Te']), sd(hist[which(hist$Pn == 1), 'Te']))

  #P(Visited TB spot)
  network$VTB = (1 + sum(hist$VTB))/(length(hist$VTB) + 2)

  #P(Smokes)
  network$Sm = (1 + sum(hist$Sm))/(length(hist$Sm) + 2)

  #P(X-ray | Pneumonia, Tuberculosis, Lung Cancer)
  network$XR = c((1 + sum(hist[hist$Pn == 0 & hist$TB == 0 & hist$LC == 0,]$XR))/(length(hist$XR[hist$LC == 0 & hist$TB == 0 & hist$Pn == 0]) + 2),
                 (1 + sum(hist[hist$Pn == 1 & hist$TB == 0 & hist$LC == 0,]$XR))/(length(hist$XR[hist$LC == 0 & hist$TB == 0 & hist$Pn == 1]) + 2),
                 (1 + sum(hist[hist$Pn == 0 & hist$TB == 1 & hist$LC == 0,]$XR))/(length(hist$XR[hist$LC == 0 & hist$TB == 1 & hist$Pn == 0]) + 2),
                 (1 + sum(hist[hist$Pn == 1 & hist$TB == 1 & hist$LC == 0,]$XR))/(length(hist$XR[hist$LC == 0 & hist$TB == 1 & hist$Pn == 1]) + 2),
                 (1 + sum(hist[hist$Pn == 0 & hist$TB == 0 & hist$LC == 1,]$XR))/(length(hist$XR[hist$LC == 1 & hist$TB == 0 & hist$Pn == 0]) + 2),
                 (1 + sum(hist[hist$Pn == 1 & hist$TB == 0 & hist$LC == 1,]$XR))/(length(hist$XR[hist$LC == 1 & hist$TB == 0 & hist$Pn == 1]) + 2),
                 (1 + sum(hist[hist$Pn == 0 & hist$TB == 1 & hist$LC == 1,]$XR))/(length(hist$XR[hist$LC == 1 & hist$TB == 1 & hist$Pn == 0]) + 2),
                 (1 + sum(hist[hist$Pn == 1 & hist$TB == 1 & hist$LC == 1,]$XR))/(length(hist$XR[hist$LC == 1 & hist$TB == 1 & hist$Pn == 1]) + 2))

  #P(Dyspnea | Bronchitis, Lung Cancer)
  network$Dy = c((1 + sum(hist$Dy[(hist$Br == 0)&(hist$LC == 0)]))/(length(hist$Dy[(hist$Br == 0)&(hist$LC == 0)])),
                 (1 + sum(hist$Dy[(hist$Br == 1)&(hist$LC == 0)]))/(length(hist$Dy[(hist$Br == 1)&(hist$LC == 0)])),
                 (1 + sum(hist$Dy[(hist$Br == 0)&(hist$LC == 1)]))/(length(hist$Dy[(hist$Br == 0)&(hist$LC == 1)])),
                 (1 + sum(hist$Dy[(hist$Br == 1)&(hist$LC == 1)]))/(length(hist$Dy[(hist$Br == 1)&(hist$LC == 1)])))

  # P(Pneumonia)
  network$Pn = (1 + sum(hist$Pn))/(length(hist$Pn))

  network$TB = c((1 + sum(hist$TB[hist$VTB == 0]))/(2 + length(hist$TB[hist$VTB == 0])),
                 (1 + sum(hist$TB[hist$VTB == 1]))/(2 + length(hist$TB[hist$VTB == 1])))

  #P(Lung Cancer | Smokes)
  network$LC = c((1 + sum(hist$LC[hist$Sm == 0]))/(2 + length(hist$LC[hist$Sm == 0])),
                 (1 + sum(hist$LC[hist$Sm == 1]))/(2 + length(hist$LC[hist$Sm == 1])))

  #P(Bronchitis | Smokes)
  network$Br = c((1 + sum(hist$Br[hist$Sm == 0]))/(2 + length(hist$Br[hist$Sm == 0])),
                 (1 + sum(hist$Br[hist$Sm == 1]))/(2 + length(hist$Br[hist$Sm == 1])))

  return(network)
}

#diagnose function to be used
diagnose = function(network, cases)
{
  #Initialize the MCMC Metropolis within Gibbs Sampling
  estimates = matrix(NA, 10, 4)

  #Keep track of the cases {Pn, TB, LC, Br}
  diagnoses = c(1,4,6,7)

  #We up the sample size to gain better performance
  nSamples = 50000

  #Designate a burn-in period
  burnIn = nSamples/10
  for(i in 1:10)
  {
    #Generate a random point number inside the loop
    randomPt = cases[i,]
    randomPt[diagnoses] = runif(4, min = 0, max = 1)>0.5
    randomPt = as.numeric(randomPt)

    samples = matrix(NA, nSamples-burnIn, 9)

    #Calculate probability for the random point
    probNew = calcProb(randomPt, network)

    for(s in 1:nSamples)
    {
      for(each in diagnoses)
      {
        randomPt[each] = candidatefcn(randomPt[each])
        probOld = calcProb(randomPt, network)

        #Decide whether to keep or to discard with probability (1-probOld/probNew)
        if((probOld < probNew) & (runif(1)- (probOld/probNew)> 0) )
        {
            randomPt[each] = candidatefcn(randomPt[each])
            probOld = probNew
        }
        probNew = probOld
      }
      #Use the burn-in period to discard samples
      if(s > burnIn)
      {
       samples[s - burnIn,] = as.numeric(randomPt)
      }
    }
    #After the sampling, estimate the probabilities for the unknown variables
    estimates[i,] = colMeans(samples[,diagnoses])
  }
  return(estimates)
}
#runDiagnostics(learn, diagnose)
runDiagnostics(learn, diagnose, verbose = 2)
