#Assignment 2
#Group 95
#Niklas Bergqvist

#ctrl+shift + c

rm(list = ls())
library(WheresCroc)

getOptions=function(point,edges)
{
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

#Normalizing function
normalize = function(data)
{
  t = sum(data)
  return(data/t)
}

initVector = function(edges)
{
  transVec = rep(1,40)

  for (i in 1:length(edges[,1]))
  {
    transVec[edges[i,1]] = transVec[edges[i,1]] + 1
    transVec[edges[i,2]] = transVec[edges[i,2]] + 1
  }
  return(transVec)
}

initState = function(positions)
{
  iState = rep(0,40)
  for(i in 1:40)
  {
    if(positions[1] != i & positions[2] != i)
    {
      #Start with 38 nodes with equal probability and two nodes with prob 0 because the croc can't start where the backpackers start
      iState[i] = 1/38
    }
  }
  return(iState)
}

#The state vector, represents the probability of each waterhole
nextState = function(state, probs, readings, transVec, moveInfo, positions, edges)
{
  for (i in 1:length(edges[,1]))
  {
    state[edges[i,1]] = state[edges[i,1]] + moveInfo$mem$probNodes[edges[i,2]]*(1/transVec[edges[i,2]])
    state[edges[i,2]] = state[edges[i,2]] + moveInfo$mem$probNodes[edges[i,1]]*(1/transVec[edges[i,1]])
  }
  #Get the emission probabilities for salinity, phosphate & nitrogen.
  for (i in 1:length(state))
  {
    salinity = dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2], FALSE)
    phosphate = dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2], FALSE)
    nitrogen = dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2], FALSE)

    prod = salinity*phosphate*nitrogen
    state[i] =(1/3)*prod*(state[i] + moveInfo$mem$probNodes[i]*(1/transVec[i]))
  }
  #normalize
  state[positions[1]] = 0
  state[positions[2]] = 0
  normalize(state)
  return(state)
}

searchHole = function(sortedState, state, positions, transMat, moveInfo, index)
{
  #Go and search the next waterhole.
  holes = 0
  for(i in 2:40)
  {
    pos = transMat [positions[3],index[i],2]
    if(!is.na(moveInfo$moves[2]) &  moveInfo$moves[2] != 0 & sortedState[i] > 0 & pos  == 0)
    {
      moveInfo$moves = transMat [positions[3],index[i],]
      holes = i
      break
    }
  }
  return(holes)
}

# Return a 40x40 transition matrix from the list of edges
transMatrix = function(edges)
{
  transMat  = array(0,c(40,40,2))
  for (i in 1:40)
  {
    if(i+1 < 41)
    {
      for (j in (i+1):40)
      {
        #Find the shortest path using bfs
        path = shortestPath(i,j,edges)


        if(length(path) == 2)
        {
          transMat [i,j,1] = path[2]
          transMat [j,i,1] = path[length(path)-1]
        }

        else
        {
          transMat [i,j,1] = path[2]
          transMat [i,j,2] = path[3]
          transMat [j,i,1] = path[length(path)-1]
          transMat [j,i,2] = path[length(path)-2]
        }
      }
    }
  }
  return(transMat )
}

#Breadth-first search (BFS)
shortestPath = function(start, goal, edges)
{
  frontier = list(list(pos = start, path = start))
  currNode = frontier[[1]]
  expanded = currNode
  while(currNode$pos != goal)
  {
    expand = getOptions(currNode$pos, edges)
    expand = setdiff(expand, expanded)
    for(i in expand)
    {
      if(currNode$pos == goal)
      {
        return(c(currNode$path, i))
      }
      frontier = c(frontier, list(list(pos = i, path = c(currNode$path, i))))
      expanded = c(expanded, i)
    }
    frontier = frontier[-1]
    currNode = frontier[[1]]
  }
  return(currNode$path)
}

#Get the state of position the croc based on readings that comes from the main function
myFunction = function (moveInfo, readings, positions, edges, probs)
{
  if(is.null(moveInfo$moves))
  {
    moveInfo$mem$probNodes = initState(positions)
  }

  #First time we run function
  if(moveInfo$mem$status == 0)
  {
    # transition vector
    transVec = initVector(edges)
    moveInfo$mem$transfer = transVec

    transMat = transMatrix(edges)
    moveInfo$mem$transMat = transMat
  }
  else
  {
    transVec = moveInfo$mem$transfer
    transMat = moveInfo$mem$transMat
  }

  state = rep(0,40)


  #Check if backpacker 1 is killed and adjust the state
  if(!is.na(positions[1]) && positions[1] < 0)
  {
    state[-1*positions[1]] = 1 #Now we know where the croc is so we set the probability to 1
  }
  #Check if backpacker 2 is killed and adjust the state
  else if(!is.na(positions[2]) && positions[2] < 0)
  {
    state[-1*positions[2]] = 1 #Now we know where the croc is so we set the probability to 1
  }
  #Otherwise we don't know where the croc is we use readings and positions to find the next state.
  else
  {

    #Get the next state using markov chain.
    state = nextState(state,probs, readings, transVec, moveInfo, positions, edges)
  }


  moveInfo$mem$probNodes = state
  sortedState = sort(state, decreasing = T)

  #Find the mmost probable hole.
  moveInfo$moves = transMat [positions[3],which.max(state),]

  index = rep(0,40)
  for (i in 1:40)
  {
    index[i] = match(sortedState[i],state)
  }

  holes = searchHole(sortedState, state, positions, transMat, moveInfo, index)


  if(  moveInfo$moves[1] == 0)
  {
    #If the croc isn't there, the state is set to 0.
    #Otherwise go to the next state.
    moveInfo$mem$probNodes[positions[3]] = 0
    normalize(moveInfo$mem$probNodes)

    if(holes != 0 )
    {
      moveInfo$moves[2] = transMat [positions[3],index[1],1]
    }
    else
    {
      moveInfo$moves[2] =  transMat [positions[3],index[2],1]
    }
  }
  else if(moveInfo$moves[2] == 0)
  {
    moveInfo$mem$probNodes[moveInfo$moves[1]] = 0;
    normalize(moveInfo$mem$probNodes)
  }
  return(moveInfo)
}

testWC(myFunction, verbose = 1, returnVec = FALSE, n = 500, seed = 21, timeLimit = 300)
