#Assignment1 - Niklas Bergqvist

rm(list = ls())
#install.packages("C:/Users/az09_/Desktop/Project1/DeliveryMan_1.1.0.zip",repos=NULL)
library(DeliveryMan)

#Move the delivery man's car.
myFunction = function(roads,car,packages)
{
  #Check if the car already has a package it goes and delivers it.
  if (car$load>0)
  {
    car = goToDest(roads,car, packages)
  }
  # Else the car doesn't have a package it goes and finds the nearest one.
  else
  {
    car = goToPack(roads,car, packages)

  }
  car$mem=list()
  return (car)
}

#manhattanDistance = function(nodeX, nodeY, nodeXX, nodeYY)
#{
#   return( abs(nodeX - nodeXX) + abs(nodeY - nodeYY))
#}

euclideanDistance  = function(nodeX, nodeY, nodeXX, nodeYY)
{
  a = abs(nodeX - nodeXX)^2
  b = abs(nodeY - nodeYY)^2
  c = sqrt(a + b)
  return (c)
}

# Find a package and then got to it.
goToPack = function(roads,car,packages)
{
  load=which(packages[,5]==0)  #discard already delivered packages
  #load=load

  packs = c()
  for(i in 1:length(load)) #Find the closest package by Euclidean (or Manhattan) distance for all packages
  {
    packs <- c(packs,euclideanDistance(packages[load[i],][1],packages[load[i],][2] ,car$x,car$y))
  }
  toGo = load[which.min(packs)] #choose closest package
  car$nextMove = a_Star(car,roads,list(x=car$x,y=car$y),list(x = packages[toGo,][1], y=packages[toGo,][2]))
  return (car)
}

# Go to a destination with a package that is already picked up.
goToDest = function(roads,car,packages)
{
  toGo=car$load
  car$nextMove = a_Star(car,roads,list(x=car$x,y=car$y),list(x = packages[toGo,][3], y=packages[toGo,][4]))

  return (car)
}


insertFrontier = function(newNode, frontier)
{
  names(newNode) = c("x","y","cost","dist","fun","move")
  index = indexOfNode(frontier,newNode)
  if(index == 0)
  {
    frontier <- append(list(newNode),frontier)
  }
  else
  {
    if(frontier[[index]]$fun > newNode$fun)
    {
      frontier[[index]] <- newNode
    }
  }
  return(frontier)
}

indexOfNode = function(frontier, newNode)
{
  if(length(frontier)==0)
  {
    return(0)
  }

  nodeXY <- function(node)(node$x==newNode$x && node$y==newNode$y)
  index = which(sapply(frontier,nodeXY))

  if (length(index)==0)
  {
    return(0)
  }
  else
  {
    return(index)
  }
}

#return a list of the closest neighbors to a node
findNeighbors = function(currNode, packages, roads, frontier)
{
  #currNode is the node to be expanded
  # check to the left
  if(currNode$x > 1)
  {
    nodeX = currNode$x -1
    nodeY = currNode$y
    fun = currNode$fun
    cost = roads$hroads[nodeX, nodeY]
    dist = euclideanDistance(nodeX,nodeY,packages$x,packages$y)
    path = fun + cost + dist

    if (currNode$move==5)
    {
      move <- 4 #Move left
    }
    else
    {
      move <- currNode$move
    }

    newNode = list(nodeX, nodeY, cost, dist, path, move)
    frontier = insertFrontier(newNode, frontier)

  }

  #check to the right
  if(currNode$x<10)
  {
    nodeX = currNode$x +1
    nodeY = currNode$y
    fun = currNode$fun
    cost = roads$hroads[nodeX-1,nodeY]
    dist = euclideanDistance(nodeX,nodeY,packages$x,packages$y)

    path = fun + cost + dist

    if (currNode$move==5)
    {
      move <- 6 #Move right
    }
    else
    {
      move <- currNode$move
    }

    newNode = list(nodeX, nodeY, cost, dist, path, move)
    frontier = insertFrontier(newNode, frontier)

  }
  #check upwards
  if(currNode$y<10)
  {
    nodeX = currNode$x
    nodeY = currNode$y+1
    cost = roads$vroads[nodeX,nodeY-1]
    dist = euclideanDistance(nodeX,nodeY,packages$x,packages$y)
    fun = currNode$fun
    path = fun  + cost + dist

    if (currNode$move==5)
    {
      move <- 8 #Move up
    }
    else
    {
      move <- currNode$move
    }

    newNode = list(nodeX, nodeY, cost, dist, path, move)
    frontier = insertFrontier(newNode, frontier)

  }

  #check downwards
  if(currNode$y>1)
  {
    nodeX = currNode$x
    nodeY = currNode$y-1
    cost = roads$vroads[nodeX,nodeY]
    dist = euclideanDistance(nodeX,nodeY,packages$x,packages$y)
    fun = currNode$fun
    path = fun  + cost + dist

    if (currNode$move==5)
    {
      move <- 2 #Move down
    }
    else
    {
      move <- currNode$move
    }

    newNode = list(nodeX, nodeY, cost, dist, path, move)
    frontier = insertFrontier(newNode, frontier)
  }
  return(frontier)
}

#function that executes the A* algorithm.
a_Star = function(car, roads, start, packages)
{
  #If we are already at the destination then we should stay.
  if(start$x == packages$x && start$y == packages$y)
  {
    return(5)
  }
  frontier = list(list(x = start$x, y = start$y, cost = 0,
                       dist = euclideanDistance(start$x,start$y,packages$x,packages$y), fun = 0, move = 5))

  #Otherwise do the A* algorithm.
  while (TRUE)
  {
    nodes = sapply(frontier,function(node)(node$fun))
    bestPath = which.min(nodes)
    currNode = frontier[[bestPath]]
    if(currNode$x == packages$x && currNode$y == packages$y)
    {
      #Stop if the destination node is about to be expanded.
      break
    }
    frontier <- findNeighbors(currNode, packages, roads, frontier[-bestPath])
  }

  bestPath=which.min(sapply(frontier,function(node)node$fun))
  return(frontier[[bestPath]]$move)
}


#runDeliveryMan(myFunction)



#Testing


# source('C:/Users/az09_/Desktop/Project1/DeliveryMan.r')
# #source('DeliveryMan.r')
#
# graphics.off()
# par("mar")
# par(mar=c(1,1,1,1))
#
# n = 5
# #n = 1000
# #ii = 0
#
# init = n
# i = 1
# j = 1
# res = 0
#
# while (j <= n)
# {
#   #ii = ii +1
#   #turns = runDeliveryMan(myFunction, 10, 2000, T, 0, 5)
#   turns = runDeliveryMan(myFunction,10,2000,F,0,5)
#   #print(ii)
#   j = j+1
#   i = i+1
#
#   res = res+turns
# }
#
# avg = res/init
#
# print("Average = ")
# print(avg)
