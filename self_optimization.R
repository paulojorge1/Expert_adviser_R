
####################################################################################
#
#################    6.1. Search for the best combination of predictors   ##########
#                    tabuSearch
####################################################################################


# objective function
ObjFun <- function(th){
  require(xgboost)
  # Exit if all zero in binary string
  if (sum(th) == 0) return(0)
  # names of predictors that correspond to 1 in the binary string
  sub <- subset[th != 0]
  # Create structure for training a model
  dtrain <- xgb.DMatrix(data = x.train[ ,sub], label = y.train)
  # Train a model
  bst = xgb.train(params = par, data = dtrain, nrounds = nround, verbose = 0)
  # Calculate forecasts with the text set
  pred <- predict(bst, x.test[ ,sub])
  # Calculate forecast error
  err <- mean(as.numeric(pred > 0.5) != y.test)
  # Return quality criterion
  return(1 - err) 
}

#---tabuSearch----------------------
require(tabuSearch)
require(magrittr)
require(xgboost)

# Output dataframe
dt <- form.data(n = 16, z = 10, len = 0)

# Names of all predictors in the initial set 
subset <- colnames(In())
set.seed(54321, kind = "L'Ecuyer-CMRG")

# Prepare sets for training and testing
DT <- prepareTrain(x = dt[  ,subset], 
                   y = dt$y, 
                   balance = FALSE, 
                   rati = 4/5, mod = "stratified", 
                   norm = FALSE, meth = method)
train <- DT$train
test <- DT$test
x.train <- train[  ,subset] %>% as.matrix()
y.train <- train$y %>% as.numeric() %>% subtract(1)
x.test <- test[  ,subset] %>% as.matrix()
y.test <- test$y %>% as.numeric() %>% subtract(1)

# Initial binary vector
th <- rep(1,length(subset))

# Model parameters
par <- list(max.depth = 3, eta = 1, silent = 0, 
            nthread = 2, objective = 'binary:logistic')
nround = 10

# Initial configuration 
conf <- matrix(1,1,17)
res <- tabuSearch(size = 17, iters = 10, 
                  objFunc = ObjFun, config = conf, listSize = 9, nRestarts = 1)

# Maximum value of objective function
max.obj <- max(res$eUtilityKeep)

# The best combination of the binary vector
best.comb <- which.max(res$eUtilityKeep)%>% res$configKeep[., ]

# The best set of predictors
best.subset <- subset[best.comb != 0]


system.time(res <- tabuSearch(size = 17, iters = 10, 
                              objFunc = ObjFun, config = conf, listSize = 9, nRestarts = 1))
max.obj
best.subset
summary(res)
# Highest value of objective fn    = 0.80426

system.time(res <- tabuSearch(size = 17, iters = 100, 
                              objFunc = ObjFun, config = conf, listSize = 9, nRestarts = 1))
max.obj
best.subset
summary(res)
# Tabu Settings
# Type                                       = binary configuration
# No of algorithm repeats                    = 1
# No of iterations at each prelim search     = 100
# Total no of iterations                     = 300
# No of unique best configurations           = 266
# Tabu list size                             = 9
# Configuration length                       = 17
# No of neighbours visited at each iteration = 17
# Results:
#   Highest value of objective fn    = 0.81237
# Occurs # of times                = 1
# Optimum number of variables      = 11
# > best.subset
# [1] "ADX"    "oscDX"  "ar"     "atr"    "cci"    "chv"    "cmo"    "sign"   "vsig"   "rsi"   
# [11] "signal" "oscKST"

# This is not the only algorithm and package that helps to select the best set of predictors 
# using GA. You can use the kofnGA, fSelector packages. Apart from those, a selection of 
# predictors is implemented by gafs() function in the "caret" package using GA.


####################################################################################
#
#################    6.2. Searching for the best parameters   ######################
#                    rgenoud
####################################################################################

genoud(fn, nvars, max = FALSE, pop.size = 1000, 
       max.generations = 100, wait.generations = 10,
       hard.generation.limit = TRUE, starting.values = NULL, MemoryMatrix = TRUE, 
       Domains = NULL, default.domains = 10, solution.tolerance = 0.001,
       gr = NULL, boundary.enforcement = 0, lexical = FALSE, gradient.check = TRUE,
       BFGS = TRUE, data.type.int = FALSE, hessian = FALSE, 
       unif.seed = 812821, int.seed = 53058,
       print.level = 2, share.type = 0, instance.number = 0, 
       output.path = "stdout", output.append = FALSE, project.path = NULL,
       P1 = 50, P2 = 50, P3 = 50, P4 = 50, P5 = 50, P6 = 50, P7 = 50, P8 = 50, 
       P9 = 0, P9mix = NULL, BFGSburnin = 0, BFGSfn = NULL, BFGShelp = NULL,
       control = list(), 
       optim.method = ifelse(boundary.enforcement < 2, "BFGS", "L-BFGS-B"), 
       transform = FALSE, debug = FALSE, cluster = FALSE, balance = FALSE, ...)

# fitness function-------------------------
fitness <- function(param, test = FALSE){
  require(TTR)
  require(magrittr)
  # define variables
  x <- pr[param[1]]
  nFast <- param[2]
  nSlow <- param[3]
  nSig <- param[4]
  macdType <- MaType[param[5]]
  sigType <- MaType[param[6]]
  percent <- per[param[7]]
  len <- param[9]*100 
  # linear restriction for macd
  if (nSlow <= nFast) return(-Inf)
  # calculate macd
  md <- MACD(x = x, nFast = nFast, nSlow = nSlow,
             nSig = nSig, percent = TRUE,
             maType = list(list(macdType), 
                           list(macdType),
                           list(sigType)))
  # calculate signals and shift to the right by 1 bar
  sig <- signal(md, param[8]) %>% Lag()
  #calculate balance on history with len length
  bal <- cumsum(tail(sig, len) * tail(price[ ,'CO'], len))
  if(test) {
            bal <<- cumsum(tail(sig, len) * tail(price[ ,'CO'], len))
           }
  # calculate quality ratio (round to integer)
  K <- ((tail(bal,1)/length(bal))* 10 ^ Dig) %>% floor()
  # return the obtained optimization criterion
  return(unname(K))
}

# Below is a listing of calculating all variables and functions
require(Hmisc)
# Types of the average = 4 -------------------------------------------
MaType <- Cs(SMA, EMA, DEMA, ZLEMA)
require(dplyr)
# Types of prices = 4 -----------------------------------------------
pr <- transmute(as.data.frame(price), Close = Close, Med = Med, 
                Typ = (High + Low + Close)/3,
                WClose = (High + Low + 2*Close)/4)
# how to calculate?
per <- c(TRUE, FALSE)
# Types of signals = 3 --------------------------
signal <- function(x, type){
  x <- na.omit(x)
  dx <- diff(x[ ,1]) %>% na.omit()
  x <- tail(x, length(dx))
  switch(type,
         (x[ ,1] - x[ ,2]) %>% sign(),
         sign(dx),
         ifelse(sign(dx) == 1 & sign(x[ ,1]) == 1, 1,
                ifelse(sign(dx) == -1 & sign(x[ ,1]) == -1,-1, 0))
  )
}
# initial configuration--------------------------- 
par <- c(2, 12, 26, 9, 2, 1, 1, 3, 5)
# search area--------------------------------------
dom <- matrix(c(1, 4,   # for types of prices
                8, 21,  # for fast МА period
                13, 54, # for slow МА period
                3, 13,  # for signal MA period
                1, 4,   # МА type for fast and slow
                1, 4,   # MA type for signal
                1, 2,   # percent type
                1, 3,   # signal option
                3,10),  # history length [300:1000]
              ncol = 2, byrow = TRUE)
# create cluster from available processing cores
puskCluster<-function(){
  library(doParallel)
  library(foreach)
  cores<-detectCores()
  cl<-makePSOCKcluster(cores)
  registerDoParallel(cl)
  #clusterSetRNGStream(cl)
  return(cl)
}

# Define quality ratio with initial (usually by default) parameters
K <- fitness(par, test = TRUE)
K
# [1] -1
plot(bal, t="l")


# In order to compare calculation speed, we will perform optimization on one core and 
# on the cluster out of two processing cores.
library(rgenoud)
pr.max <- genoud(fitness, nvars = 9, max = TRUE, 
                 pop.size = 500, max.generation = 300,
                 wait.generation = 50, 
                 hard.generation.limit = FALSE,
                 starting.values = par, Domains = dom, 
                 boundary.enforcement = 1,
                 data.type.int = TRUE,
                 solution.tolerance = 0.01,
                 cluster = FALSE,
                 print.level = 2)
# Solution Fitness Value: 4.000000e+00
# 
# Parameters at the Solution:
#   
#   X[ 1] :	2.000000e+00
# X[ 2] :	1.800000e+01
# X[ 3] :	2.700000e+01
# X[ 4] :	5.000000e+00
# X[ 5] :	1.000000e+00
# X[ 6] :	4.000000e+00
# X[ 7] :	2.000000e+00
# X[ 8] :	1.000000e+00
# X[ 9] :	9.000000e+00
# 
# Solution Found Generation 6
# Number of Generations Run 57
# Optimal parameters
pr.max$par
# price type pr[ ,2]= Med
# nFast = 18
# nSlow = 27
# nSig = 5
# macdType = SMA
# sigType = ZLEMA
# percent = FALSE
# signal = intersection of macd and signal lines
# history length = 900 bars.

# Let's see how the balance line with optimal parameters appears. For this purpose we will
# perform a fitness function with these parameters and with the test = TRUE option.
K.opt <- fitness(pr.max$par, test = TRUE)
K.opt
# [1] 4
plot(bal, t="l")

# We will calculate the same on the cluster that contains two cores
# start the cluster
cl <- puskCluster()
# maximize fitness function
# send necessary variables and functions to every core in the cluster
clusterExport(cl, list("price", "pr", "MaType", "par", "dom", "signal", 
                       "fitness", "Lag", "Dig", "per" ) )
pr.max <- genoud(fitness, nvars = 9, max = TRUE, 
                 pop.size = 500, max.generation = 300,
                 wait.generation = 50, 
                 hard.generation.limit = FALSE,
                 starting.values = par, Domains = dom, 
                 boundary.enforcement = 1,
                 data.type.int = TRUE,
                 solution.tolerance = 0.01,
                 cluster = cl,
                 print.level = 2) # only for experiments. To set in 0 in EA
# menos de metade do tempo
# Solution Fitness Value: 5.000000e+00
# 
# Parameters at the Solution:
#   
#   X[ 1] :	3.000000e+00
# X[ 2] :	1.900000e+01
# X[ 3] :	4.300000e+01
# X[ 4] :	3.000000e+00
# X[ 5] :	1.000000e+00
# X[ 6] :	4.000000e+00
# X[ 7] :	1.000000e+00
# X[ 8] :	1.000000e+00
# X[ 9] :	3.000000e+00
# 
# Solution Found Generation 18
# Number of Generations Run 69
# 
# Mon Oct 31 18:03:39 2016
# Total run time : 0 hours 1 minutes and 29 seconds
# stop the cluster
stopCluster(cl)
K.opt <- fitness(pr.max$par, test = TRUE)
K.opt
# [1] 5 ->melhorou o resultado
plot(bal, t="l")

pr.max <- genoud(fitness, nvars = 9, max = TRUE, 
                 pop.size = 500, max.generation = 100, # "play around" with parameters
                 wait.generation = 10, 
                 hard.generation.limit = TRUE,
                 starting.values = par, Domains = dom, 
                 boundary.enforcement = 0,
                 data.type.int = TRUE,
                 solution.tolerance = 0.01,
                 cluster = FALSE,
                 print.level = 2)
# Solution Fitness Value: 5.000000e+00
# 
# Parameters at the Solution:
#   
#   X[ 1] :	1.000000e+00
# X[ 2] :	1.900000e+01
# X[ 3] :	5.400000e+01
# X[ 4] :	3.000000e+00
# X[ 5] :	1.000000e+00
# X[ 6] :	4.000000e+00
# X[ 7] :	1.000000e+00
# X[ 8] :	1.000000e+00
# X[ 9] :	3.000000e+00
# 
# Solution Found Generation 3
# Number of Generations Run 14
# 
# Mon Oct 31 18:10:06 2016
# Total run time : 0 hours 1 minutes and 22 seconds
K.opt <- fitness(pr.max$par, test = TRUE)
K.opt
# [1] 5 ->melhorou o resultado
plot(bal, t="l")


####################################################################################
#
#################    6.2. Searching for the best parameters   ######################
#                    SOMA(Self-Organising Migrating Algorithm) 
####################################################################################

# Let's conduct few experiments to compare results of genetic algorithms with evolutionary
# algorithms. First, we will test SOMA(Self-Organising Migrating Algorithm) implemented 
# in the "soma" package
require(soma)
xsoma <- soma(fitness, bounds = list(min = c(1,8,13,3,1,1,1,1,3), 
                                    max = c(4,21,54,13,4,4,2,3,10)), 
          options = list(minAbsoluteSep = 3,
                         minRelativeSep = -1,
                         nMigrations = 20,
                         populationSize = 20)
          # ,opp = TRUE
          )

xsoma$population[ ,1] %>% floor
# [1]  2 14 14 11  3  2  1  1  6


####################################################################################
#
#################    6.2. Searching for the best parameters   ######################
#                    Generalized Simulated Annealing Function 
####################################################################################

require(GenSA)
pr.max_gensa <- GenSA(par, fitness, lower = c(1,8,13,3,1,1,1,1,3),
                upper = c(4,21,54,13,4,4,2,3,10), 
                control = list(verbose = TRUE, simple.function = TRUE, 
                               max.time = 60)
                # , opp = TRUE
                )
pr.max_gensa$value * (-1)
# [1] 6
par1 <- pr.max_gensa$par 
par1 %>% floor
# [1]  1  8 50 12  1  2  1  1  3

# Calculate value of the fitness function with these parameters and see the balance line:
f1 <- fitness(par1, test = TRUE)
plot(-1 * bal, t="l")

# These and many similar algorithms (packages dfoptim, nlopt,CEoptim, DEoptim,RcppDE etc.) 
# optimize the function by one criterion. For multiple criteria optimization, the mco package 
# is intended.




############################################################
## Using Genetic Algorithms in Quantitative Trading   
## https://www.r-bloggers.com/using-genetic-algorithms-in-quantitative-trading/
## https://gist.github.com/thertrader/9509411
## thertrader@gmail.com - Mar 2014
############################################################
library(PerformanceAnalytics)
library(rgenoud)
library(quantmod)
library(TTR)


###############
outputPath <- "your_path"
theInstrument <- "SPY"
remove(data)
data <- getSymbols(Symbols = theInstrument, 
                   src = "yahoo", 
                   from = "2000-01-01", 
                   auto.assign = FALSE)
head(data)
colnames(data) <- c("open","high","low","close","volume","adj.")


###############
fitnessFunction <- function(xx=c(1,1,1,1)){
  print(xx)
  rtn <- ROC(data[,"close"],n=1)
  rsi <- RSI(data[,"close"],n=xx[1],maType="SMA")
  smas <- SMA(data[,"close"],n=xx[3])
  smal <- SMA(data[,"close"],n=xx[4])
  aa <- cbind(data[,"close"],rtn,rsi,smas,smal)
  colnames(aa) <- c("close","rtn","rsi","smas","smal")
  
  isData <- aa[index(aa) < "2011-01-01"]
  
  posBuySignal <- which(isData[,"rsi"] <= (1 - xx[2]) & isData[,"smas"] > isData[,"smal"]) + 1
  if (length(posBuySignal) == 0)
    posBuySignal <- NULL
  posSellSignal <- which(isData[,"rsi"] > xx[2] & isData[,"smas"] < isData[,"smal"]) + 1
  if (length(posSellSignal) == 0)
    posSellSignal <- NULL
  allSignals <- c(posBuySignal,posSellSignal)
  allSignals <- allSignals[which(allSignals <= nrow(isData))]
  
  if (!is.null(allSignals) && length(allSignals) >= 50)
    theStat <- SharpeRatio.annualized(isData[sort(allSignals),"rtn"])
  if (is.null(allSignals) | length(allSignals) < 50)
    theStat <- 0  
  
  return(theStat)
}


###############
tradingStatistics <- function(isOrOos = TRUE, xx = c(1,1,1,1)){
  print(xx)
  rtn <- ROC(data[,"close"],n=1)
  rsi <- RSI(data[,"close"],n=xx[1],maType="SMA")
  smas <- SMA(data[,"close"],n=xx[3])
  smal <- SMA(data[,"close"],n=xx[4])
  aa <- cbind(data[,"close"],rtn,rsi,smas,smal)
  colnames(aa) <- c("close","rtn","rsi","smas","smal")
  
  if (isOrOos == TRUE)
    sampleData <- aa[index(aa) < "2011-01-01"]
  if (isOrOos == FALSE)
    sampleData <- aa[index(aa) >= "2011-01-01"]
  
  posBuySignal <- which(sampleData[,"rsi"] <= (1 - xx[2]) & sampleData[,"smas"] > sampleData[,"smal"]) + 1
  if (length(posBuySignal) == 0)
    posBuySignal <- NULL
  posSellSignal <- which(sampleData[,"rsi"] > xx[2] & sampleData[,"smas"] < sampleData[,"smal"]) + 1
  if (length(posSellSignal) == 0)
    posSellSignal <- NULL
  allSignals <- c(posBuySignal,posSellSignal)
  allSignals <- allSignals[which(allSignals <= nrow(sampleData))]
  
  totalRtn <- sum(sampleData[sort(allSignals),"rtn"])
  numberOfTrades <- length(sampleData[sort(allSignals),"rtn"])
  hitRatio <- length(which(sampleData[sort(allSignals),"rtn"] > 0))/numberOfTrades
  
  return(list(totalRtn=totalRtn,numberOfTrades=numberOfTrades,hitRatio=hitRatio))
} 


###########
optimum <- genoud(fitnessFunction, 
                  nvars = 4,  
                  max = TRUE,  
                  pop.size = 30, 
                  max.generations = 50, 
                  wait.generations = 15,
                  hard.generation.limit = TRUE, 
                  starting.values = c(5,70,30,100), 
                  MemoryMatrix = TRUE,
                  Domains = matrix(c(5,50,10,50,
                                     50,90,50,200),
                                   nrow=4,ncol=2), 
                  default.domains = 4, 
                  solution.tolerance = 0.00001,
                  gr = NULL, 
                  boundary.enforcement = 2, 
                  lexical = FALSE, 
                  gradient.check = FALSE, 
                  data.type.int = TRUE,  
                  hessian = FALSE, 
                  unif.seed = 812821, 
                  int.seed = 53058,
                  print.level = 2, 
                  share.type = 0, 
                  instance.number = 0,
                  output.path = paste(outputPath,theInstrument,"_Summary.txt",sep=""), 
                  output.append = FALSE,  
                  project.path = paste(outputPath,theInstrument,"_Details.txt",sep=""), 
                  P1=2, P2=8, P3=8, P4=6, P5=6, P6=6, P7=8, P8=6, P9=0,
                  P9mix = NULL, 
                  BFGSburnin = 0, 
                  BFGSfn = NULL, 
                  BFGShelp = NULL,
                  cluster = FALSE, 
                  balance = FALSE, 
                  debug = FALSE, 
                  control = list())

solution <- optimum$par
