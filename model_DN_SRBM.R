
library(magrittr)
library(dplyr)
library(markovchain)
library(TTR)
library(caret)
library(rminer)
library(pracma)
library(darch)
library(randomUniformForest)
library(deepnet)
library(Hmisc)
library(mhsmm)
library(fTrading)

getwd()

#---2-Input variables (signs, predictors)--------------------------------------------
In <- function(p = 16){
  require(TTR)
  require(dplyr)
  require(magrittr)
  adx <- ADX(price, n = p) %>% as.data.frame %>% 
    mutate(.,oscDX = DIp - DIn) %>% 
    transmute(.,DX, ADX, oscDX) %>% 
    as.matrix()
  ar <- aroon(price[ ,c('High', 'Low')], n = p) %>% 
    extract(,3)
  atr <- ATR(price, n = p, maType = "EMA") %>%
    extract(,1:2)
  cci <- CCI(price[ ,2:4], n = p)
  chv <- chaikinVolatility(price[ ,2:4], n = p)
  cmo <- CMO(price[ ,'Med'], n = p)
  macd <- MACD(price[ ,'Med'], 12, 26, 9) %>% 
    as.data.frame() %>% 
    mutate(., vsig = signal %>% 
             diff %>% c(NA,.) %>% multiply_by(10)) %>% 
    transmute(., sign = signal, vsig) %>% 
    as.matrix()
  rsi <- RSI(price[ ,'Med'], n = p)
  stoh <- stoch(price[ ,2:4], nFastK = p, 
                nFastD =3, nSlowD = 3, 
                maType = "EMA") %>%
    as.data.frame() %>% 
    mutate(., oscK = fastK - fastD) %>%
    transmute(.,slowD, oscK) %>% 
    as.matrix()
  smi <- SMI(price[ ,2:4],n = p, nFast = 2, 
             nSlow = 25, nSig = 9)
  kst <- KST(price[ ,4])%>% as.data.frame() %>% 
    mutate(., oscKST = kst - signal) %>%
    select(.,oscKST) %>% as.matrix()
  In <- cbind(adx, ar, atr, cci, chv, cmo, macd, 
              rsi, stoh, smi, kst)
  return(In)
}

# As a target variable we take signals obtained with ZZ. The function calculating a zigzag and a signal:
#----3-Output data (target variable)-----------------------------------------------  
ZZ <- function(pr = price, ch = ch , mode="m") {
  require(TTR)
  require(magrittr)
  if (ch > 1) ch <- ch/(10 ^ (Dig - 1))
  if (mode == "m") {pr <- pr[ ,'Med']}
  if (mode == "hl") {pr <- pr[ ,c("High", "Low")]}
  if (mode == "cl") {pr <- pr[ ,c("Close")]}
  zz <- ZigZag(pr, change = ch, percent = F, 
               retrace = F, lastExtreme = T)
  n <- 1:length(zz)
  dz <- zz %>% diff %>% c(., NA)
  sig <- sign(dz)
  for (i in n) { if (is.na(zz[i])) zz[i] = zz[i - 1]}
  return(cbind(zz, sig))
}

price1 <- format(price, decimal.mark=",")
write.table(price1, file ="price.csv",row.names=TRUE,sep=";")
out <- ZZ(ch = 10, mode = "hl")
table(out[,2])
out <- format(out, decimal.mark=",")
write.csv(out,"out_10.csv")

out <- ZZ(ch=10, mode = "hl") 
out1 <- ZZ(ch=10, mode = "m") 
matplot(tail(cbind(out[,1],out1[,1]),500),t="l")
out2 <- ZZ(ch=20)
out3 <- ZZ(ch=40)
matplot(tail(cbind(out1[,1],out2[,1],out3[,1]),500),t="l")

# function that will create the initial data frame, clean it from uncertain data (NA) and convert the target variable to the factor with two classes "-1" and "+1". 
# This function combines previously written functions In() and ZZ(). 
# We will instantly crop the last 500 bars that will be used to evaluate the quality of the model's prediction
#-----4---------------------------------
form.data <- function(n = 16, z = 10, len = 0){
  require(magrittr)
  x <- In(p = n)
  out <- ZZ(ch = z, mode = "m")
  data <- cbind(x, y = out[ ,2]) %>% 
    as.data.frame %>% head(., (nrow(x)-len))%>%
    na.omit
  data$y <- as.factor(data$y)
  return(data)
}

# We will delete variables with a correlation coefficient above 0.9 from our initial set. We will write a function that will form the initial data frame, remove highly correlated variables and return clean data.
# We can check in advance which variables have a correlation above 0.9
data <- form.data(n = 16, z = 10) # prepare data frame
descCor <- cor(data[ ,-ncol(data)])# remove a target variable
summary(descCor[upper.tri(descCor)])
highCor <- caret::findCorrelation(descCor, cutoff = 0.9)
highCor
# [1] 12  9 15 10
colnames(data[ ,highCor])
#  the above listed variables are subject to removal
data.f <- data[ ,-highCor]
colnames(data.f)

# We will write it compactly in one function
#---5-----------------------------------------------
cleaning <- function(n = 16, z = 10, cut = 0.9){
  data <- form.data(n, z)
  descCor <- cor(data[ ,-ncol(data)])
  highCor <- caret::findCorrelation(descCor, cutoff = cut)
  data.f <- data[ ,-highCor]
  return(data.f)
}

data.f <- cleaning()

# Not all authors of packages and researchers agree that highly correlated data should be removed from the sets. However, results using both options should be compared here. In our case, we will select the option with deleting. 



#-----6-Selection of the most important variables-----------------------------------------------
# with best variables in contribution and interaction;
# with best variables for the class "-1";
# with best variables for the class "+1".
prepareBest1 <- function(n, z, cut, method){
  require(randomUniformForest) 
  require(magrittr)
  data.f <<- cleaning(n = n, z = z, cut = cut)
  idx <- rminer::holdout(y = data.f$y)
  prep <- caret::preProcess(x = data.f[idx$tr, -ncol(data.f)], method = method)
  x.train <- predict(prep, data.f[idx$tr, -ncol(data.f)])
  x.test <- predict(prep, data.f[idx$ts, -ncol(data.f)])
  y.train <- data.f[idx$tr, ncol(data.f)]
  y.test <- data.f[idx$ts, ncol(data.f)]
  #---------
  ruf <- randomUniformForest( X = x.train, Y = y.train,
                              xtest = x.test, ytest = y.test,
                              mtry = 1, ntree = 300,
                              threads = 2, nodesize = 1
                            )
  imp.ruf <- importance(ruf, Xtest = x.test)
  best <- imp.ruf$localVariableImportance$classVariableImportance %>% 
    head(., 10) %>% rownames()
  #-----partImport
  best.sell <- partialImportance(X = x.test, 
                                 imp.ruf, 
                                 whichClass = "-1",
                                 nLocalFeatures = 7) %>% 
    row.names() %>% 
    as.numeric() %>% 
    colnames(x.test)[.]
  best.buy <- partialImportance(X = x.test,
                                imp.ruf,
                                whichClass = "1",
                                nLocalFeatures = 7) %>% 
    row.names() %>% 
    as.numeric() %>% 
    colnames(x.test)[.]
  dt <- list(best = best, buy = best.buy, sell = best.sell)
  return(dt)
}

dt1 <- prepareBest1(16, 10, 0.9, c("center", "scale","spatialSign"))

# con la función del robot
n = 16
z = 10
cut = 0.9
norm = T
method = c("center", "scale","spatialSign")
best <- prepareBest(n = n, z = z, cut = cut, norm = T, meth = method)
dt <- prepareBest(16, 10, 0.9, T, c("center", "scale","spatialSign"))

#---7-Balancing classes and pre-processing---------------------------------------------------
prepareTrain <- function(x , y, 
                         rati, mod = "stratified", 
                         balance = F, 
                         norm, meth)
{
  require(magrittr)
  require(dplyr)
  t <- rminer::holdout(y = y, ratio = rati,
                       mode = mod)
  train <- cbind(x[t$tr, ], y = y[t$tr])
  if(balance){
    train <- caret::upSample(x = train[ ,best], 
                             y = train$y, 
                             list = F)%>% tbl_df
    train <- cbind(train[ ,best], select(train, y = Class))
  }
  test <- cbind(x[t$ts, ], y = y[t$ts])
  if (norm) {
    prepr <<- caret::preProcess(train[ ,best], method = meth)
    train = predict(prepr, train[ ,best])%>% cbind(., y = train$y)
    test =  predict(prepr, test[ ,best] %>% cbind(., y = test$y))
  }
  DT <- list(train = train,
             test = test)
  return(DT)
}


# We should set in advance the distribution of hidden neurons in layers in a form of vector (for example):
L<- c( 14, 50, 50, 2)
# Number of neurons in the input layer equals the number of input variables. Two hidden layers will contain 50 neurons each, the output layer will have two.

# Training the model
#----8-Pre-training-------------------------------------------------------------
pretrainDBN <- function(L, Bs, dS, nE, nCD, InM = 0.5, FinM = 0.9)
{
  require(darch)
  # create object DArch
  dbn <- newDArch(layers = L, batchSize = Bs, logLevel = 5)
  # set initial moment 
  setInitialMomentum(dbn) <- InM 
  # set final moment   
  setFinalMomentum(dbn) <- FinM
  # set time of switching moments from initial to final    
  setMomentumSwitch(dbn) <- round(0.8 * nE)
  dbn <- preTrainDArch(dbn, 
                       dataSet = dS, 
                       numEpoch = nE,
                       numCD = nCD, 
                       trainOutputLayer = T)
  return(dbn)
}


#-----9-Fine-tuning----------------------------------------
fineMod <- function(variant=1, dbnin, dS, 
                    hd = 0.5, id = 0.2,
                    act = c(2,1), nE = 10)
{
  setDropoutOneMaskPerEpoch(dbnin) <- FALSE
  setDropoutHiddenLayers(dbnin) <- hd
  setDropoutInputLayer(dbnin) <- id
  layers <<- getLayers(dbnin)
  stopifnot(length(layers)==length(act))
  if(variant < 0 || variant >2) {variant = 1}
  for(i in 1:length(layers)){
    fun <- actFun %>% extract2(act[i])
    layers[[i]][[2]] <- fun
  }
  setLayers(dbnin) <- layers
  if(variant == 1 || variant == 2){ # backpropagation
    if(variant == 2){# rpropagation
      #setDropoutHiddenLayers(dbnin) <- 0.0
      setFineTuneFunction(dbnin) <- rpropagation
    }
    mod = fineTuneDArch(darch = dbnin, 
                        dataSet = dS, 
                        numEpochs = nE,
                        bootstrap = T)
    return(mod)
  }
}

# To calculate Accuracy we need values of the target variable, and the ZigZag, as we remember from before, most frequently is not defined on the last bars. Therefore, the testing sample for calculating Accuracy we will determine with the prepareTrain() function, and for qualitative indicators we will use the following function
#---10-Testing the model. Мetrics.------------------------------------------
prepareTest <- function(n, z, norm, len = 501)
{
  x <- In(p = n ) %>% na.omit %>% extract( ,best) %>% 
    tail(., len)
  CO <- price[ ,"CO"] %>% tail(., len)
  if (norm) {
    x <- predict(prepr,x)
  }
  dt <- cbind(x = x, CO = CO) %>% as.data.frame()
  return(dt)
}

# The models will be tested on the last 500 bars of the history.
# The first function returns Acc and the target variable values (real or predicted) for a possible further analysis. 
#---11-----
testAcc <- function(obj, typ = "bin"){
  x.ts <- DT$test[ ,best] %>% as.matrix()
  y.ts <- DT$test$y %>% as.integer() %>% subtract(1)
  out <- predict(obj, newdata = x.ts, type = typ) 
  if (soft){out <- max.col(out)-1} else {out %<>% as.vector()}               
  acc <- length(y.ts[y.ts == out])/length(y.ts) %>% 
    round(., digits = 4)
  return(list(Acc = acc, y.ts = y.ts, y = out))
}

# The second function returns the predicted signals sig for the EA, the balance obtained based on these signals (bal), quality coefficient (К), maximum value of this coefficient on the tested area (Kmax) and the maximum drawdown (dd) in the same area.
#---12-----
testBal <- function(obj, typ = "bin") {
  require(fTrading)
  x <- DT.test[ ,best]
  CO <- DT.test$CO
  out <- predict(obj, newdata = x, type = typ) 
  if(soft){out <- max.col(out)-1} else {out %<>% as.vector()} 
  sig <- ifelse(out == 0, -1, 1)
  sig1 <- Hmisc::Lag(sig) %>% na.omit
  bal <- cumsum(sig1 * tail(CO, length(sig1)))
  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
  dd <- maxDrawDown(bal)
  return(list(sig = sig, bal = bal, K = K, 
              Kmax = Kmax, dd = dd))
}

# Decoding predictions
# The obtained result can be decoded (converted from matrix to vector) using the "WTA" method. The class equals the column number with a maximum value of probability, and the value threshold of this probability can be set, below which the class is not determined.
# if the threshold is set as 0.5, and the biggest probability in the columns is below this threshold, we will obtain an additional class ("not defined"). It should be taken into consideration when calculating metrics like Accuracy.
out <- classmat2classvec(out, threshold = 0.5) 
or
out <- max.col(out)-1


# Improving the prediction results
# Calibration
CORElearn::calibrate(correctClass, predictedProb, class1 = 1,  
                     method = c("isoReg", "binIsoReg", "binning", "mdlMerge"),  
                     weight=NULL, noBins=10, assumeProbabilities=FALSE)

# Smoothing prediction results with the Markov chain model
# We will use the "mhsmm" package designed for calculating hidden Markov and semi-Markov models for these calculations. We will use the smooth.discrete() function, that simply smooths the time series of discrete values.
obj <- smooth.discrete(y)
sm.y <- predict(obj, x = new.y)


# Metrics 
# To evaluate the quality of model prediction, the whole range of metrics (Accuracy, AUC, ROC and other) is applied. In the previous article I have mentioned that formal metrics can't define quality in our case. The Expert Advisor's goal is to get the maximum profit with an acceptable drawdown.
# For this purpose, K quality indicator was introduced, and it shows average profit in points for one bar on the fixed history segment with N length. It is calculated through dividing the cumulative Return(sig, N) by the length of the N segment. Accuracy will be calculated only indicatively.




# ########################################################################################
# 
# 
#                   perform calculations and obtain testing results
# 
# 
##########################################################################################


# Find  constants  
n = 34; z = 37; cut = 0.9; soft = TRUE 

# Find preprocessing method  
method = c("center", "scale","spatialSign")

# form the initial set of data
data.f <- form.data(n = n, z = z)

# find the set of important predictors 
# best <- prepareBest(n = n, z = z, cut = cut, method) 
# Calculations take about 3 minutes on the 2-core processor. You can skip this stage if you like, 
# and use the whole set of predictors in the future. Therefore, comment the previous line and
# uncomment two lowest lines.
data.f <- form.data(n = n, z = z)
best <- colnames(data.f) %>% head(., ncol(data.f) - 1)

# Prepare the set for training neural network
DT <- prepareTrain(x = data.f[ , best],
                   y = data.f$y,
                   rati = 501,
                   mod = "stratified",
                   balance = TRUE,
                   norm = TRUE, 
                   meth = method) 

# Download required libraries
require(darch)
require(foreach)

# Identify available functions for activation
actFun <- list(sig = sigmoidUnitDerivative,
               tnh = tanSigmoidUnitDerivative,
               lin = linearUnitDerivative,
               soft = softmaxUnitDerivative)

# Convert the target variable
if (soft) { y <- DT$train$y %>% classvec2classmat()} # into matrix
if (!soft) {y = DT$train$y %>% as.integer() %>% subtract(1)} # to vector with values [0, 1]

# create dataSet for training
dataSet <- createDataSet(
  data = DT$train[ ,best] %>% as.matrix(), 
  targets = y ,
  scale = F
)

# Identify constants for neural network
# Number of neurones in the input layer (equals the amount of predictors)
nIn <- ncol(dataSet@data)

# Number of neurones in the output layer 
nOut <- ncol(dataSet@targets)

# Vector with a number of neurones in every layer of neural network
# If you use another structure of neural network, this vector should be rewritten
Layers = c(nIn, 2 * nIn , nOut)

# Other data related to training
Bath = 50
nEp = 100
ncd = 1

# Pre-training of neural network
preMod <- pretrainDBN(Layers, Bath, dataSet, nEp, ncd)

# Additional parameters for fine-tune
Hid = 0.5; Ind = 0.2; nEp = 10

# Train two models, one with backpropagation, other with rpropagation
# We only do this to compare results
model <- foreach(i = 1:2, .packages = "darch") %do% {
  dbn <- preMod
  if (!soft) {act = c(2, 1)}
  if (soft) {act = c(2, 4)}
  fineMod(variant = i, 
          dbnin = dbn, 
          hd = Hid, 
          id = Ind,
          dS = dataSet, 
          act = act, 
          nE = nEp)
}

# Test to get Accuracy
resAcc <- foreach(i = 1:2, .packages = "darch") %do% {
  testAcc(model[[i]]) 
}

# Prepare sample of data to test on quality coefficient
DT.test <- prepareTest(n = n, z = z, T)

# Test
resBal <- foreach(i = 1:2, .packages = "darch") %do% {
  testBal(model[[i]]) 
}

# Let's see the result:
resAcc[[1]]$Acc
# 0.6926148
resAcc[[2]]$Acc
# 0.6926148

# As for the quality coefficient:
resBal[[1]]$K
# 1.9
resBal[[2]]$K
# 1.9
resBal[[1]]$Kmax
# 2.607477
resBal[[2]]$Kmax
# 2.607477
resBal[[1]]$dd$maxdrawdown
# 0.00415
resBal[[2]]$dd$maxdrawdown
# 0.00415

# We will try to correct the drawdown with a correction signal obtained from the below calculation:
bal <- resBal[[1]]$bal

# signal on the last 500 bars
sig <- resBal[[1]]$sig[1:500]

# average from the balance line
ma <- pracma::movavg(bal,16, "t")

# momentum from the average 
roc <- TTR::momentum(ma, 3) %>% na.omit

# balance line deviation from the average
dbal <- (bal - ma) %>% tail(., length(roc))

# summarize two vectors
dbr <- (roc + dbal) %>% as.matrix()

# calculate correction signal
sig.cor <- ifelse(dbr > 0, 1, -1) # sign(dbr) gives the same result

# resulting signal
S <- sig.cor * tail(sig, length(sig.cor))

# balance on resulting signal
Bal <- cumsum(S * (price[ ,"CO"]%>% tail(.,length(S))))

# quality coefficient on the corrected signal
Kk <- tail(Bal, 1)/length(Bal) * 10 ^ Dig
Kk
# 9.235412 pipettes

matplot(cbind(dbr, dbal, roc), t="l", col=c(1,2,4), lwd=c(2,1,1))
abline(h=0, col=2)
grid()

plot(c(NA,NA,NA,Bal), t="l")
lines(bal, col= 2)
lines(ma, col= 4)

# the "signal" in the article is a sequence of integer -1 and 1. The "state" is a sequence of integers 1 and 2 corresponding to these signals. For mutual conversions we will use the functions:
#---13----------------------------------
sig2stat <- function(x) {x %>% as.factor %>% as.numeric}
stat2sig <- function(x) ifelse(x==1, -1, 1)

#----14--correct-----------------------------------
correct <- function(sig){
  sig <- Hmisc::Lag(sig) %>% na.omit
  bal <- cumsum(sig * (price[ ,6] %>% tail(.,length(sig))))
  ma <- pracma::movavg(bal, 16, "t")
  roc <- TTR::momentum(ma, 3)%>% na.omit
  dbal <- (bal - ma) %>% tail(., length(roc))
  dbr <- (roc + dbal) %>% as.matrix()
  sig.cor <- sign(dbr)
  S <- sig.cor * tail(sig, length(sig.cor))
  bal <- cumsum(S * (price[ ,6]%>% tail(.,length(S))))
  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
  dd <- fTrading::maxDrawDown(bal)
  corr <<- list(sig.c = sig.cor, sig.res = S, bal = bal, Kmax = Kmax, K = K, dd = dd)
  return(corr)
}

sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", "b", "a", "a", 
              "b", "b", "b", "a")        
sequenceMatr <- createSequenceMatrix(sequence, sanitize = FALSE)
mcFitMLE <- markovchainFit(data = sequence)
mcFitBSP <- markovchainFit(data = sequence, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")


# In order to obtain the signal vector with prediction of 1 bar ahead, we will use the "markovchain" package and the pred.sig() function.
#---15---markovchain----------------------------------
pred.sig <- function(sig, prev.bar = 10, nahead = 1){
  require(markovchain)
  # Transform the observed correction signals into states  
  stat <- sig2stat(sig)
  # Calculate model parameters
  # if there is no model in the environment       
  if(!exists('MCbsp')){
    MCbsp <<- markovchainFit(data = stat, 
                             method = "bootstrap",
                             nboot = 10L,
                             name="Bootstrap MС")
  }
  # Set necessary constants
  newData <- tail(stat, prev.bar)
  pr <- predict(object = MCbsp$estimate, 
                newdata = newData,
                n.ahead = nahead)
  # attach the predicted signal to input signal
  sig.pr <- c(sig, stat2sig(pr))
  return(sig.pr = sig.pr)
}

# Now, we will write down the resulting signal calculation for the Expert Advisor to perform compactly:
sig <- resBal[[1]]$sig
sig.cor <- correct(sig)
sig.c <- sig.cor$sig.c
# pr.sig.cor <- pred.sig(sig.c)
# sig.pr <- pr.sig.cor$sig.pr
sig.pr <- pred.sig(sig.c)
# Resulting vector of signals for Expert Advisor 
S <- sig.pr * tail(sig, length(sig.pr))

# We will write a function that will smooth the discrete signal using the model of the hidden Markov chain. 
# For this purpose, we will use the "mhsmm" package.
#---16---Smoothing the predicted signal------------------------------------
smoooth <- function(sig){
  # smooth predicted signal
  # define parameters of hidden Markov model
  # if there is no model in the environment yet
  require(mhsmm)
  if(!exists('obj.sm')){
    obj.sm <<- sig2stat(sig)%>% smooth.discrete()
  }
  # smooth the signal with the obtained model
  sig.s <- predict(obj.sm, x = sig2stat(sig))%>% 
    extract2(1)%>% stat2sig()
  # calculate balance with smoothed signal
  sig.s1 <- Hmisc::Lag(sig.s) %>% na.omit
  bal <- cumsum(sig.s1 * (price[ ,6]%>% tail(.,length(sig.s1))))
  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
  dd <- fTrading::maxDrawDown(bal)
  return(list(sig = sig.s, bal = bal, Kmax = Kmax, K = K, dd = dd))
}

# We will calculate and compare the balance based on predicted and smoothed signals.
sig <- resBal[[1]]$sig
sig.sm <- smoooth(sig)
plot(sig.sm$bal, t="l")
lines(resBal[[1]]$bal, col=2)

sig.sm$dd
# 0.00392
resBal[[1]]$dd$maxdrawdown
# 0.00415


# 4.2. Self-control and Self-training
# The quality control of predicting signals with a model is performed using the К coefficient.
# There are two ways to identify limits of the acceptable quality. First — to set the maximum 
# fall of the coefficient in relation to its maximum value. If К < Kmax * 0.8, then we should 
# re-train or stop the Expert Advisor from performing signals. Second — to set the minimum 
# value of К, that after being reached requires the same actions. We will use the second 
# method in the Expert Advisor.

