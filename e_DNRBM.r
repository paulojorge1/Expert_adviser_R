#v.1.0 01.03.16 
if(first)
{
#-- Load the library and functions
	packets <- c("magrittr", "dplyr", "markovchain", "TTR", "caret", "rminer", "pracma", 
				"darch", "randomUniformForest", "deepnet", "Hmisc", "mhsmm", "fTrading", "darch")
	for(i in 1:length(packets)) {
	  if (! (packets[i] %in% rownames(installed.packages()))) { 
		install.packages(packets[i]) }
	}
	library(magrittr)
	library(dplyr)
#------SetDir------------------
	source('C:/...../SetDir.r')
# Load functions --------------
	source('C:/..../hmisc.r')
	source('C:/...../e_DNRBM_FUN.r')
#======Calculate===============================================
#1----We form a matrix of quotations -------------------
	price <- pr.OHLC(Open, High, Low, Close);
	rm(list=c("Open","High","Low","Close"));
#  n = 34; z = 37; cut = 0.9; soft = TRUE
	cut = 0.9
	method = c("center", "spatialSign") 
# Form the data sets
	#best <- prepareBest(n = n, z = z, cut = cut, norm = T, method = method)
	data.f <- form.data(n = n, z = z)
	best <- colnames(data.f) %>% head(.,ncol(data.f)-1)
	DT <- prepareTrain(x = data.f[ ,best], 
                   y = data.f$y, 
                   balance = TRUE, 
                   rati = 501, mod = "stratified", 
                   norm = TRUE, meth = method)
# Prepare data for the neural network
	require(darch)
	actFun <- list(sig = sigmoidUnitDerivative,
               tnh = tanSigmoidUnitDerivative,
               lin = linearUnitDerivative,
               soft = softmaxUnitDerivative)
	if (soft) { y <- DT$train$y %>% classvec2classmat()
				act = c(2, 4)}
	if (!soft) {y <- DT$train$y %>% as.integer() %>% subtract(1)
				act = c(2, 1)}
	dataSet <- createDataSet(
		data = DT$train[ ,best] %>% as.matrix(), 
		targets = y,
		scale = F
	)
	nIn <- ncol(dataSet@data)
	nOut <- ncol(dataSet@targets)
	Layers = c(nIn, 2 * nIn , nOut)
	Bath = 50
	nEp = 20
	ncd = 3
# Pretraining DARCH ----------------------------------------
	preMod <- pretrainDBN(Layers, Bath, dataSet, nEp, ncd)
# The constants for fine-tuning
	Hid = 0.5; Ind = 0.2; nEp = 5
# Fine tuning DARCH -------------------	
	model <- fineMod(1, dbnin = preMod, 
					hd = Hid, id = Ind,
					dS = dataSet, act = act, nE = nEp)
# Testing --------------------------  
	resAcc <- testAcc(model) 
	DT.test <- prepareTest(n = n, z = z, T)
	resBal <-  testBal(model) 
# adjust the signal on the balance line
	sig <- resBal$sig
	sig.pr <- correct(sig)%>% extract2('sig.c')%>% pred.sig()
# The resulting vector signal for the expert 
	S <- sig.pr * tail(sig, length(sig.pr))
# quality index
	Kmax <- corr$Kmax %>% round()
	K <- corr$K %>% round()
	maxDD <- (corr$dd$maxdrawdown*(10 ^ Dig))%>% round()
# Quality control
    if(K >= Kmin){first = FALSE}
	rm(list = c('data.f', 'DT', 'DT.test', 'preMod'))
#---Write image
	save.image(file = fS)
}else{
	price <- pr.OHLC(Open, High, Low, Close);
	rm(list=c("Open","High","Low","Close"));
	DT.test <- prepareTest(n, z, TRUE)
	resBal <- testBal(model)
	sig <- resBal$sig
	sig.pr <- correct(sig)%>% extract2('sig.c')%>% pred.sig()
# The resulting vector signal for the expert  
	S <- sig.pr * tail(sig, length(sig.pr))
	Kmax <- corr$Kmax %>% round()
	K <- corr$K %>% round()
	maxDD <- (corr$dd$maxdrawdown*(10 ^ Dig))%>% round()
	if(K < Kmin){first = TRUE}
#---Write image
	save.image(file = fS)
}
