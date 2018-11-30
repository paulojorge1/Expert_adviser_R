#v.1.0 01.03.16 
if(first)
{
	#-- Load the library and functions
	packets <- c("magrittr", "dplyr", "markovchain", "TTR", "caret", "rminer", "pracma", 
						"randomUniformForest", "deepnet", "Hmisc", "mhsmm", "fTrading", "darch")
	for(i in 1:length(packets)) {
	  if (! (packets[i] %in% rownames(installed.packages()))) { 
		install.packages(packets[i]) }
	}
	library(magrittr)
	library(dplyr)
	#------SetDir-------
	if(flag_setdir) {source('C:/Users/Administrator/Documents/SetDir.r')}
	source('C:/Users/Administrator/Documents/hmisc.r')
	source('C:/Users/Administrator/Documents/e_DNSAE_FUN.r')
	#===========Calculate=========================================
	#1----We form a matrix of quotations -------------------
	price <- pr.OHLC(Open, High, Low, Close);
	rm(list=c("Open","High","Low","Close"));
	#n = 16; z = 30;  soft = TRUE
	cut = 0.9
	method = c("center", "spatialSign")
		
	#best <- prepareBest(n = n, z = z, cut = cut, norm = T, method = method)
	data.f <- form.data(n = n, z = z)
	best <- colnames(data.f) %>% head(.,ncol(data.f)-1)
	DT <- prepareTrain(x = data.f[ ,best], 
					   y = data.f$y, 
					   balance = FALSE, 
					   rati = 4/5, mod = "stratified", 
					   norm = TRUE, meth = method)
	SAE <- train_SAE(DT, h = c(20, 20, 20), Ep = 20, Bs =50)
	resAcc <- testAcc(SAE)
	DT.test <- prepareTest(n, z, TRUE)
	resBal <- testBal(SAE)
	sig <- resBal$sig
	sig.pr <- correct(sig) %>% extract2('sig.c') %>% pred.sig() 
	S <- sig.pr * tail(sig, length(sig.pr))
	Kmax <- corr$Kmax %>% round()
	K <- corr$K %>% round()
	maxDD <- (corr$dd$maxdrawdown*(10 ^ Dig)) %>% round()

	#sig <- resBal$sig
	#sig.sm <- smoooth(sig)
	#S <- sig.sm$sig
	#Kmax <- sig.sm$Kmax
	#K <- sig.sm$K
	#maxDD <- (sig.sm$dd$maxdrawdown*(10 ^ Dig))%>% round()
	if(K >= Kmin){first = FALSE}
	rm(list = c('data.f', 'DT', 'DT.test' ))
	flag_setdir = FALSE
	#---Write image
	save.image(file = fS)
}else{
	price <- pr.OHLC(Open, High, Low, Close);
	rm(list=c("Open","High","Low","Close"));
	DT.test <- prepareTest(n, z, TRUE)
	resBal <- testBal(SAE)
	sig <- resBal$sig
	sig.pr <- correct(sig)%>% extract2('sig.c')%>% pred.sig()
	S <- sig.pr * tail(sig, length(sig.pr))
	Kmax <- corr$Kmax %>% round()
	K <- corr$K %>% round()
	maxDD <- (corr$dd$maxdrawdown*(10 ^ Dig))%>% round()
	if(K < Kmin){first = TRUE}
	flag_setdir = FALSE
	#---Write image
	save.image(file = fS)
}










