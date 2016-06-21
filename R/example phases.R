
compile("bla.cpp")
dyn.load("bla.so")
fillNA <- function(x){rep(as.factor(NA), length(x))}

#fitTMB <- function(dm, it=1, DLL="bla", CI=FALSE, outPutPhases=FALSE){
	it=1; DLL="bla"
	Ldat <- dm[[it]]$Ldat
	Lpin <- dm[[it]]$Lpin	
	#========================================================================
	#Make maps
	#========================================================================
	#Higher map numbers are subsets of lower numbers. So build up complexity.
	map4 <- list()
	if(Ldat$srCV<0 | (Ldat$Rmodel %in% c(3,4))){map4$rbpar <- fillNA(Lpin$rbpar)}
	if(Ldat$srCV<0){map4$rapar <- fillNA(Lpin$rapar)}
		
	map3 <- map4
	map3$ny1par <- fillNA(Lpin$ny1par)
	map3$rbpar <- fillNA(Lpin$rbpar)
	map3$rapar <- fillNA(Lpin$rapar)
	
	map2 <- map3
	map2$fpar <- fillNA(Lpin$fpar)
	
	map1 <- map2
	map1$qpar <- fillNA(Lpin$qpar)
	
	maps <- list(map1, map2, map3, map4)
	optPar <- list()
	#========================================================================
	#Fit each phase
	#========================================================================
	#Phase 1
	#start with initial values from Lpin
	mod <- MakeADFun(Ldat, Lpin, DLL=DLL, map=map1, silent=TRUE)
	opt  <-  nlminb(mod $par, mod $fn, mod $gr)
	optPar[[1]]  <-  mod$env$parList(opt$par)
	
	#Phase 2 to 4
	#start with initial values from previous phase
	for(p in 2:4)
	{
		mod <- MakeADFun(Ldat, optPar[[p-1]], DLL=DLL, map=maps[[p]], silent=TRUE)
		opt  <-  nlminb(mod $par, mod $fn, mod $gr, 
					control=list(eval.max=10000, iter.max=10000))
		optPar[[p]]  <-  mod$env$parList(opt$par)
	}	