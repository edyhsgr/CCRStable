##########
##HAMILTON-PERRY WITH STOCHASTIC COMPONENTS POPULATION PROJECTION CODE
##THIS FILE IS SUPPORTING FOR https://raw.githubusercontent.com/edyhsgr/CCRStable/master/CCR_Unc_CA.R
##
##EDDIE HUNSINGER, NOVEMBER 2020 (UPDATED JANUARY 2022)
##https://edyhsgr.github.io/eddieh/
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, PLEASE CITE THE SOURCE
##This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 International License (more information: https://creativecommons.org/licenses/by-sa/3.0/igo/).
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R TO SEE IT WORK
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN PEER-REVIEWED OR CAREFULLY TESTED - QUESTIONS AND COMMENTS ARE WELCOME, OF COURSE (edyhsgr@gmail.com)
##########

##########
##PROJECTION
##########

	##CALCULATE SURVIVAL ADJUSTMENT (Yx, lx, Lx, Sx)
	YxF<-YxM<-NULL
	for (i in 1:length(lxF)){YxF[i]<-.5*log(lxF[i]/(1-lxF[i]))}
	for (i in 1:length(lxM)){YxM[i]<-.5*log(lxM[i]/(1-lxM[i]))}
	
	lxFStart<-array(0,c(length(lxF),ITER))
	lxMStart<-array(0,c(length(lxM),ITER))
	for (i in 1:length(lxF)){for(j in 1:ITER) {lxFStart[i,j]<-1/(1+exp(-2*BA_start_init[j]-2*BB*YxF[i]))}}
	for (i in 1:length(lxM)){for(j in 1:ITER) {lxMStart[i,j]<-1/(1+exp(-2*BA_start_init[j]-2*BB*YxM[i]))}}
	
	LxFStart<-array(,c(length(lxF),ITER))
	LxMStart<-array(,c(length(lxM),ITER))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFStart[,1])-1){LxFStart[i,]<-.5*(lxFStart[i,]+lxFStart[i+1,])}
	for (i in 1:length(LxMStart[,1])-1){LxMStart[i,]<-.5*(lxMStart[i,]+lxMStart[i+1,])}
	
	SxFStart<-array(0,c(length(lxF)-1,ITER))
	SxMStart<-array(0,c(length(lxM)-1,ITER))
	for (i in 1:length(SxFStart[,1])-1){SxFStart[i,]<-(LxFStart[i+1,]/LxFStart[i,])}
	for (i in 1:length(SxMStart[,1])-1){SxMStart[i,]<-(LxMStart[i+1,]/LxMStart[i,])}	

	##(OPEN-ENDED AGE GROUP OPTION (FEMALE))
	for (i in 1:ITER) {SxFStart[length(SxFStart[,1])-1,i]<-LxFStart[length(SxFStart[,1]),i]/(LxFStart[length(SxFStart[,1])-1,i]+LxFStart[length(SxFStart[,1]),i])}
	for (i in 1:ITER) {SxFStart[length(SxFStart[,1]),i]<-SxFStart[length(SxFStart[,1])-1,i]}
	
	##(OPEN-ENDED AGE GROUP OPTION (MALE))
	for (i in 1:ITER) {SxMStart[length(SxMStart[,1])-1,i]<-LxMStart[length(SxMStart[,1]),i]/(LxMStart[length(SxMStart[,1])-1,i]+LxMStart[length(SxMStart[,1]),i])}
	for (i in 1:ITER) {SxMStart[length(SxMStart[,1]),i]<-SxMStart[length(SxMStart[,1])-1,i]}

	##INITIAL e0
	e0FStart<-e0MStart<-array(,ITER)
	for (i in 1:ITER) {e0FStart[i]<-sum(LxFStart[1:length(lxF)-1,i]*5)}
	for (i in 1:ITER) {e0MStart[i]<-sum(LxMStart[1:length(lxM)-1,i]*5)}

	lxFAdj<-array(0,c(length(lxF),ITER))
	lxMAdj<-array(0,c(length(lxM),ITER))

	##ADJUST SURVIVORSHIP FOR THE STEP
	if(CURRENTSTEP<=STEPS){
	for (j in 1:ITER){for (i in 1:length(lxF)) {lxFAdj[i,j]<-1/(1+exp(-2*(SurvChange[j])-2*BB*YxF[i]))}}
	for (j in 1:ITER){for (i in 1:length(lxF)) {lxMAdj[i,j]<-1/(1+exp(-2*(SurvChange[j])-2*BB*YxM[i]))}}}

	##SURVIVAL ADJUSTMENTS (Lx, Sx)
	LxFAdj<-array(,c(length(lxF),ITER))
	LxMAdj<-array(,c(length(lxM),ITER))
	##**THIS IS A LITTLE OFF FOR THE FIRST AGE GROUP**
	for (i in 1:length(LxFAdj[,1])-1){LxFAdj[i,]<-.5*(lxFAdj[i,]+lxFAdj[i+1,])}
	for (i in 1:length(LxMAdj[,1])-1){LxMAdj[i,]<-.5*(lxMAdj[i,]+lxMAdj[i+1,])}

	SxFAdj<-array(0,c(length(lxF)-1,ITER))
	SxMAdj<-array(0,c(length(lxM)-1,ITER))
	for (i in 1:length(SxFAdj[,1])-1){SxFAdj[i,]<-(LxFAdj[i+1,]/LxFAdj[i,])}
	for (i in 1:length(SxMAdj[,1])-1){SxMAdj[i,]<-(LxMAdj[i+1,]/LxMAdj[i,])}

	##(OPEN-ENDED AGE GROUP OPTION (FEMALE))
	for (i in 1:ITER) {SxFAdj[length(SxFAdj[,1])-1,i]<-LxFAdj[length(SxFAdj[,1]),i]/(LxFAdj[length(SxFAdj[,1])-1,i]+LxFAdj[length(SxFAdj[,1]),i])}
	for (i in 1:ITER) {SxFAdj[length(SxFAdj[,1]),i]<-SxFAdj[length(SxFAdj[,1])-1,i]}

	##(OPEN-ENDED AGE GROUP OPTION (MALE))
	for (i in 1:ITER) {SxMAdj[length(SxMAdj[,1])-1,i]<-LxMAdj[length(SxMAdj[,1]),i]/(LxMAdj[length(SxMAdj[,1])-1,i]+LxMAdj[length(SxMAdj[,1]),i])}
	for (i in 1:ITER) {SxMAdj[length(SxMAdj[,1]),i]<-SxMAdj[length(SxMAdj[,1])-1,i]}

	##ADJUSTED e0
	e0FAdj<-e0MAdj<-array(,ITER)
	for (i in 1:ITER) {e0FAdj[i]<-sum(LxFAdj[1:length(lxF)-1,i]*5)}
	for (i in 1:ITER) {e0MAdj[i]<-sum(LxMAdj[1:length(lxM)-1,i]*5)}

#	##ADJUST GROSS MIGRATION OPTION - WOULD NEED TO UPDATE TO INCLUDE IN THIS STOCHASTIC IMPLEMENTATION
#        if(GrossMigrationAdjustLevel!=1){
#            RatiosGrossMigAdj<-Ratios
#            for (i in 2:HALFSIZE) {RatiosGrossMigAdj[i]<-(Ratios[i]-SxFStart[i])*GrossMigrationAdjustLevel+SxFStart[i]}
#            SGrossMigAdj_F<-array(0,c(HALFSIZE,HALFSIZE))
#            SGrossMigAdj_F<-rbind(0,cbind(diag(RatiosGrossMigAdj[2:HALFSIZE]),0))
#            ##OPEN-ENDED AGE GROUP (FEMALE)
#            SGrossMigAdj_F[HALFSIZE,HALFSIZE]<-SGrossMigAdj_F[HALFSIZE,HALFSIZE-1]
#            S_F<-SGrossMigAdj_F
#            A_F<-B_F+S_F
#            
#            for (i in (HALFSIZE+2):SIZE) {RatiosGrossMigAdj[i]<-(Ratios[i]-SxMStart[i-HALFSIZE])*GrossMigrationAdjustLevel+SxMStart[i-HALFSIZE]}
#            SGrossMigAdj_M<-array(0,c(HALFSIZE,HALFSIZE))
#            SGrossMigAdj_M<-rbind(0,cbind(diag(RatiosGrossMigAdj[(HALFSIZE+2):SIZE]),0))
#            ##OPEN-ENDED AGE GROUP (MALE)
#            SGrossMigAdj_M[HALFSIZE,HALFSIZE]<-SGrossMigAdj_M[HALFSIZE,HALFSIZE-1]
#            S_M<-SGrossMigAdj_M
#            }

	##CONSTRUCT PROJECTION MATRICES WITH SURVIVAL ADJUSTMENT
	SAdj_F<-array(0,c(HALFSIZE,HALFSIZE,ITER))
	for (i in 1:ITER){SAdj_F[,,i]<-rbind(0,cbind(diag(SxFAdj[1:(HALFSIZE)-1,i]-SxFStart[1:(HALFSIZE)-1,i]),0))}
	SAdj_F<-SAdj_F+S_F
	AAdj_F<-B_F+SAdj_F

	SAdj_M<-array(0,c(HALFSIZE,HALFSIZE,ITER))
	for (i in 1:ITER){SAdj_M[,,i]<-rbind(0,cbind(diag(SxMAdj[1:(HALFSIZE)-1,i]-SxMStart[1:(HALFSIZE)-1,i]),0))}
	SAdj_M<-SAdj_M+S_M

	AAdj_Zero<-A_Zero<-array(0,c(HALFSIZE,HALFSIZE,ITER))

	Acolone<-array(0,c(HALFSIZE,SIZE,ITER))
	Acoltwo<-array(0,c(HALFSIZE,SIZE,ITER))
	A<-array(0,c(SIZE,SIZE,ITER))

	for (i in 1:ITER) {Acolone[,,i]<-cbind(A_F[,,i],A_Zero[,,i])}
	for (i in 1:ITER) {Acoltwo[,,i]<-cbind(B_M[,,i],S_M[,,i])}
	for (i in 1:ITER) {A[,,i]<-rbind(Acolone[,,i],Acoltwo[,,i])}

	AAdjcolone<-array(0,c(HALFSIZE,SIZE,ITER))
	AAdjcoltwo<-array(0,c(HALFSIZE,SIZE,ITER))
	AAdj<-array(0,c(SIZE,SIZE,ITER))

	for (i in 1:ITER) {AAdjcolone[,,i]<-cbind(AAdj_F[,,i],AAdj_Zero[,,i])}
	for (i in 1:ITER) {AAdjcoltwo[,,i]<-cbind(B_M[,,i],SAdj_M[,,i])}
	for (i in 1:ITER) {AAdj[,,i]<-rbind(AAdjcolone[,,i],AAdjcoltwo[,,i])}

	##PROJECTION IMPLEMENTATION (WITH FERTILITY AND MIGRATION ADJUSTMENTS)
	TMinusOneAgeNew<-TMinusZeroAge
	for(i in 1:ITER) {TMinusZeroAge[,,i]<-AAdj[,,i]%*%TMinusZeroAge[,,i]}

if(CURRENTSTEP<2) {NetMigrAdjust<-array(0,ITER)}
		
		if(input$AdjustMigr=="YES")
			for (i in 1:ITER){	
			NetMigrAdjust[i]<-NetMigrAdjust[i]*NetMigration_ar[i]+NetMigrationAdjustLevel[i]*(1-NetMigration_ar[i])+rnorm(1,0,NetMigration_se[i])}

		for (i in 1:ITER){			
			TMinusZeroAge[,,i]<-(NetMigrAdjust[i])*5*sum(TMinusOneAgeNew[,,i])*Migration+TMinusZeroAge[,,i]}

		if(UseImposedTFR=="YES") 
			for (i in 1:ITER){
			{TMinusZeroAge[1,,i]<-(ImpliedTFR[i]*ImposedTFR_ar[i]+ImposedTFR[i]*(1-ImposedTFR_ar[i])+rnorm(1,0,ImposedTFR_se[i]))*(sum(TMinusZeroAge[4:10,,i])/FERTWIDTH)*5*ffab
			TMinusZeroAge[HALFSIZE+1,,i]<-(ImpliedTFR[i]*ImposedTFR_ar[i]+ImposedTFR[i]*(1-ImposedTFR_ar[i])+rnorm(1,0,ImposedTFR_se[i]))*(sum(TMinusZeroAge[4:10,,i])/FERTWIDTH)*5*(1-ffab)}}

		if(UseImposedTFR=="NO"){ 
			for (i in 1:ITER){
			TMinusZeroAge[1,,i]<-ImpliedTFR[i]*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*ffab
			TMinusZeroAge[HALFSIZE+1,,i]<-ImpliedTFR[i]*(sum(TMinusZeroAge[4:10])/FERTWIDTH)*5*(1-ffab)}}
						
	##CALCULATE iTFR
	ImpliedTFR<-array(0,ITER)
	for (i in 1:ITER){ImpliedTFR[i]<-((TMinusZeroAge[1,,i]+TMinusZeroAge[HALFSIZE+1,,i])/5)/sum(TMinusZeroAge[4:10,,i])*FERTWIDTH}
	ImpliedTFRNew<-ImpliedTFR

