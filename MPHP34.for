	REAL*8,DIMENSION(:)::
     /DATAW(30),DATAWK(30),DATAC(30),ZX(10),SENSORS(31),VMAX(6)
        
        REAL*8,ALLOCATABLE,DIMENSION(:)::AMASS
        
	REAL*8,ALLOCATABLE,DIMENSION(:,:)::
     /TW1,TWK1,TC1,TW2,TWK2,TC2,
     /UWK1,UC1,UWK2,UC2,
     /VWK1,VC1,VWK2,VC2,
     /PWK1,PC1,PWK2,PC2,
     /RUWK1,RUC1,RUC2,
     /TI,UI,VI,APRI,
     /DTTRACK

	REAL(8)::TIME,TIMENOW,DT,ALY,
     /DXW,DYW,DXWK,
     /DYWK,DXC,DYC,
     /APOP1,APOP2,AMOV1,AMOV2,AMOL1,AMOL2,RUWKL2,
     /AITERINTERVALS,SUPERHEAT,SUMMASS,URFC,DEFFMASS,
     /TEMPERROR,VELERROR,URFCF,AMASSEVAP, AMASSECOND,
     /AQOUT,AQIN,YYSEPT,VAVG,TMAX,TMIN,
     /UUPPER,ULOWER
	INTEGER NXC,NYC   !?????????????

C=====================       PROPERTIES & DIMENSIONS   ================================================
C=====================       PROPERTIES & DIMENSIONS   ================================================
	TIME=1000.0D0
        DT=0.20D0
	NITS=(INT(TIME/DT)/2000+1.0D-6)
        IF (NITS.LT.1) NITS=1.0D0
c        NITS=1.0D0

	ALY=0.060D0   !LY
	NY=400.0D0+1.0D0      !NY
        URFCF=0.950D0
        SUPERHEAT=0.0D0
        TEMPERROR=9.0D0	
        VELERROR=4.0D0	        
        AITERINTERVALS=8.0D0     !ITERATION INTERVAL MULTI-GRID	        
C============== WALL	
	DATAW(1)=0.00010D0     !LX
	DATAW(2)=ALY     !LY
	DATAW(3)=1400.0D0     !DENSITY
	DATAW(4)=0.200D0     !CONDUCTIVITY
	DATAW(5)=840.0D0     !HEAT CAPACITY
	DATAW(6)=0.0D0     !VISCOSITY
	DATAW(12)=(8933.0D0+1400.0D0)/2.0D0          !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective DENSITY
	DATAW(13)=(385.0D0+840.0D0)/2.0D0              !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective HEAT CAPACITY
	DATAW(14)=(401.0D0+0.200D0)/2.0D0                !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity	
	DATAW(15)=DT        !DT
	DATAW(16)=10.0D0+1.0D0     !NX
	DATAW(17)=NY     !NY
	DATAW(18)=DATAW(1)/(DATAW(16)-1.0D0)     !DX
	DATAW(19)=DATAW(2)/(DATAW(17)-1.0D0)     !DY
	DATAW(22)=0.70D0     !T UNDER RELAXATION
        DATAW(25)=AITERINTERVALS     !ITERATION INTERVAL MULTI-GRID	
        DATAW(26)=-TEMPERROR     !TEMPERATURE ERROR
        DATAW(27)=-VELERROR     !VELOCITY ERROR
C============== WIECK 	
	DATAWK(1)=0.00020D0     !LX
	DATAWK(2)=ALY    !LY
	DATAWK(3)=8933.0D0     !DENSITY SOLID 
	DATAWK(4)=401.0D0     !CONDUCTIVITY SOLID 
	DATAWK(5)=385.0D0     !HEAT CAPACITY SOLID 
	DATAWK(6)=0.0D0    !HHHFFF
	DATAWK(7)=1000.0D0     !DENSITY LIQUID  ????
	DATAWK(8)=0.60D0      !CONDUCTIVITY  LIQUID
	DATAWK(9)=4200.0D0      !HEAT CAPACITY  LIQUID
	DATAWK(10)=0.00080D0     !VISCOSITY  LIQUID  ???
	DATAWK(11)=0.750D0     !PHI
	DATAWK(12)=9.110D-10        !PermeBility
	DATAWK(13)=0.550D0        !Ergun constAnt 
	DATAWK(14)=87.0D0        !Effective Conductivity
	DATAWK(15)=DT        !DT
	DATAWK(16)=10.0D0+1.0D0     !NX
	DATAWK(17)=NY     !NY
	DATAWK(18)=DATAWK(1)/(DATAWK(16)-1.0D0)     !DX
	DATAWK(19)=DATAWK(2)/(DATAWK(17)-1.0D0)     !DY
	DATAWK(20)=0.70D0/2.0D0    !U UNDER RELAXATION
	DATAWK(21)=0.70D0/2.0D0   !V UNDER RELAXATION
	DATAWK(22)=0.70D0     !T UNDER RELAXATION
	DATAWK(23)=0.30D0     !P UNDER RELAXATION
	DATAWK(24)=-1.0D0     !INTERFACES UNDERRelAxAtion FActor
        DATAWK(25)=AITERINTERVALS     !ITERATION INTERVAL MULTI-GRID
        DATAWK(26)=-TEMPERROR     !TEMPERATURE ERROR
        DATAWK(27)=-VELERROR     !VELOCITY ERROR	
C============== CORE 	
	DATAC(1)=0.00080D0     !LX  
	DATAC(2)=ALY     !LY
c	DATAC(3)=0.01940D0   !DENSITY 
	DATAC(4)=0.01890D0     !CONDUCTIVITY
	DATAC(5)=1861.540D0     !HEAT CAPACITY
	DATAC(6)=0.00000840D0     !VISCOSITY
	DATAC(7)=0.0D0     !HHHGGG
	DATAC(8)=2370000.0D0     !HHHGGG
	DATAC(15)=DT        !DT
	DATAC(16)=20.0D0+1.0D0     !NX
	DATAC(17)=NY     !NY
	DATAC(18)=DATAC(1)/(DATAC(16)-1.0D0)     !DX
	DATAC(19)=DATAC(2)/(DATAC(17)-1.0D0)     !DY
	DATAC(20)=0.70D0/2.0D0    !U UNDER RELAXATION
	DATAC(21)=0.70D0/2.0D0    !V UNDER RELAXATION
	DATAC(22)=0.70D0/2.0D0     !T UNDER RELAXATION
	DATAC(23)=0.30D0/1.0D0   !P UNDER RELAXATION	
	DATAC(24)=-1.0D0    !INTERFACES UNDERRelAxAtion FActor	
        DATAC(25)=AITERINTERVALS     !ITERATION INTERVAL MULTI-GRID	
        DATAC(26)=-TEMPERROR     !TEMPERATURE ERROR
        DATAC(27)=-VELERROR     !VELOCITY ERROR	
C=====================       READY TO START    ======================================================
C=====================       READY TO START    ======================================================

	NXW=DATAW(16)
	NYW=DATAW(17)

	NXWK=DATAWK(16)
	NYWK=DATAWK(17)

	NXC=DATAC(16)
	NYC=DATAC(17)	

	ALLOCATE(
     /TW1(NXW+1,NYW+1),TW2(NXW+1,NYW+1),
     /TWK1(NXWK+1,NYWK+1),TC1(NXC+1,NYC+1),
     /TWK2(NXWK+1,NYWK+1),TC2(NXC+1,NYC+1),
     /UWK1(NXWK+1,NYWK+1),UC1(NXC+1,NYC+1),
     /UWK2(NXWK+1,NYWK+1),UC2(NXC+1,NYC+1),
     /VWK1(NXWK+1,NYWK+1),VC1(NXC+1,NYC+1),
     /VWK2(NXWK+1,NYWK+1),VC2(NXC+1,NYC+1),
     /PWK1(NXWK+1,NYWK+1),PC1(NXC+1,NYC+1),
     /PWK2(NXWK+1,NYWK+1),PC2(NXC+1,NYC+1),
     /RUWK1(NXWK+1,NYWK+1),RUC1(NXC+1,NYC+1),
     /RUC2(NXC+1,NYC+1),     
     /TI(4,NYC+1),UI(4,NYC+1),VI(4,NYC+1),     
     /APRI(3,NYC+1),AMASS(NYC+1),DTTRACK(1000,10)
     /)

	DXW=DATAW(18)     !DX
	DYW=DATAW(19)     !DY
	DXWK=DATAWK(18)     !DX
	DYWK=DATAWK(19)     !DY
	DXC=DATAC(18)     !DX
	DYC=DATAC(19)     !DY
	
        WRITE(*,*)DXW,DXWk,DXC,DYC
        WRITE(*,*)DYC/DXW,DYC/DXWk,DYC/DXC,DYC/DYC
        reAd(*,*)AA
C==============INITIAL SETTING   
        UUPPER=DATAWK(24)
        ULOWER=DATAC(24)
     
	CALL  INITIALSETTING (NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW1,TWK1,TC1,TW2,TWK2,TC2,
     /UWK1,VWK1,UC1,VC1,UWK2,VWK2,UC2,VC2,        
     /TI,AMASS,ZX,SENSORS,RUC1,RUC2,
     /APOP1,APOP2,AMOV1,AMOV2,AMOL1,AMOL2,URFC)
C=====================       SOLVEING START HERE    ==================================================
C=====================       SOLVEING START HERE    ==================================================
	ITS=0.0D0
        TIMENOW=0.0D0
1	ITS=ITS+1	
	TIMENOW=TIMENOW+DT
c       READ(*,*)AAA
	ITER=0
2	ITER=ITER+1
c       READ(*,*)AAA

c        AMASS=0.0D0
c        UI=0.0D0
c        DEFFMASS=0.0D0  
c        UWK1=0.0D0
c        VWK1=0.0D0
c        UWK2=0.0D0
c        VWK2=0.0D0
c        UC1=0.0D0
c        VC1=0.0D0
c        UC2=0.0D0
c        VC2=0.0D0
        
	CALL INTERFACES(NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TWK1,TC1,
     /TI,UI,VI,APRI,AMASS,SUPERHEAT,URFC,AQOUT,AQIN,         
     /ITER,ITS,UUPPER,ULOWER,DEFFMASS,RUC1,RUC2,APOP2)	
c             write(*,*)"INTERFACES"
	CALL DENSITYPRESSURE(NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TWK1,TC1,TI,UI,VI,APRI,
     /RUWK1,APOP1,APOP2,AMOV1,AMOV2,AMOL1,AMOL2,
     /AMASS,SUMMASS,AMASSEVAP, AMASSECOND,YYSEPT,ITER,ITS,DT,
     /PWK2,PC2,RUC1,RUC2,RUWKL2,VAVG,TMAX,TMIN,VMAX)
c             write(*,*)"DENSITYPRESSURE"     
	CALL WALLTEMP(NXW,NYW,DATAW,TW1,TW2,TI,ITS,ITER)
C             write(*,*)"WALLTEMP"     	
	CALL WICKTEMP(NXWK,NYWK,DATAWK,TWK1,TWK2,UWK2,VWK2,TI,ITER)
c             write(*,*)"WICKTEMP"     		
	CALL CORETEMP(NXC,NYC,DATAC,TC1,TC2,UC2,VC2,TI,RUC1,RUC2,ITER)
c             write(*,*)"CORETEMP"     			
	CALL WICKVELC(NXWK,NYWK,DATAW,DATAWK,DATAC,UWK1,VWK1,UWK2,VWK2,
     /PWK1,PWK2,TWK2,UI,VI,ITER)
c             write(*,*)"WICKVELC"     			     
	CALL COREVELC(NXC,NYC,DATAW,DATAWK,DATAC,UC1,VC1,UC2,VC2,
     /PC1,PC2,TC2,UI,VI,ITER,RUC1,RUC2)
c             write(*,*)"COREVELC"     			     

            write(*,*)"V_Core=",VC2((NXC+1)/2,(NYC-1)/2)

	IF (ITER.EQ.1000*INT(ITER/1000)) THEN    
	CALL EXPORT(NXW,NYW,NXWK,NYWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,ITS,DT,NITS,AMOV1,AMOV2,
     /AMOL1,AMOL2,APOP1,APOP2,SUMMASS,AMASSEVAP, AMASSECOND,
     /AQOUT,AQIN,ITER,SENSORS,YYSEPT,AMASS,APRI,TI,TIMENOW,RUC2,
     /VAVG,TMAX,TMIN,VMAX)
        read(*,*)  AAA
        END IF              

C=====================       EXPORT AND TRANSIENT   ==================================================
C=====================       EXPORT AND TRANSIENT   ==================================================
	WRITE(*,99)TIMENOW/TIME*100,ITER,
     /INT(DATAW(28)),INT(DATAWK(28)),INT(DATAC(28)),
     /ITS,INT(DATAWK(30)),INT(DATAC(30)),
     /DATAWK(29),DATAC(29)


99	FORMAT(F6.2,"%",7I6,2E11.2)
98	FORMAT(5E15.7)


        CALL UNDERRALAXATIONFACTOR(1,DATAW,DATAWK,DATAC,URFCF,ITER,ZX)

	IF (ITER.LT.200) GOTO 2
        IF (DEFFMASS.GE.1.0D-1) GOTO 2	
	IF (DATAW(28).LT.0.0D0) GOTO 2
	IF (DATAWK(30).LT.-10.0D0) GOTO 2
	IF (DATAWK(28).LT.0.0D0) GOTO 2
	IF (DATAC(30).LT.-10.0D0) GOTO 2
	IF (DATAC(28).LT.0.0D0) GOTO 2
11        AAA=AAA

        	

        WRITE(*,91)ITS,TIMENOW,ITER,DATAWK(29),DATAC(29)
     /,UWK2((NXWK-1)/2+1,(NYWK-1)/2+1) 
91	FORMAT(I6, F6.2,I6,12E13.4)     


	CALL EXPORT(NXW,NYW,NXWK,NYWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,ITS,DT,NITS,AMOV1,AMOV2,
     /AMOL1,AMOL2,APOP1,APOP2,SUMMASS,AMASSEVAP, AMASSECOND,
     /AQOUT,AQIN,ITER,SENSORS,YYSEPT,AMASS,APRI,TI,TIMENOW,RUC2,
     /VAVG,TMAX,TMIN,VMAX)     

        TW1=TW2	
	UWK1=UWK2
	VWK1=VWK2
	PWK1=PWK2
	TWK1=TWK2
	UC1=UC2
	VC1=VC2
	PC1=PC2
	TC1=TC2
	RUC1=RUC2
	DATAWK(7)=RUWKL2
        APOP1=APOP2
        AMOV1= AMOV2
        AMOL1=AMOL2
        UUPPER=DATAWK(24)
        ULOWER=DATAC(24)
        
        CALL UNDERRALAXATIONFACTOR(2,DATAW,DATAWK,DATAC,URFCF,ITER,ZX)
        
c        CALL DTADAPT(ITER,DT, ITS,DTTRACK,NXW,NXWK,NXC,NYC,
c     /AQOUT,AQIN,AMASSEVAP,APOP2,DATAW,DATAWK,DATAC,
c     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TIMENOW)


	IF (TIMENOW.LT.TIME) GOTO 1

111	    WRITE(*,*)"*********************************"
            WRITE(*,*)"*****      DONE      *****************"
            WRITE(*,*)"*********************************"            
	STOP
	END
C================================================================================================================================================================================================
C===============================================================================================================================================================================================
C===============================================================================================================================================================================================
C===============================================================================================================================================================================================	

C===================== Time Intervals Adoptation  ==============================================
C===================== Time Intervals Adoptation  ==============================================
        SUBROUTINE DTADAPT(ITER,DT, ITS,DTTRACK,NXW,NXWK,NXC,NYC,
     /AQOUT,AQIN,AMASSEVAP,APOP2,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TIMENOW)
     

	REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30)
	REAL*8,DIMENSION (:,:)::
     /TW2(NXW+1,NYC+1),TWK2(NXWK+1,NYC+1),TC2(NXC+1,NYC+1),
     /UWK2(NXWK+1,NYC+1),VWK2(NXWK+1,NYC+1),
     /UC2(NXC+1,NYC+1),VC2(NXC+1,NYC+1),DTTRACK(1000,10)
    
       REAL(8)::AA,BB, DT,TIMENOW,AQOUT,AQIN,AMASSEVAP,APOP2

        IF (ITS.EQ.1) THEN
        DTTRACK(1,1)=ITS
        DTTRACK(1,2)=AMASSEVAP
        DTTRACK(1,3)=AQIN        
        DTTRACK(1,4)=VC2((NXC-1)/2+1,(NYC-1)/2+1)
        END IF

        IF (TIMENOW-INT(TIMENOW+(1.0D-10)).LT.1.0D-10) THEN
	OPEN(11,FILE='7DT_ADAPT.txt')	
	
	JJ=INT(TIMENOW+1.0D-10)
        DTTRACK(1+JJ,1)=ITS
        DTTRACK(1+JJ,2)=AMASSEVAP
        DTTRACK(1+JJ,3)=AQOUT        
        DTTRACK(1+JJ,4)=VC2((NXC-1)/2+1,(NYC-1)/2+1)

        II=2
        AA=DTTRACK(JJ,II)
        BB=DTTRACK(1+JJ,II)

        IF (100.0D0*ABS((BB-AA)/BB).LT.10.0D0)THEN
        DT=0.020D0
        END IF

        IF (100.0D0*ABS((BB-AA)/BB).LT.1.0D0)THEN
        DT=0.050D0
        END IF


        WRITE(11,*) JJ,BB,AA,100.0D0*ABS((BB-AA)/BB),DT
        
      
        WRITE(11,*)"DT=",DT
        
	DATAW(15)=DT        
	DATAWK(15)=DT
	DATAC(15)=DT	

        DO J=1,JJ+1
        WRITE(11,*)DTTRACK(J,1),DTTRACK(J,2),
     /DTTRACK(J,3),DTTRACK(J,4)
        END DO
        CLOSE(11)
        
        END IF

        RETURN
        END
C===================== INTERFACE TEMPERATURE AND VELOCITY  ==============================================
C===================== INTERFACE TEMPERATURE AND VELOCITY  ==============================================
	SUBROUTINE INTERFACES(NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TWK1,TC1,
     /TI,UI,VI,APRI,AMASS,SUPERHEAT,URFC, AQOUT,AQIN,
     /ITER,ITS,UUPPER,ULOWER,DEFFMASS,RUC1,RUC2,APOP2)	     
	
	REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30),
     /AMASS(NYC+1), DELT(NYC+1),DELM(NYC+1),ALFA(NYC+1)
	REAL*8,DIMENSION (:,:)::
     /TW2(NXW+1,NYC+1),TWK1(NXWK+1,NYC+1),TC1(NXC+1,NYC+1),
     /UWK2(NXWK+1,NYC+1),VWK2(NXWK+1,NYC+1),TWK2(NXWK+1,NYC+1),
     /UC2(NXC+1,NYC+1),VC2(NXC+1,NYC+1),TC2(NXC+1,NYC+1),
     /TI(4,NYC+1),UI(4,NYC+1),VI(4,NYC+1),APRI(3,NYC+1),
     /RUC1(NXC+1,NYC+1),RUC2(NXC+1,NYC+1)
     
       REAL(8):: AA,BB,AKW,AKC,AKWKL,AKWKEF,DYWK,DXC,DYC,
     /DXW,DYW,DXWK,ALXW,ALXWK,ALXC,ALY,ZIGMA,AM,API,RUNIVERSAL,R,
     /YY,AMASSJ,RUWKS,RUWKL,AQ,HTC,TINF,
     /HF,HG,HFG,CV,CL,BETA,A,B,C,APREF,TREF,DEFFMASS,
     /SUPERHEAT,URFC,AQOUT,AQIN,UUPPER,ULOWER,APOP2
     
        NYW=NYC
        NYWK=NYC

        DXW=DATAW(18)     !DX
	DYW=DATAW(19)     !DY
	DXWK=DATAWK(18)     !DX
  	DYWK=DATAWK(19)     !DY
	DXC=DATAC(18)     !DX
	DYC=DATAC(19)     !DY

	ALXW=DATAW(1)
	ALXWK=DATAWK(1)
	ALXC=DATAC(1)
	ALY=DATAW(2)
	
	RUWKS=DATAWK(3)
	RUWKL=DATAWK(7)

	AKW=DATAW(4)
        AKC=DATAC(4)
       	AKWKL=DATAWK(8)       !Fluid Conductivity
      	AKWKEF=DATAWK(14)       !Effective Conductivity

        ZIGMA=0.0010D0    !?????
        AM=18.0D0   !???????
        API=3.1415926535897932384626430D0
        RUNIVERSAL=8.31440D0*1.0D3   !???????
        R=RUNIVERSAL/AM     !???????      	

C=============== TEMPERATURE	
C===WALL HEAT FLUX           
	AQ=3.980D4                 !!!!!!!!!*3.0D0
        HTC=913.260D0           !!!!!!!*0.50D0
        TINF=295.130D0
c        2.85	0.950	296.690
c        4.71	1.570	450.890
c        7.5	        2.500	658.940
c        9.11	3.0370	750.540
c        11.94	3.980	913.260
c        18	        6	        1344.610
c        27	        9	        1954.660
c        36	        12	        2564.710
c        45	        15	        3174.760
c        60	        20	        4191.510

                
        B=0.0D0
        A=0.0D0                
        DO J=1,NYW+1
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	AK=DATAW(14)  !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity	
	TI(1,J)=0.0D0+TW2(2,J)
	IF((YY/ALY.LE.2.0D0/6.0D0).AND.((YY/ALY.GE.1.0D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	AA=AQ*DXW/AK+TW2(2,J)  	
        TI(1,J)=(AA+TW2(2,J))/2.0D0
        B=B+AK*(TW2(2,J)-TW2(1,J))/DXW*DYW
        END IF
	IF((YY/ALY.LE.5.50D0/6.0D0).AND.((YY/ALY.GE.3.50D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	BB=(AK*TW2(2,J)/DXW-HTC*TW2(2,J)/2.0D0+HTC*TINF)
     //(HTC/2.0D0+AK/DXW)  
        TI(1,J)=(BB+TW2(2,J))/2.0D0
        A=A+AK*(TW2(2,J)-TW2(1,J))/DXW*DYW
        END IF
	END DO
        AQOUT=A
        AQIN=-B
        write(*,*)" Q In=",AQIN,"    Q Out=",AQOUT
C===WALL-WICK INTERFACE
        DO J=1,NYW+1
      	AK=DATAW(4)
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	IF((YY/ALY.LE.2.0D0/6.0D0).AND.((YY/ALY.GE.1.0D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	AK=DATAW(14)  !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity		
        END IF
	IF((YY/ALY.LE.5.50D0/6.0D0).AND.((YY/ALY.GE.3.50D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	AK=DATAW(14)  !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity		
        END IF
        
	BB=AKWKEF/AK*DXW/DXWK
	AA=TW2(NXW,J)+BB*(TWK2(2,J)-(TW2(NXW,J)-TWK2(2,J)))
	AA=AA/(1.0D0+BB)
	TI(2,J)=(AA+TW2(NXW,J))/2.0D0
	END DO
C===WICK-VAPOR INTERFACE
c	HF=DATAWK(6)
c	HG=DATAC(8)
        HFG=DATAC(8)    !HFG ????????
        CL=DATAWK(9)*0.0D0
        CV=DATAC(5)*0.0D0
        
	DO J=2,NYW
        BETA=(APRI(2,J)*HFG)/(R*TI(3,J)**2.0D0)   !???**2
	ALFA(J)=1.0D0
	ALFA(J)=ALFA(J)*2.0D0*ZIGMA/(2.0D0-ZIGMA)
	ALFA(J)=ALFA(J)*DYC/DSQRT(2.0D0*API*R)/DSQRT(TI(3,J))
	ALFA(J)=ALFA(J)*(BETA-APRI(2,J)/(2.0D0*TI(3,J)))
CCCCCCCCCCCCCCCCCCC
        ALFA=0.0D0
CCCCCCCCCCCCCCCCCCC	
        AMASSJ=AMASS(J)    !!*0.0D0 !!!!!!!!!!/DYC      

        A=0.0D0
        A=TWK2(NXWK,J)*(AKWKEF*DYWK/(DXWK/2.0D0)
     /+CL*MAX(-AMASSJ,0.0D0))
     /+TC2(2,J)*(AKC*DYC/(DXC/2.0D0)
     /+CV*MAX(AMASSJ,0.0D0))
     /-MAX(-AMASSJ,0.0D0)*HFG
     /+MAX(AMASSJ,0.0D0)*HFG-ALFA(J)*HFG*TI(3,J)
        B=0.0D0
        B=AKC*DYC/(DXC/2.0D0)+CV*MAX(-AMASSJ,0.0D0)
     /+AKWKEF*DYWK/(DXWK/2.0D0)
     /+CL*MAX(AMASSJ,0.0D0)
     /-ALFA(J)*HFG
        DELT(J)= (A/B-TI(3,J))
        TI(3,J)= TI(3,J)+DATAWK(22)*(- TI(3,J)+A/B) 
c        TI(3,J)= TI(3,J)+1.0D0*(- TI(3,J)+A/B)         
c        IF (J.EQ.50*INT(J/50.))  WRITE(*,*)J, ALFA(J),DELT(J)        
	END DO

C=============== PRESSURE
        TREF=22.0D0+273.150D0    !???????????? 
        APREF=2645.0D0   !!????????
        DO J=1,NYC+1     
        B=HFG/R*(1.0D0/TREF-1.0D0/TI(3,J))
        APRI(2,J)=APREF*DEXP(B)
        END DO     

C============= VELOCITY

        UUPPER=DATAWK(24)
        ULOWER=DATAC(24)
	
          URFC=UUPPER+ITER/4000.0D0*(ULOWER-UUPPER)	
         IF (ITER.GT.4000) URFC=ULOWER
          URFC=10.0D0**(URFC)
          IF (URFC.GT.0.0010D0)URFC=0.0010D0
c          write(*,*)"URFC>>>",URFC

        DEFFMASS=0.0D0
        DO J=2,NYC    !???? 1, nyc+1 HALF CEEL, MAS FLUX, 
        A=2.0D0*ZIGMA/(2.0D0-ZIGMA)
        B=DYC/DSQRT(2.0D0*API*R)
        C=(APOP2+APRI(3,J))/DSQRT(TC2(2,J))-APRI(2,J)/DSQRT(TI(3,J))   !pRESSURE
c        C=(APOP2)/DSQRT(TC2(2,J))-APRI(2,J)/DSQRT(TI(3,J))   !pRESSURE        !!!!!??????????????????????
C        if (j.eq.20*int(j/20.)) then
c        write(*,*)"C=",C,TC2(2,J)-TI(3,J),APRI(2,J)-APC(2,J)
C        end if
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	
        AMASS(J)=AMASS(J)+(URFC)*(-AMASS(J)+(A*B*C))         !!!!!!!!?????????!!!!!!!!!!!
        DEFFMASS=DEFFMASS+ABS(-AMASS(J)+A*B*C)/NYC/ABS(A*B*C)
c        DELM(J)=ABS(AMASS(J))-ABS(((A*B*C)))
c        AMASS(J)=AMASS(J)+URFC*(-AMASS(J)+(A*B*C)+ALFA(J)*DELT(J))         !!!!!!!!?????????!!!!!!!!!!!   
c        AMASS(J)=(AMASS(J)+ALFA(J)*DELT(J))         !!!!!!!!?????????!!!!!!!!!!!           

c        IF (J.EQ.25*INT(J/25)) THEN
c        WRITE(*,*) AMASS(J),A*B*C,ABS(AMASS(J)-A*B*C)/(A*B*C)
c        END IF
85     FORMAT(I4, 5E15.5)        
        
         UI(2,J)=UI(2,J)+DATAWK(20)*(-UI(2,J)-A*B*C/RUWKL/DYC)
         UI(3,J)=UI(3,J)+DATAWK(20)*(-UI(3,J)-A*B*C/RUC1(1,J)/DYC)                
        
c        AMASS=0.0D0
c        UI=0.0D0
c        DEFFMASS=0.0D0        

        END DO
        DEFFMASS=DEFFMASS*100.0D0
        write(*,*)'DEFF MASS INTERFACE==',DEFFMASS
c        read(*,*)AA
c         UI(2,1)=0.0D0
c         UI(3,1)=0.0D0 
         UI(2,NYC+1)=0.0D0
         UI(3,NYC+1)=0.0D0          

C==============
      RETURN
      END
C===================== DENSITY  AND PRESSURE  ==============================================
C===================== DENSITY  AND PRESSURE  ==============================================
	SUBROUTINE DENSITYPRESSURE(NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,TWK1,TC1,TI,UI,VI,APRI,
     /RUWK1,APOP1,APOP2,AMOV1,AMOV2,AMOL1,AMOL2,
     /AMASS,SUMMASS,AMASSEVAP, AMASSECOND,YYSEPT,ITER,ITS,DT,
     /PWK2,PC2,RUC1,RUC2,RUWKL2,VAVG,TMAX,TMIN,VMAX)
	
	REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30),
     /AMASS(NYC+1), VMAX(6)
	REAL*8,DIMENSION (:,:)::
     /TW2(NXW+1,NYC+1),TWK1(NXWK+1,NYC+1),TC1(NXC+1,NYC+1),
     /UWK2(NXWK+1,NYC+1),VWK2(NXWK+1,NYC+1),TWK2(NXWK+1,NYC+1),
     /RUWK1(NXWK+1,NYC+1),
     /RUC1(NXC+1,NYC+1),RUC2(NXC+1,NYC+1),
     /UC2(NXC+1,NYC+1),VC2(NXC+1,NYC+1),TC2(NXC+1,NYC+1),
     /TI(4,NYC+1),UI(4,NYC+1),VI(4,NYC+1), APRI(3,NYC+1),
     /PWK2(NXWK+1,NYC+1),PC2(NXC+1,NYC+1)	

        REAL(8):: DXW,DYW,DXWK,DYWK,DXC,DYC,DT,
     /ALXW,ALXWK,ALXC,ALY,RUWKS,RUWKL,
     /AKW,AKC,AKWKL,AKWKEF,APHI,AM,API,
     /RUNIVERSAL,R,SUMVAPOR,SUMINERFACE1,
     /APOP1,APOP2,SUMMASS,AMOV2,
     /AMOV1,AMOL2,AMOL1,RUWKL2,
     /AMASSEVAP, AMASSECOND,
     /YY1,YY2,YYSEPT,VAVG,TMAX,TMIN
     
	DXW=DATAW(18)     !DX
	DYW=DATAW(19)     !DY
	DXWK=DATAWK(18)     !DX
	DYWK=DATAWK(19)     !DY
	DXC=DATAC(18)     !DX
	DYC=DATAC(19)     !DY
	
	NYWK=NYC
        NYW=NYC

	ALXW=DATAW(1)
	ALXWK=DATAWK(1)
	ALXC=DATAC(1)
	ALY=DATAW(2)
	
	RUWKS=DATAWK(3)
	RUWKL=DATAWK(7)

	AKW=DATAW(4)
        AKC=DATAC(4)
       	AKWKL=DATAWK(8)       !Fluid Conductivity
      	AKWKEF=DATAWK(14)       !Effective Conductivity
        APHI=DATAWK(11)
        
        AM=18.0D0   !???????
        API=3.1415926535897932384626430D0
        RUNIVERSAL=8.31440D0*1.0D3   !???????
        R=RUNIVERSAL/AM     !???????      	

c        write(*,*)'PRESSURE'
C=============== PRESSURE
        DO J=2,NYC
        APRI(1,J)=PWK2(NXWK,J)-PWK2(NXWK,NYC)
        APRI(3,J)=PC2(NXC,J)-PC2(NXC,NYC)
        END DO
        
        SUMVAPOR=0.0D0
        DO I=2,NXC
        DO J=2,NYC
        SUMVAPOR=SUMVAPOR+DXC*DYC/TC2(I,J)
        END DO
        END DO
        SUMINERFACE1=0.0D0
        AMASSEVAP=0.0D0
        AMASSECOND=0.0D0        
         DO J=2,NYC
        SUMINERFACE1=SUMINERFACE1-AMASS(J)
        IF (AMASS(J).LT.0.0D0) AMASSEVAP=AMASSEVAP+ AMASS(J)
        IF (AMASS(J).GT.0.0D0) AMASSECOND=AMASSECOND+ AMASS(J)        
        END DO
         APOP2=(AMOV1+DT*SUMINERFACE1)/(SUMVAPOR/R)
c        write(*,*)'MASS BALANCE'         
C=============== MASS BALANCE
        JJSEPT=3
        SUMMASS=0.0D0
        DO J=3,NYC-1
        IF (AMASS(J)*AMASS(J+1).LE.0.0)    THEN    
        YY1=DYW/2.0D0+DFLOAT(J-2)*DYW        
        YY2=DYW/2.0D0+DFLOAT(J-1)*DYW
        YYSEPT=(YY1*AMASS(J+1)-YY2*AMASS(J))/(AMASS(J+1)-AMASS(J))
        JJSEPT=J   !!!???????????
        END IF
        SUMMASS=SUMMASS+AMASS(J)
        END DO
        AMOV2=AMOV1-DT*SUMMASS
        AMOL2=AMOL1+DT*SUMMASS
c        write(*,*)'DENSITY'         
C=============== DENSITY
        DO I=1,NXC+1
        DO J=1,NYC+1
        RUC2(I,J)=APOP2/R/TC2(I,J)        
        END DO
        END DO
        RUWKL2=AMOL2/APHI/ALXWK/ALY
c        write(*,*)'VELOCITY'                 
C=============== VELOCITY
        SUMMASS=0.0D0
        YY1=YYSEPT-(DYW/2.0D0+DFLOAT(JJSEPT-2)*DYW)        
        YY2=YYSEPT-(DYW/2.0D0+DFLOAT(JJSEPT-1)*DYW)
        DO I=1,NXC+1
        VAVG=(VC2(I,JJSEPT)*YY2-VC2(I,JJSEPT+1)*YY1)/(YY2-YY1)
        SUMMASS=SUMMASS+VAVG
        END DO
        VAVG=SUMMASS/(NXC+1.0D0)

        VMAX(1)=VWK2(2,2)
        VMAX(4)=VC2(2,2)
        DO I=2,NXW
        DO J=2,NYW       
        IF (VMAX(1).LT.VWK2(I,J)) THEN
        VMAX(1)=VWK2(I,J)
	VMAX(2)=DXWK/2.0D0+DFLOAT(I-2)*DXWK+ALXW
	VMAX(3)=DYW/2.0D0+DFLOAT(J-2)*DYW                
        END IF
        IF (VMAX(4).LT.VC2(I,J)) THEN
        VMAX(4)=VC2(I,J)
        VMAX(5)=DXC/2.0D0+DFLOAT(I-2)*DXC+ALXW+ALXWK
        VMAX(6)=DYW/2.0D0+DFLOAT(J-2)*DYW                
        END IF
        END DO        
        END DO
C=============== TEMPERATURE
        TMAX=TW2(2,2)
        TMIN=TW2(2,2)
        DO I=2,NXW
        DO J=2,NYW       
        IF (TMAX.LT.TW2(I,J)) TMAX=TW2(I,J)
        IF (TMIN.GT.TW2(I,J)) TMIN=TW2(I,J)
        END DO        
        END DO

c        write(*,*)TMAX,TMIN
c        read(*,*)AAA
c        DO I=1,NXWK+1
c        DO J=1,NYWK+1        
c        IF (TMAX.LT.TWK22(I,J))TMAX=TW22(I,J)
c        IF (TMIN.GT.TWK22(I,J))TMAX=TW22(I,J)
c        END DO        
c        END DO        
c        DO I=1,NXC+1
c        DO J=1,NYC+1        
c        IF (TMAX.LT.TC22(I,J))TMAX=TC22(I,J)
c        IF (TMIN.GT.TC22(I,J))TMAX=TC22(I,J)
c        END DO        
c        END DO                
C==============
      RETURN
      END       
C=====================       EXPORT DATA INTO FILES   ==============================================
C=====================       EXPORT DATA INTO FILES   ==============================================
	SUBROUTINE EXPORT(NXW,NYW,NXWK,NYWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW2,TWK2,UWK2,VWK2,TC2,UC2,VC2,ITS,DT,NITS,AMOV1,AMOV2,
     /AMOL1,AMOL2,APOP1,APOP2,SUMMASS,AMASSEVAP, AMASSECOND,
     /AQOUT,AQIN,ITER,SENSORS,YYSEPT,AMASS,APRI,TI,TIMENOW,RUC2,
     /VAVG,TMAX,TMIN,VMAX)
	
	REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30),AMASS(NYC+1),
     /SENSORS(31),XYSENS(10),VMAX (6)
	REAL*8,DIMENSION (:,:)::
     /TW2(NXW+1,NYW+1),APRI(3,NYC+1),TI(4,NYC+1),
     /UWK2(NXWK+1,NYWK+1),VWK2(NXWK+1,NYWK+1),TWK2(NXWK+1,NYWK+1),
     /UC2(NXC+1,NYC+1),VC2(NXC+1,NYC+1),TC2(NXC+1,NYC+1),
     /RUC2(NXC+1,NYC+1),TI22(4,NYC+1),
     /TW22(NXW+1,NYW+1),TWK22(NXWK+1,NYWK+1),TC22(NXC+1,NYC+1)

        REAL(8)::DXW,DYW,DXWK,DYWK,DXC,DYC,DT,ALXW,ALXWK,ALXC,ALY,XX,YY,
     /AMOV1,AMOV2,AMOL1,AMOL2,APOP1,APOP2,SUMMASS,AMASSEVAP, 
     /AMASSECOND,AQOUT,AQIN,YYSEPT,TIMENOW,VAVG,TMAX,TMIN


	DXW=DATAW(18)     !DX
	DYW=DATAW(19)     !DY
	DXWK=DATAWK(18)     !DX
	DYWK=DATAWK(19)     !DY
	DXC=DATAC(18)     !DX
	DYC=DATAC(19)     !DY

	ALXW=DATAW(1)
	ALXWK=DATAWK(1)
	ALXC=DATAC(1)
	ALY=DATAW(2)
	
        TW22=TW2-273.150D0
        TWK22=TWK2-273.150D0
        TC22=TC2-273.150D0
        TI22=TI-273.150D0
        TMAX=TMAX-273.150D0
        TMIN=TMIN-273.150D0        
C=======================	
c	IF (ITS.EQ.NITS*INT(ITS/NITS)) THEN
c        IF (TIMENOW-INT(TIMENOW+(1.0D-10)).LT.1.0D-10) THEN
	OPEN(0,FILE='0Details_Run.txt', ACCESS='Append')	
        WRITE(0,91)ITS,TIMENOW,ITER,DATAWK(29),DATAC(29),
     /VWK2((NXWK-1)/2+1,(NYWK-1)/2+1),VC2((NXC-1)/2+1,(NYC-1)/2+1),
     /TC22((NXC-1)/2+1,(NYC-1)/2+1),RUC2((NXC-1)/2+1,(NYC-1)/2+1),
     /DATAWK(7),AMOV2,AMOL2,APOP2,SUMMASS,
     /AMASSEVAP, AMASSECOND,AQOUT,AQIN,YYSEPT,VAVG ,TMAX,TMIN,
     /VMAX(1),VMAX(2),VMAX(3),VMAX(4),VMAX(5),VMAX(6)
        CLOSE (0)	
c        END IF

C========================
c        IF (TIMENOW-INT(TIMENOW+(1.0D-10)).LT.1.0D-10) THEN
        OPEN(3,FILE='6Temp_Sensors.plt', ACCESS='Append')
        
        DO I=1,4
        II=INT(SENSORS((I-1)*3+1))
        IX=INT(SENSORS((I-1)*3+2))
        IY=INT(SENSORS((I-1)*3+3))              
        IF (II.EQ.1) XYSENS(I)=TW22(IX,IY)
        IF (II.EQ.2) XYSENS(I)=TWK22(IX,IY)
        IF (II.EQ.3) XYSENS(I)=TC22(IX,IY)

        END DO

        WRITE(3,*)ITS,TIMENOW,
     /XYSENS(1),XYSENS(2),XYSENS(3),XYSENS(4)

        CLOSE (3)	        
c        END IF
C============ INTERFACES MAS TRANSFER
c        IF (TIMENOW-INT(TIMENOW+(1.0D-10)).LT.1.0D-10) THEN
        OPEN(5,FILE='4Interface.plt', ACCESS='Append')
	WRITE(5,*)
	WRITE(5,90)TIMENOW
	WRITE(5,*)'ZONE'	
        DO J=2,NYC
        YY=DYC/2.0D0+DFLOAT(J-2)*DYC
        WRITE(5,94)YY,AMASS(J),APRI(1,J),APRI(2,J),APRI(3,J)
        END DO
        CLOSE(5)
c        END IF
C============ WALL TEMPERROR
c        IF (TIMENOW-INT(TIMENOW+(1.0D-10)).LT.1.0D-10) THEN
        OPEN(6,FILE='5Temp_Wall.plt', ACCESS='Append')
	WRITE(6,*)
	WRITE(6,90)TIMENOW
	WRITE(6,*)'ZONE'
        DO J=1,NYC+1
        YY=DYC/2.0D0+DFLOAT(J-2)*DYC
        WRITE(6,94)YY,TI22(1,J),TI22(2,J),TI22(3,J),TI22(4,J)
        END DO 
        CLOSE(6)       
c        END IF
C=======================        
90        FORMAT('#  Time=', F10.5)	        
91	    FORMAT(I6, F10.3,I6,25E14.5)     	
92        FORMAT(2E16.8)	
93        FORMAT(3E14.6)	
94        FORMAT(5E14.6)	

c        IF (TIMENOW/10.0D0-
c     /INT(TIMENOW/10.0D0+(1.0D-10)).LT.1.0D-10) THEN

	OPEN(1,FILE='2Velocity_VaporCore.plt', ACCESS='Append')
	OPEN(4,FILE='3Velocity_Wick.plt', ACCESS='Append')
	OPEN(2,FILE='1Temp_Domain.plt', ACCESS='Append')
C============ TEMPERATURE
	WRITE(2,*)
	WRITE(2,90)TIMENOW
	WRITE(2,*)'ZONE  J=',(NYW+1)/2.0D0, '  I=',NXW+1+NXWK+NXC
	
	DO J=1,NYW+1,2

	DO I=1,NXW+1
	IF (I.EQ.1) THEN   
		XX=0.0D0
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TW22(I+1,J)+TW22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYW+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TW22(I+1,J)+TW22(I,J-1))/2.0D0
		ELSE 
		YY=DYW/2.0D0+DFLOAT(J-2)*DYW
		WRITE(2,93)XX,YY,(TW22(I+1,J)+TW22(I,J))/2.0D0
		END IF
	ELSE IF (I.EQ.NXW+1) THEN
		XX=ALXW
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TW22(I-1,J)+TW22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYW+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TW22(I-1,J)+TW22(I,J-1))/2.0D0
		ELSE 
		YY=DYW/2.0D0+DFLOAT(J-2)*DYW
		WRITE(2,93)XX,YY,(TW22(I-1,J)+TW22(I,J))/2.0D0
		END IF
	ELSE
		XX=DXW/2.0D0+DFLOAT(I-2)*DXW
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TW22(I,J)+TW22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYW+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TW22(I,J)+TW22(I,J-1))/2.0D0
		ELSE 
		YY=DYW/2.0D0+DFLOAT(J-2)*DYW
		WRITE(2,93)XX,YY,TW22(I,J)
		END IF
	END IF
	END DO

	DO I=2,NXWK+1
	IF (I.EQ.1) THEN   
	ELSE IF (I.EQ.NXWK+1) THEN
		XX=ALXW+ALXWK
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TWK22(I-1,J)+TWK22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYWK+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TWK22(I-1,J)+TWK22(I,J-1))/2.0D0
		ELSE 
		YY=DYWK/2.0D0+DFLOAT(J-2)*DYWK
		WRITE(2,93)XX,YY,(TWK22(I-1,J)+TWK22(I,J))/2.0D0
		END IF
	ELSE
		XX=DXWK/2.0D0+DFLOAT(I-2)*DXWK+ALXW
		IF (J.EQ.1.0D0) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TWK22(I,J)+TWK22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYWK+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TWK22(I,J)+TWK22(I,J-1))/2.0D0
		ELSE 
		YY=DYWK/2.0D0+DFLOAT(J-2)*DYWK
		WRITE(2,93)XX,YY,TWK22(I,J)
		END IF
	END IF
	END DO

	DO I=2,NXC+1
	IF (I.EQ.1) THEN   
	ELSE IF (I.EQ.NXC+1) THEN
		XX=ALXW+ALXWK+ALXC
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TC22(I-1,J)+TC22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYC+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TC22(I-1,J)+TC22(I,J-1))/2.0D0
		ELSE 
		YY=DYC/2.0D0+DFLOAT(J-2)*DYC
		WRITE(2,93)XX,YY,(TC22(I-1,J)+TC22(I,J))/2.0D0
		END IF
	ELSE
		XX=DXC/2.0D0+DFLOAT(I-2)*DXC+ALXW+ALXWK
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(2,93)XX,YY,(TC22(I,J)+TC22(I,J+1))/2.0D0
		ELSE IF (J.EQ.NYC+1)THEN
		YY=ALY
		WRITE(2,93)XX,YY,(TC22(I,J)+TC22(I,J-1))/2.0D0
		ELSE 
		YY=DYC/2.0D0+DFLOAT(J-2)*DYC
		WRITE(2,93)XX,YY,TC22(I,J)
		END IF
	END IF
	END DO

	END DO
C============== VELOCITY

	WRITE(4,*)
	WRITE(4,90)TIMENOW
	WRITE(4,*)'ZONE  J=',(NYW+1)/2.0D0, '  I=',NXWK+1
	WRITE(1,*)
	WRITE(1,90) TIMENOW
	WRITE(1,*)'ZONE  J=',(NYW+1)/2.0D0, '  I=',NXC+1
	
	DO J=1,NYW+1,2

	DO I=1,NXWK+1
	IF (I.EQ.1) THEN   
		XX=ALXW
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(4,94)XX,YY,0.,0.,0
		ELSE IF (J.EQ.NYW+1)THEN
		YY=ALY
		WRITE(4,94)XX,YY,0.,0.,0
		ELSE 
		YY=DYW/2.0D0+DFLOAT(J-2)*DYW
		WRITE(4,94)XX,YY,UWK2(I,J),(VWK2(I,J)
     /+VWK2(I+1,J))/2.0D0,
     /DSQRT(UWK2(I,J)**2.0D0+((VWK2(I,J)+
     /VWK2(I+1,J))/2.0D0)**2.0D0)
		END IF
	ELSE IF (I.EQ.NXWK+1) THEN
		XX=ALXW+ALXWK
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(4,94)XX,YY,0.,0.,0
		ELSE IF (J.EQ.NYWK+1)THEN
		YY=ALY
		WRITE(4,94)XX,YY,0.,0.,0
		ELSE 
		YY=DYWK/2.0D0+DFLOAT(J-2)*DYWK
C??????????????????????		
		WRITE(4,94)XX,YY,UWK2(I,J),
     /0*(VWK2(I,J)+VWK2(I-1,J))/2.0D0, UWK2(I,J)   !!!!?????? 0*(
		END IF
	ELSE
		XX=DXWK/2.0D0+DFLOAT(I-2)*DXWK+ALXW
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(4,94)XX,YY,(UWK2(I,J)+UWK2(I,J+1))/2.0D0,
     /VWK2(I,J),
     /DSQRT(((UWK2(I,J)+UWK2(I,J+1))/2.0D0)**2.0D0
     /+VWK2(I,J)**2.0D0)
		ELSE IF (J.EQ.NYWK+1)THEN
		YY=ALY
		WRITE(4,94)XX,YY,(UWK2(I,J)+UWK2(I,J-1))/2.0D0,
     /VWK2(I,J),
     /DSQRT(((UWK2(I,J)+UWK2(I,J-1))/2.0D0)**2.0D0+
     /VWK2(I,J)**2.0D0)
		ELSE 
		YY=DYWK/2.0D0+DFLOAT(J-2)*DYWK
	WRITE(4,94)XX,YY,(UWK2(I,J)+UWK2(I-1,J))/2.0D0,
     /(VWK2(I,J)+VWK2(I,J-1))/2.0D0,
     /DSQRT((UWK2(I,J)+UWK2(I-1,J))**2.0D0/4.0D0
     /+(VWK2(I,J)+VWK2(I,J-1))**2.0D0/4.0D0)	
		END IF
	END IF
	END DO
	
	DO I=1,NXC+1
	IF (I.EQ.1) THEN   
		XX=ALXW+ALXWK
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(1,94)XX,YY,0.,0.,0
		ELSE IF (J.EQ.NYW+1)THEN
		YY=ALY
		WRITE(1,94)XX,YY,0.,0.,0
		ELSE 
		YY=DYW/2.0D0+DFLOAT(J-2)*DYW
		WRITE(1,94)XX,YY,UC2(I,J),(VC2(I,J)
     /+VC2(I+1,J))/2.0D0,
     /DSQRT((UC2(I,J))**2.0D0+((VC2(I,J)
     /+VC2(I+1,J))/2.0D0)**2.0D0)			
		END IF
	ELSE IF (I.EQ.NXC+1) THEN
		XX=ALXW+ALXWK+ALXC
		IF (J.EQ.1) THEN
		YY=0.0D0
		WRITE(1,94)XX,YY,0.,0.,0
		ELSE IF (J.EQ.NYC+1)THEN
		YY=ALY
		WRITE(1,94)XX,YY,0.,0.,0
		ELSE 
		YY=DYC/2.0D0+DFLOAT(J-2)*DYC
		WRITE(1,94)XX,YY,UC2(I,J),(VC2(I,J)
     /+VC2(I-1,J))/2.0D0,
     /DSQRT(UC2(I,J)**2.0D0+((VC2(I,J)
     /+VC2(I-1,J))/2.0D0)**2.0D0)
		END IF
	ELSE
	XX=DXC/2.0D0+DFLOAT(I-2)*DXC+ALXW+ALXWK
		IF (J.EQ.1) THEN
		YY=0.0D0
	WRITE(1,94)XX,YY,(UC2(I,J)+UC2(I,J+1))/2.0D0,
     /VC2(I,J),
     /DSQRT(((UC2(I,J)+UC2(I,J+1))/2.0D0)**2.0D0
     /+VC2(I,J)**2.0D0)		
		ELSE IF (J.EQ.NYC+1)THEN
		YY=ALY
	WRITE(1,94)XX,YY,(UC2(I,J)+UC2(I,J-1))/2.0D0,
     /VC2(I,J),
     /DSQRT(((UC2(I,J)+UC2(I,J-1))/2.0D0)**2.0D0
     /+VC2(I,J)**2.0D0)		
		ELSE 
		YY=DYC/2.0D0+DFLOAT(J-2)*DYC
	WRITE(1,94)XX,YY,(UC2(I,J)+UC2(I-1,J))/2.0D0,
     /(VC2(I,J)+VC2(I,J-1))/2.0D0,
     /DSQRT(((UC2(I,J)+UC2(I-1,J))/2.0D0)**2.0D0
     /+((VC2(I,J)+VC2(I,J-1))/2.0D0)**2.0D0)
		END IF
	END IF
	END DO

	END DO
C==============	
	CLOSE (1)
	CLOSE (2)
	CLOSE (4)	
c	END IF
C==============
      RETURN
      END
C=====================       WALL TEMPERATURE SOLVER   =============================================
C=====================       WALL TEMPERATURE SOLVER   =============================================

	SUBROUTINE WALLTEMP(NX,NY,DATAW,T1,T2,TI,ITS,ITER)

	REAL*8,DIMENSION (:):: DATAW(30),
     /A(NY+1),B(NY+1),C(NY+1),D(NY+1),X(NY+1)
	REAL*8,DIMENSION (:,:)::
     /T2(NX+1,NY+1),T1(NX+1,NY+1),TP(NX+1,NY+1),TI(4,NY+1),
     /RES(NX+1,NY+1),RESR(NX+1,NY+1),ER(NX+1,NY+1)
     

         REAL(8)::RU,AK,CP,DT,DX,DY,URT,FE,FW,FN,FS,ALY,YY,
     /DE,DW,DN,DS,AE,AW,AN,AS,AP,DXM,DYM,AITERINTERVALS
 
	ALY=DATAW(2)
	RU=DATAW(3)    !DENSITY
	AK=DATAW(4)     !CONDUCTIVITY
	CP=DATAW(5)     !HEAT CAPACITY
	DT=DATAW(15)     !DT
	DX=DATAW(18)     !DX
	DY=DATAW(19)     !DY
	URT=DATAW(22)     !T UNDER RELAXATION
        AITERINTERVALS=DATAW(25)
	TP=T2
C========================== T SOLVER
	DO I=2,NX
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=2,NY

      	AK=DATAW(4)
	RU=DATAW(3)  
      	CP=DATAW(5)
	YY=DY/2.0D0+DFLOAT(J-2)*DY
	IF((YY/ALY.LE.2.0D0/6.0D0).AND.((YY/ALY.GE.1.0D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	AK=DATAW(14)  !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity		
        RU=	DATAW(12) !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective DENSITY
        CP=DATAW(13)   !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective HEAT CAPACITY
        END IF
	IF((YY/ALY.LE.5.50D0/6.0D0).AND.((YY/ALY.GE.3.50D0/6.0D0)))THEN !CCCCCCCCCCCCCCCCCC
	AK=DATAW(14)  !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective Conductivity		
        RU=	DATAW(12) !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective DENSITY
        CP=DATAW(13)   !!!!!!!!!!???????????!!!!!!!!!!!!!        !Effective HEAT CAPACITY
        END IF
        
        FE=0.0D0
        FW=0.0D0
        FN=0.0D0
        FS=0.0D0

        DE=AK/DX*DY
        DW=AK/DX*DY    !?????*DY
        DN=AK/DY*DX
        DS=AK/DY*DX

        AE=DE-FE/2.0D0
        AW=DW+FW/2.0D0
        AN=DN-FN/2.0D0
        AS=DS+FS/2.0D0

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RU*CP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RU*CP*(DX*DY)/DT*T1(I,J)
    
	END DO
	A(NY+1)=1.0D0
	B(NY+1)=-1.0D0
	D(NY+1)=0.0D0

	CALL TRID3(A,B,C,X,D,NY+1,1)

	DO J=1,NY+1
	T2(I,J)=X(J)
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO J=1,NY+1
	I=1
	T2(I,J)=2.0D0*TI(1,J)-T2(I+1,J)
	I=NX+1
	T2(I,J)=2.0D0*TI(2,J)-T2(I-1,J)
	END DO	

C==============MultiGrid------MultiGrid
C==============MultiGrid------MultiGrid
        ER=0.0D0
        goto 99        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF (AITERINTERVALS*INT(ITER/AITERINTERVALS).NE.ITER) GOTO 99
        IF (ITER.LT.100) GOTO 99        
	IF (DATAW(28).GT.-30) GOTO 99
C============== RESIDUALS
        RES=0.0D0        
        DO I=2,NX

	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	 RES(I,1)=0.0D0
	 
	DO J=2,NY

        FE=0.0D0
        FW=0.0D0
        FN=0.0D0
        FS=0.0D0

        DE=AK/DX*DY
        DW=AK/DX*DY    !?????*DY
        DN=AK/DY*DX
        DS=AK/DY*DX

        AE=DE-FE/2.0D0
        AW=DW+FW/2.0D0
        AN=DN-FN/2.0D0
        AS=DS+FS/2.0D0

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RU*CP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RU*CP*(DX*DY)/DT*T1(I,J)
     
        RES(I,J)=D(J)-A(J)*T2(I,J-1)-B(J)*T2(I,J)-C(J)*T2(I,J+1)
    
	END DO
	A(NY+1)=1.0D0
	B(NY+1)=-1.0D0
	D(NY+1)=0.0D0
	RES(I,NY+1)=0.0D0
	
        END DO

C============== RESTRICTION
        RESR=0.0D0
        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESR(I,J)=1.0D0/4.0D0*RES(I,J)+
     /1.0D0/8.0D0*(RES(I+1,J)+RES(I-1,J)+RES(I,J+1)+RES(I,J-1))+
     /1.0D0/16.0D0*(RES(I+1,J+1)+RES(I-1,J-1)+RES(I-1,J+1)+RES(I+1,J-1))
        END DO
        END DO
C============== COARSE GRID SOLUTION        
        ER=0.0D0
        DXM=2.0D0*DX
        DYM=2.0D0*DY        

        DO K=1,10

	DO I=3,NX-1,2
         JJ=1
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=3,NY-1,2
	JJ=JJ+1
        FE=0.0D0
        FW=0.0D0
        FN=0.0D0
        FS=0.0D0

        DE=AK/DXM*DYM
        DW=AK/DXM*DYM    !?????*DY
        DN=AK/DYM*DXM
        DS=AK/DYM*DXM

        AE=DE-FE/2.0D0
        AW=DW+FW/2.0D0
        AN=DN-FN/2.0D0
        AS=DS+FS/2.0D0

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(JJ)=-AS
	B(JJ)=AP+RU*CP*(DXM*DYM)/DT
	C(JJ)=-AN
	
	D(JJ)=AE*ER(I+2,J)+AW*ER(I-2,J)+RESR(I,J)    !CCCCCCCCCCCCCCCCCCC ++ - - ++RESR(I,J)
    
	END DO
	A(JJ+1)=-1.0D0
	B(JJ+1)=1.0D0
	D(JJ+1)=0.0D0

	CALL TRID3(A,B,C,X,D,JJ+1,1)

	DO JJJ=1,JJ+1
        J=2*JJJ-1
	ER(I,J)=ER(I,J)+1.0D0*(-ER(I,J)+X(JJJ))
	END DO
        ER(I,1)=0.0D0
        ER(I,NY+1)=0.0D0        
	!+++++++ TDMA x-sweep
	END DO
	DO J=1,NY+1
	I=1
	ER(I,J)=0.0D0
	I=NX+1
	ER(I,J)=0.0D0
	END DO	
	
	END DO

C============== ProlognAtion
        DO I=2,NX
        DO J=2,NY
        
        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ER(I,J)=ER(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ER(I,J)=1.0D0/4.0D0*
     /(ER(I+1,J+1)+ER(I-1,J+1)+ER(I+1,J-1)+ER(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I,J+1)+ER(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I+1,J)+ER(I-1,J)) 
        END IF

        END DO
        END DO

99        CONTINUE
C============== ERROR CHECK
	DATAW(28)=0.0D0
	DO I=1,NX+1
	DO J=1,NY+1	
	T2(I,J)=T2(I,J)+ER(I,J)
	T2(I,J)=TP(I,J)+URT*(-TP(I,J)+T2(I,J))
         IF (T2(I,J).NE.0) THEN
         IF (DABS((T2(I,J)-TP(I,J))/T2(I,J)).GT.10.**(DATAW(26))) THEN
         DATAW(28)=DATAW(28)-1.0D0
         END IF
         END IF
        
	END DO
	END DO

      RETURN
      END
C=====================       WICK TEMPERATURE SOLVER   =============================================
C=====================       WICK TEMPERATURE SOLVER   =============================================

	SUBROUTINE WICKTEMP(NX,NY,DATAWK,T1,T2,U2,V2,TI,ITER)
	
	REAL*8,DIMENSION (:):: DATAWK(30),
     /A(NY+1),B(NY+1),C(NY+1),D(NY+1),X(NY+1)
	REAL*8,DIMENSION (:,:)::
     /U2(NX+1,NY+1),V2(NX+1,NY+1),
     /T2(NX+1,NY+1),T1(NX+1,NY+1),TP(NX+1,NY+1),TI(4,NY+1),
     /RES(NX+1,NY+1),RESR(NX+1,NY+1),ER(NX+1,NY+1)
     
       REAL(8):: RUS,RUCP,AKS,CPS,RUL,AKL,CPL,APHI,AKEF,DT,DX,
     /DY,URT,FE,FW,FN,FS,DE,DW,DN,DS,AE,AW,AN,AS,AP,DXM,DYM,
     /AITERINTERVALS     
     
	RUS=DATAWK(3)     !DENSITY SOLID 
	AKS=DATAWK(4)     !CONDUCTIVITY SOLID 
	CPS=DATAWK(5)     !HEAT CAPACITY SOLID 
	RUL=DATAWK(7)     !DENSITY LIQUID
	AKL=DATAWK(8)      !CONDUCTIVITY  LIQUID
	CPL=DATAWK(9)      !HEAT CAPACITY  LIQUID

	APHI=DATAWK(11)     !PHI
	AKEF=DATAWK(14)     !Effective Conductivity
	RUCP=(1.0D0-APHI)*RUS*CPS+APHI*RUL*CPL
	
	DT=DATAWK(15)     !DT
	DX=DATAWK(18)     !DX
	DY=DATAWK(19)     !DY
	URT=DATAWK(22)     !T UNDER RELAXATION
        AITERINTERVALS=	DATAWK(25)

	NXW=DATAWK(16)
c	AKEF=0.35*(APHI*AKL+(1.-APHI)*AKS)+(1-0.35)*(APHI/AKL+(1.-APHI)/AKS)**(-1)
     
	TP=T2
C========================== T SOLVER
	DO I=2,NX
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=2,NY

        FE=RUL*CPL*U2(I,J)*DY          !RU(I,J)??????????
        FW=RUL*CPL*U2(I-1,J)*DY      
        FN=RUL*CPL*V2(I,J)*DX
        FS=RUL*CPL*V2(I,J-1)*DX

        DE=AKEF/DX*DY
        DW=AKEF/DX*DY    !?????*DY
        DN=AKEF/DY*DX
        DS=AKEF/DY*DX
     

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)        
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RUCP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RUCP*(DX*DY)/DT*T1(I,J)

	END DO
	A(NY+1)=1.0D0
	B(NY+1)=-1.0D0
   	D(NY+1)=0.0D0

	CALL TRID3(A,B,C,X,D,NY+1,1)

	DO J=1,NY+1
	T2(I,J)=X(J)
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO J=1,NY+1
	I=1
	T2(I,J)=2.0D0*TI(2,J)-T2(I+1,J)
c	T2(I,J)=T2(I+1,J)	
	I=NX+1
	T2(I,J)=2.0D0*TI(3,J)-T2(I-1,J)
c	T2(I,J)=T2(I-1,J)	
	END DO	
c	write(*,*)"========Weick",DT, RUCP,CPL
c	write(*,*)"========Weick",AKEF, T1(NX/2+1,Ny/2+1)
C==============MultiGrid------MultiGrid
C==============MultiGrid------MultiGrid
        ER=0.0D0
        goto 99  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
        IF (AITERINTERVALS*INT(ITER/AITERINTERVALS).NE.ITER) GOTO 99
        IF (ITER.LT.100) GOTO 99        
	IF (DATAWk(28).GT.-30) GOTO 99        
C============== RESIDUALS
        RES=0.0D0        
        DO I=2,NX

	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	 RES(I,1)=0.0D0
	 
	DO J=2,NY

        FE=RUL*CPL*U2(I,J)*DY          !RU(I,J)??????????
        FW=RUL*CPL*U2(I-1,J)*DY      
        FN=RUL*CPL*V2(I,J)*DX
        FS=RUL*CPL*V2(I,J-1)*DX

        DE=AKEF/DX*DY
        DW=AKEF/DX*DY    !?????*DY
        DN=AKEF/DY*DX
        DS=AKEF/DY*DX

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)        

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RUCP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RUCP*(DX*DY)/DT*T1(I,J)
     
        RES(I,J)=D(J)-A(J)*T2(I,J-1)-B(J)*T2(I,J)-C(J)*T2(I,J+1)
    
	END DO
	A(NY+1)=1.0D0
	B(NY+1)=-1.0D0
	D(NY+1)=0.0D0
	RES(I,NY+1)=0.0D0
	
        END DO

C============== RESTRICTION
        RESR=0.0D0
        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESR(I,J)=1.0D0/4.0D0*RES(I,J)+
     /1.0D0/8.0D0*(RES(I+1,J)+RES(I-1,J)+RES(I,J+1)+RES(I,J-1))+
     /1.0D0/16.0D0*(RES(I+1,J+1)+RES(I-1,J-1)+RES(I-1,J+1)+RES(I+1,J-1))
        END DO
        END DO
C============== COARSE GRID SOLUTION        
        ER=0.0D0
        DXM=2.0D0*DX
        DYM=2.0D0*DY        

        DO K=1,5

	DO I=3,NX-1,2
         JJ=1
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=3,NY-1,2
	JJ=JJ+1
        FE=RUL*CPL*U2(I,J)*DYM          !RU(I,J)??????????
        FW=RUL*CPL*U2(I-1,J)*DYM      
        FN=RUL*CPL*V2(I,J)*DXM
        FS=RUL*CPL*V2(I,J-1)*DXM

        DE=AKEF/DXM*DYM
        DW=AKEF/DXM*DYM    !?????*DY
        DN=AKEF/DYM*DXM
        DS=AKEF/DYM*DXM

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)              

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(JJ)=-AS
	B(JJ)=AP+RUCP*(DXM*DYM)/DT
	C(JJ)=-AN
	
	D(JJ)=AE*ER(I+2,J)+AW*ER(I-2,J)+RESR(I,J)    !CCCCCCCCCCCCCCCCCCC ++ - - ++RESR(I,J)
    
	END DO
	A(JJ+1)=-1.0D0
	B(JJ+1)=1.0D0
	D(JJ+1)=0.0D0

	CALL TRID3(A,B,C,X,D,JJ+1,1)

	DO JJJ=1,JJ+1
        J=2*JJJ-1
	ER(I,J)=ER(I,J)+(-ER(I,J)+X(JJJ))
	END DO
        ER(I,1)=0.0D0
        ER(I,NY+1)=0.0D0        
	!+++++++ TDMA x-sweep
	END DO
	DO J=1,NY+1
	I=1
	ER(I,J)=0.0D0
	I=NX+1
	ER(I,J)=0.0D0
	END DO	
	
	END DO

C============== ProlognAtion
        DO I=2,NX
        DO J=2,NY
        
        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ER(I,J)=ER(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ER(I,J)=1.0D0/4.0D0*
     /(ER(I+1,J+1)+ER(I-1,J+1)+ER(I+1,J-1)+ER(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I,J+1)+ER(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I+1,J)+ER(I-1,J)) 
        END IF

        END DO
        END DO

99        CONTINUE
C============== ERROR CHECK
	DATAWK(28)=0.0D0
	DO I=1,NX+1
	DO J=1,NY+1	
	T2(I,J)=T2(I,J)+ER(I,J)
        T2(I,J)=TP(I,J)+URT*(-TP(I,J)+T2(I,J))
	 IF (T2(I,J).NE.0) THEN
         IF (DABS((T2(I,J)-TP(I,J))/T2(I,J)).GT.10.**(DATAWK(26)))THEN
         DATAWK(28)=DATAWK(28)-1.0D0
         END IF
         END IF
	END DO
	END DO
C==============

      RETURN
      END

C=====================       CORE TEMPERATURE SOLVER   =============================================
C=====================       CORE TEMPERATURE SOLVER   =============================================

	SUBROUTINE CORETEMP(NX,NY,DATAC,T1,T2,U2,V2,TI,RUC1,RUC2,ITER)
	
	REAL*8,DIMENSION (:):: DATAC(30),
     /A(NY+1),B(NY+1),C(NY+1),D(NY+1),X(NY+1)
	REAL*8,DIMENSION (:,:)::
     /U2(NX+1,NY+1),V2(NX+1,NY+1),
     /T2(NX+1,NY+1),T1(NX+1,NY+1),TP(NX+1,NY+1),TI(4,NY+1),
     /RES(NX+1,NY+1),RESR(NX+1,NY+1),ER(NX+1,NY+1),
     /RUC1(NX+1,NY+1),RUC2(NX+1,NY+1)

       REAL(8)::AK,CP,VIS,DT,DX,DY,URT,
     /FE,FW,FN,FS,DE,DW,DN,DS,AE,AW,AN,AS,AP,DXM,DYM,
     /AITERINTERVALS
     
	AK=DATAC(4)     !CONDUCTIVITY
	CP=DATAC(5)     !HEAT CAPACITY
	VIS=DATAC(6)     !VISCOSITY
	DT=DATAC(15)     !DT
	DX=DATAC(18)     !DX
	DY=DATAC(19)     !DY
	URT=DATAC(22)     !T UNDER RELAXATION
        AITERINTERVALS=DATAC(25)
	TP=T2
C========================== T SOLVER
	DO I=2,NX
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=2,NY

        FE=RUC1(I,J)*CP*U2(I,J)*DY          !RU(I,J)??????????
        FW=RUC1(I,J)*CP*U2(I-1,J)*DY      !!!???????J-1,I-1
        FN=RUC1(I,J)*CP*V2(I,J)*DX          !!!???????J-1,I-1
        FS=RUC1(I,J)*CP*V2(I,J-1)*DX       !!!???????J-1,I-1

        DE=AK/DX*DY
        DW=AK/DX*DY    !?????*DY
        DN=AK/DY*DX
        DS=AK/DY*DX       


        AE=DE+MAX(0.0D0,-FE)    !!!!??????????
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)   

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RUC1(I,J)*CP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RUC1(I,J)*CP*(DX*DY)/DT*T1(I,J)
    
	END DO
	B(NY+1)=1.0D0
	A(NY+1)=-1.0D0
	D(NY+1)=0.0D0

	CALL TRID3(A,B,C,X,D,NY+1,1)

	DO J=1,NY+1
	T2(I,J)=X(J)
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO J=1,NY+1
        	I=1
	        T2(I,J)=2.0D0*TI(3,J)-T2(I+1,J)
        	I=NX+1
	        T2(I,J)=T2(I-1,J)
	END DO	

c	write(*,*)"========Core",DT, RU*CP,CP
c	write(*,*)"========Core",AK, T1(NX/2+1,Ny/2+1)
C==============MultiGrid------MultiGrid
C==============MultiGrid------MultiGrid
        ER=0.0D0
        goto 99    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
        IF (AITERINTERVALS*INT(ITER/AITERINTERVALS).NE.ITER) GOTO 99
        IF (ITER.LT.100) GOTO 99        
	IF (DATAC(28).GT.-30) GOTO 99        
C============== RESIDUALS
        RES=0.0D0        
        DO I=2,NX

	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	 RES(I,1)=0.0D0
	 
	DO J=2,NY

        FE=RUC1(I,J)*CP*U2(I,J)*DY          !RU(I,J)??????????
        FW=RUC1(I,J)*CP*U2(I-1,J)*DY      
        FN=RUC1(I,J)*CP*V2(I,J)*DX
        FS=RUC1(I,J)*CP*V2(I,J-1)*DX

        DE=AK/DX*DY
        DW=AK/DX*DY    !?????*DY
        DN=AK/DY*DX
        DS=AK/DY*DX

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)   

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(J)=-AS
	B(J)=AP+RUC1(I,J)*CP*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*T2(I+1,J)+AW*T2(I-1,J)
     /+RUC1(I,J)*CP*(DX*DY)/DT*T1(I,J)
     
        RES(I,J)=D(J)-A(J)*T2(I,J-1)-B(J)*T2(I,J)-C(J)*T2(I,J+1)
    
	END DO
	A(NY+1)=1.0D0
	B(NY+1)=-1.0D0
	D(NY+1)=0.0D0
	RES(I,NY+1)=0.0D0
	
        END DO

C============== RESTRICTION
        RESR=0.0D0
        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESR(I,J)=1.0D0/4.0D0*RES(I,J)+
     /1.0D0/8.0D0*(RES(I+1,J)+RES(I-1,J)+RES(I,J+1)+RES(I,J-1))+
     /1.0D0/16.0D0*(RES(I+1,J+1)+RES(I-1,J-1)+RES(I-1,J+1)+RES(I+1,J-1))
        END DO
        END DO
C============== COARSE GRID SOLUTION        
        ER=0.0D0
        DXM=2.0D0*DX
        DYM=2.0D0*DY        

        DO K=1,5

	DO I=3,NX-1,2
c	        write(*,*)I
c	        reAd(*,*)AA
         JJ=1
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	C(1)=-1.0D0
	D(1)=0.0D0
	DO J=3,NY-1,2
	JJ=JJ+1
        FE=RUC1(I,J)*CP*U2(I,J)*DYM          !RU(I,J)??????????
        FW=RUC1(I,J)*CP*U2(I-1,J)*DYM      
        FN=RUC1(I,J)*CP*V2(I,J)*DXM
        FS=RUC1(I,J)*CP*V2(I,J-1)*DXM

        DE=AK/DXM*DYM
        DW=AK/DXM*DYM    
        DN=AK/DYM*DXM
        DS=AK/DYM*DXM

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)               

        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
	A(JJ)=-AS
	B(JJ)=AP+RUC1(I,J)*CP*(DXM*DYM)/DT
	C(JJ)=-AN
	
	D(JJ)=AE*ER(I+2,J)+AW*ER(I-2,J)+RESR(I,J)    !CCCCCCCCCCCCCCCCCCC ++ - - ++RESR(I,J)
    
	END DO
	A(JJ+1)=-1.0D0
	B(JJ+1)=1.0D0
	D(JJ+1)=0.0D0

	CALL TRID3(A,B,C,X,D,JJ+1,1)

	DO JJJ=1,JJ+1
        J=2*JJJ-1
	ER(I,J)=ER(I,J)+(-ER(I,J)+X(JJJ))
	END DO
        ER(I,1)=0.0D0
        ER(I,NY+1)=0.0D0        
	!+++++++ TDMA x-sweep
	END DO
	DO J=1,NY+1
	I=1
	ER(I,J)=0.0D0
	I=NX+1
	ER(I,J)=0.0D0
	END DO	
	
	END DO

C============== ProlognAtion
        DO I=2,NX
        DO J=2,NY
        
        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ER(I,J)=ER(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ER(I,J)=1.0D0/4.0D0*
     /(ER(I+1,J+1)+ER(I-1,J+1)+ER(I+1,J-1)+ER(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I,J+1)+ER(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ER(I,J)=1.0D0/2.0D0*(ER(I+1,J)+ER(I-1,J)) 
        END IF

        END DO
        END DO

99        CONTINUE
C============== ERROR CHECK
	DATAC(28)=0.0D0
	DO I=1,NX+1
	DO J=1,NY+1	
	T2(I,J)=T2(I,J)+ER(I,J)	
        T2(I,J)=TP(I,J)+URT*(-TP(I,J)+T2(I,J))
	 IF (T2(I,J).NE.0) THEN
         IF (DABS((T2(I,J)-TP(I,J))/T2(I,J)).GT.10.**(DATAC(26)))THEN
         DATAC(28)=DATAC(28)-1.0D0
         END IF
         END IF
	END DO
	END DO
C==============
      RETURN
      END
C=====================     WICK VELOCITY SOLVER   =============================================
C=====================     WICK VELOCITY SOLVER   =============================================

	SUBROUTINE WICKVELC(NX,NY,DATAW,DATAWK,DATAC,
     /U1,V1,U2,V2,P1,P2,T2,UI,VI,ITER)
	
	REAL*8,DIMENSION (:):: DATAW(30), DATAWK(30),DATAC(30),
     /A(NY+1),B(NY+1),C(NY+1),D(NY+1),X(NY+1)
	REAL*8,DIMENSION (:,:)::
     /U1(NX+1,NY+1),U2(NX+1,NY+1),UP(NX+1,NY+1),
     /V1(NX+1,NY+1),V2(NX+1,NY+1),VP(NX+1,NY+1),
     /P2(NX+1,NY+1),PP(NX+1,NY+1),T2(NX+1,NY+1),
     /P1(NX+1,NY+1),
     /AU(NX+1,NY+1),AV(NX+1,NY+1),
     /PR(NX+1,NY+1),PRP(NX+1,NY+1)
     /,UI(4,NY+1),VI(4,NY),
     /ERU(NX+1,NY+1),ERV(NX+1,NY+1),     
     /RESU(NX+1,NY+1),RESV(NX+1,NY+1),     
     /RESRU(NX+1,NY+1),RESRV(NX+1,NY+1)          
C     /PR2(NX+1,NY+1)   !!!!!!!!!?????

       REAL(8)::RU,VIS,APHI,APERM,CE,DT,DX,DY,
     /UUR,VUR,URP,FE,FW,FN,FS,DE,DW,DN,DS,AE,AW,AN,AS,AP,
     /RESMAX,AA1,AA2,AA3,AA4,Res,BB1,BB2,BB3,BB4,
     /DXM,DYM     
	
        RU=DATAWK(7)    !DENSITY
	VIS=DATAWK(10)     !VISCOSITY
	APHI=DATAWK(11)         !PHI
	APERM=DATAWK(12)        !PermeBility
	CE=DATAWK(13)        !Ergun constAnt 	
	
        DT=DATAWK(15)     !DT
	DX=DATAWK(18)     !DX
	DY=DATAWK(19)     !DY
	UUR=DATAWK(20)     !U UNDER RELAXATION
	VUR=DATAWK(21)     !V UNDER RELAXATION
        URP=DATAWK(23)     !V UNDER RELAXATION
        
        UP=U2  
        VP=V2  
	PP=P2
C==========================UUU MOMENTUM
	AU=0.0D0
	DO J=2,NY
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	DO I=2,NX-1
        FE=RU*(U2(I,J)+U2(I+1,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RU*(U2(I,J)+U2(I-1,J))/2.0D0*DY      
        FN=RU*(V2(I,J)+V2(I+1,J))/2.0D0*DX
        FS=RU*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DX
        DE=VIS/DX*DY
        DW=VIS/DX*DY    !?????*DY
        DN=VIS/DY*DX
        DS=VIS/DY*DX  

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    

        BB1=(V2(I,J)+V2(I,J-1)+V2(I+1,J)+V2(I+1,J-1))/4.0D0
        BB2=DSQRT((U2(I,J))**2.0D0+(BB1)**2.0D0)        
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(I)=-AW
	B(I)=AP+RU*(DX*DY)/DT+VIS*APHI/APERM*(DX*DY)
     /+CE*APHI/DSQRT(APERM)*RU*BB2*(DX*DY)	
	C(I)=-AE

	D(I)=AN*U2(I,J+1)+AS*U2(I,J-1)
     /+RU*(DX*DY)/DT*U1(I,J)
     /+((P2(I+1,J)-P2(I,J))/DX)*APHI*(DX*DY)  !DX*DY????????
	
	AU(I,J)=DY/B(I)    !*DX???????
	END DO
	B(NX)=1.0D0
	D(NX)=UI(2,J)

	CALL TRID3(A,B,C,X,D,NX,1)

	DO I=1,NX
	U2(I,J)=X(I)
C	WRITE(*,*)I,J,U2(I,J)
	END DO
C	READ(*,*)AAA
	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	U2(I,J)=-U2(I,J+1)
	J=NY+1
	U2(I,J)=-U2(I,J-1)
	END DO	
C===========================VVV MOMENTUM
	AV=0.0D0
	DO I=2,NX
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	DO J=2,NY-1	
        FE=RU*(U2(I,J+1)+U2(I,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RU*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DY      
        FN=RU*(V2(I,J)+V2(I,J+1))/2.0D0*DX
        FS=RU*(V2(I,J)+V2(I,J-1))/2.0D0*DX
        DE=VIS/DX*DY
        DW=VIS/DX*DY    !?????*DY
        DN=VIS/DY*DX
        DS=VIS/DY*DX
      
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)

        BB1=(U2(I,J)+U2(I,J+1)+U2(I-1,J)+U2(I-1,J+1))/4.0D0
        BB2=DSQRT((BB1)**2.0D0+(V2(I,J))**2.0D0)          
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(J)=-AS
	B(J)=AP+RU*(DX*DY)/DT+VIS*APHI/APERM*(DX*DY)
     /+CE*APHI/DSQRT(APERM)*RU*BB2*(DX*DY)

	C(J)=-AN
	
	D(J)=AE*V2(I+1,J)+AW*V2(I-1,J)
     /+RU*(DX*DY)/DT*V1(I,J)
     /+((P2(I,J+1)-P2(I,J))/DY)*APHI*(DX*DY)

	AV(I,J)=DX/B(J)       !!!*DY?????   AP???
	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	CALL TRID3(A,B,C,X,D,NY,1)

	DO J=1,NY
	V2(I,J)=X(J)
	END DO
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	V2(I,J)=-V2(I+1,J)
	I=NX+1
	V2(I,J)=-V2(I-1,J)
	END DO
C===============PRESSURE CORRECTION
	PR=0.0D0
	PRP=0.0D0

	DO K=1,10
	RESMAX=0.0D0
      !+++++++ TDMA x-sweep  +++++++
	DO J=2,NY
	
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0
	
	Do I=2,NX
	
	AA1=AU(I-1,J)*DY  !PCP(I-1,J)
	AA2=AU(I,J)*DY    !PCP(I+1,J)
	AA3=PRP(I,J-1)*AV(I,J-1)*DX
	AA4=PRP(I,J+1)*AV(I,J)*DX
	
	Res=(U2(I,J)*DY-U2(I-1,J)*DY+V2(I,J)*DX-V2(I,J-1)*DX)
	IF (DABS(RES).GT.RESMAX)RESMAX=DABS(RES)

	BB1=AU(I-1,J)*DY
	BB2=AU(I,J)*DY
	BB3=AV(I,J-1)*DX
	BB4=AV(I,J)*DX
	
	IF (I.EQ.2) THEN
	AA1=0.0D0
	BB1=0.0D0
	END IF

	IF (I.EQ.NX) THEN
	AA2=0.0D0
	BB2=0.0D0
	END IF
	
	IF (J.EQ.2)THEN
	AA3=0.0D0
	BB3=0.0D0
	END IF

	IF (J.EQ.NY)THEN
	AA4=0.0D0
	BB4=0.0D0
	END IF

	A(I)=-AA1
	B(I)=BB1+BB2+BB3+BB4
	C(I)=-AA2
	D(I)=AA3+AA4+Res
	END DO
	CALL TRID3(A,B,C,X,D,NX,2)
	DO I=2,NX
	PR(I,J)=X(I)
	END DO

	END DO
	END DO
C============= VELOCITY CORRECTION
	DO I=2,NX-1
	DO J=2,NY
	U2(I,J)=U2(I,J)+(PR(I,J)-PR(I+1,J))*AU(I,J)    !!//AU(I,J)

	END DO
	END DO

	DO I=2,NX
	DO J=2,NY-1	
	V2(I,J)=V2(I,J)+(PR(I,J)-PR(I,J+1))*AV(I,J)
	END DO
	END DO
	DO I=2,NX
	DO J=2,NY
	P2(I,J)=URP*PR(I,J)+PP(I,J)
	END DO
	END DO
C==============MultiGrid------MultiGridC==============MultiGrid------MultiGridC==============MultiGrid------MultiGrid
C==============MultiGrid------MultiGridC==============MultiGrid------MultiGridC==============MultiGrid------MultiGrid
        ERU=0.0D0
        ERV=0.0D0        
        goto 99        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF (AITERINTERVALS*INT(ITER/AITERINTERVALS).NE.ITER) GOTO 99
        IF (ITER.LT.100) GOTO 99        
	IF (DATAWK(30).GT.-30) GOTO 99
C============== RESIDUALS
        RESU=0.0D0        
        RESV=0.0D0        

C==========================UUU MOMENTUM
	AU=0.0D0
	DO J=2,NY
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	RESU(1,J)=0.0D0
	DO I=2,NX-1
        
        FE=RU*(U2(I,J)+U2(I+1,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RU*(U2(I,J)+U2(I-1,J))/2.0D0*DY      
        FN=RU*(V2(I,J)+V2(I+1,J))/2.0D0*DX
        FS=RU*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DX

        DE=VIS/DX*DY
        DW=VIS/DX*DY    
        DN=VIS/DY*DX
        DS=VIS/DY*DX
        
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(I)=-AW
	B(I)=AP+RU*(DX*DY)/DT
	C(I)=-AE

	D(I)=AN*U2(I,J+1)+AS*U2(I,J-1)
     /+RU*(DX*DY)/DT*U1(I,J)
     /+((P2(I+1,J)-P2(I,J))/DX)*(DX*DY)  !DX*DY????????
	
	RESU(I,J)=D(I)-A(I)*U2(I-1,J)-B(I)*U2(I,J)-C(I)*U2(I+1,J)
	
	END DO
	B(NX)=1.0D0
	D(NX)=UI(2,J)
	RESU(NX,J)=0.0D0

	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	RESU(I,J)=0.0D0
	J=NY+1
	RESU(I,J)=0.0D0
	END DO	
C===========================VVV MOMENTUM

	DO I=2,NX
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	RESV(I,1)=0.0D0
	DO J=2,NY-1	

        FE=RU*(U2(I,J+1)+U2(I,J))/2.0D0*DY          
        FW=RU*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DY      
        FN=RU*(V2(I,J)+V2(I,J+1))/2.0D0*DX
        FS=RU*(V2(I,J)+V2(I,J-1))/2.0D0*DX

        DE=VIS/DX*DY
        DW=VIS/DX*DY   
        DN=VIS/DY*DX
        DS=VIS/DY*DX
      
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)
      
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(J)=-AS
	B(J)=AP+RU*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*V2(I+1,J)+AW*V2(I-1,J)
     /+RU*(DX*DY)/DT*V1(I,J)
     /+((P2(I,J+1)-P2(I,J))/DY)*(DX*DY)

	RESV(I,J)=D(J)-A(J)*V2(I,J-1)-B(J)*V2(I,J)-C(J)*V2(I,J+1)
	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	RESV(I,NY)=0.0D0	
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	RESV(I,J)=0.0D0
	I=NX+1
	RESV(I,J)=0.0D0
	END DO
C============== RESTRICTION
        RESRU=0.0D0
        RESRV=0.0D0        
        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESRU(I,J)=1.0D0/4.0D0*RESU(I,J)+
     /1.0D0/8.0D0*(RESU(I+1,J)+RESU(I-1,J)+RESU(I,J+1)+RESU(I,J-1))+
     /1.0D0/16.0D0*(RESU(I+1,J+1)+RESU(I-1,J-1)
     /+RESU(I-1,J+1)+RESU(I+1,J-1))
        END DO
        END DO

        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESRV(I,J)=1.0D0/4.0D0*RESV(I,J)+
     /1.0D0/8.0D0*(RESV(I+1,J)+RESV(I-1,J)+RESV(I,J+1)+RESV(I,J-1))+
     /1.0D0/16.0D0*(RESV(I+1,J+1)+RESV(I-1,J-1)
     /+RESV(I-1,J+1)+RESV(I+1,J-1))
        END DO
        END DO        
C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        
C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        
        ERU=0.0D0
        ERV=0.0D0        
        DXM=2.0D0*DX
        DYM=2.0D0*DY        

        DO K=1,10

C==========================UUU MOMENTUM
	DO J=3,NY-1,2
	II=1
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	DO I=3,NX-1,2
	II=II+1
        FE=RU*(U2(I,J)+U2(I+1,J))/2.0D0*DYM          
        FW=RU*(U2(I,J)+U2(I-1,J))/2.0D0*DYM      
        FN=RU*(V2(I,J)+V2(I+1,J))/2.0D0*DXM
        FS=RU*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DXM
        DE=VIS/DXM*DYM
        DW=VIS/DXM*DYM 
        DN=VIS/DYM*DXM
        DS=VIS/DYM*DXM
        
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(II)=-AW
	B(II)=AP+RU*(DX*DY)/DT
	C(II)=-AE

	D(II)=AN*ERU(I,J+1)+AS*ERU(I,J-1)+RESRU(I,J)
	
	END DO
	B(II+1)=1.0D0
	D(II+1)=0.0D0

	CALL TRID3(A,B,C,X,D,II+1,1)

	DO III=1,II+1
	I=2*III-1
	ERU(I,J)=ERU(I,J)+1.0D0*(-ERU(I,J)+X(III))
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	ERU(I,J)=0.0D0
	J=NY+1
	ERU(I,J)=0.0D0
	END DO	
C===========================VVV MOMENTUM
	DO I=3,NX-1,2
	JJ=1
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	ERV(I,1)=0.0D0
	DO J=3,NY-1,2
        JJ=JJ+1
        FE=RU*(U2(I,J+1)+U2(I,J))/2.0D0*DYM         
        FW=RU*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DYM      
        FN=RU*(V2(I,J)+V2(I,J+1))/2.0D0*DXM
        FS=RU*(V2(I,J)+V2(I,J-1))/2.0D0*DXM

        DE=VIS/DXM*DYM
        DW=VIS/DXM*DYM   
        DN=VIS/DYM*DXM
        DS=VIS/DYM*DXM


        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)
      
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(JJ)=-AS
	B(JJ)=AP+RU*(DX*DY)/DT
	C(JJ)=-AN
	
	D(JJ)=AE*ERV(I+1,J)+AW*ERV(I-1,J)+RESRV(I,J)

	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	ERV(I,JJ+1)=0.0D0
	
	CALL TRID3(A,B,C,X,D,JJ+1,1)

	DO JJJ=1,JJ+1
        J=2*JJJ-1
	ERV(I,J)=ERV(I,J)+1.0D0*(-ERV(I,J)+X(JJJ))
	END DO
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	ERV(I,J)=0.0D0
	I=NX+1
	ERV(I,J)=0.0D0
	END DO
C======	
	END DO
C============== ProlognAtionC============== ProlognAtionC============== ProlognAtion
C============== ProlognAtionC============== ProlognAtionC============== ProlognAtion

        DO I=2,NX
        DO J=2,NY
        
        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ERU(I,J)=ERU(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ERU(I,J)=1.0D0/4.0D0*
     /(ERU(I+1,J+1)+ERU(I-1,J+1)+ERU(I+1,J-1)+ERU(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ERU(I,J)=1.0D0/2.0D0*(ERU(I,J+1)+ERU(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ERU(I,J)=1.0D0/2.0D0*(ERU(I+1,J)+ERU(I-1,J)) 
        END IF

        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ERV(I,J)=ERV(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ERV(I,J)=1.0D0/4.0D0*
     /(ERV(I+1,J+1)+ERV(I-1,J+1)+ERV(I+1,J-1)+ERV(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ERV(I,J)=1.0D0/2.0D0*(ERV(I,J+1)+ERV(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ERV(I,J)=1.0D0/2.0D0*(ERV(I+1,J)+ERV(I-1,J)) 
        END IF
        
        END DO
        END DO

99        CONTINUE
C=====================
	DO I=1,NX+1
	DO J=1,NY+1

	U2(I,J)=U2(I,J)+ERU(I,J)
	V2(I,J)=V2(I,J)+ERV(I,J)
	
	U2(I,J)=UUR*U2(I,J)+(1.0D0-UUR)*UP(I,J)  !??????	
	V2(I,J)=VUR*V2(I,J)+(1.0D0-VUR)*VP(I,J)   !???????	

	END DO
	END DO

	IF (DATAW(28).GT.-100) THEN
	IF (DATAWK(28).GT.-100) THEN
	IF (DATAC(28).GT.-100) THEN	
c	U2=UP
c        V2=VP	
        END IF
	END IF
	END IF	
C============== ERROR CHECK
	DATAWK(30)=0.0D0
	DATAWK(29)=RESMAX
	DO I=1,NX
	DO J=1,NY	
        IF (DABS(U2(I,J)).GT.1.0D-16) THEN
	IF (DABS((U2(I,J)-UP(I,J))/U2(I,J)).GT.10.**(DATAWK(27)))THEN
         DATAWK(30)=DATAWK(30)-1.0D0
         END IF
	END IF
	IF (DABS(V2(I,J)).GT.1.0D-16) THEN
	IF (DABS((V2(I,J)-VP(I,J))/V2(I,J)).GT.10.**(DATAWK(27)))THEN
         DATAWK(30)=DATAWK(30)-1.0D0
         END IF
	END IF
	IF (P2(I,J).NE.0.0) THEN
C	IF (DABS((P2(I,J)-PP(I,J))/P2(I,J)).GT.1e-2) DATAC(30)=DATAC(30)-1
	END IF
	END DO
	END DO
	
C==============
      RETURN
      END
C=====================     CORE VELOCITY SOLVER   =============================================
C=====================     CORE VELOCITY SOLVER   =============================================

	SUBROUTINE COREVELC(NX,NY,DATAW,DATAWK,DATAC,
     /U1,V1,U2,V2,P1,P2,T2,UI,VI,ITER,RUC1,RUC2)
	
	REAL*8,DIMENSION (:)::DATAW(30),DATAWK(30), DATAC(30),
     /A(NY+1),B(NY+1),C(NY+1),D(NY+1),X(NY+1)
	REAL*8,DIMENSION (:,:)::
     /U1(NX+1,NY+1),U2(NX+1,NY+1),UP(NX+1,NY+1),
     /V1(NX+1,NY+1),V2(NX+1,NY+1),VP(NX+1,NY+1),
     /P2(NX+1,NY+1),PP(NX+1,NY+1),T2(NX+1,NY+1),
     /P1(NX+1,NY+1),RUC1(NX+1,NY+1),RUC2(NX+1,NY+1),
     /AU(NX+1,NY+1),AV(NX+1,NY+1),
     /PR(NX+1,NY+1),PRP(NX+1,NY+1),
     /UI(4,NY+1),VI(4,NY+1),
     /ERU(NX+1,NY+1),ERV(NX+1,NY+1),     
     /RESU(NX+1,NY+1),RESV(NX+1,NY+1),     
     /RESRU(NX+1,NY+1),RESRV(NX+1,NY+1)     
     
        REAL(8)::AK,CP,VIS,DT,DX,DY,UUR,VUR,URP,
     /FE,FW,FN,FS,DE,DW,DN,DS,AE,AW,AN,AS,AP,
     /AA1,AA2,AA3,AA4,Res,RESMAX,BB1,BB2,BB3,BB4,
     /DXM,DYM
        
	AK=DATAC(4)     !CONDUCTIVITY
	CP=DATAC(5)     !HEAT CAPACITY
	VIS=DATAC(6)     !VISCOSITY
	DT=DATAC(15)     !DT
	DX=DATAC(18)     !DX
	DY=DATAC(19)     !DY
	UUR=DATAC(20)     !U UNDER RELAXATION
	VUR=DATAC(21)     !V UNDER RELAXATION
	URP=DATAC(23)     !V UNDER RELAXATION

	UP=U2
	VP=V2
	PP=P2
C==========================UUU MOMENTUM
	AU=0.0D0
	DO J=2,NY
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=UI(3,J)
	DO I=2,NX-1
        FE=RUC1(I,J)*(U2(I,J)+U2(I+1,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RUC1(I,J)*(U2(I,J)+U2(I-1,J))/2.0D0*DY      
        FN=RUC1(I,J)*(V2(I,J)+V2(I+1,J))/2.0D0*DX
        FS=RUC1(I,J)*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DX
        DE=VIS/DX*DY
        DW=VIS/DX*DY    !?????*DY
        DN=VIS/DY*DX
        DS=VIS/DY*DX

        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(I)=-AW
	B(I)=AP+RUC1(I,J)*(DX*DY)/DT
	C(I)=-AE

	D(I)=AN*U2(I,J+1)+AS*U2(I,J-1)
     /+RUC1(I,J)*(DX*DY)/DT*U1(I,J)
     /+((P2(I+1,J)-P2(I,J))/DX)*(DX*DY)  !DX*DY????????
	
	AU(I,J)=DY/B(I)    !*DX???????
	END DO
	B(NX)=1.0D0
	D(NX)=0.0D0

	CALL TRID3(A,B,C,X,D,NX,1)

	DO I=1,NX
	U2(I,J)=X(I)
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	U2(I,J)=-U2(I,J+1)
	J=NY+1
	U2(I,J)=-U2(I,J-1)
	END DO	
C===========================VVV MOMENTUM
	AV=0.
	DO I=2,NX
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	DO J=2,NY-1	
        FE=RUC1(I,J)*(U2(I,J+1)+U2(I,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RUC1(I,J)*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DY      
        FN=RUC1(I,J)*(V2(I,J)+V2(I,J+1))/2.0D0*DX
        FS=RUC1(I,J)*(V2(I,J)+V2(I,J-1))/2.0D0*DX
        DE=VIS/DX*DY
        DW=VIS/DX*DY    !?????*DY
        DN=VIS/DY*DX
        DS=VIS/DY*DX

C        AE=DE+MAX(0.0,DE-FE/2,-FE)
C        AW=DW+MAX(0.0,DW+FW/2,FW)
C        AN=DN+MAX(0.0,DN-FN/2,-FN)        
C        AS=DS+MAX(0.0,DS+FS/2,FS)  
      
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)
      
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(J)=-AS
	B(J)=AP+RUC1(I,J)*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*V2(I+1,J)+AW*V2(I-1,J)
     /+RUC1(I,J)*(DX*DY)/DT*V1(I,J)
     /+((P2(I,J+1)-P2(I,J))/DY)*(DX*DY)

	AV(I,J)=DX/B(J)       !!!*DY?????   AP???
	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	CALL TRID3(A,B,C,X,D,NY,1)

	DO J=1,NY
	V2(I,J)=X(J)
	END DO
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	V2(I,J)=-V2(I+1,J)
	I=NX+1
	V2(I,J)=-V2(I-1,J)
	END DO
C===============PRESSURE CORRECTION
	PR=0.0D0
	PRP=0.0D0

	DO K=1,10
	RESMAX=0.0D0
      !+++++++ TDMA x-sweep  +++++++
	DO J=2,NY
	
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0
	
	Do I=2,NX
	
	AA1=AU(I-1,J)*DY  !PCP(I-1,J)
	AA2=AU(I,J)*DY    !PCP(I+1,J)
	AA3=PRP(I,J-1)*AV(I,J-1)*DX
	AA4=PRP(I,J+1)*AV(I,J)*DX
	
	Res=(U2(I,J)*DY-U2(I-1,J)*DY+V2(I,J)*DX-V2(I,J-1)*DX)
	IF (DABS(RES).GT.RESMAX)RESMAX=DABS(RES)

	BB1=AU(I-1,J)*DY
	BB2=AU(I,J)*DY
	BB3=AV(I,J-1)*DX
	BB4=AV(I,J)*DX
	
	IF (I.EQ.2) THEN
	AA1=0.0D0
	BB1=0.0D0
	END IF

	IF (I.EQ.NX) THEN
	AA2=0.0D0
	BB2=0.0D0
	END IF
	
	IF (J.EQ.2)THEN
	AA3=0.0D0
	BB3=0.0D0
	END IF

	IF (J.EQ.NY)THEN
	AA4=0.0D0
	BB4=0.0D0
	END IF

	A(I)=-AA1
	B(I)=BB1+BB2+BB3+BB4
	C(I)=-AA2
	D(I)=AA3+AA4+Res
	END DO
	CALL TRID3(A,B,C,X,D,NX,2)
	DO I=2,NX
	PR(I,J)=X(I)
	END DO

	END DO
	END DO
C============= VELOCITY CORRECTION
	DO I=2,NX-1
	DO J=2,NY
	U2(I,J)=U2(I,J)+(PR(I,J)-PR(I+1,J))*AU(I,J)    !!//AU(I,J)

	END DO
	END DO

	DO I=2,NX
	DO J=2,NY-1	
	V2(I,J)=V2(I,J)+(PR(I,J)-PR(I,J+1))*AV(I,J)

	END DO
	END DO

	DO I=2,NX
	DO J=2,NY
	P2(I,J)=URP*PR(I,J)+PP(I,J)
	END DO
	END DO
C==============MultiGrid------MultiGridC==============MultiGrid------MultiGridC==============MultiGrid------MultiGrid
C==============MultiGrid------MultiGridC==============MultiGrid------MultiGridC==============MultiGrid------MultiGrid
        ERU=0.0D0
        ERV=0.0D0        
        goto 99        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF (AITERINTERVALS*INT(ITER/AITERINTERVALS).NE.ITER) GOTO 99
        IF (ITER.LT.100) GOTO 99        
	IF (DATAC(30).GT.-30) GOTO 99
C============== RESIDUALS
        RESU=0.0D0        
        RESV=0.0D0        

C==========================UUU MOMENTUM
	AU=0.0D0
	DO J=2,NY
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	RESU(1,J)=0.0D0
	DO I=2,NX-1
        
        FE=RUC1(I,J)*(U2(I,J)+U2(I+1,J))/2.0D0*DY          !RU(I,J)??????????
        FW=RUC1(I,J)*(U2(I,J)+U2(I-1,J))/2.0D0*DY      
        FN=RUC1(I,J)*(V2(I,J)+V2(I+1,J))/2.0D0*DX
        FS=RUC1(I,J)*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DX

        DE=VIS/DX*DY
        DW=VIS/DX*DY    
        DN=VIS/DY*DX
        DS=VIS/DY*DX
        
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(I)=-AW
	B(I)=AP+RUC1(I,J)*(DX*DY)/DT
	C(I)=-AE

	D(I)=AN*U2(I,J+1)+AS*U2(I,J-1)
     /+RUC1(I,J)*(DX*DY)/DT*U1(I,J)
     /+((P2(I+1,J)-P2(I,J))/DX)*(DX*DY)  !DX*DY????????
	
	RESU(I,J)=D(I)-A(I)*U2(I-1,J)-B(I)*U2(I,J)-C(I)*U2(I+1,J)
	
	END DO
	B(NX)=1.0D0
	D(NX)=UI(2,J)
	RESU(NX,J)=0.0D0

	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	RESU(I,J)=0.0D0
	J=NY+1
	RESU(I,J)=0.0D0
	END DO	
C===========================VVV MOMENTUM

	DO I=2,NX
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	RESV(I,1)=0.0D0
	DO J=2,NY-1	

        FE=RUC1(I,J)*(U2(I,J+1)+U2(I,J))/2.0D0*DY          
        FW=RUC1(I,J)*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DY      
        FN=RUC1(I,J)*(V2(I,J)+V2(I,J+1))/2.0D0*DX
        FS=RUC1(I,J)*(V2(I,J)+V2(I,J-1))/2.0D0*DX

        DE=VIS/DX*DY
        DW=VIS/DX*DY   
        DN=VIS/DY*DX
        DS=VIS/DY*DX
      
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)
      
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(J)=-AS
	B(J)=AP+RUC1(I,J)*(DX*DY)/DT
	C(J)=-AN
	
	D(J)=AE*V2(I+1,J)+AW*V2(I-1,J)
     /+RUC1(I,J)*(DX*DY)/DT*V1(I,J)
     /+((P2(I,J+1)-P2(I,J))/DY)*(DX*DY)

	RESV(I,J)=D(J)-A(J)*V2(I,J-1)-B(J)*V2(I,J)-C(J)*V2(I,J+1)
	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	RESV(I,NY)=0.0D0	
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	RESV(I,J)=0.0D0
	I=NX+1
	RESV(I,J)=0.0D0
	END DO
C============== RESTRICTION
        RESRU=0.0D0
        RESRV=0.0D0        
        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESRU(I,J)=1.0D0/4.0D0*RESU(I,J)+
     /1.0D0/8.0D0*(RESU(I+1,J)+RESU(I-1,J)+RESU(I,J+1)+RESU(I,J-1))+
     /1.0D0/16.0D0*(RESU(I+1,J+1)+RESU(I-1,J-1)
     /+RESU(I-1,J+1)+RESU(I+1,J-1))
        END DO
        END DO

        DO I=3,NX-1,2
        DO J=3,NY-1,2
        RESRV(I,J)=1.0D0/4.0D0*RESV(I,J)+
     /1.0D0/8.0D0*(RESV(I+1,J)+RESV(I-1,J)+RESV(I,J+1)+RESV(I,J-1))+
     /1.0D0/16.0D0*(RESV(I+1,J+1)+RESV(I-1,J-1)
     /+RESV(I-1,J+1)+RESV(I+1,J-1))
        END DO
        END DO        
C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        
C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        C============== COARSE GRID SOLUTION        
        ERU=0.0D0
        ERV=0.0D0        
        DXM=2.0D0*DX
        DYM=2.0D0*DY        

        DO K=1,10

C==========================UUU MOMENTUM
	DO J=3,NY-1,2
	II=1
	!+++++++ TDMA x-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	DO I=3,NX-1,2
	II=II+1
        FE=RUC1(I,J)*(U2(I,J)+U2(I+1,J))/2.0D0*DYM          
        FW=RUC1(I,J)*(U2(I,J)+U2(I-1,J))/2.0D0*DYM      
        FN=RUC1(I,J)*(V2(I,J)+V2(I+1,J))/2.0D0*DXM
        FS=RUC1(I,J)*(V2(I,J-1)+V2(I+1,J-1))/2.0D0*DXM
        DE=VIS/DXM*DYM
        DW=VIS/DXM*DYM 
        DN=VIS/DYM*DXM
        DS=VIS/DYM*DXM
        
        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)    
        
        AP=AE+AW+AN+AS+FE-FW+FN-FS
        
        A(II)=-AW
	B(II)=AP+RUC1(I,J)*(DX*DY)/DT
	C(II)=-AE

	D(II)=AN*ERU(I,J+1)+AS*ERU(I,J-1)+RESRU(I,J)
	
	END DO
	B(II+1)=1.0D0
	D(II+1)=0.0D0

	CALL TRID3(A,B,C,X,D,II+1,1)

	DO III=1,II+1
	I=2*III-1
	ERU(I,J)=ERU(I,J)+1.0D0*(-ERU(I,J)+X(III))
	END DO
	!+++++++ TDMA x-sweep
	END DO

	DO I=1,NX
	J=1
	ERU(I,J)=0.0D0
	J=NY+1
	ERU(I,J)=0.0D0
	END DO	
C===========================VVV MOMENTUM
	DO I=3,NX-1,2
	JJ=1
	!+++++++ TDMA y-sweep 
	A=0.0D0
	B=0.0D0
	C=0.0D0
	D=0.0D0

	B(1)=1.0D0
	D(1)=0.0D0
	ERV(I,1)=0.0D0
	DO J=3,NY-1,2
        JJ=JJ+1
        FE=RUC1(I,J)*(U2(I,J+1)+U2(I,J))/2.0D0*DYM         
        FW=RUC1(I,J)*(U2(I-1,J+1)+U2(I-1,J))/2.0D0*DYM      
        FN=RUC1(I,J)*(V2(I,J)+V2(I,J+1))/2.0D0*DXM
        FS=RUC1(I,J)*(V2(I,J)+V2(I,J-1))/2.0D0*DXM

        DE=VIS/DXM*DYM
        DW=VIS/DXM*DYM   
        DN=VIS/DYM*DXM
        DS=VIS/DYM*DXM


        AE=DE+MAX(0.0D0,-FE)
        AW=DW+MAX(0.0D0,FW)
        AN=DN+MAX(0.0D0,-FN)        
        AS=DS+MAX(0.0D0,FS)
      
        AP=AE+AW+AN+AS+FE-FW+FN-FS

        A(JJ)=-AS
	B(JJ)=AP+RUC1(I,J)*(DX*DY)/DT
	C(JJ)=-AN
	
	D(JJ)=AE*ERV(I+1,J)+AW*ERV(I-1,J)+RESRV(I,J)

	END DO
	B(NY)=1.0D0
	D(NY)=0.0D0
	ERV(I,JJ+1)=0.0D0
	
	CALL TRID3(A,B,C,X,D,JJ+1,1)

	DO JJJ=1,JJ+1
        J=2*JJJ-1
	ERV(I,J)=ERV(I,J)+1.0D0*(-ERV(I,J)+X(JJJ))
	END DO
	!+++++++ TDMA x-sweep 
	END DO
c=======
	DO J=1,NY
	I=1
	ERV(I,J)=0.0D0
	I=NX+1
	ERV(I,J)=0.0D0
	END DO
C======	
	END DO
C============== ProlognAtionC============== ProlognAtionC============== ProlognAtion
C============== ProlognAtionC============== ProlognAtionC============== ProlognAtion

        DO I=2,NX
        DO J=2,NY
        
        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ERU(I,J)=ERU(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ERU(I,J)=1.0D0/4.0D0*
     /(ERU(I+1,J+1)+ERU(I-1,J+1)+ERU(I+1,J-1)+ERU(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ERU(I,J)=1.0D0/2.0D0*(ERU(I,J+1)+ERU(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ERU(I,J)=1.0D0/2.0D0*(ERU(I+1,J)+ERU(I-1,J)) 
        END IF

        IF ((2*INT((J+1)/2)-1.EQ.J).AND.(2*INT((I+1)/2)-1.EQ.I)) THEN
        ERV(I,J)=ERV(I,J)
        ELSE IF ((2*INT((J+1)/2)-1.NE.J).AND.(2*INT((I+1)/2)-1.NE.I))
     / THEN
        ERV(I,J)=1.0D0/4.0D0*
     /(ERV(I+1,J+1)+ERV(I-1,J+1)+ERV(I+1,J-1)+ERV(I-1,J-1)) 
        ELSE IF (2*INT((J+1)/2)-1.EQ.J) THEN
        ERV(I,J)=1.0D0/2.0D0*(ERV(I,J+1)+ERV(I,J-1))
        ELSE IF (2*INT((I+1)/2)-1.EQ.I) THEN
        ERV(I,J)=1.0D0/2.0D0*(ERV(I+1,J)+ERV(I-1,J)) 
        END IF
        
        END DO
        END DO

99        CONTINUE
C=====================
	DO I=1,NX+1
	DO J=1,NY+1

	U2(I,J)=U2(I,J)+ERU(I,J)
	V2(I,J)=V2(I,J)+ERV(I,J)
	
	U2(I,J)=UUR*U2(I,J)+(1.0D0-UUR)*UP(I,J)  
	V2(I,J)=VUR*V2(I,J)+(1.0D0-VUR)*VP(I,J)   
	
	END DO
	END DO

	IF (DATAW(28).GT.-100) THEN
	IF (DATAWK(28).GT.-100) THEN
	IF (DATAC(28).GT.-100) THEN	
c	U2=UP
c        V2=VP	
        END IF
	END IF
	END IF
	
	
	
C============== ERROR CHECK
	DATAC(30)=0.0D0
	DATAC(29)=RESMAX
	DO I=1,NX
	DO J=1,NY	
        IF (DABS(U2(I,J)).GT.1.0D-16) THEN
	IF (DABS((U2(I,J)-UP(I,J))/U2(I,J)).GT.10.**(DATAWK(27)))THEN
         DATAC(30)=DATAC(30)-1.0D0
         END IF
	END IF
	IF (DABS(V2(I,J)).GT.1.0D-16) THEN
	IF (DABS((V2(I,J)-VP(I,J))/V2(I,J)).GT.10.**(DATAWK(27)))THEN
         DATAC(30)=DATAC(30)-1.0D0
         END IF
	END IF
	IF (P2(I,J).NE.0.0) THEN
C	IF (DABS((P2(I,J)-PP(I,J))/P2(I,J)).GT.1e-2) DATAC(30)=DATAC(30)-1
	END IF
	END DO
	END DO
C==============
      RETURN
      END
C=====================       TRADIAGONAL SOLVER EQ   ================================================
C=====================       TRADIAGONAL SOLVER EQ   ================================================
	SUBROUTINE TRID3(A,B,C,X,R,N,L)
C     This routine solves tri-diAgonAl lineAr system of equAtions
	REAL *8 A(N),B(N),C(N),X(N),R(N),BN
      L1=L+1
	L2=L+2
	NL=N+L2
	A(N)=A(N)/B(N)
	R(N)=R(N)/B(N)
	DO 10 I=L2,N
	K=NL-I
	J=K-1
	BN=1.0D0/(B(J)-A(K)*C(J))
	A(J)=A(J)*BN
	R(J)=(R(J)-C(J)*R(K))*BN
   10	CONTINUE
      X(L)=(R(L)-C(L)*R(L1))/(B(L)-A(L1)*C(L))
      DO 20 I=L1,N
   	X(I)=R(I)-A(I)*X(I-1)
   20	CONTINUE
      RETURN
      END

C=====================       INITIALSETTING     ================================================
C=====================       INITIALSETTING    ================================================
	SUBROUTINE INITIALSETTING (NXW,NXWK,NXC,NYC,DATAW,DATAWK,DATAC,
     /TW1,TWK1,TC1,TW2,TWK2,TC2,
     /UWK1,VWK1,UC1,VC1,UWK2,VWK2,UC2,VC2,        
     /TI,AMASS,ZX,SENSORS,RUC1,RUC2,
     /APOP1,APOP2,AMOV1,AMOV2,AMOL1,AMOL2,URFC)


	REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30),
     /AMASS(NYC+1),ZX(10),SENSORS(31)
	
	REAL*8,DIMENSION (:,:)::
     /TW1(NXW+1,NYC+1),TWK1(NXWK+1,NYC+1),TC1(NXC+1,NYC+1),
     /TW2(NXW+1,NYC+1),TWK2(NXWK+1,NYC+1),TC2(NXC+1,NYC+1),
     /UWK1(NXWK+1,NYC+1),VWK1(NXWK+1,NYC+1), 
     /UC1(NXC+1,NYC+1),VC1(NXC+1,NYC+1),
     /UWK2(NXWK+1,NYC+1),VWK2(NXWK+1,NYC+1),
     /UC2(NXC+1,NYC+1),VC2(NXC+1,NYC+1),        
     /TI(4,NYC+1),
     /RUC1(NXC+1,NYC+1),RUC2(NXC+1,NYC+1)             
     
       REAL(8)::TINIT3,AM,RUNIVERSAL,R,APOP1,APOP2,AMOV1,AMOV2,
     /ALY,SUMVAPOR,AMOL1,AMOL2,URFC,YY,DYW

        DXW=DATAW(18)     !DX
	DYW=DATAW(19)     !DY
	DXWK=DATAWK(18)     !DX
  	DYWK=DATAWK(19)     !DY
	DXC=DATAC(18)     !DX
	DYC=DATAC(19)     !DY
	ALXW=DATAW(1)
	ALXWK=DATAWK(1)
	ALXC=DATAC(1)
	ALY=DATAW(2)

        TINIT3=295.130D0
        APOP1=2645.0D0
        APOP2=APOP1
	TW1=TINIT3
	TWK1=TINIT3
	TC1=TINIT3
	TI=TINIT3
    
        AM=18.0D0
        RUNIVERSAL=8.31440D0*1.0D3   
        R=RUNIVERSAL/AM     

        SUMVAPOR=0.0D0
        DO I=2,NXC
        DO J=2,NYC
        SUMVAPOR=SUMVAPOR+DXC*DYC/TC1(I,J)
        END DO
        END DO

        AMOV1=APOP1*(SUMVAPOR/R)
        AMOV2=AMOV1         
        AMOL1=DATAWK(11)*DATAWK(7)*(DATAWK(1)*ALY)    !!ccccccccccccccccccccccccccc times the fluid rAtio
        AMOL2=AMOL1
        
        write(*,*)AMOL1,AMOV1
        read(*,*)AAA

        RUC1=APOP1/R/TINIT3
        RUC2=RUC1
C        WRITE(*,*)AMOV2,APOP1,SUMVAPOR,R
C        WRITE(*,*)RUC1(1,1),RUC1(4,60)
C        READ(*,*)aaa
        
	UWK1=0.0D0
        VWK1=0.0D0
	UC1=0.0D0
	VC1=0.0D0

	ZX(1)=DATAW(22)
        ZX(2)=DATAC(20)
	ZX(3)=DATAC(21)
	ZX(4)=DATAC(22)
	ZX(5)=DATAC(23)
	ZX(6)=DATAWK(20)
	ZX(7)=DATAWK(21)
	ZX(8)=DATAWK(22)
	ZX(9)=DATAWK(23)   

        AMASS=0.0D0
        SUMMASS=0.0D0
	TW2=TW1
	TWK2=TWK1
	TC2=TC1
	UWK2=UWK1
	VWK2=VWK1
	TWK2=TWK1
	UC2=UC1
	VC2=VC1
	TC2=TC1
        URFC=DATAWK(24)	
c====================	 SENSORS
        DO J=1,NYC+1
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	 IF (YY/ALY*100.0D0.LE.16.6660D0) THEN
        SENSORS(1)=1
        SENSORS(2)=1
        SENSORS(3)=J        
	END IF
        END DO	

        DO J=1,NYC+1
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	 IF (YY/ALY*100.0D0.LE.83.3330D0) THEN
        SENSORS(4)=1
        SENSORS(5)=1
        SENSORS(6)=J        
	END IF
        END DO	
        DO J=1,NYC+1
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	 IF (YY/ALY*100.0D0.LE.50.0D0) THEN
        SENSORS(7)=2
        SENSORS(8)=NXWK/2+1
        SENSORS(9)=J        
	END IF
        END DO	
        DO J=1,NYC+1
	YY=DYW/2.0D0+DFLOAT(J-2)*DYW
	 IF (YY/ALY*100..LE.50.0D0) THEN
        SENSORS(10)=3
        SENSORS(11)=NXC/2+1
        SENSORS(12)=J        
	END IF
        END DO	

        SENSORS(31)=4.0D0
C==========================
	OPEN(0,FILE='0Details_Run.txt')
	OPEN(1,FILE='2Velocity_VaporCore.plt')
	OPEN(4,FILE='3Velocity_Wick.plt')        
	OPEN(2,FILE='1Temp_Domain.plt')
        OPEN(3,FILE='6Temp_Sensors.plt')	
        OPEN(5,FILE='4Interface.plt')	
        OPEN(6,FILE='5Temp_Wall.plt')
        WRITE(0,*)	
        WRITE(1,*)	        
        WRITE(2,*)	        
        WRITE(3,*)	
        WRITE(4,*)	
        WRITE(5,*)	
        WRITE(6,*)	        
	CLOSE(0)
	CLOSE(1)	
	CLOSE(2)	
	CLOSE(3)	
	CLOSE(4)		
	CLOSE(5)		
	CLOSE(6)	      
	
	RETURN
	END
C=====================       UNDER RALAXATION FACTOR     ================================================
C=====================       UNDER RALAXATION FACTOR     ================================================
        SUBROUTINE UNDERRALAXATIONFACTOR(IS,
     /DATAW,DATAWK,DATAC,URFCF,ITER,ZX)
        
        REAL*8,DIMENSION (:):: DATAW(30),DATAWK(30),DATAC(30),ZX(10)
	REAL(8)::URFCF
        
        IF (IS.EQ.1)THEN
        IF (ITER.EQ.INT(ITER/100)*100) THEN
        IF (ITER.GT.20000) THEN     !CCCCCCCCCCCCCCCCCC

        write(*,*)DATAW(22),DATAWK(20),DATAWK(21),DATAWK(22)
     /,DATAWK(23),DATAC(20),DATAC(21),DATAC(22),DATAC(23)
     
	DATAW(22)=DATAW(22)*URFCF	
	DATAC(20)=DATAC(20)*URFCF
	DATAC(21)=DATAC(21)*URFCF
	DATAC(22)=DATAC(22)*URFCF
	DATAC(23)=DATAC(23)*URFCF
	DATAWK(20)=DATAWK(20)*URFCF
	DATAWK(21)=DATAWK(21)*URFCF
	DATAWK(22)=DATAWK(22)*URFCF
	DATAWK(23)=DATAWK(23)*URFCF	
        END  IF
        END IF
        END IF

        IF (IS.EQ.2)THEN        
        URFC=DATAWK(24)	
	DATAW(22)=ZX(1)
        DATAC(20)=ZX(2)
	DATAC(21)=ZX(3)
	DATAC(22)=ZX(4)
	DATAC(23)=ZX(5)
	DATAWK(20)=ZX(6)
	DATAWK(21)=ZX(7)
        DATAWK(22)=ZX(8)
	DATAWK(23)=ZX(9)        
        END IF
        
        
        RETURN
        END

