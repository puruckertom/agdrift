C**AGSMPL
C  Continuum Dynamics, Inc.
C  Version 2.08 07/11/03
C
C  THIS SUBROUTINE CANNOT BE OPTIMIZED BY THE COMPILER
C
      SUBROUTINE AGSMPL(NPTS,YV,DV,NEXAM,NUMD,DEPD,DEPV,
     $                  NUMP,PIDD,PIDV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSMPL
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: NEXAM
!MS$ATTRIBUTES REFERENCE :: NUMD
!MS$ATTRIBUTES REFERENCE :: DEPD
!MS$ATTRIBUTES REFERENCE :: DEPV
!MS$ATTRIBUTES REFERENCE :: NUMP
!MS$ATTRIBUTES REFERENCE :: PIDD
!MS$ATTRIBUTES REFERENCE :: PIDV
C
C  AGSMPL performs controlled sampling and produces multiple
C  application assessment results
C
C  NPTS   - Number of points in composite deposition array
C  YV     - Composite downwind distance array (m)
C  DV     - Composite deposition array (fraction applied)
C  NEXAM  - Number of stochastic profiles for EXAMS
C  NUMD   - Number of points in deposition array
C  DEPD   - Downwind distance array (m)
C  DEPV   - Deposition array (fraction applied)
C  NUMP   - Number of points in pond-integrated deposition array
C  PIDD   - Downwind distance array (m)
C  PIDV   - Pond-integrated deposition array (fraction applied)
C
      DIMENSION SV(25),YV(25),DV(25,2),WDIR(30,100),NSPD(30,100)
      DIMENSION DEPD(2),DEPV(2),PIDD(2),PIDV(2)
C
      INCLUDE 'AGSAMPLE.INC'
C
      DATA SV /    0.0 ,    5.0 ,   10.0 ,   15.0 ,   20.0 ,   30.0 ,
     $            40.0 ,   50.0 ,   75.0 ,  100.0 ,  125.0 ,  150.0 ,
     $           200.0 ,  250.0 ,  300.0 ,  400.0 ,  500.0 ,  600.0 ,
     $           700.0 ,  800.0 ,  900.0 , 1000.0 , 1200.0 , 1400.0 ,
     $          1600.0 /
C
C  Set deposition distances
C
      DO N=1,25
        YEXPT(N)=SV(N)
        IF (YEXPT(N).LE.YDEPA(NDEPA)) NEXPT=N
      ENDDO
C
      ISEED=0.42/4.656612875E-10
C
C  Loop for replications in each year
C
      NEXMX=0
      DO NY=1,NYEARS
        DO NR=1,100
          CALL METEPA(NXSPD,FREQ,NEVNTS,ISEED,WDIR(1,NR),NSPD(1,NR))
        ENDDO
        CALL STCALC(NEVNTS,100,WDIR,NSPD)
      ENDDO
C
C  Set plotting results
C
      IF (ITIER.EQ.1) THEN
        NAMAX=1
      ELSE
        NAMAX=7
      ENDIF
      NPTS=NEXPT
      NF=NXSPD+1
      DO N=1,NEXPT
        YV(N)=YEXPT(N)
        DV(N,2)=0.0
        DO NN=1,NF
          DO NA=1,NAMAX
            DTEM=AGINT(NDEPA,YDEPA,ZDEPV(1,NN,NA),YV(N))
            DV(N,2)=AMAX1(DV(N,2),DTEM)
          ENDDO
        ENDDO
        DAVE=0.0
        DO NE=1,NEXMX
          ITEM=IXAMV(NE)
          DTEM=EXAMV(N,NE)
          IF (ITEM.EQ.1) DAVE=DAVE+DTEM
        ENDDO
        DV(N,1)=DAVE/NEXMX
      ENDDO
      NEXAM=NEXMX
      CALL AGSPLN(NPTS,YV,DV,NUMD,DEPD,DEPV,NUMP,PIDD,PIDV,DTEM)
      RETURN
      END
C**METEPA
      SUBROUTINE METEPA(NWSC,F,NI,ISEED,WDIR,NSPD)
C
C  METEPA selects the wind speeds and wind directions
C  for the stochastic assessment (following the program METPRO)
C
C  NWSC   - Number of wind speeds
C  F      - Frequency distribution array
C  NI     - Number of applications (events) per year
C  ISEED  - Random number
C  WDIR   - Wind direction array
C  NSPD   - Wind speed pointer array
C
      DIMENSION F(36,19),WDIR(2),NSPD(2)
      DIMENSION G(36),XXV(20),NPCK(36),GJOINT(19)
      DIMENSION NRDER(19),INVER(19),FJOINT(19),NNDEX(30)
C
      DATA NSEC / 36 /
C
      DISPL=10.0
      WIDTH=10.0
C
C  Cumulative sum all wind speed data for each wind direction
C
      G(1)=0.0
      DO L=1,NWSC
        G(1)=G(1)+F(1,L)
      ENDDO
      DO K=2,NSEC
        G(K)=G(K-1)
        DO L=1,NWSC
          G(K)=G(K)+F(K,L)
        ENDDO
      ENDDO
      G(NSEC)=1.0
C
C  Determine wind directions to use
C
      CALL GGUBS(ISEED,2,XXV)
      RN=XXV(2)/FLOAT(NI)
      ISEC=1
      CUM=G(1)
      TMP=0.0
      DO N=1,NI
10      IF (RN.GT.G(ISEC)) THEN
          TMP=G(ISEC)
          ISEC=ISEC+1
          CUM=G(ISEC)-TMP
          GOTO 10
        ENDIF
        WDBND=DISPL+(ISEC-1)*WIDTH-WIDTH/2.0
        WDIR(N)=WDBND+WIDTH*(RN-TMP)/CUM
        IF (WDIR(N).GT.360.0) WDIR(N)=WDIR(N)-360.0
        IF (WDIR(N).LE.0.0) WDIR(N)=WDIR(N)+360.0
        RN=RN+1.0/FLOAT(NI)
      ENDDO
C
C  Locate which wind direction intervals actually participate
C
      DO I=1,NSEC
        KOUNT=0
        WDBND=DISPL+(I-1)*WIDTH-WIDTH/2.0
        WDTOP=WDBND+WIDTH
        IF (WDTOP.GE.360.0) THEN
          WDBND=WDBND-360.0
          WDTOP=WDTOP-360.0
        ENDIF
        DO J=1,NI
          IF (WDBND.GT.0.0) THEN
            IF (WDIR(J).GE.WDBND.AND.WDIR(J).LT.WDTOP) KOUNT=KOUNT+1
          ELSE
            WDB=WDBND+360.0
            WDT=WDTOP+360.0
            WD=WDIR(J)
            IF (WDIR(J).LT.90.0) WD=WDIR(J)+360.0
            IF (WD.GE.WDB.AND.WD.LT.WDT) KOUNT=KOUNT+1
          ENDIF
        ENDDO
        NPCK(I)=KOUNT
      ENDDO
C
C  Now select wind speed for nonzero NPCK wind directions
C
      KSEL=0
      DO I=1,NSEC
        IF (NPCK(I).NE.0) THEN
          DO L=1,NWSC
            GJOINT(L)=F(I,L)
          ENDDO
          CALL RANDOR(ISEED,XXV,NRDER,NWSC)
          DO IN=1,NWSC
            INDEX=NRDER(IN)
            INVER(INDEX)=IN
            FJOINT(INDEX)=GJOINT(IN)
          ENDDO
          NS=NPCK(I)
          CALL SELECT(ISEED,XXV,NS,FJOINT,NWSC,NNDEX)
          DO II=1,NS
            KSEL=KSEL+1
            NSPD(KSEL)=INVER(NNDEX(II))
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
C**RANDOR
      SUBROUTINE RANDOR(ISEED,XXV,NRDER,NUXS)
C
C  RANDOR randomly permutes the numbers from 1 to 36
C
C  ISEED  - Random number
C  XXV    - Array to hold random numbers
C  NRDER  - Array for order of elements
C  NUXS   - Number of elements
C
      DIMENSION XXV(2),NRDER(2),AV(20)
C
      CALL GGUBS(ISEED,NUXS+1,XXV)
      DO I=1,NUXS
        II=I+1
        AV(I)=XXV(II)
      ENDDO
      DO KT=1,NUXS
        ALOW=1.0
        DO I=1,NUXS
          IF (AV(I).LT.ALOW) THEN
            ALOW=AV(I)
            NZ=I
          ENDIF
        ENDDO
        NRDER(NZ)=KT
        AV(NZ)=1.0
      ENDDO
      RETURN
      END
C**SELECT
      SUBROUTINE SELECT(ISEED,XXV,NN,H,NC,NPICK)
C
C  SELECT makes the controlled sampling selections
C
C  ISEED  - Random number
C  XXV    - Array to hold random numbers
C  NN     - Number of samples
C  H      - Joint distribution
C  NC     - Size of joint distribution
C  NPICK  - Sampling selection index
C
      DIMENSION XXV(2),H(2),NPICK(2),FCUM(19)
C
      RANGE=0.0
      DO I=1,NC
        RANGE=RANGE+H(I)
        FCUM(I)=RANGE
      ENDDO
      CALL GGUBS(ISEED,2,XXV)
      A=XXV(2)*RANGE
      I=1
10    IF (A.GT.FCUM(I)) THEN
        I=I+1
        GOTO 10
      ENDIF
      NPICK(1)=I
      IF (NN.GT.1) THEN
        GAP=RANGE/FLOAT(NN)
        FPICK=A
        DO I=2,NN
          FPICK=FPICK+GAP
          IF (FPICK.GE.RANGE) FPICK=FPICK-RANGE
          J=1
20        IF (FPICK.GT.FCUM(J)) THEN
            J=J+1
            GOTO 20
          ENDIF
          NPICK(I)=J
        ENDDO
      ENDIF
      RETURN
      END
C**GGUBS
      SUBROUTINE GGUBS(ISEED,NS,AV)
C
C  GGUBS accesses the random number generator
C
C  ISEED  - Random number
C  NS     - Number of samples desired
C  AV     - Array to hold random numbers
C
      DIMENSION AV(2)
C
      DO I=1,NS
        AV(I)=RAN(ISEED)
      ENDDO
      RETURN
      END
C**RAN
      FUNCTION RAN(IY)
C
C  IY     - Random number (updated integer)
C
      DATA IA   /      16807 /
      DATA IB15 /      32768 /
      DATA IB16 /      65536 /
      DATA IP   / 2147483647 /
C
      IXHI=IY/IB16
      IXLO=(IY-IXHI*IB16)*IA
      LEFTLO=IXLO/IB16
      IFHI=IXHI*IA+LEFTLO
      K=IFHI/IB15
      IY=(((IXLO-LEFTLO*IB16)-IP)+(IFHI-K*IB15)*IB16)+K
      IF (IY.LT.0) IY=IY+IP
      RAN=FLOAT(IY)*4.656612875E-10
      RETURN
      END
C**STCALC
      SUBROUTINE STCALC(NI,NR,WDIR,NSPD)
C
C  STCALC calculates the average application deposition
C  and recovers the 95th percentile
C
C  NI     - Number of applications (events) per year
C  NR     - Number of repetitions
C  WDIR   - Wind direction array
C  NSPD   - Wind speed pointer array
C
      DIMENSION WDIR(30,100),NSPD(30,100),DVAL(30,25),IVAL(30)
      DIMENSION TSAV(6),NSAV(6),XMEAN(25),XSDEV(25)
C
      INCLUDE 'AGSAMPLE.INC'
C
C  Recover deposition at selected downwind locations
C
      DO NS=1,6
        TSAV(NS)=-1.0E+20
      ENDDO
      DO N=1,NR
        DO NN=1,NI
          WTEM=ABS(WDIR(NN,N)-180.0)
          IF (WTEM.LE.59.9) THEN
            IVAL(NN)=1
          ELSE
            IVAL(NN)=0
            IF (WTEM.GT.90.0) WTEM=180.0-WTEM
            WTEM=AMIN1(WTEM,59.9)
          ENDIF
          IF (ITIER.EQ.1) THEN
            YTEM=1.0/COS(0.017453292*WTEM)
            DO NX=1,NEXPT
              DVAL(NN,NX)=AGINT(NDEPA,YDEPA,ZDEPV,YTEM*YEXPT(NX))
            ENDDO
          ELSE
C            XTEM=COS(0.017453292*WTEM)*(NSPD(NN,N)+1)
C            XTEM=AMAX1(XTEM,1.0)
C            NM=MIN0(NXSPD,MAX0(1,INT(XTEM)))
C            NP=NM+1
            NS=NSPD(NN,N)
            XTEM=WTEM/10.0
            NM=XTEM
            NP=MIN0(6,NM+1)
            DO NX=1,NEXPT
              DMIN=AGINT(NDEPA,YDEPA,ZDEPV(1,NS,NM+1),YEXPT(NX))
              DMAX=AGINT(NDEPA,YDEPA,ZDEPV(1,NS,NP+1),YEXPT(NX))
              DVAL(NN,NX)=DMIN*(NP-XTEM)+DMAX*(XTEM-NM)
            ENDDO
          ENDIF
        ENDDO
C
C  Find level mean deposition values and add together
C
        TMEAN=0.0
        TSDEV=0.0
        DO NX=1,NEXPT
          XMEAN(NX)=0.0
          XSDEV(NX)=0.0
          NM=0
          DO NN=1,NI
            IF (IVAL(NN).EQ.1) THEN
              NM=NM+1
              XMEAN(NX)=XMEAN(NX)+DVAL(NN,NX)
              XSDEV(NX)=XSDEV(NX)+DVAL(NN,NX)*DVAL(NN,NX)
            ENDIF
          ENDDO
          IF (NM.GT.0) THEN
            XMEAN(NX)=XMEAN(NX)/NM
            IF (XMEAN(NX).NE.0.0)
     $        XSDEV(NX)=SQRT(ABS(XSDEV(NX)/NM-XMEAN(NX)*XMEAN(NX)))
     $                  /XMEAN(NX)
            TMEAN=TMEAN+XMEAN(NX)
            TSDEV=TSDEV+XSDEV(NX)
          ENDIF
        ENDDO
        TMEAN=TMEAN/NEXPT
        TSDEV=TSDEV/NEXPT
        TOTAL=0.0
        DO NX=1,NEXPT
          DO NN=1,NI
            IF (IVAL(NN).EQ.1.AND.XSDEV(NX).NE.0.0)
     $        TOTAL=TOTAL+TMEAN+(DVAL(NN,NX)-XMEAN(NX))*TSDEV/XSDEV(NX)
          ENDDO
        ENDDO
C
C  Sort deposition level to find 95th percentile
C
        TMIN=TSAV(1)
        NMIN=1
        DO NS=2,6
          IF (TSAV(NS).LT.TMIN) THEN
            TMIN=TSAV(NS)
            NMIN=NS
          ENDIF
        ENDDO
        IF (TOTAL.GT.TMIN) THEN
          TSAV(NMIN)=TOTAL
          NSAV(NMIN)=N
        ENDIF
      ENDDO
C
C  Find smallest of TSAV as 95th percentile repetition
C
      TMIN=TSAV(1)
      NMIN=1
      DO NS=2,6
        IF (TSAV(NS).LT.TMIN) THEN
          TMIN=TSAV(NS)
          NMIN=NS
        ENDIF
      ENDDO
      N=NSAV(NMIN)
C
C  Reconstruct the deposition array
C
      DO NN=1,NI
        WTEM=ABS(WDIR(NN,N)-180.0)
        IF (WTEM.LE.59.9) THEN
          IVAL(NN)=1
        ELSE
          IVAL(NN)=0
          IF (WTEM.GT.90.0) WTEM=180.0-WTEM
          WTEM=AMIN1(WTEM,59.9)
        ENDIF
        IF (ITIER.EQ.1) THEN
          YTEM=1.0/COS(0.017453292*WTEM)
          DO NX=1,NEXPT
            DVAL(NN,NX)=AGINT(NDEPA,YDEPA,ZDEPV,YTEM*YEXPT(NX))
          ENDDO
        ELSE
C          XTEM=COS(0.017453292*WTEM)*(NSPD(NN,N)+1)
C          XTEM=AMAX1(XTEM,1.0)
C          NM=MIN0(NXSPD,MAX0(1,INT(XTEM)))
C          NP=NM+1
          NS=NSPD(NN,N)
          XTEM=WTEM/10.0
          NM=XTEM
          NP=MIN0(6,NM+1)
          DO NX=1,NEXPT
            DMIN=AGINT(NDEPA,YDEPA,ZDEPV(1,NS,NM+1),YEXPT(NX))
            DMAX=AGINT(NDEPA,YDEPA,ZDEPV(1,NS,NP+1),YEXPT(NX))
            DVAL(NN,NX)=DMIN*(NP-XTEM)+DMAX*(XTEM-NM)
          ENDDO
        ENDIF
      ENDDO
C
C  Save results for later export to EXAMS
C
      DO NN=1,NI
        NEXMX=NEXMX+1
        IXAMV(NEXMX)=IVAL(NN)
        DO NX=1,NEXPT
          EXAMV(NX,NEXMX)=DVAL(NN,NX)
        ENDDO
      ENDDO
      RETURN
      END