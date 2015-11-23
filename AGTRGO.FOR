C**AGTRGO
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGTRGO(NTRGO,AV,JAV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGTRGO
!MS$ATTRIBUTES REFERENCE :: NTRGO
!MS$ATTRIBUTES REFERENCE :: AV
!MS$ATTRIBUTES REFERENCE :: JAV
C
C  AGEQN integrates the equations of motion
C
C  NTRGO  - Number of active nozzles (0 to end)
C  AV     - Running position array for all nozzles (m)
C  JAV    - Position counter (1 = active; 0 = inactive)
C
      DIMENSION AV(3,60),JAV(60),SV(3,60),XNV(9,60),DV(6,60)
      DIMENSION YNR(2),ZNR(2),YNL(2),ZNL(2),YNP(4),ZNP(4)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /TRAJ/ TSLIM(6),XOV(9,60),T,DMIN,ISTT,NTRAJ,NTRPL
C
C  Integrate to TMAX
C
      DO I=1,NVAR
        DO J=1,3
          SV(J,I)=XOV(J,I)
        ENDDO
      ENDDO
      NSAVE=NSTEP+1
      NTRAJ=NTRAJ+1
C
10    NSTEP=NSTEP+1
      CALL AGBKG(XOV,DV,T,DTMN,VMAX,1)
      XMOVE=AMIN1(0.2,AMAX1(0.1,0.001*DMIN))
      DT=XMOVE*AMAX1(1.0,10.0/SQRT(DMIN))/VMAX
      IF (ISTT.EQ.0) THEN
        NTEM=1.4427*ALOG(4.0*DT/DTMN)
        IF (NTEM.GT.NSTEP) THEN
          DT=0.25*DTMN*2.0**NSTEP
        ELSE
          ISTT=1
        ENDIF
      ENDIF
      CALL AGBKG(XOV,DV,T,DTMN,VMAX,2)
C
C  Solve the equations of motion for the DT time step
C    X  Y  Z  U  V  W  XX  XU  UU
C
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          EXPT=0.0
          IF (DV(1,I).GT.0.0) EXPT=EXP(-AMIN1(DT/DV(1,I),25.0))
          TEM1=DV(2,I)+9.8*STU*DV(1,I)
          TEM2=XOV(4,I)-TEM1
          XNV(1,I)=XOV(1,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(4,I)=TEM1+TEM2*EXPT
          TEM1=DV(3,I)-9.8*CTU*STS*DV(1,I)
          TEM2=XOV(5,I)-TEM1
          XNV(2,I)=XOV(2,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(5,I)=TEM1+TEM2*EXPT
          TEM1=DV(4,I)-9.8*CTU*CTS*DV(1,I)
          TEM2=XOV(6,I)-TEM1
          XNV(3,I)=XOV(3,I)+TEM1*DT+TEM2*DV(1,I)*(1.0-EXPT)
          XNV(6,I)=TEM1+TEM2*EXPT
C
          IF (XNV(3,I).LE.ZREF) THEN
            RATE=(XOV(3,I)-ZREF)/(XOV(3,I)-XNV(3,I))
            DO J=1,9
              XNV(J,I)=XOV(J,I)+RATE*(XNV(J,I)-XOV(J,I))
            ENDDO
            XNV(3,I)=ZREF
            ISW(I)=-1
          ENDIF
        ENDIF
      ENDDO
C
C  Determine new positions of vortices
C
      IF (LMVEL.EQ.4) JHEL=1
      DO N=1,NVOR
        CALL AGVEL(XO,YBAR(N),ZBAR(N),TEM,VBAR,WBAR)
        YNR(N)=YBAR(N)+DT*VBAR
        ZNR(N)=ZBAR(N)+DT*WBAR
        CALL AGVEL(XO,YBAL(N),ZBAL(N),TEM,VBAL,WBAL)
        YNL(N)=YBAL(N)+DT*VBAL
        ZNL(N)=ZBAL(N)+DT*WBAL
      ENDDO
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          CALL AGVEL(XO-XPRP(N),YPRP(N),ZPRP(N),TEM,VBAR,WBAR)
          YNP(N)=YPRP(N)+DT*VBAR
          ZNP(N)=ZPRP(N)+DT*WBAR
        ENDDO
      ENDIF
      IF (LMVEL.EQ.4) THEN
        NVOR=0
        CALL AGVEL(XO,YHEL,ZHEL,TEM,VBAR,WBAR)
        YHEL=YHEL+DT*VBAR
        NVOR=1
        JHEL=0
      ENDIF
      DO N=1,NVOR
        YBAR(N)=YNR(N)
        ZBAR(N)=ZNR(N)
        YBAL(N)=YNL(N)
        ZBAL(N)=ZNL(N)
      ENDDO
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          YPRP(N)=YNP(N)
          ZPRP(N)=ZNP(N)
        ENDDO
      ENDIF
C
C  Correct circulation decay
C
      IF (XO.GT.0.0) THEN
        DO N=1,NVOR
          IF (ABS(GDKV(N)).GT.1.0E-10) THEN
            TEM=AMAX1(1.0,YBAR(N)-YBAL(N))
            GDKV(N)=GDKV(N)*EXP(-AMIN1(ABS(GDK*DT/TEM),25.0))
          ENDIF
        ENDDO
      ENDIF
C
C  Correct model parameters
C
      XO=XO+UO*DT
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          CPXI(N)=CPXI(N)+UO*DT
          RN=CPXI(N)/11.785
          VPRP(N)=VPRP(N)*(RPRP(N)/RN)**2
          RPRP(N)=RN
        ENDDO
      ENDIF
      IF (LMVEL.EQ.4.AND.XO.GT.0.0) THEN
        IF (XO.LT.4.0*S) THEN
          FN=EXP(-AMIN1(CHF*XO,25.0))
          G2PI(1)=CHG*(1.0-FN)
          ZBAR(1)=HHEL-FHEL*(1.0-FN)
          ZBAL(1)=HHEL-FHEL*(1.0-FN)
          WHEL=CHW*SQRT(FN)
        ELSE
          WHEL=0.0
        ENDIF
      ENDIF
C
C  Check solution and continue
C
      T=T+DT
      ISWC=0
      AVDST=0.0
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          ISWC=ISWC+1
          AVDST=AVDST+(XNV(1,I)-SV(1,I))**2
     $          +(XNV(2,I)-SV(2,I))**2+(XNV(3,I)-SV(3,I))**2
          DO J=1,9
            XOV(J,I)=XNV(J,I)
          ENDDO
          IF (LEVAP.NE.0) THEN
            EDOV(I)=AMAX1(EDNV(I),DCUT)
            DMIN=AMIN1(DMIN,EDOV(I))
          ENDIF
          IF (ABS(XNV(2,I)).GT.GRDMX) ISW(I)=-2
          IF (ISW(I).LT.0) ISW(I)=0
        ELSE
          IF (NSTEP.EQ.NSAVE) JAV(I)=0
        ENDIF
      ENDDO
      AVDST=SQRT(AVDST/MAX0(ISWC,1))
      IF (T.GE.TMAX.OR.NTRAJ.EQ.500) ISWC=0
      IF (ISWC.GT.0.AND.AVDST.LT.1.0) GO TO 10
      DO I=1,NVAR
        IF (NTRPL.EQ.1) THEN
          AV(1,I)=-XOV(1,I)*CTU-(XOV(2,I)*STS+XOV(3,I)*CTS)*STU
          AV(2,I)=XOV(2,I)*CTS-XOV(3,I)*STS
          AV(3,I)=-XOV(1,I)*STU+(XOV(2,I)*STS+XOV(3,I)*CTS)*CTU
        ELSE
          AV(1,I)=-XOV(1,I)
          AV(2,I)=XOV(2,I)
          AV(3,I)=XOV(3,I)
        ENDIF
        IF ((AV(1,I).LT.TSLIM(1).OR.AV(1,I).GT.TSLIM(2)).AND.
     $      (AV(2,I).LT.TSLIM(3).OR.AV(2,I).GT.TSLIM(4)).AND.
     $      (AV(3,I).LT.TSLIM(5).OR.AV(3,I).GT.TSLIM(6))) ISW(I)=0
      ENDDO
      NTRGO=ISWC
      RETURN
      END