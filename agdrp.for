C**AGDRP
C  Continuum Dynamics, Inc.
C  Version 2.02 06/01/01
C
      SUBROUTINE AGDRP(UD,DDROP,DRELH,DDIAM,DDIST,TTIME,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDRP
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: DDROP
!MS$ATTRIBUTES REFERENCE :: DRELH
!MS$ATTRIBUTES REFERENCE :: DDIAM
!MS$ATTRIBUTES REFERENCE :: DDIST
!MS$ATTRIBUTES REFERENCE :: TTIME
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGDRP computes the drop distance information
C
C  UD     - USERDATA data structure
C  DDROP  - Drop diameter at initialization (micrometers)
C  DRELH  - Release height (m)
C  DDIAM  - Drop diameter at the ground (micrometers)
C  DDIST  - Downwind distance traveled by drop (m)
C  TTIME  - Time to reach ground (sec)
C  CHSTR  - Character string
C  JCHSTR - Length of character string (0 = null)
C
      CHARACTER*40 CHSTR
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      DIMENSION XV(6),DV(4),DB(6)
C
      DATA DB / 0.9144112, 30.4803707,
     $          0.9144112, 30.4803707,
     $          0.3048038, 91.4111200 /
      DATA TPI / 6.2831853 /
C
C  Check bounds on drop diameter and release height
C
      JCHSTR=0
      IF (DDROP.LT.10.0.OR.DDROP.GT.3000.0) THEN
        DDROP=AMAX1(10.0,AMIN1(3000.0,DDROP))
        CHSTR='Drop Diameter'
        JCHSTR=13
      ENDIF
      ITIER=2*UD.TIER-1
      IF (DRELH.LT.DB(ITIER).OR.DRELH.GT.DB(ITIER+1)) THEN
        DRELH=AMAX1(DB(ITIER),AMIN1(DB(ITIER+1),DRELH))
        IF (JCHSTR.NE.0) THEN
          CHSTR(JCHSTR+1:)=', '
          JCHSTR=JCHSTR+2
        ENDIF
        CHSTR(JCHSTR+1:)='Release Height'
        JCHSTR=JCHSTR+14
      ENDIF
      JEND=0
C
C  Establish initial values based on data structure
C
      CTU=1.0
      STU=0.0
      CTS=1.0
      STS=0.0
      IF (UD.TIER.EQ.3.AND.UD.SMOKEY.EQ.1) THEN
        CTU=COS(UD.TRN.UPSLOPE*TPI/360.0)
        STU=SIN(UD.TRN.UPSLOPE*TPI/360.0)
        CTS=COS(UD.TRN.SIDESLOPE*TPI/360.0)
        STS=SIN(UD.TRN.SIDESLOPE*TPI/360.0)
      ENDIF
      XV(1)=0.0
      XV(2)=DRELH*STS
      XV(3)=DRELH*CTS
      XV(4)=0.0
      XV(5)=0.0
      XV(6)=0.0
      IF (UD.SM.TYPE.EQ.0) THEN
        IF (UD.SM.BASICTYP.EQ.0) THEN
          DENF=0.92
          DENN=0.92
          ERATE=0.0
          LEVAP=0
        ELSE
          DENF=1.0
          DENN=1.0
          ERATE=84.76
          LEVAP=1
        ENDIF
      ELSE
        DENF=UD.SM.SPECGRAV
        DENN=UD.SM.NONVGRAV
        ERATE=UD.SM.EVAPRATE
        LEVAP=2
        IF (ERATE.EQ.0.0) LEVAP=0
      ENDIF
      VFRAC=1.0-UD.SM.NVFRAC
      IF (ABS(VFRAC).LT.0.001) LEVAP=0
      DIAM=DDROP
      DCUT=DIAM*(1.0-VFRAC)**0.33333
      IF (UD.TIER.EQ.1.OR.UD.TIER.EQ.2) THEN
        ZTEM=2.0
      ELSE
        ZTEM=UD.MET.WINDHGT
      ENDIF
      ZO=UD.MET.SURFRUFF
      WINDSP=UD.MET.WINDSPD
      USK=WINDSP/ALOG((ZTEM+ZO)/ZO)
      IF (UD.TIER.EQ.2) THEN
        CCW=0.0
        SCW=-1.0
      ELSE
        CCW=COS(UD.MET.WINDDIR*TPI/360.0)
        SCW=SIN(UD.MET.WINDDIR*TPI/360.0)
      ENDIF
      TEMPTR=UD.MET.TEMP
      RHUMTR=UD.MET.HUMIDITY
      IF (UD.TIER.EQ.1.OR.UD.TIER.EQ.2) THEN
        PRTR=1013.0
        ZREF=0.0
      ELSE
        PRTR=UD.MET.PRESSURE
        ZREF=UD.TRN.ZREF
      ENDIF
      CALL AGWTB(TEMPTR,RHUMTR,PRTR,DTEMP)
C
      LCANF=0
      IF (UD.SMOKEY.EQ.1) THEN
        HCAN=UD.CAN.HEIGHT
        IF (UD.TIER.EQ.2) THEN
          ZREF=AMAX1(ZREF,HCAN)
          IF (HCAN.GT.0.0) THEN
            LCANF=-1
            ZOC=UD.CAN.NDRUFF
            DOC=UD.CAN.NDDISP
          ENDIF
        ELSE
          LCANF=UD.CAN.TYPE
          IF (LCANF.NE.0) THEN
            ZOC=UD.CAN.NDRUFF
            DOC=UD.CAN.NDDISP
            IF (LCANF.EQ.3) THEN
              LCANF=-1
              ZREF=AMAX1(ZREF,HCAN)
            ENDIF
            ZTEM=2.0*UD.CTL.HEIGHT
            WINDSP=USK*ALOG((ZTEM+ZO)/ZO)
            TEM=1.0-DOC+ZOC
            ALPHAC=1.0/TEM/ALOG(TEM/ZOC)
            UOPN=WINDSP/ALOG((ZTEM/HCAN-DOC+ZOC)/ZOC)
            UCAN=UOPN*ALOG(TEM/ZOC)
            TTEM=UD.CAN.TEMP
            HTEM=UD.CAN.HUMIDITY
            CALL AGWTB(TTEM,HTEM,PRTR,DTEMC)
          ENDIF
        ENDIF
      ENDIF
C
C  Initialize integration
C
      DMIN=DIAM
      ISTT=0
      NSTEP=0
      T=0.0
      DT=0.0
C
C  Integrate to the ground
C
10    NSTEP=NSTEP+1
C
C  Determine mean velocity at the drop position
C
      IF (LCANF.EQ.0) THEN
        B=USK*ALOG((XV(3)+ZO)/ZO)
      ELSE
        IF (XV(3).LE.HCAN) THEN
          B=UCAN*EXP(ALPHAC*(XV(3)/HCAN-1.0))
        ELSE
          B=UOPN*ALOG((XV(3)/HCAN-DOC+ZOC)/ZOC)
        ENDIF
      ENDIF
      U=B*CCW
      V=-B*SCW
      W=0.0
      VMAX=AMAX1(0.1,SQRT(ABS(XV(5)**2+XV(6)**2)),SQRT(ABS(V*V+W*W)))
C
C  Determine decay constant
C
      VREL=SQRT(ABS((XV(4)-U)**2+(XV(5)-V)**2+(XV(6)-W)**2))
C
C  Time decay evaluation
C
      DENC=((DIAM**3-DCUT**3)*DENF+DCUT**3*DENN)/DIAM**3
      DTAU=3.12E-06*DIAM*DIAM*DENC
      REYNO=0.0688*DIAM*VREL
      IF (VREL.GT.0.0)
     $  DTAU=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
      DTMN=AMAX1(0.01,DTAU)
      IF (LEVAP.NE.0) THEN
        IF (DIAM.GT.DCUT) THEN
          EFACT=1.0
          IF (REYNO.LT.5.16) EFACT=0.4+0.116*REYNO
          DTEM=DTEMP
          IF (LCANF.GT.0.AND.XV(2).LE.HCAN) DTEM=DTEMC
          ETAU=DIAM*DIAM/DTEM/ERATE/EFACT
          IF (VREL.GT.0.0) ETAU=ETAU/(1.0+0.27*SQRT(REYNO))
          IF (ETAU.EQ.0.0) THEN
            DIAM=DCUT
          ELSE
            DIAM=DIAM*SQRT(AMAX1(1.0-DT/ETAU,(DCUT/DIAM)**2))
          ENDIF
        ENDIF
      ENDIF
C
C  Evaluate background parameters
C
      DV(1)=DTAU
      DV(2)=U
      DV(3)=V
      DV(4)=W
C
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
      TEM=4.0*(200.0/DMIN)**3
      DT=DT*2.0**AMAX1(2.0,AMIN1(4.0,TEM))
C
C  Solve the equations of motion for the DT time step
C    X  Y  Z  U  V  W
C
      EXPT=0.0
      IF (DV(1).GT.0.0) EXPT=EXP(-AMIN1(DT/DV(1),25.0))
      TEM1=DV(2)+9.8*STU*DV(1)
      TEM2=XV(4)-TEM1
      XV(1)=XV(1)+TEM1*DT+TEM2*DV(1)*(1.0-EXPT)
      XV(4)=TEM1+TEM2*EXPT
      TEM1=DV(3)-9.8*CTU*STS*DV(1)
      TEM2=XV(5)-TEM1
      XV(2)=XV(2)+TEM1*DT+TEM2*DV(1)*(1.0-EXPT)
      XV(5)=TEM1+TEM2*EXPT
      TEM1=DV(4)-9.8*CTU*CTS*DV(1)
      TEM2=XV(6)-TEM1
      XV(3)=XV(3)+TEM1*DT+TEM2*DV(1)*(1.0-EXPT)
      XV(6)=TEM1+TEM2*EXPT
C
C  Check solution and continue
C
      IF (LEVAP.NE.0) THEN
        DIAM=AMAX1(DIAM,DCUT)
        DMIN=AMIN1(DMIN,DIAM)
      ENDIF
      T=T+DT
      DDIST=SQRT(ABS(XV(1)**2+XV(2)**2))
      IF (DDIST.GT.3048.03706) THEN
        IF (JCHSTR.NE.0) THEN
          CHSTR(JCHSTR+1:)=', '
          JCHSTR=JCHSTR+2
        ENDIF
        CHSTR(JCHSTR+1:)='Distance'
        JCHSTR=JCHSTR+8
        JEND=1
      ELSEIF (XV(3).GT.ZREF) THEN
        GO TO 10
      ENDIF
C
C  Save needed results
C
      IF (JEND.EQ.0) THEN
        DDIAM=DIAM
        TTIME=T
      ELSE
        DDIAM=-1.0
        TTIME=-1.0
      ENDIF
      RETURN
      END