C**AGBKG
C  Continuum Dynamics, Inc.
C  Version 1.17 04/01/01
C
      SUBROUTINE AGBKG(XV,DV,T,DTMN,VMAX,LFL)
C
C  AGBKG evaluates the background at every drop location
C
C  XV     - Array of current locations, velocities, etc.
C  DV     - Array of background information (determined here)
C  T      - Time
C  DTMN   - Minimum time step
C  VMAX   - Maximum velocity
C  LFL    - Completion flag (1 = determine only DTMN)
C                           (2 = compute everything)
C
      DIMENSION XV(9,2),DV(6,2)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA UXI,UVI / 0.0 , 0.0 /
C
C  Loop for all drops
C
      DTMN=TMAX
      VMAX=0.1
      DO I=1,NVAR
        IF (ISW(I).NE.0) THEN
          X=XO+XV(1,I)
          Y=XV(2,I)
          Z=XV(3,I)
C
C  Determine mean velocity at the drop position
C
          CALL AGVEL(X,Y,Z,U,V,W)
          VMAX=AMAX1(VMAX,
     $         SQRT(ABS(XV(5,I)**2+XV(6,I)**2)),SQRT(ABS(V*V+W*W)))
C
C  Determine decay constant
C
          VREL=SQRT(ABS((XV(4,I)-U)**2+(XV(5,I)-V)**2+(XV(6,I)-W)**2))
C
C  Time decay evaluation
C
          D=EDOV(I)
          DENC=((D**3-DCUT**3)*DENF+DCUT**3*DENN)/D**3
          DTAU=3.12E-06*D*D*DENC
          REYNO=0.0688*D*VREL
          IF (VREL.GT.0.0)
     $      DTAU=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
          DTMN=AMAX1(0.01,AMIN1(DTMN,DTAU))
          IF (LFL.EQ.1) GO TO 20
          IF (LEVAP.NE.0) THEN
            IF (D.GT.DCUT) THEN
              EFACT=1.0
              IF (REYNO.LT.5.16) EFACT=0.4+0.116*REYNO
              DTEM=DTEMP
              IF (LCANF.GT.0.AND.Z.LE.HCAN) DTEM=DTEMC
              ETAU=D*D/DTEM/ERATE/EFACT
              IF (VREL.GT.0.0) ETAU=ETAU/(1.0+0.27*SQRT(REYNO))
              IF (ETAU.EQ.0.0) THEN
                EDNV(I)=DCUT
              ELSE
                EDNV(I)=D*SQRT(AMAX1(1.0-DT/ETAU,(DCUT/D)**2))
              ENDIF
            ENDIF
          ENDIF
C
C  Scale length
C
          SL=0.65*Z
          QQ=0.0
          DO N=1,NVOR
            R=SQRT(ABS((Y-YBAR(N))**2+(Z-ZBAR(N))**2))
            SL=AMIN1(SL,0.6*R)
            R=SQRT(ABS((Y-YBAL(N))**2+(Z-ZBAL(N))**2))
            SL=AMIN1(SL,0.6*R)
          ENDDO
          IF (SL.EQ.0.0) GO TO 10
C
C  Turbulence
C
          QQ=QQMX
          IF (LCANF.GT.0.AND.Z.LE.HCAN)
     $      QQ=QQMC*Z*Z*EXP(2.0*ALPHAC*(Z/HCAN-1.0))
          IF (NPRP.NE.0) THEN
            DO N=1,NPRP
              R=SQRT(ABS((Y-YPRP(N))**2+(Z-ZPRP(N))**2))
              E=15.174*R/CPXI(N)
              UA=11.785*CPUR/CPXI(N)/(1.0+0.25*E*E)**2
              QQ=QQ+0.2034*UA*UA
            ENDDO
          ENDIF
C
C  Determine analytic turbulent correlations with the droplet
C
          IF (QQ.NE.0.0) THEN
            WTAU=SL/(VREL+0.375*SQRT(QQ))
            C=T/WTAU
            EXPC=EXP(-AMIN1(C,25.0))
            EXPT=0.0
            IF (D.GT.0.0) EXPT=EXP(-AMIN1(T/DTAU,25.0))
            B=(DTAU/WTAU)**2
            IF (ABS(B-1.0).GT.0.01) THEN
              SUM1=0.5*(3.0-B)/(B-1.0)**2
              SUM2=0.5/(B-1.0)
              XK1=SUM1*(1.0-DTAU/WTAU)+SUM2
              XK2=SUM1*(EXPC-EXPT*DTAU/WTAU)+SUM2*EXPC*(1.0+C)
              XK3=SUM1*(EXPC-EXPT)+SUM2*EXPC*C
            ELSE
              XK1=0.375
              XK2=(3.0+3.0*C-C*C)*EXPC/8.0
              XK3=(5.0-C)*C*EXPC/8.0
            ENDIF
            XK4=0.5*(1.0+EXPC*(C-1.0))
            UXI=-DTAU*XK1+DTAU*EXPT*(XK2-XK3*DTAU/WTAU)+WTAU*XK4
            UVI=XK1-EXPT*(XK2-XK3*DTAU/WTAU)
          ENDIF
C
C  Evaluate background parameters
C
10        DV(1,I)=DTAU
          DV(2,I)=U
          DV(3,I)=V
          DV(4,I)=W
          DV(5,I)=UXI*QQ/3.0
          DV(6,I)=UVI*QQ/3.0
        ENDIF
20      CONTINUE
      ENDDO
      RETURN
      END