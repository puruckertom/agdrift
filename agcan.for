C**AGCAN
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGCAN(XOV,XNV,DO,DN,CAN)
C
C  AGCAN handles canopy penetration
C
C  XOV    - Old position array
C  XNV    - New position array
C  DO     - Old droplet diameter (micrometers)
C  DN     - New droplet diameter (micrometers)
C  CAN    - Canopy factor for this nozzle and droplet
C
      DIMENSION XOV(6),XNV(6),XTEM(6)
      DIMENSION XKV(22),XCV(22),XXV(22)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA XKV /   0.2 ,   0.3 ,   0.4 ,   0.5 ,   0.6 ,   0.7 ,
     $             0.8 ,   0.9 ,   1.0 ,   2.0 ,   3.0 ,   4.0 ,
     $             5.0 ,   6.0 ,   7.0 ,   8.0 ,   9.0 ,  10.0 ,
     $            20.0 ,  30.0 ,  40.0 ,  50.0 /
      DATA XCV / 0.027 , 0.091 , 0.153 , 0.213 , 0.257 , 0.300 ,
     $           0.343 , 0.377 , 0.405 , 0.588 , 0.686 , 0.750 ,
     $           0.794 , 0.827 , 0.850 , 0.868 , 0.884 , 0.893 ,
     $           0.946 , 0.961 , 0.968 , 0.973 /
      DATA XXV / 0.007 , 0.020 , 0.036 , 0.050 , 0.054 , 0.059 ,
     $           0.068 , 0.074 , 0.073 , 0.070 , 0.060 , 0.059 ,
     $           0.052 , 0.052 , 0.050 , 0.050 , 0.050 , 0.048 ,
     $           0.042 , 0.036 , 0.033 , 0.029 /
C
      DH=XNV(3)-XOV(3)
      NH=MAX0(IFIX(ABS(DH)/0.1)+1,2)
      XTEM(3)=XOV(3)
      DO N=2,NH
        XTEMO=XTEM(3)
        DO I=1,6
          XTEM(I)=XOV(I)+(N-1)*(XNV(I)-XOV(I))/(NH-1)
        ENDDO
        IF (XTEM(3).GT.ZREF.AND.XTEM(3).LT.CANHV(NCAN)) THEN
          DTEM=SQRT(ABS(DO*DO+(N-1)*(DN*DN-DO*DO)/(NH-1)))
C
C  Collection efficiency
C
          UTEM=SQRT(ABS(XTEM(4)*XTEM(4)+XTEM(5)*XTEM(5)
     $         +XTEM(6)*XTEM(6)))
          VTEM=SQRT(ABS(XTEM(4)*XTEM(4)+XTEM(5)*XTEM(5)))
          DENC=((DTEM**3-DCUT**3)*DENF+DCUT**3*DENN)/DTEM**3
          STK=DENC*DTEM*DTEM*UTEM/ESIZE/1600.0
          EFF=AGINT(22,XKV,XCV,STK)
          DXX=AGINT(22,XKV,XXV,STK)
          PHI=AMAX1(7.6*ESIZE*UTEM/DENC,1.0)
          EFF=EFF-0.25*DXX*ALOG10(PHI)**2
          CINT=1.0+0.33*STK**(-0.96)
          EFF=EFF+0.0001*CINT*DTEM/ESIZE
          EFF=AMAX1(AMIN1(EFF,1.0),0.0)
C
C  Story canopy
C
          IF (LCANF.EQ.1) THEN
            WIDTH=AGINT(NCAN,CANHV,CANIV,XTEM(3))
            DZ=ABS(DH)/(NH-1)
            IF (DZ.EQ.0.0) THEN
              XLK=0.0
            ELSE
              XLK=UTEM/(VTEM+0.7854*WIDTH*ABS(XTEM(6))/DZ)
            ENDIF
            PROB=AGINT(NCAN,CANHV,CANPV,XTEM(3))
            PK=1.0-EFF*(1.0-PROB**XLK)
            IF (XTEM(6).EQ.0.0) THEN
              TTEM=0.0
            ELSE
              TTEM=VTEM*DZ/ABS(XTEM(6))
            ENDIF
            FRAC=0.0001*(0.7854*WIDTH+TTEM)*WIDTH*STEMS
            IFRAC=FRAC
            FRAC=FRAC-IFRAC
            PTK=(1.0-FRAC*(1.0-PK))*PK**IFRAC
C
C  Optical canopy
C
          ELSE
            IF (LCANF.EQ.2) THEN
              DLAI=ABS(AGINT(NCAN,CANHV,CANIV,XTEMO)
     $                -AGINT(NCAN,CANHV,CANIV,XTEM(3)))
            ELSE
              DLAI=ABS(WBULL(CANHV(1),CANIV(1),BBULL,CBULL,XTEMO)
     $                -WBULL(CANHV(1),CANIV(1),BBULL,CBULL,XTEM(3)))
            ENDIF
            ZEN=ABS(XTEM(6))/UTEM
            IF (ZEN.LT.0.0001) THEN
              PK=0.0
            ELSE
              PK=AMIN1(EXP(-AMIN1(DLAI/ZEN,25.0)),1.0)
            ENDIF
            PTK=1.0-EFF*(1.0-PK)
          ENDIF
          CAN=PTK*CAN
        ENDIF
      ENDDO
      RETURN
      END
C**WBULL
      FUNCTION WBULL(XH,XL,XB,XC,XZ)
C
C  WBULL recovers the Weibull Distribution
C
C  XH     - Tree height (m)
C  XL     - Cumulative LAI
C  XB     - Weibull fitting coefficient b
C  XC     - Weibull fitting coefficient c
C  XZ     - Height desired (m)
C
      WBULL=XL*(1.0-EXP(-AMIN1(((1.0-AMIN1(XZ/XH,1.0))/XB)**XC,25.0)))
      RETURN
      END