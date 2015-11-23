C**AGNND
C  Continuum Dynamics, Inc.
C  Version 2.01 05/25/01
C
      FUNCTION AGNND(FLOW,PRES,ANGLE,SPEED,DENS,DST,SV,E,VMD,SLOPE,DIAM)
C
C  AGNND computes the neural network root-normal VMD
C
C  FLOW   - Flow (L/min)
C  PRES   - Pressure (psig)
C  ANGLE  - Nozzle angle (deg)
C  SPEED  - Aircraft speed (m/s)
C  DENS   - Density (gm/cm3)
C  DST    - Dynamic surface tension (dynes/cm)
C  SV     - Shear viscosity (cP)
C  E      - Trouton ratio (elongational viscosity to shear viscosity)
C  VMD    - Nozzle reference VMD (micrometers)
C  SLOPE  - Nozzle reference slope
C  DIAM   - Equivalent orifice diameter (m)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL FLOW,PRES,ANGLE,SPEED,DENS,DST,SV,E,VMD,SLOPE,DIAM
      REAL AGNND
C
      COMMON /N2D/ AN2D(12,19)
      COMMON /N3D/ AN3D(12,19)
      COMMON /N4D/ AN4D(12,19)
C
      DIMENSION AIV(11),FV2(19),FV3(19),FV4(19)
C
      FCNTR(A,AMIN,AMAX)=2.0D0*(DMIN1(DMAX1(A,AMIN),AMAX)-AMIN)
     $                   /(AMAX-AMIN)-1.0D0
      FSUMR(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)=
     $      A0+A1*AIV(1)+A2*AIV(2)+A3*AIV(3)+A4*AIV(4)
     $      +A5*AIV(5)+A6*AIV(6)+A7*AIV(7)+A8*AIV(8)
     $      +A9*AIV(9)+A10*AIV(10)+A11*AIV(11)
C
      AIV(1)=FLOW
      AIV(2)=PRES
      AIV(3)=ANGLE
      AIV(4)=SPEED
      AIV(5)=DENS
      AIV(6)=DST
      AIV(7)=SV
      AIV(8)=E
      AIV(9)=VMD
      AIV(10)=SLOPE
      AIV(11)=DIAM
C
      AIV(1)=FCNTR(AIV(1),0.1D0,6.0D0)
      AIV(2)=FCNTR(AIV(2),10.0D0,120.0D0)
      AIV(3)=FCNTR(AIV(3),0.0D0,90.0D0)
      AIV(4)=FCNTR(AIV(4),0.0D0,75.0D0)
      AIV(5)=FCNTR(AIV(5),0.78D0,1.35D0)
      AIV(6)=FCNTR(AIV(6),25.0D0,80.0D0)
      AIV(7)=FCNTR(AIV(7),1.0D0,55.0D0)
      AIV(8)=FCNTR(AIV(8),3.0D0,600.0D0)
      AIV(9)=FCNTR(AIV(9),50.0D0,1500.0D0)
      AIV(10)=FCNTR(AIV(10),0.156457D0,0.3412903D0)
      AIV(11)=FCNTR(AIV(11),0.0002D0,0.004D0)
C
      DO N=1,19
        SUM=FSUMR(AN2D( 1,N),AN2D( 2,N),AN2D( 3,N),AN2D( 4,N),
     $            AN2D( 5,N),AN2D( 6,N),AN2D( 7,N),AN2D( 8,N),
     $            AN2D( 9,N),AN2D(10,N),AN2D(11,N),AN2D(12,N))
        FV2(N)=EXP(-SUM*SUM)
        SUM=FSUMR(AN3D( 1,N),AN3D( 2,N),AN3D( 3,N),AN3D( 4,N),
     $            AN3D( 5,N),AN3D( 6,N),AN3D( 7,N),AN3D( 8,N),
     $            AN3D( 9,N),AN3D(10,N),AN3D(11,N),AN3D(12,N))
        FV3(N)=TANH(SUM)
        SUM=FSUMR(AN4D( 1,N),AN4D( 2,N),AN4D( 3,N),AN4D( 4,N),
     $            AN4D( 5,N),AN4D( 6,N),AN4D( 7,N),AN4D( 8,N),
     $            AN4D( 9,N),AN4D(10,N),AN4D(11,N),AN4D(12,N))
        FV4(N)=1.0-EXP(-SUM*SUM)
      ENDDO
C
      SUM= 0.126731           +0.2647432 *FV2( 1) +0.3315211 *FV2( 2)
     $    -0.1901671 *FV2( 3) -0.1543798 *FV2( 4) -0.1616583 *FV2( 5)
     $    -0.02464969*FV2( 6) +0.7392632 *FV2( 7) +0.1282013 *FV2( 8)
     $    +0.3666063 *FV2( 9)-0.0009328476*FV2(10)-0.07572702*FV2(11)
     $    -0.5107323 *FV2(12) +0.2471476 *FV2(13) -0.5124    *FV2(14)
     $    +0.2538244 *FV2(15) -0.4019312 *FV2(16) -0.5142902 *FV2(17)
     $    +0.06078173*FV2(18) +0.3881461 *FV2(19)
C
      SUM=SUM -0.02959085     +0.02190127*FV3( 1) +0.07595401*FV3( 2)
     $    -0.162384  *FV3( 3) +0.0483914 *FV3( 4) +0.09540201*FV3( 5)
     $    +0.1146112 *FV3( 6) -0.3322506 *FV3( 7) +0.2007961 *FV3( 8)
     $    -0.09427102*FV3( 9) -0.1867228 *FV3(10) +0.2249944 *FV3(11)
     $    -0.03215534*FV3(12) +0.4510432 *FV3(13) +0.03464548*FV3(14)
     $    +0.3217542 *FV3(15) -0.1377321 *FV3(16) +0.03183016*FV3(17)
     $    -0.3334904 *FV3(18) +0.1294901 *FV3(19)
C
      SUM=SUM -0.1111121      -0.3928249 *FV4( 1) +0.3416864 *FV4( 2)
     $    -0.2463946 *FV4( 3) -0.4753516 *FV4( 4) -0.01173702*FV4( 5)
     $    -0.4544836 *FV4( 6) -0.2777074 *FV4( 7) -0.4711895 *FV4( 8)
     $    +0.09666353*FV4( 9) +0.05264138*FV4(10) -0.1470148 *FV4(11)
     $    -0.4649073 *FV4(12) -0.3912697 *FV4(13) -0.40594   *FV4(14)
     $    +0.07987981*FV4(15) +0.1807186 *FV4(16) -0.7603123 *FV4(17)
     $    -0.3448831 *FV4(18) +0.4567359 *FV4(19)
C
      AOV=1.0/(1.0+EXP(-SUM))
      AOV=1610.548*(AOV-0.1)/0.8+50.0
      AOV=DMIN1(DMAX1(AOV,50.0),1686.086)
      AGNND=0.95*AOV
      RETURN
      END