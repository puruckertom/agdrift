C**AGNNS
C  Continuum Dynamics, Inc.
C  Version 2.01 05/25/01
C
      FUNCTION AGNNS(FLOW,PRES,ANGLE,SPEED,DENS,DST,SV,E,VMD,SLOPE,DIAM)
C
C  AGNNS computes the neural network root-normal slope
C
C  FLOW   - Flow (L/min)
C  PRES   - Pressure (psig)
C  ANGLE  - Nozzle angle (deg)
C  SPEED  - Aircraft speed (m/s)
C  DENS   - Density (gm/cm3)
C  DST    - Dynamic surface tension (dynes/cm)
C  SV     - Shear viscosity (cP)
C  E      - Trouton ratio (elongational viscosity to shear viscosity)
C  VMD    - Nozzle reference vmd (micrometers)
C  SLOPE  - Nozzle reference slope
C  DIAM   - Equivalent orifice diameter (m)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL FLOW,PRES,ANGLE,SPEED,DENS,DST,SV,E,VMD,SLOPE,DIAM
      REAL AGNNS
C
      DIMENSION AIV(11),FV2(19),FV3(19),FV4(19)
C
      COMMON /N2S/ AN2S(12,19)
      COMMON /N3S/ AN3S(12,19)
      COMMON /N4S/ AN4S(12,19)
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
      AIV(11)=FCNTR(AIV(11),0.0005D0,0.004D0)
C
      DO N=1,19
        SUM=FSUMR(AN2S( 1,N),AN2S( 2,N),AN2S( 3,N),AN2S( 4,N),
     $            AN2S( 5,N),AN2S( 6,N),AN2S( 7,N),AN2S( 8,N),
     $            AN2S( 9,N),AN2S(10,N),AN2S(11,N),AN2S(12,N))
        FV2(N)=EXP(-SUM*SUM)
        SUM=FSUMR(AN3S( 1,N),AN3S( 2,N),AN3S( 3,N),AN3S( 4,N),
     $            AN3S( 5,N),AN3S( 6,N),AN3S( 7,N),AN3S( 8,N),
     $            AN3S( 9,N),AN3S(10,N),AN3S(11,N),AN3S(12,N))
        FV3(N)=TANH(SUM)
        SUM=FSUMR(AN4S( 1,N),AN4S( 2,N),AN4S( 3,N),AN4S( 4,N),
     $            AN4S( 5,N),AN4S( 6,N),AN4S( 7,N),AN4S( 8,N),
     $            AN4S( 9,N),AN4S(10,N),AN4S(11,N),AN4S(12,N))
        FV4(N)=1.0-EXP(-SUM*SUM)
      ENDDO
C
      SUM= 0.4890545          +0.2914752 *FV2( 1) +0.4514982 *FV2( 2)
     $    +0.312638  *FV2( 3) +0.7622963 *FV2( 4) +0.6069253 *FV2( 5)
     $    +0.5538972 *FV2( 6) +0.3275014 *FV2( 7) +0.4199648 *FV2( 8)
     $    +0.5333502 *FV2( 9) -0.1373974 *FV2(10) -0.2488236 *FV2(11)
     $    +0.1326094 *FV2(12) +0.3368584 *FV2(13) -0.4217912 *FV2(14)
     $    +0.6295778 *FV2(15) -0.3777581 *FV2(16) +0.00992193*FV2(17)
     $    +0.5021474 *FV2(18) -0.3080629 *FV2(19)
C
      SUM=SUM +0.3327582      -0.04109107*FV3( 1) +0.02198887*FV3( 2)
     $    -0.3894097 *FV3( 3) -0.405326  *FV3( 4) +0.05389751*FV3( 5)
     $    -0.04326699*FV3( 6) -0.6678625 *FV3( 7) +0.8150558 *FV3( 8)
     $    -0.2678666 *FV3( 9) +0.3069418 *FV3(10) +0.5842834 *FV3(11)
     $    -0.7717303 *FV3(12) +0.8292698 *FV3(13) +0.1667272 *FV3(14)
     $    -0.24502   *FV3(15) -0.227306  *FV3(16) +0.4215285 *FV3(17)
     $    -0.06441888*FV3(18) +0.8057373 *FV3(19)
C
      SUM=SUM +0.2512354      -0.7218335 *FV4( 1) +0.3615306 *FV4( 2)
     $    -0.3612305 *FV4( 3) -0.4923548 *FV4( 4) +0.7073628 *FV4( 5)
     $    -0.36618   *FV4( 6) -0.3554559 *FV4( 7) -0.5658603 *FV4( 8)
     $    -0.5025348 *FV4( 9) +0.511806  *FV4(10) -0.561632  *FV4(11)
     $    -0.7268416 *FV4(12) -0.06014237*FV4(13) -0.5948579 *FV4(14)
     $    +0.1252143 *FV4(15) +0.4768883 *FV4(16) +0.2422339 *FV4(17)
     $    +0.3425831 *FV4(18) +0.4092532 *FV4(19)
C
      AOV=1.0/(1.0+EXP(-SUM))
      AOV=0.3925497*(AOV-0.1)/0.8+0.1459794
      AOV=DMIN1(DMAX1(AOV,0.1459794),0.5385291)
      AGNNS=AOV
      RETURN
      END