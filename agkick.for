C**AGKICK
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGKICK(DDK,IUNIT,LFL,ICLS,NPTS,DDV,XXV,IER,
     $                  REALWD,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGKICK
!MS$ATTRIBUTES REFERENCE :: DDK
!MS$ATTRIBUTES REFERENCE :: IUNIT
!MS$ATTRIBUTES REFERENCE :: LFL
!MS$ATTRIBUTES REFERENCE :: ICLS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DDV
!MS$ATTRIBUTES REFERENCE :: XXV
!MS$ATTRIBUTES REFERENCE :: IER
!MS$ATTRIBUTES REFERENCE :: REALWD
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGKICK runs the DropKick analysis, then reconstructs the
C  drop size distribution by calling the appropriate function
C
C  DDK    - DKDATA data structure
C  IUNIT  - Units flag: 0 = English; 1 = metric
C  LFL    - Operations flag: 0 = initialization of calculation
C  ICLS   - Size class flag: -1 = no; 0-10 = class to use
C  NPTS   - Number of points in drop size distribution
C  DDV    - Drop size distribution array
C  XXV    - Volume fraction array
C  IER    - Error flag: 0 = no error -- result acceptable
C                       1 = warning with real data and character string
C                       2 = error with real data and character string
C                       3 = warning with character string only
C                       4 = error with character string only
C                       5 = information with character string
C  REALWD - Real data array (value, minimum, maximum)
C  CHSTR  - Character string
C  JCHSTR - Length of character string
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /DKDATA/ DDK
C
      CHARACTER*40 CHSTR
C
      DIMENSION REALWD(3),DDV(2),XXV(2)
C
C  Set all of the necessary parameters for DropKick regressions
C
      IER=0
C
      VMDRF=DDK.VMD
      IF (LFL.EQ.0) THEN
        LFL=1
        CALL AGCHK(VMDRF,197.8,1400.1,3,25.0,2500.0,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Volume Median Diameter ('//CHAR(181)//'m)'
          JCHSTR=27
          RETURN
        ENDIF
      ENDIF
C
      RSRF=DDK.RSPAN
      IF (LFL.EQ.1) THEN
        LFL=2
        CALL AGCHK(RSRF,0.8020,1.7664,3,0.5,2.5,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Relative Span'
          JCHSTR=13
          RETURN
        ENDIF
      ENDIF
      SLRF=RSRF/5.126915
C
      EDRF=DDK.EFFDIAM
      IF (LFL.EQ.2) THEN
        LFL=3
        CALL AGCHK(EDRF,0.068,0.343,3,0.02,0.50,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Effective Nozzle Diameter (cm)'
          JCHSTR=30
          RETURN
        ENDIF
      ENDIF
      EDRF=0.01*EDRF
C
C  Spray angle is not used in this version of DropKick
C
      R=DDK.SPRANGLE
      IF (LFL.EQ.3) THEN
        LFL=4
        CALL AGCHK(R,0.0,120.0,3,0.0,150.0,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Spray Angle (deg)'
          JCHSTR=17
          RETURN
        ENDIF
      ENDIF
C
      T=DDK.SURFTENS
      IF (LFL.EQ.4) THEN
        LFL=5
        CALL AGCHK(T,25.0,80.0,3,10.0,100.0,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Dynamic Surface Tension (dynes/cm)'
          JCHSTR=34
          RETURN
        ENDIF
      ENDIF
C
      V=DDK.SHEARVIS
      IF (LFL.EQ.5) THEN
        LFL=6
        CALL AGCHK(V,0.9,54.0,3,0.3,100.0,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Shear Viscosity (cp)'
          JCHSTR=20
          RETURN
        ENDIF
      ENDIF
C
      E=DDK.ELONGVIS
      IF (LFL.EQ.6) THEN
        LFL=7
        CALL AGCHK(E,0.9,2296.2,3,V,3000.0,IER,1.0,REALWD)
        IF (E.LT.V) THEN
          IER=3
          CHSTR='Elongational Visc less than Shear Visc'
          JCHSTR=38
          RETURN
        ELSE
          IF (IER.GT.0) THEN
            CHSTR='Elongational Viscosity (cp)'
            JCHSTR=27
            RETURN
          ENDIF
        ENDIF
      ENDIF
      E=E/V
C
      SG=DDK.DENSITY
      IF (LFL.EQ.7) THEN
        LFL=8
        CALL AGCHK(SG,0.78,1.35,3,0.4,2.5,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Specific Gravity'
          JCHSTR=16
          RETURN
        ENDIF
      ENDIF
C
      S=DDK.SPEED
      IF (LFL.EQ.8) THEN
        LFL=9
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=1.0/0.447
        CALL AGCHK(S,17.88,71.52,3,4.47,111.75,IER,FAC,REALWD)
        IF (IER.GT.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Air Speed (mph)'
          ELSE
            CHSTR='Air Speed (m/s)'
          ENDIF
          JCHSTR=15
          RETURN
        ENDIF
      ENDIF
C
      A=DDK.NOZANGLE
      IF (LFL.EQ.9) THEN
        LFL=10
        CALL AGCHK(A,0.0,90.0,3,0.0,150.0,IER,1.0,REALWD)
        IF (IER.GT.0) THEN
          CHSTR='Nozzle Orientation (deg)'
          JCHSTR=24
          RETURN
        ENDIF
      ENDIF
C
      P=DDK.PRESSURE
      IF (LFL.EQ.10) THEN
        LFL=11
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=14.5
        CALL AGCHK(P,0.65,8.32,3,0.2,25.0,IER,FAC,REALWD)
        IF (IER.GT.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Pressure (psig)'
            JCHSTR=15
          ELSE
            CHSTR='Pressure (bar)'
            JCHSTR=14
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
C  Neither is relative speed
C
C      RELS=S-COS(0.0174533*A)*SQRT(200.0*P/SG)
      P=14.5*P
C
      Q=DDK.FLOW
      IF (LFL.EQ.11) THEN
        LFL=12
        FAC=1.0
        IF (IUNIT.EQ.0) FAC=0.2642
        CALL AGCHK(Q,0.491,12.752,3,0.2,20.0,IER,FAC,REALWD)
        IF (IER.GT.0) THEN
          IF (IUNIT.EQ.0) THEN
            CHSTR='Flow Rate per Nozzle (gal/min)'
            JCHSTR=15
          ELSE
            CHSTR='Flow Rate per Nozzle (L/min)'
            JCHSTR=14
          ENDIF
          RETURN
        ENDIF
      ENDIF
C
C  Compute the DropKick parameters and construct the distribution
C
      IF (LFL.EQ.12) THEN
        VMDNEW=AGNND(Q,P,A,S,SG,T,V,E,VMDRF,SLRF,EDRF)
        SLNEW=AGNNS(Q,P,A,S,SG,T,V,E,VMDRF,SLRF,EDRF)
        RSNEW=5.126915*SLNEW
        LTEM=DDK.SPRTYPE
        CALL AGPARX(LTEM,ICLS,0,VMDNEW,RSNEW,NPTS,DDV,XXV)
      ENDIF
      RETURN
      END