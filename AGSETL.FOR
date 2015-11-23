C**AGSETL
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGSETL(SSM,NPTS,DTV,STV,DNV,SNV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSETL
!MS$ATTRIBUTES REFERENCE :: SSM
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DTV
!MS$ATTRIBUTES REFERENCE :: STV
!MS$ATTRIBUTES REFERENCE :: DNV
!MS$ATTRIBUTES REFERENCE :: SNV
C
C  AGSETL calculates settling velocity
C
C  SSM    - SMDATA data structure
C  NPTS   - Number of drop sizes
C  DTV    - Carrier drop size distribution (micrometers)
C  STV    - Carrier settling velocity (m/sec)
C  DNV    - Nonvolatile drop size distribution (micrometers)
C  SNV    - Nonvolatile settling velocity (m/sec)
C
      DIMENSION DTV(2),STV(2),DNV(2),SNV(2)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /SMDATA/ SSM
C
C  Set specific gravity and volatile fraction
C
      IF (SSM.TYPE.EQ.0) THEN
        IF (SSM.BASICTYP.EQ.0) THEN
          DENF=0.92
          DENN=0.92
        ELSE
          DENF=1.0
          DENN=1.0
        ENDIF
      ELSE
        DENF=SSM.SPECGRAV
        DENN=SSM.NONVGRAV
      ENDIF
      VFRAC=1.0-SSM.NVFRAC
C
      DNEW=4.0
      DINC=1.1
      NPTS=0
10    NPTS=NPTS+1
      DCUT=DNEW*(1.0-VFRAC)**0.33333
C
C  Carrier
C
      DTV(NPTS)=DNEW
      WTN=1.0
      DTAU=3.12E-06*DNEW*DNEW*DENF
20    WTO=WTN
      REYNO=0.0688*DNEW*WTO
      DTEM=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
      WTN=9.8*DTEM
      IF (ABS(WTN/WTO-1.0).GT.0.0001) GO TO 20
      STV(NPTS)=WTN
C
C  Nonvolatile
C
      DNV(NPTS)=DCUT
      WTN=1.0
      DTAU=3.12E-06*DCUT*DCUT*DENN
30    WTO=WTN
      REYNO=0.0688*DCUT*WTO
      DTEM=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
      WTN=9.8*DTEM
      IF (ABS(WTN/WTO-1.0).GT.0.0001) GO TO 30
      SNV(NPTS)=WTN
C
      DNEW=DNEW*DINC
      IF (DNEW.LT.4000.0) GO TO 10
      RETURN
      END