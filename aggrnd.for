C**AGGRND
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGGRND(ITYPE,ITIER,XDWND,ISWTH,IDEP,NPTS,YV,DV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGGRND
!MS$ATTRIBUTES REFERENCE :: ITYPE
!MS$ATTRIBUTES REFERENCE :: ITIER
!MS$ATTRIBUTES REFERENCE :: XDWND
!MS$ATTRIBUTES REFERENCE :: ISWTH
!MS$ATTRIBUTES REFERENCE :: IDEP
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
C
C  AGGRND transfers ground sprayer ground deposition profiles
C  back to the tiers
C
C  ITYPE  - Type: 0 = Low boom fine drop size distribution (50%)
C                 1 = Low boom medium/coarse drop size distribution (50%)
C                 2 = High boom fine drop size distribution (50%)
C                 3 = High boom medium/coarse drop size distribution (50%)
C     Regulatory: 4 = Low boom fine drop size distribution (90%)
C                 5 = Low boom medium/coarse drop size distribution (90%)
C                 6 = High boom fine drop size distribution (90%)
C                 7 = High boom medium/coarse drop size distribution (90%)
C  ITIER  - Tier
C  XDWND  - Maximum downwind direction (m)
C  ISWTH  - Number of swaths
C  IDEP   - Deposition flag: 0 = deposition; 1 = pond-integrated deposition
C  NPTS   - Number of points in deposition array
C  YV     - Y distance array (m)
C  DV     - Deposition array (fraction applied)
C
      DIMENSION YV(2),DV(2)
C
      CALL AGGRNX(ITYPE,ITIER,XDWND,ISWTH,IDEP,NPTS,YV,DV)
      RETURN
      END
C**AGGRNX
      SUBROUTINE AGGRNX(ITYPE,ITIER,XDWND,ISWTH,IDEP,NPTS,YV,DV)
C
      DIMENSION YV(2),DV(2),YYV(10)
      DIMENSION XGRND(6,8),AGRND(4),BGRND(4)
C
      COMMON /TEMP/ NTEMP,YTEMP(1620),ZTEMP(1620)
      COMMON /SSBL/ SSBLF,SSBLM,SSBLS,SSBLT
C
      DATA SWATH / 13.7162 /
      DATA YYV   / 0.0, 0.03125, 0.0625, 0.125, 0.25,
     $             0.5, 1.0    , 1.5   , 2.0  , 3.0  /
      DATA XGRND / 2.6514, 1.5208, 1.0, 3.1986, 1.5085, 1.2628,
     $             4.5086, 1.4863, 1.0, 5.2539, 1.4803, 1.2205,
     $             0.4622, 1.8605, 1.0, 3.1986, 1.5085, 1.2628,
     $             2.5598, 1.5183, 1.0, 5.2539, 1.4803, 1.2205,
     $             1.3389, 1.5866, 1.0, 1.7262, 1.5548, 1.3322,
     $             6.8613, 1.2572, 1.0, 1.0455, 1.3885, 0.1419,
     $             0.4078, 1.9100, 1.0, 1.7262, 1.5548, 1.3322,
     $             4.2842, 1.2714, 1.0, 1.0455, 1.3885, 0.1419 /
      DATA AGRND / 5.4877, 1.0639, 2.1749, 0.7089 /
      DATA BGRND / 0.005057, 0.002986, 0.003888, 0.002473 /
C
C  Set Tier limits
C
      I=1+ITYPE
      J=0
      IF (I.EQ.3.OR.I.EQ.4) J=I-2
      IF (I.EQ.7.OR.I.EQ.8) J=I-4
      IF (ITIER.EQ.1.OR.ITIER.EQ.2) THEN
        NPTS=193
        NSWTH=20
      ELSE
        NPTS=XDWND/2.0+41
        NSWTH=50
      ENDIF
      ISWMN=1
      ISWMX=MIN0(NSWTH,MAX0(1,ISWTH))
      SSBLT=ISWMX*SWATH
C
C  IDEP < 0 recovers a specific spray line for AGSBLK or AGSTRM
C
      IF (IDEP.LT.0) THEN
        ISWMN=-IDEP
        ISWMX=-IDEP
        SSBLF=-(IDEP+0.5)*SWATH
        SSBLM=NSWTH*SWATH-SSBLF
        SSBLS=SWATH
        NPTS=NPTS+SSBLM/2.0+1
      ENDIF
C
C  Ground sprayers
C
      DO N=1,NPTS
        IF (N.LE.10) THEN
          YV(N)=YYV(N)
        ELSE
          YV(N)=2.0*(N-9)
        ENDIF
        DV(N)=0.0
        DO NS=ISWMN,ISWMX
          YSD=YV(N)+(NS-1)*SWATH
          IF (YSD.LT.7.6201) THEN
            DV(N)=DV(N)+XGRND(3,I)/(1.0+XGRND(1,I)*YSD)**XGRND(2,I)
          ELSE
            TEM=XGRND(6,I)/(1.0+XGRND(4,I)*YSD)**XGRND(5,I)
            IF (J.GT.0) TEM=TEM*(1.0+AGRND(J)*EXP(-BGRND(J)*YSD))
            DV(N)=DV(N)+TEM
          ENDIF
        ENDDO
      ENDDO
C
C  Compute pond-integrated deposition
C
      IF (IDEP.EQ.1) THEN
        CALL AGAVE(NPTS,YV,DV,NTEMP,YTEMP,ZTEMP)
        NPTS=NTEMP
        DO N=1,NPTS
          YV(N)=YTEMP(N)
          DV(N)=ZTEMP(N)
        ENDDO
      ELSE
        NPTS=NPTS-32
      ENDIF
      RETURN
      END