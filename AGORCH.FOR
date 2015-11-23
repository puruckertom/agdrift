C**AGORCH
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGORCH(ITYPE,ITIER,XDWND,IBTROW,IETROW,IDEP,
     $                  NPTS,YV,DV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGORCH
!MS$ATTRIBUTES REFERENCE :: ITYPE
!MS$ATTRIBUTES REFERENCE :: ITIER
!MS$ATTRIBUTES REFERENCE :: XDWND
!MS$ATTRIBUTES REFERENCE :: IBTROW
!MS$ATTRIBUTES REFERENCE :: IETROW
!MS$ATTRIBUTES REFERENCE :: IDEP
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
C
C  AGORCH transfers orchard airblast ground deposition profiles
C  back to the tiers
C
C  ITYPE  - Type: 0 = Normal
C                 1 = Dense
C                 2 = Sparse
C                 3 = Grapes (wrap-around)
C                 4 = Grapes (airblast)
C                 5 = Apples
C                 6 = Almonds
C                 7 = Oranges
C                 8 = Grapefruit
C                 9 = Grapefruit (mist blower)
C                10 = Small grapefruit
C                11 = Small grapefruit (mist blower)
C                12 = Pecans
C                13 = Dormant apples
C    Regulatory: 14 = Vineyard
C                15 = Orchard
C  ITIER  - Tier
C  XDWND  - Maximum downwind distance (m)
C  IBTROW - Beginning tree row
C  IETROW - Ending tree row
C  IDEP   - Deposition flag: 0 = deposition; 1 = pond-integrated deposition
C  NPTS   - Number of points in deposition array
C  YV     - Y distance array (m)
C  DV     - Deposition array (fraction applied)
C
      DIMENSION YV(2),DV(2)
C
      CALL AGORCX(ITYPE,ITIER,XDWND,IBTROW,IETROW,IDEP,NPTS,YV,DV)
      RETURN
      END
C**AGORCX
      SUBROUTINE AGORCX(ITYPE,ITIER,XDWND,IBTROW,IETROW,IDEP,
     $                  NPTS,YV,DV)
C
      DIMENSION YV(2),DV(2),YYV(10)
      DIMENSION DORCH(16),WORCH(16),FORCH(4,16),XORCH(6,16)
C
      COMMON /TEMP/ NTEMP,YTEMP(1620),ZTEMP(1620)
      COMMON /SSBL/ SSBLF,SSBLM,SSBLS,SSBLT
C
      DATA YYV   / 0.0, 0.03125, 0.0625, 0.125, 0.25,
     $             0.5, 1.0    , 1.5   , 2.0  , 3.0  /
      DATA DORCH / 4.03, 7.10, 5.95, 3.6, 3.6, 4.9, 6.7, 6.7,
     $             7.6 , 7.6 , 7.0 , 7.0, 7.0, 4.9, 3.6, 6.4 /
      DATA WORCH / 4.03, 8.27, 5.95, 3.6,  3.6, 4.9, 6.7, 6.7,
     $             7.6 , 7.6 , 7.0 , 7.0, 14.0, 4.9, 3.6, 7.4 /
      DATA FORCH / 1.0, 1.0, 0.833, 0.667,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 1.0, 1.0,
     $             1.0, 1.0, 1.0, 1.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 0.5, 0.0,
     $             1.0, 1.0, 1.0, 1.0,
     $             1.0, 1.0, 0.5, 0.0 /
      DATA XORCH / 3.3762, 1.9286,  0.9737,  4.5803, 1.4996, 0.1320,
     $             0.4264, 2.5037,  2.9451,  5.2986, 1.1886, 0.2190,
     $             0.2065, 3.3201,  5.1769,  0.5190, 2.7868, 5.2652,
     $            10.6191, 1.7868,  1.1947, 18.1554, 1.1168, 0.02105,
     $             1.1749, 2.7104,  2.4409,  5.0988, 1.7194, 0.7219,
     $            10.3876, 1.5828,  1.2169, 10.8822, 1.1252, 0.04129,
     $             1.3613, 2.7371, 35.8067,  3.0583, 1.9353, 0.7170,
     $             0.5880, 2.7642,  7.5758,  3.8397, 1.4462, 0.2911,
     $             0.3217, 3.4704, 17.8012,  0.7412, 2.7445, 6.3094,
     $             0.8487, 2.4396,  8.4885,  2.9578, 1.6231, 0.9163,
     $             0.2175, 3.2829,  7.0252,  0.7293, 2.5276, 6.4054,
     $             0.7800, 2.2655,  4.3581,  2.6206, 1.6183, 0.9538,
     $             0.3887, 2.3832,  3.2411,  6.6414, 0.9754, 0.1235,
     $             0.1788, 3.8349,  8.0537,  0.3812, 3.1353, 4.3169,
     $             1.1749, 2.7104,  2.4409,  5.0988, 1.7194, 0.7219,
     $             0.3882, 2.7872,  5.8017,  3.5101, 1.3649, 0.3620 /
C
C  Set Tier limits
C
      I=1+ITYPE
      IF (ITIER.EQ.1.OR.ITIER.EQ.2) THEN
        NPTS=193
        NSWTH=20
      ELSE
        NPTS=XDWND/2.0+41
        NSWTH=50
      ENDIF
      ISWMN=MIN0(NSWTH,MAX0(1,IBTROW))
      ISWMX=MIN0(NSWTH,MAX0(ISWMN,IETROW))
      SSBLT=(ISWMX-ISWMN+1)*WORCH(I)
C
C  IDEP < 0 recovers a specific tree row for AGSBLK or AGSTRM
C
      IF (IDEP.LT.0) THEN
        ISWMN=-IDEP
        ISWMX=-IDEP
        SSBLF=-(IDEP+1)*WORCH(I)+DORCH(I)
        SSBLM=NSWTH*WORCH(I)+DORCH(I)-SSBLF
        SSBLS=WORCH(I)
        NPTS=NPTS+SSBLM/2.0+1
      ENDIF
C
C  Orchard airblast sprayers
C
      DO N=1,NPTS
        IF (N.LE.10) THEN
          YV(N)=YYV(N)
        ELSE
          YV(N)=2.0*(N-9)
        ENDIF
        DV(N)=0.0
        DO NS=ISWMN,ISWMX
          YSD=YV(N)+(NS-1)*WORCH(I)+DORCH(I)
          IF (NS.LE.4) THEN
            DV(N)=DV(N)+FORCH(NS,I)
     $            *XORCH(3,I)/(1.0+XORCH(1,I)*YSD)**XORCH(2,I)
     $            +(1.0-FORCH(NS,I))
     $            *XORCH(6,I)/(1.0+XORCH(4,I)*YSD)**XORCH(5,I)
          ELSE
            DV(N)=DV(N)+XORCH(6,I)/(1.0+XORCH(4,I)*YSD)**XORCH(5,I)
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