C**AGREAD
C  Continuum Dynamics, Inc.
C  Version 2.09 10/19/05
C
      SUBROUTINE AGREAD(IUNIT,IER,IDK,IWR,REALWD,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGREAD
!MS$ATTRIBUTES REFERENCE :: IUNIT
!MS$ATTRIBUTES REFERENCE :: IER
!MS$ATTRIBUTES REFERENCE :: IDK
!MS$ATTRIBUTES REFERENCE :: IWR
!MS$ATTRIBUTES REFERENCE :: REALWD
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGREAD processes all input data
C
C  IUNIT  - Units flag: 0 = English; 1 = metric
C  IER    - Error flag: 0 = No warning or error message
C                       1 = Write warning information
C                       2 = Write error information
C                       3 = No more data
C                       4 = Write DSD model warning message in Tier III
C                       5 = Write DSD model error message in Tier II
C  IDK    - Extra flag: 0 = DSD model density value in REALWD(1)
C                       1 = DSD model speed value in REALWD(1)
C                       2 = DSD model pressure value in REALWD(1)
C  IWR    - Write flag: 0 = No write to screen
C                       1 = String only to screen
C                       2 = String plus real value to screen
C                       3 = String plus integer value to screen
C  REALWD - Real data array (value, minimum, maximum)
C  CHSTR  - Character string
C  JCHSTR - Length of character string (0 = null)
C
      CHARACTER*40 CHSTR
C
      DIMENSION REALWD(3)
C
      CALL AGREAX(IUNIT,IER,IDK,IWR,REALWD,CHSTR,JCHSTR)
      RETURN
      END
C**AGREAX
      SUBROUTINE AGREAX(IUNIT,IER,IDK,IWR,REALWD,CHSTR,JCHSTR)
C
      CHARACTER*40 CHSTR
      CHARACTER*27 CSTAB(7)
C
      DIMENSION REALWD(3),JSTAB(7)
C
      INCLUDE 'AGCOMMON.INC'
      INCLUDE 'AGSAMPLE.INC'
C
      DATA TPI / 6.2831853 /
      DATA CSTAB / 'Strong Solar Insolation    ',
     $             'Moderate Solar Insolation  ',
     $             'Slight Solar Insolation    ',
     $             'Weak Solar Insolation      ',
     $             'Overcast Cloud Cover       ',
     $             'Thinly Overcast Cloud Cover',
     $             '< 3/8th Cloud Cover        ' /
      DATA JSTAB / 23 , 25 , 23 , 21 , 20 , 27 , 19 / 
C
      IER=0
      IWR=0
      JCHSTR=0
      ICARD=ICARD+1
      I=ICV(ICARD)
      FAC=1.0
C
C  0000  Comment cards
C
      IF (I.EQ.0) THEN
        J=0
C
C  0010  DropKick inconsistency card
C
      ELSEIF (I.EQ.10) THEN
        IER=7-ITRTYP
        IWR=2
        JCARD=ICARD
        JNOZL=1
        IF (JCARD.GT.3) THEN
          JCARD=JCARD-3
          JNOZL=2
          IF (JCARD.GT.3) THEN
            JCARD=JCARD-3
            JNOZL=3
          ENDIF
        ENDIF
        IF (JCARD.EQ.1) THEN
          IDK=0
          REALWD(1)=JNOZL-1
          REALWD(2)=DENF
          REALWD(3)=DKDENF(JNOZL)
          CHSTR='Specific gravity mismatch in DSD model'
          JCHSTR=38
          IF (IDKD(JNOZL).EQ.1) IER=4
        ELSEIF (JCARD.EQ.2) THEN
          IDK=1
          REALWD(1)=JNOZL-1
          REALWD(2)=UO
          REALWD(3)=DKSPD(JNOZL)
          CHSTR='Speed (m/s) mismatch in DSD model'
          JCHSTR=33
        ELSE
          IDK=2
          REALWD(1)=JNOZL-1
          REALWD(2)=QFLOW
          REALWD(3)=DKFLOW(JNOZL)
          CHSTR='Flow rate (L/min) mismatch in DSD model'
          JCHSTR=39
        ENDIF
C
C  0015  System card
C
      ELSEIF (I.EQ.15) THEN
        IF (ICARD.EQ.28) THEN
          CALL AGCHK(TMAX,588.0,612.0,3,120.0,86400.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Maximum Computation Time (s)'
          JCHSTR=28
        ELSE
          CALL AGCHK(GDK,1.10,1.14,3,0.0,2.24,IER,0.5,REALWD)
          IWR=2
          CHSTR='Vortex Decay Rate (m/s)'
          JCHSTR=23
        ENDIF
C
C  0020  Aircraft characteristics card
C
      ELSEIF (I.EQ.20) THEN
        IF (IUNIT.EQ.0) FAC=3.2808
        IF (ICARD.EQ.10) THEN
          IWR=1
          CHSTR=CACNM
          JCHSTR=LACNM
        ELSEIF (ICARD.EQ.11) THEN
          IF (ISMKY.EQ.0) THEN
            CALL AGCHK(S,3.5757,8.9277,3,1.7879,40.8212,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(S,3.5757,20.4106,3,1.7879,40.8212,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IACTYP.EQ.5) THEN
            IF (IUNIT.EQ.0) THEN
              CHSTR='Semispan (ft)'
              JCHSTR=13
            ELSE
              CHSTR='Semispan (m)'
              JCHSTR=12
            ENDIF
          ELSE
            IF (IUNIT.EQ.0) THEN
              CHSTR='Rotor Radius (ft)'
              JCHSTR=17
            ELSE
              CHSTR='Rotor Radius (m)'
              JCHSTR=16
            ENDIF
          ENDIF
        ELSEIF (ICARD.EQ.12) THEN
          IF (ISMKY.EQ.0) THEN
            CALL AGCHK(BOOMHT,0.9144111-ZOSMN,9.1441112,ITRTYP,
     $                 0.3048037-ZOSMN,91.4411120,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(BOOMHT,0.9144111-ZOSMN,45.7205560,ITRTYP,
     $                 0.3048037-ZOSMN,91.4411120,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Boom Height (ft)'
            JCHSTR=16
          ELSE
            CHSTR='Boom Height (m)'
            JCHSTR=15
          ENDIF
        ELSEIF (ICARD.EQ.13) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(BOOMVT,-1.5240186,0.0,3,
     $                 -4.8768594,0.0,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(BOOMVT,-3.5402,-1.9614,3,
     $                 -6.0960742,0.0,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Boom Vertical Position (ft)'
            JCHSTR=27
          ELSE
            CHSTR='Boom Vertical Position (m)'
            JCHSTR=26
          ENDIF
        ELSEIF (ICARD.EQ.14) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(BOOMFD,-0.6096075,0.0,3,
     $                 -1.8288223,0.9144112,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(BOOMFD,-0.6096075,6.0960742,3,
     $                 -1.8288223,18.2882224,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Boom Forward Position (ft)'
            JCHSTR=26
          ELSE
            CHSTR='Boom Forward Position (m)'
            JCHSTR=25
          ENDIF
        ELSEIF (ICARD.EQ.15) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(WINGVT,0.0,1.8779,3,
     $                 0.0,4.8768594,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Wing Vertical Position (ft)'
              JCHSTR=27
            ELSE
              CHSTR='Wing Vertical Position (m)'
              JCHSTR=26
            ENDIF
          ENDIF
        ELSE
          IF (IUNIT.EQ.0) FAC=1.0/0.447
          IF (ISMKY.EQ.0) THEN
            CALL AGCHK(UO,17.88,101.03,3,4.47,157.86,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(UO,17.88,155.76,3,4.47,240.97,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Flying Speed (mph)'
          ELSE
            CHSTR='Flying Speed (m/s)'
          ENDIF
          JCHSTR=18
        ENDIF
C
C  0021  Biplane characteristics card
C
      ELSEIF (I.EQ.21) THEN
        DZBP=DZBPD
        IF (DZBP.NE.0.0) THEN
          PSBP=1.0
          PGBP=1.0
          IF (LFMAA.EQ.1) THEN
            IF (IUNIT.EQ.0) FAC=3.2808
            CALL AGCHK(DZBP,1.5087,2.3089,3,
     $                 0.3048037,2.4384297,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Biplane Wing Separation (ft)'
              JCHSTR=28
            ELSE
              CHSTR='Biplane Wing Separation (m)'
              JCHSTR=27
            ENDIF
          ENDIF
        ENDIF
C
C  0023  Wing loading card
C
      ELSEIF (I.EQ.23) THEN
        IF (IACTYP.EQ.5.AND.LFMAA.EQ.1) THEN
          FAC=1.0/9.81
          IF (IUNIT.EQ.0) FAC=1.0/4.448
          IF (ISMKY.EQ.0) THEN
            CALL AGCHK(WT,4305.0,54746.0,3,444.8,444800.0,
     $                 IER,FAC,REALWD)
          ELSE
            CALL AGCHK(WT,4305.0,624678.0,3,444.8,1249356.0,
     $                 IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Weight (lbs)'
            JCHSTR=12
          ELSE
            CHSTR='Weight (kg)'
            JCHSTR=11
          ENDIF
        ENDIF
        NVOR=1
        G2PIS(1)=WT/(1.0+PGBP)/S/UO/TPI/1.9267
        YBARS(1)=0.7854*S
        ZBARS(1)=DIST
        YBALS(1)=-0.7854*S
        ZBALS(1)=DIST
        IF (DZBP.GT.0.0) THEN
          NVOR=2
          G2PIS(2)=G2PIS(1)*PGBP/PSBP
          Y=0.7854*S*PSBP
          Z=DIST+DZBP
          YBARS(2)=Y
          ZBARS(2)=Z
          YBALS(2)=-Y
          ZBALS(2)=Z
        ENDIF
        RLIM=0.1*S
C
C  0028  Crosswind card
C
      ELSEIF (I.EQ.28) THEN
        IF (ICARD.EQ.30) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(ZO,0.005,0.0488,3,0.001,1.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Surface Roughness (ft)'
            JCHSTR=22
          ELSE
            CHSTR='Surface Roughness (m)'
            JCHSTR=21
          ENDIF
        ELSEIF (ICARD.EQ.31) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(WINDHT,1.96,2.04,3,1.0,30.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Wind Speed Height (ft)'
            JCHSTR=22
          ELSE
            CHSTR='Wind Speed Height (m)'
            JCHSTR=21
          ENDIF
        ELSEIF (ICARD.EQ.32) THEN
          IF (LFMAA.EQ.0) THEN
            CALL AGCHK(WINDDR,-100.0,-20.0,3,-100.0,-20.0,
     $                 IER,1.0,REALWD)
          ELSE
            IF (ISMKY.EQ.0) THEN
              CALL AGCHK(WINDDR,-91.8,-88.2,3,-150.0,-30.0,
     $                   IER,1.0,REALWD)
            ELSE
              CALL AGCHK(WINDDR,-91.8,-88.2,3,-170.0,-10.0,
     $                   IER,1.0,REALWD)
            ENDIF
          ENDIF
          IWR=2
          CHSTR='Wind Direction (deg)'
          JCHSTR=20
        ELSEIF (ICARD.EQ.33) THEN
          IF (IUNIT.EQ.0) FAC=1.0/0.447
          IF (LFMAA.EQ.0) THEN
            CALL AGCHK(WINDSP,0.9,20.1,ITRTYP,0.9,20.1,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(WINDSP,0.447,8.94,ITRTYP,
     $                 0.2235,17.88,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Wind Speed (mph)'
          ELSE
            CHSTR='Wind Speed (m/s)'
          ENDIF
          JCHSTR=16
          LMCRS=1
          USK=WINDSP/ALOG((WINDHT+ZO)/ZO)
          QQMX=0.845*QSTAB*USK**2
          USK=WINDSP/(ALOG((WINDHT+ZO)/ZO)-PSTAB)
          TEM=WINDDR*TPI/360.0
          CCW=USK*COS(TEM)
          SCW=USK*SIN(TEM)
          CHZ=ZO
        ELSE
          IWR=1
          CHSTR=CSTAB(KSTAB)
          JCHSTR=JSTAB(KSTAB)
        ENDIF
C
C  0030  Helicopter input card
C
      ELSEIF (I.EQ.30) THEN
        IF (IACTYP.EQ.6) THEN
          IF (ICARD.EQ.17) THEN
            CALL AGCHK(BDOT,262.0,503.0,3,100.0,1000.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Helicopter RPM'
            JCHSTR=14
          ELSEIF (LFMAA.EQ.1) THEN
            FAC=1.0/9.81
            IF (IUNIT.EQ.0) FAC=1.0/4.448
            IF (ISMKY.EQ.0) THEN
              CALL AGCHK(WT,4262.0,54746.0,3,444.8,137428.0,
     $                   IER,FAC,REALWD)
            ELSE
              CALL AGCHK(WT,4262.0,68714.0,3,444.8,137428.0,
     $                   IER,FAC,REALWD)
            ENDIF
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Weight (lbs)'
              JCHSTR=12
            ELSE
              CHSTR='Weight (kg)'
              JCHSTR=11
            ENDIF
          ENDIF
        ENDIF
        IF (ICARD.EQ.18.AND.IER.NE.2) THEN
          RHEL=S
          XMU=9.549*UO/S/BDOT
          NVOR=1
          CHG=WT/S/UO/TPI/1.9267
          G2PIS(1)=0.0
          YBARS(1)=0.7854*S
          ZBARS(1)=DIST
          YBALS(1)=-0.7854*S
          ZBALS(1)=DIST
          RLIM=0.1*S
          CHW=SQRT(WT/TPI/1.2266)/S
          WHEL=CHW
          YHELS=0.0
          ZHELS=DIST
          CHF=1.0/S
          HHEL=DIST
          FHEL=AMIN1(0.5*DIST,S)
        ENDIF
C
C  0040  Propeller input card
C
      ELSEIF (I.EQ.40) THEN
        IF (IACTYP.EQ.5) THEN
          IF (ICARD.EQ.19) THEN
            CALL AGCHK(DRAG,0.098,0.102,3,0.02,1.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Aircraft Drag Coefficient'
            JCHSTR=25
          ELSEIF (ICARD.EQ.20) THEN
            CALL AGCHK(PROP,0.784,0.816,3,0.5,1.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Propeller Efficiency'
            JCHSTR=20
          ELSEIF (ICARD.EQ.21) THEN
            IF (IUNIT.EQ.0) FAC=10.7636
            IF (ISMKY.EQ.0) THEN
              CALL AGCHK(AS,16.722,41.622,3,
     $                   3.7162287,92.9057193,IER,FAC,REALWD)
            ELSE
              CALL AGCHK(AS,13.888,168.433,3,
     $                   3.7162287,278.7158984,IER,FAC,REALWD)
            ENDIF
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Planform Area (ft'//CHAR(178)//')'
              JCHSTR=19
            ELSE
              CHSTR='Planform Area (m'//CHAR(178)//')'
              JCHSTR=18
            ENDIF
          ELSEIF (ICARD.EQ.22) THEN
            CALL AGCHK(TDOT,890.0,3030.0,3,
     $                 100.0,10000.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Propeller RPM'
            JCHSTR=13
          ELSEIF (ICARD.EQ.23) THEN
            IF (IUNIT.EQ.0) FAC=3.2808
            CALL AGCHK(RPRPS,0.6336,2.3089,3,
     $                 0.3048037,3.6576445,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Propeller Radius (ft)'
              JCHSTR=21
            ELSE
              CHSTR='Propeller Radius (m)'
              JCHSTR=20
            ENDIF
          ELSEIF (ICARD.EQ.24) THEN
            IF (IUNIT.EQ.0) FAC=3.2808
            CALL AGCHK(DZPRP,-1.446,1.816,3,
     $                 -3.6576445,3.6576445,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Engine Vertical Position (ft)'
              JCHSTR=29
            ELSE
              CHSTR='Engine Vertical Position (m)'
              JCHSTR=28
            ENDIF
          ELSEIF (LFMAA.EQ.1) THEN
            IF (IUNIT.EQ.0) FAC=3.2808
            CALL AGCHK(-XPRPS,2.715,11.544,3,
     $                 0.0,14.6305780,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Engine Forward Position (ft)'
              JCHSTR=28
            ELSE
              CHSTR='Engine Forward Position (m)'
              JCHSTR=27
            ENDIF
          ENDIF
        ENDIF
        IF (ICARD.EQ.25.AND.IER.NE.2) THEN
          APRP=0.5*TPI*RPRPS**2
          UI=0.5*UO*(-1.0+SQRT(1.0+DRAG*AS/APRP))
          VPRPS=60.0*DRAG*AS*UO**3/(TPI*PROP*TDOT*APRP*RPRPS*(UO+UI))
          CPXIS=11.785*RPRPS
          CPUR=UI*RPRPS
          ZPRPS=BOOMHT+DZPRP-BOOMVT
        ENDIF
C
C  0045  Propeller horizontal input card
C
      ELSEIF (I.EQ.45) THEN
        IF (IACTYP.EQ.5) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          IF (ICARD.EQ.26) THEN
            CALL AGCHK(YPRPS(1),-S,S,3,-S,S,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Engine Horizontal Position (ft)'
              JCHSTR=31
            ELSE
              CHSTR='Engine Horizontal Position (m)'
              JCHSTR=30
            ENDIF
          ELSE
            CALL AGCHK(YPRPS(3),-S,S,3,-S,S,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Engine Horizontal Position (ft)'
              JCHSTR=31
            ELSE
              CHSTR='Engine Horizontal Position (m)'
              JCHSTR=30
            ENDIF
          ENDIF
        ENDIF
C
C  All additional data cards
C
      ELSE
        CALL AGSOME(I,IUNIT,IER,IWR,REALWD,CHSTR,JCHSTR)
      ENDIF
      RETURN
      END