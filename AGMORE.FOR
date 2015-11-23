C**AGMORE
C  Continuum Dynamics, Inc.
C  Version 2.06 05/23/02
C
      SUBROUTINE AGMORE(I,IUNIT,IER,IWR,REALWD,CHSTR,JCHSTR)
C
C  AGMORE processes the rest of the input data
C
C  I      - Card number
C  IUNIT  - Units flag: 0 = English; 1 = metric
C  IER    - Error flag: 0 = No warning or error message
C                       1 = Write warning information
C                       2 = Write error information
C                       3 = No more data
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
      INCLUDE 'AGCOMMON.INC'
      INCLUDE 'AGSAMPLE.INC'
C
      FAC=1.0
C
C  0100  0110  0120  0125  0130  Canopy description data cards
C
      IF (I.EQ.100) THEN
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(HCAN,0.0,0.9*(BOOMHT+ZOSMN),ITRTYP,0.0,BOOMHT+ZOSMN,
     $             IER,FAC,REALWD)
        IWR=2
        IF (IUNIT.EQ.0) THEN
          CHSTR='Canopy Height (ft)'
          JCHSTR=18
        ELSE
          CHSTR='Canopy Height (m)'
          JCHSTR=17
        ENDIF
C
      ELSEIF (I.EQ.110) THEN
        IF (ICARD.EQ.69) THEN
          FAC=100.0
          IF (IUNIT.EQ.0) FAC=39.3696
          CALL AGCHK(ESIZE,0.001,0.25,3,0.001,0.25,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Element Size (in)'
            JCHSTR=24
          ELSE
            CHSTR='Canopy Element Size (cm)'
            JCHSTR=24
          ENDIF
        ELSEIF (ICARD.EQ.70) THEN
          CALL AGCHK(TEMPC,0.0,51.6666667,3,
     $                 0.0,51.6666667,IER,1.0,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            DO I=1,3
              REALWD(I)=1.8*REALWD(I)+32.0
            ENDDO
            CHSTR='Canopy Temperature (deg F)'
          ELSE
            CHSTR='Canopy Temperature (deg C)'
          ENDIF
          JCHSTR=26
        ELSEIF (ICARD.EQ.71) THEN
          CALL AGCHK(RHUMC,5.0,100.0,3,1.0,100.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Canopy Relative Humidity (%)'
          JCHSTR=28
          IF (IER.NE.2) CALL AGWTB(TEMPC,RHUMC,PRTR,DTEMC)
        ELSEIF (ICARD.EQ.72) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(ZOC,0.01,0.5,ITRTYP,0.01,DOC,IER,FAC*HCAN,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Roughness (ft)'
            JCHSTR=21
          ELSE
            CHSTR='Canopy Roughness (m)'
            JCHSTR=20
          ENDIF
        ELSE
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(DOC,0.01,0.9,ITRTYP,0.01,1.0,IER,FAC*HCAN,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Displacement (ft)'
            JCHSTR=24
          ELSE
            CHSTR='Canopy Displacement (m)'
            JCHSTR=23
          ENDIF
        ENDIF
C
      ELSEIF (I.EQ.120) THEN
        IF (ICARD.EQ.74) THEN
          IF (IUNIT.EQ.0) FAC=1.0/2.471
          CALL AGCHK(STEMS,1.0,1500.0,3,1.0,1500.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Stand Density (st/ac)'
            JCHSTR=21
          ELSE
            CHSTR='Stand Density (st/ha)'
            JCHSTR=21
          ENDIF
        ELSEIF (ICARD.EQ.75) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANHV(1),0.0,BOOMHT,3,0.0,DIST,IER,FAC,REALWD)
          IF (IER.NE.0) THEN
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Smallest Story Tree Height (ft)'
              JCHSTR=31
            ELSE
              CHSTR='Smallest Story Tree Height (m)'
              JCHSTR=30
            ENDIF
          ENDIF
        ELSEIF (ICARD.EQ.76) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANHV(NCAN),0.0,0.9*(BOOMHT+ZOSMN),3,0.0,
     $               BOOMHT+ZOSMN,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Height (ft)'
            JCHSTR=18
          ELSE
            CHSTR='Canopy Height (m)'
            JCHSTR=17
          ENDIF
        ELSEIF (ICARD.EQ.77) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANIMN,0.0,100.0,3,0.0,100.0,IER,FAC,REALWD)
          IF (IER.NE.0) THEN
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Smallest Story Tree Diameter (ft)'
              JCHSTR=33
            ELSE
              CHSTR='Smallest Story Tree Diameter (m)'
              JCHSTR=32
            ENDIF
          ENDIF
        ELSEIF (ICARD.EQ.78) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANIMX,0.0,100.0,3,0.0,100.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Largest Story Tree Diameter (ft)'
            JCHSTR=32
          ELSE
            CHSTR='Largest Story Tree Diameter (m)'
            JCHSTR=31
          ENDIF
        ELSEIF (ICARD.EQ.79) THEN
          CALL AGCHK(CANPMN,0.0,1.0,3,0.0,1.0,IER,FAC,REALWD)
          IF (IER.NE.0) THEN
            IWR=2
            CHSTR='Smallest Penetration Probability'
            JCHSTR=32
          ENDIF
        ELSE
          CALL AGCHK(CANPMX,0.0,1.0,3,0.0,1.0,IER,FAC,REALWD)
          IWR=2
          CHSTR='Largest Penetration Probability'
          JCHSTR=31
        ENDIF
C
      ELSEIF (I.EQ.125) THEN
        IF (ICARD.EQ.74) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANHV(1),0.0,BOOMHT,3,0.0,DIST,IER,FAC,REALWD)
          IF (IER.NE.0) THEN
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Smallest Optical Tree Height (ft)'
              JCHSTR=33
            ELSE
              CHSTR='Smallest Optical Tree Height (m)'
              JCHSTR=32
            ENDIF
          ENDIF
        ELSEIF (ICARD.EQ.75) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANHV(NCAN),0.0,0.9*(BOOMHT+ZOSMN),3,0.0,
     $               BOOMHT+ZOSMN,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Height (ft)'
            JCHSTR=18
          ELSE
            CHSTR='Canopy Height (m)'
            JCHSTR=17
          ENDIF
        ELSEIF (ICARD.EQ.76) THEN
          CALL AGCHK(CANIMN,0.0,10.0,3,0.0,10.0,IER,1.0,REALWD)
          IF (IER.NE.0) THEN
            IWR=2
            CHSTR='Smallest Cumulative LAI'
            JCHSTR=23
          ENDIF
        ELSEIF (ICARD.EQ.77) THEN
          CALL AGCHK(CANIMX,0.0,10.0,3,0.0,10.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Largest Cumulative LAI'
          JCHSTR=23
        ELSE
          IF (ICANMN.NE.0) THEN
            IER=2
            REALWD(1)=CANIV(ICANMN)
            REALWD(2)=0.0
            REALWD(3)=CANIV(ICANMN-1)
            IWR=2
            CHSTR='Noncumulative LAI'
            JCHSTR=17
          ENDIF
        ENDIF
C
      ELSEIF (I.EQ.130) THEN
        IF (ICARD.EQ.74) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(CANHV(1),0.0,0.9*(BOOMHT+ZOSMN),3,0.0,BOOMHT+ZOSMN,
     $               IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Canopy Height (ft)'
            JCHSTR=18
          ELSE
            CHSTR='Canopy Height (m)'
            JCHSTR=17
          ENDIF
        ELSE
          CALL AGCHK(CANIV(1),0.0,10.0,3,0.0,10.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Cumulative LAI'
          JCHSTR=14
        ENDIF
C
C  0140  Terrain data cards
C
      ELSEIF (I.EQ.140) THEN
        IF (ICARD.EQ.81) THEN
          CALL AGCHK(ANGTU,-45.0,45.0,3,-60.0,60.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Terrain Upslope Angle (deg)'
          JCHSTR=27
        ELSEIF (ICARD.EQ.82) THEN
          CALL AGCHK(ANGTS,-45.0,45.0,3,-60.0,60.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Terrain Sideslope Angle (deg)'
          JCHSTR=29
        ELSE
          IF (HOSMN.LE.0.0) THEN
            IER=2
            IWR=1
            CHSTR='Terrain Interferes With Aircraft'
            JCHSTR=32
          ENDIF
        ENDIF
C
C  Error messages
C
      ELSE
        IER=3
      ENDIF
      RETURN
      END