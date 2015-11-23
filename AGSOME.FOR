C**AGSOME
C  Continuum Dynamics, Inc.
C  Version 2.08 07/11/03
C
      SUBROUTINE AGSOME(I,IUNIT,IER,IWR,REALWD,CHSTR,JCHSTR)
C
C  AGSOME processes the rest of the input data
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
      DIMENSION REALWD(3),DV(100),FV(100)
C
      INCLUDE 'AGCOMMON.INC'
      INCLUDE 'AGSAMPLE.INC'
C
      EQUIVALENCE (DV(1),YDEPR(1)),(FV(1),ZDEPR(1))
C
      FAC=1.0
C
C  0060  Nozzle data card
C
      IF (I.EQ.60) THEN
        IF (ICARD.EQ.34) THEN
          IF (NVAR.LT.1.OR.NVAR.GT.60) IER=2
          IWR=3
          REALWD(1)=NVAR
          REALWD(2)=1
          REALWD(3)=60
          CHSTR='Number of Nozzles'
          JCHSTR=17
        ELSEIF (ICARD.EQ.35) THEN
          FTEST=FLOW/AMAX1(SWATH,1.0)/UO/0.001585
          IF (IUNIT.EQ.0) FAC=0.2642/2.471
          CALL AGCHK(FTEST,2.3381907,280.58290,ITRTYP,
     $               0.4676381,935.27631,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Spray Volume Rate (gal/ac)'
            JCHSTR=26
          ELSE
            CHSTR='Spray Volume Rate (L/ha)'
            JCHSTR=24
          ENDIF
        ELSEIF (ICARD.EQ.36) THEN
          CALL AGCHK(DENF,0.78,1.35,3,0.4,2.5,IER,1.0,REALWD)
          IWR=2
          CHSTR='Specific Gravity of Carrier'
          JCHSTR=27
        ELSEIF (ICARD.EQ.37) THEN
          CALL AGCHK(DENN,0.4,2.5,3,0.1,10.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Specific Gravity of Nonvolatile'
          JCHSTR=31
        ELSE
          IF (ERATE.GT.0.0) THEN
            CALL AGCHK(ERATE,2.49,84.76,3,1.0,120.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Evaporation Rate ('//CHAR(181)//'m'//CHAR(178)//
     $            '/sec-deg C)'
            JCHSTR=32
          ELSE
            IF (VFRAC.GE.0.001) THEN
              IWR=1
              IER=1
              CHSTR='Zero Evaporation Rate'
              JCHSTR=21
            ENDIF
            LEVAP=0
          ENDIF
        ENDIF
C
C  0061  Nozzle horizontal location data cards
C
      ELSEIF (I.EQ.61) THEN
        IF (IUNIT.EQ.0) FAC=3.2808
        IF (ICARD.EQ.39) THEN
          N=1
        ELSE
          N=NVAR
        ENDIF
        CALL AGCHK(XS(2,N),-0.85*S,0.85*S,3,
     $             -1.25*S,1.25*S,IER,FAC,REALWD)
        IWR=2
        IF (IUNIT.EQ.0) THEN
          IF (N.EQ.1) THEN
            CHSTR='Left Nozzle Horizontal Position (ft)'
            JCHSTR=36
          ELSE
            CHSTR='Right Nozzle Horizontal Position (ft)'
            JCHSTR=37
          ENDIF
        ELSE
          IF (N.EQ.1) THEN
            CHSTR='Left Nozzle Horizontal Position (m)'
            JCHSTR=35
          ELSE
            CHSTR='Right Nozzle Horizontal Position (m)'
            JCHSTR=36
          ENDIF
        ENDIF
C
C  0062  Nozzle vertical/forward location data cards
C
      ELSEIF (I.EQ.62) THEN
        IF (IUNIT.EQ.0) FAC=3.2808
        IF (ICARD.EQ.41) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(ZOSMN,-3.51,-BOOMVT,3,
     $                 -4.8768594,-BOOMVT,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(ZOSMN,-3.51,-BOOMVT,3,
     $                 -6.0960742,-BOOMVT,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Minimum Nozzle Vertical Position (ft)'
            JCHSTR=37
          ELSE
            CHSTR='Minimum Nozzle Vertical Position (m)'
            JCHSTR=36
          ENDIF
        ELSEIF (ICARD.EQ.42) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(ZOSMX,-3.51,-BOOMVT,3,
     $                 -4.8768594,-BOOMVT,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(ZOSMX,-3.51,-BOOMVT,3,
     $                 -6.0960742,-BOOMVT,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Maximum Nozzle Vertical Position (ft)'
            JCHSTR=37
          ELSE
            CHSTR='Maximum Nozzle Vertical Position (m)'
            JCHSTR=36
          ENDIF
        ELSEIF (ICARD.EQ.43) THEN
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(XOSMX,-0.6096075,-BOOMFD,3,
     $                 -1.8288223,0.9144112-BOOMFD,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(XOSMX,-0.6096075,6.0960742,3,
     $                 -1.8288223,18.2882224,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Minimum Nozzle Forward Position (ft)'
            JCHSTR=36
          ELSE
            CHSTR='Minimum Nozzle Forward Position (m)'
            JCHSTR=35
          ENDIF
        ELSE
          IF (IACTYP.EQ.5) THEN
            CALL AGCHK(XOSMN,-0.6096075,-BOOMFD,3,
     $                 -1.8288223,0.9144112-BOOMFD,IER,FAC,REALWD)
          ELSE
            CALL AGCHK(XOSMN,-0.6096075,6.0960742,3,
     $                 -1.8288223,18.2882224,IER,FAC,REALWD)
          ENDIF
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Maximum Nozzle Forward Position (ft)'
            JCHSTR=36
          ELSE
            CHSTR='Maximum Nozzle Forward Position (m)'
            JCHSTR=35
          ENDIF
        ENDIF
C
C  0064  Droplet size data cards
C
      ELSEIF (I.EQ.64) THEN
        JCARD=ICARD-44
        JNOZL=1
        IF (JCARD.GT.4) THEN
          JCARD=JCARD-4
          JNOZL=2
          IF (JCARD.GT.4) THEN
            JCARD=JCARD-4
            JNOZL=3
          ENDIF
        ENDIF
        IF (JCARD.EQ.1) THEN
          IF (NDRP(JNOZL).LT.1.OR.NDRP(JNOZL).GT.100) IER=2
          IWR=3
          REALWD(1)=NDRP(JNOZL)
          REALWD(2)=1
          REALWD(3)=100
          CHSTR='Number of Drop Sizes'
          JCHSTR=20
        ELSEIF (JCARD.EQ.2.OR.JCARD.EQ.3) THEN
          IF (JCARD.EQ.2) THEN
            N=1
          ELSE
            N=NDRP(JNOZL)
          ENDIF
          CALL AGCHK(DIAMV(N,JNOZL),4.0,3830.0,ITRTYP,1.0,10000.0,
     $               IER,1.0,REALWD)
          IWR=2
          IF (N.EQ.1) THEN
            CHSTR='Smallest Drop Diameter ('//CHAR(181)//'m)'
            JCHSTR=27
          ELSE
            CHSTR='Largest Drop Diameter ('//CHAR(181)//'m)'
            JCHSTR=26
          ENDIF
        ELSE
          IF (NDRP(JNOZL).GT.3) THEN
            NPTS=1
            DV(1)=DIAMV(1,JNOZL)
            FV(1)=DMASS(1,JNOZL)
            DO N=2,NDRP(JNOZL)
              NDIV=0
10            NDIV=NDIV+1
              IF (DMASS(N,JNOZL)/NDIV.GT.0.02) GO TO 10
              IF (NPTS+NDIV+NDRP(JNOZL)-N.LE.100) THEN
                VMIN=DIAMV(N-1,JNOZL)**3
                VINC=(DIAMV(N,JNOZL)**3-VMIN)/NDIV
                DO NN=1,NDIV
                  NPTS=NPTS+1
                  VDD=VMIN+NN*VINC
                  DV(NPTS)=VDD**0.33333
                  FV(NPTS)=DMASS(N,JNOZL)/NDIV
                ENDDO
              ELSE
                NPTS=NPTS+1
                DV(NPTS)=DIAMV(N,JNOZL)
                FV(NPTS)=DMASS(N,JNOZL)
              ENDIF
            ENDDO
            NDRP(JNOZL)=0
            DO N=1,NPTS
              IF (FV(N).GT.0.00001) THEN
                NDRP(JNOZL)=NDRP(JNOZL)+1
                DIAMV(NDRP(JNOZL),JNOZL)=DV(N)
                DMASS(NDRP(JNOZL),JNOZL)=FV(N)
              ENDIF
            ENDDO
          ENDIF
          IF (LFMAA.EQ.1) THEN
            XMASS=0.0
            DO N=1,NDRP(JNOZL)
              XMASS=XMASS+DMASS(N,JNOZL)
            ENDDO
            CALL AGCHK(XMASS,0.95,1.05,ITRTYP,0.05,1.95,IER,1.0,REALWD)
            IF (IER.NE.0) THEN
              IWR=2
              CHSTR='Total Cumulative Volume Fraction'
              JCHSTR=32
            ENDIF
          ENDIF
        ENDIF
C
C  0065  Evaporation data card
C
      ELSEIF (I.EQ.65) THEN
        IF (ISMKY.EQ.0) THEN
          FAC=FLOW*DENN/AMAX1(SWATH,1.0)/UO/0.001585
          IF (IUNIT.EQ.0) FAC=2.2055*FAC/2.471
        ELSE
          FAC=1.0
        ENDIF
        IF (ICARD.EQ.57) THEN
          CALL AGCHK(1.0-VFRAC,0.0001,1.001,ITRTYP,
     $               0.00005,1.01,IER,FAC,REALWD)
          IWR=2
          IF (ISMKY.EQ.0) THEN
            IF (IUNIT.EQ.0) THEN
              CHSTR='Nonvolatile Rate (lbs/ac)'
              JCHSTR=25
            ELSE
              CHSTR='Nonvolatile Rate (kg/ha)'
              JCHSTR=24
            ENDIF
          ELSE
            CHSTR='Nonvolatile Fraction'
            JCHSTR=20
          ENDIF
        ELSE
          TEM=0.00005*AMIN1(1.0,1.0/FAC)
          CALL AGCHK(AFRAC,0.0001,1.001-VFRAC,ITRTYP,
     $               TEM,1.01-VFRAC,IER,FAC,REALWD)
          IWR=2
          IF (AFRAC.GT.1.001-VFRAC) THEN
            IWR=1
            IF (ISMKY.EQ.0) THEN
              CHSTR='Active Rate > Nonvolatile Rate'
              JCHSTR=30
            ELSE
              CHSTR='Active Fraction > Nonvolatile Fraction'
              JCHSTR=38
            ENDIF
          ELSE
            IF (ISMKY.EQ.0) THEN
              IF (IUNIT.EQ.0) THEN
                CHSTR='Active Rate (lbs/ac)'
                JCHSTR=20
              ELSE
                CHSTR='Active Rate (kg/ha)'
                JCHSTR=19
              ENDIF
            ELSE
              CHSTR='Active Fraction'
              JCHSTR=15
            ENDIF
          ENDIF
        ENDIF
C
C  0066  Wet bulb computation data card
C
      ELSEIF (I.EQ.66) THEN
        IF (ICARD.EQ.59) THEN
          CALL AGCHK(PRTR,992.74,1033.26,3,400.0,1600.0,IER,1.0,REALWD)
          IWR=2
          CHSTR='Ambient Pressure (mb)'
          JCHSTR=21
        ELSEIF (ICARD.EQ.60) THEN
          IF ((LFMAA.EQ.0.AND.LFMAC*LFMDR.EQ.1).OR.LFMAA.EQ.1) THEN
            CALL AGCHK(TEMPTR,0.0,51.6666667,ITRTYP,
     $                 0.0,51.6666667,IER,1.0,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              DO I=1,3
                REALWD(I)=1.8*REALWD(I)+32.0
              ENDDO
              CHSTR='Temperature (deg F)'
            ELSE
              CHSTR='Temperature (deg C)'
            ENDIF
            JCHSTR=19
          ENDIF
        ELSE
          IF ((LFMAA.EQ.0.AND.LFMAC*LFMDR.EQ.1).OR.LFMAA.EQ.1) THEN
            CALL AGCHK(RHUMTR,5.0,100.0,ITRTYP,1.0,100.0,IER,1.0,REALWD)
            IWR=2
            CHSTR='Relative Humidity (%)'
            JCHSTR=21
          ENDIF
          IF (IER.NE.2) CALL AGWTB(TEMPTR,RHUMTR,PRTR,DTEMP)
        ENDIF
C
C  0070  Apparent surface height data card
C
      ELSEIF (I.EQ.70) THEN
        IF (IUNIT.EQ.0) FAC=3.2808
        CALL AGCHK(ZREF,0.0,HOSMN-0.6096074,ITRTYP,
     $             0.0,HOSMN-0.3048037,IER,FAC,REALWD)
        IWR=2
        IF (IUNIT.EQ.0) THEN
          CHSTR='Apparent Surface Height (ft)'
          JCHSTR=28
        ELSE
          CHSTR='Apparent Surface Height (m)'
          JCHSTR=27
        ENDIF
C
C  0072  Half boom card
C
      ELSEIF (I.EQ.72) THEN
        IWR=1
        CHSTR='Half Boom Calculation'
        JCHSTR=21
C
C  0075  Multiple application assessment data cards
C
      ELSEIF (I.EQ.75) THEN
        IF (LFMAC*LFMDR.EQ.1) THEN
          IF (ICARD.EQ.64) THEN
            IWR=3
            REALWD(1)=NEVNTS
            REALWD(2)=1
            REALWD(3)=30
            CHSTR='Number of Applications (Events) per Year'
            JCHSTR=40
          ELSE
            IWR=3
            REALWD(1)=NYEARS
            REALWD(2)=1
            REALWD(3)=60
            CHSTR='Number of Years'
            JCHSTR=15
          ENDIF
        ENDIF
C
C  0085  Swath data card
C
      ELSEIF (I.EQ.85) THEN
        IF (ICARD.EQ.64) THEN
          IF (NSWTH.LT.1) IER=2
          IF (ITRTYP.EQ.2) THEN
            IF (NSWTH.GT.20) IER=2
          ELSE
            IF (NSWTH.GT.50) IER=2
          ENDIF
          IWR=3
          REALWD(1)=NSWTH
          REALWD(2)=1
          IF (ITRTYP.EQ.2) THEN
            REALWD(3)=20
          ELSE
            REALWD(3)=50
          ENDIF
          CHSTR='Number of Flight Lines'
          JCHSTR=22
        ELSE
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(SWATH,4.5720555,30.4803707,ITRTYP,
     $               3.0480370,152.4018533,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Swath Width (ft)'
            JCHSTR=16
          ELSE
            CHSTR='Swath Width (m)'
            JCHSTR=15
          ENDIF
        ENDIF
C
C  0086  Swath displacement card
C
      ELSEIF (I.EQ.86) THEN
        IF (ISDTYP.EQ.1) THEN
          CALL AGCHK(SDISP,0.1,0.9,ITRTYP,0.01,0.99,IER,1.0,REALWD)
          IWR=2
          CHSTR='Swath Displacement (fraction of applied)'
          JCHSTR=40
        ELSE
          FAC=SWATH
          IF (IUNIT.EQ.0) FAC=3.2808*FAC
          CALL AGCHK(-SDISP,-0.5,5.0,ITRTYP,-0.5,10.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Swath Displacement (ft)'
            JCHSTR=23
          ELSE
            CHSTR='Swath Displacement (m)'
            JCHSTR=22
          ENDIF
        ENDIF
C
C  0090  Deposition data cards
C
      ELSEIF (I.EQ.90) THEN
        YDEPN=-60.0
        IF (ITRTYP.EQ.3.AND.LFMAA.EQ.1) THEN
          IF (IUNIT.EQ.0) FAC=3.2808
          CALL AGCHK(YGRID,305.0,795.0,3,SWATH,1615.0,IER,FAC,REALWD)
          IWR=2
          IF (IUNIT.EQ.0) THEN
            CHSTR='Maximum Downwind Distance (ft)'
            JCHSTR=30
          ELSE
            CHSTR='Maximum Downwind Distance (m)'
            JCHSTR=29
          ENDIF
        ENDIF
        GRDMX=AMAX1(GRDMX,1.1*(YDEPX+(NSWTH-0.5)*SWATH))
C
C  0095  Vertical flux data cards
C
      ELSEIF (I.EQ.95) THEN
        IF (ISDTYP.NE.1) THEN
          FTEST=(0.5*(1-IBOOM)-SDISP)*SWATH
          IF (LFMAA.EQ.1) THEN
            IF (IUNIT.EQ.0) FAC=3.2808
            CALL AGCHK(YFLXV,0.0,304.8037065,ITRTYP,
     $                 0.0,792.4896368,IER,FAC,REALWD)
            IWR=2
            IF (IUNIT.EQ.0) THEN
              CHSTR='Flux Plane Location (ft)'
              JCHSTR=24
            ELSE
              CHSTR='Flux Plane Location (m)'
              JCHSTR=23
            ENDIF
          ENDIF
          YFLXV=YFLXV+FTEST
        ELSE
          YFLXV=0.5*SWATH
        ENDIF
        TEMH=BOOMHT+5.0*S
        TEM=AMAX1(0.5,TEMH/199)
        NFLXR=TEMH/TEM+1
        ZFLXN=0.0
        ZFLXX=TEM*(NFLXR-1)
        GRDMX=AMAX1(GRDMX,1.1*(YFLXV+(NSWTH-0.5)*SWATH))
C
C  All additional data cards
C
      ELSE
        CALL AGMORE(I,IUNIT,IER,IWR,REALWD,CHSTR,JCHSTR)
      ENDIF
      RETURN
      END