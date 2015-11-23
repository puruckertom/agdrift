C**AGSMCK
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGSMCK(ITRTYP,TPAA,RHAA,NEV,NYR,PROB,NTSPD,IWR,
     $                  REALWD,CHSTR,JCHSTR)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSMCK
!MS$ATTRIBUTES REFERENCE :: ITRTYP
!MS$ATTRIBUTES REFERENCE :: TPAA
!MS$ATTRIBUTES REFERENCE :: RHAA
!MS$ATTRIBUTES REFERENCE :: NEV
!MS$ATTRIBUTES REFERENCE :: NYR
!MS$ATTRIBUTES REFERENCE :: PROB
!MS$ATTRIBUTES REFERENCE :: NTSPD
!MS$ATTRIBUTES REFERENCE :: IWR
!MS$ATTRIBUTES REFERENCE :: REALWD
!MS$ATTRIBUTES REFERENCE :: CHSTR
!MS$ATTRIBUTES REFERENCE :: JCHSTR
C
C  AGSMCK checks multiple application assessment inputs and fills
C  AgDRIFT common blocks appropriately
C
C  ITRTYP - Tier number
C  TPAA   - Temperature (deg C)
C  RHAA   - Relative humidity (%)
C  NEV    - Number of applications (events) per year
C  NYR    - Number of years
C  PROB   - Probability distribution array
C  NTSPD  - Number of wind speeds to compute with AgDRIFT
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
      DIMENSION REALWD(3),PROB(36,20)
C
      INCLUDE 'AGSAMPLE.INC'
C
      JCHSTR=0
      ITIER=ITRTYP
C
C  Set multiple application assessment parameters
C
      IF (ITRTYP.GT.1) THEN
        TEMPAA=TPAA
        RHUMAA=RHAA
      ENDIF
      NEVNTS=NEV
      NYEARS=NYR
      NEG=0
      TOT=0.0
      DO NF=2,20
        TEM=0.0
        DO N=1,36
          TEM=TEM+PROB(N,NF)
          IF (PROB(N,NF).LT.0.0) NEG=NEG+1
          FREQ(N,NF-1)=PROB(N,NF)
        ENDDO
        TOT=TOT+TEM
        IF (TEM.GT.0.0) NXSPD=NF-1
      ENDDO
      NTSPD=NXSPD+1
C
C  Check limits
C
      IF (ITRTYP.GT.1) THEN
        CALL AGCHK(TEMPAA,0.0,51.6666667,ITRTYP,
     $             0.0,51.6666667,IWR,1.0,REALWD)
        IF (IWR.EQ.2) THEN
          CHSTR='Temperature (deg C)'
          JCHSTR=19
          RETURN
        ENDIF
        CALL AGCHK(RHUMAA,5.0,100.0,ITRTYP,1.0,100.0,IWR,1.0,REALWD)
        IF (IWR.EQ.2) THEN
          CHSTR='Relative Humidity (%)'
          JCHSTR=21
          RETURN
        ENDIF
      ENDIF
      IF (NEVNTS.LT.1.OR.NEVNTS.GT.30) THEN
        IWR=3
        REALWD(1)=NEVNTS
        REALWD(2)=1
        REALWD(3)=30
        CHSTR='Number of Applications (Events) per Year'
        JCHSTR=40
        RETURN
      ENDIF
      IF (NYEARS.LT.1.OR.NYEARS.GT.60) THEN
        IWR=3
        REALWD(1)=NYEARS
        REALWD(2)=1
        REALWD(3)=60
        CHSTR='Number of Years'
        JCHSTR=15
        RETURN
      ENDIF
      IF (NEG.GT.0) THEN
        IWR=1
        CHSTR='Negative entries in Probability Table'
        JCHSTR=37
        RETURN
      ENDIF
      CALL AGCHK(TOT,0.999,1.001,3,0.999,1.001,IWR,1.0,REALWD)
      IF (IWR.EQ.2) THEN
        CHSTR='Total Probability'
        JCHSTR=17
        RETURN
      ENDIF
      IWR=0
      RETURN
      END