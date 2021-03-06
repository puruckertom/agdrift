C**AGTOX
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGTOX(UD,NUMD,DEPD,DEPV,NUMP,PIDD,PIDV,
     $                 ISTYPE,INTYPE,XLENG,XDEEP,XACT,XPOND,
     $                 XAPPL,XDEPS,XDEPP,XCONC,NUMU,USRD,USRV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGTOX
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: NUMD
!MS$ATTRIBUTES REFERENCE :: DEPD
!MS$ATTRIBUTES REFERENCE :: DEPV
!MS$ATTRIBUTES REFERENCE :: NUMP
!MS$ATTRIBUTES REFERENCE :: PIDD
!MS$ATTRIBUTES REFERENCE :: PIDV
!MS$ATTRIBUTES REFERENCE :: ISTYPE
!MS$ATTRIBUTES REFERENCE :: INTYPE
!MS$ATTRIBUTES REFERENCE :: XLENG
!MS$ATTRIBUTES REFERENCE :: XDEEP
!MS$ATTRIBUTES REFERENCE :: XACT
!MS$ATTRIBUTES REFERENCE :: XPOND
!MS$ATTRIBUTES REFERENCE :: XAPPL
!MS$ATTRIBUTES REFERENCE :: XDEPS
!MS$ATTRIBUTES REFERENCE :: XDEPP
!MS$ATTRIBUTES REFERENCE :: XCONC
!MS$ATTRIBUTES REFERENCE :: NUMU
!MS$ATTRIBUTES REFERENCE :: USRD
!MS$ATTRIBUTES REFERENCE :: USRV
C
C  AGTOX performs the aquatic assessment calculations
C
C  UD     - USERDATA data structure
C  NUMD   - Number of points in deposition array
C  DEPD   - Downwind distance array (m)
C  DEPV   - Deposition array (fraction applied)
C  NUMP   - Number of points in pond-integrated deposition array
C  PIDD   - Downwind distance array (m)
C  PIDV   - Pond-integrated deposition array (fraction applied)
C  ISTYPE - Selection type: 0 = EPA pond
C                           1 = EPA wetland
C                           2 = User-defined
C  INTYPE - Input type: 0 = Downwind distance known
C                       1 = Fraction applied known
C                       2 = g/ha known
C                       3 = lb/ac known
C                       4 = ng/L known
C  XLENG  - Length of pond (m)
C  XDEEP  - Pond depth (m)
C  XACT   - Active fraction (Tier I only)
C  XPOND  - Distance to pond (m)
C  XAPPL  - Average deposition on pond (fraction applied)
C  XDEPS  - Average deposition on pond (g/ha)
C  XDEPP  - Average deposition on pond (lb/ac)
C  XCONC  - Average pond concentration (ng/L)
C  NUMU   - Number of points in user-defined pond-integrated deposition array
C  USRD   - Downwind distance array (m)
C  USRV   - User-defined pond-integrated deposition array (fraction applied)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      DIMENSION DEPD(2),DEPV(2),PIDD(2),PIDV(2),USRD(2),USRV(2)
C
      COMMON /TEMP/ NTEMP,YTEMP(1620),ZTEMP(1620)
      COMMON /TBLK/ YN(4900),ZN(4900)
C
C  Compute pond-integrated deposition
C
      IF (ISTYPE.EQ.0.OR.ISTYPE.EQ.1) THEN
        NUMU=0
        NTEMP=NUMP
        DO N=1,NTEMP
          YTEMP(N)=PIDD(N)
          ZTEMP(N)=PIDV(N)
        ENDDO
      ELSE
        CALL AGEXTD(NUMD,DEPD,DEPV,XLENG,NNTEMP,YTEMP,ZTEMP)
        NUMU=NUMD
        NTEMP=NUMD
        DO N=1,NTEMP
          USRD(N)=YTEMP(N)
          NN=N
          XAVE=0.5*(YTEMP(NN+1)-YTEMP(NN))*(ZTEMP(NN+1)+ZTEMP(NN))
10        NN=NN+1
          IF (NN.LT.NNTEMP) THEN
            IF (YTEMP(NN+1).LT.DEPD(N)+XLENG) THEN
              XAVE=XAVE+0.5*(YTEMP(NN+1)-YTEMP(NN))
     $                     *(ZTEMP(NN+1)+ZTEMP(NN))
              GO TO 10
            ELSE
              DD=AGINT(NNTEMP,YTEMP,ZTEMP,DEPD(N)+XLENG)
              XAVE=XAVE+0.5*(DEPD(N)+XLENG-YTEMP(NN))*(DD+ZTEMP(NN))
            ENDIF
          ENDIF
          ZN(N)=XAVE/AMAX1(XLENG,0.1)
        ENDDO
        DO N=1,NTEMP
          ZTEMP(N)=ZN(N)
          USRV(N)=ZTEMP(N)
        ENDDO
      ENDIF
C
C  Compute conversion factors
C
      IF (UD.TIER.EQ.1) THEN
        ACTIVE=XACT*UD.SM.FLOWRATE
      ELSE
        ACTIVE=UD.SM.ACFRAC*UD.SM.FLOWRATE*UD.SM.NONVGRAV
      ENDIF
      IF (XLENG.LT.0.1.OR.XLENG.GT.YTEMP(NTEMP)) ACTIVE=-1.0
      IF (XDEEP.LE.0.01.OR.XDEEP.GT.100.0) ACTIVE=-1.0
      X1=1000.0*ACTIVE
      X2=100000.0*ACTIVE
      X3=X1/1120.66
C
C  Convert requests to fraction applied
C
      IF (INTYPE.EQ.0) THEN
        IF (ACTIVE.LE.0.0.OR.XPOND.LT.0.0.OR.XPOND.GT.YTEMP(NTEMP)) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=AGINT(NTEMP,YTEMP,ZTEMP,XPOND)
        ENDIF
      ELSEIF (INTYPE.EQ.2) THEN
        IF (ACTIVE.LE.0.0.OR.XDEPS.LE.0.0) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=XDEPS/X1
        ENDIF
      ELSEIF (INTYPE.EQ.3) THEN
        IF (ACTIVE.LE.0.0.OR.XDEPP.LE.0.0) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=XDEPP/X3
        ENDIF
      ELSEIF (INTYPE.EQ.4) THEN
        IF (ACTIVE.LE.0.0.OR.XCONC.LE.0.0) THEN
          XAPPL=-1.0
        ELSE
          XAPPL=XCONC*XDEEP/X2
        ENDIF
      ENDIF
C
C  Compute distance to fraction applied
C
      IF (INTYPE.NE.0) THEN
        TEMM=1.0
        DO N=1,NTEMP
          TEMM=AMIN1(TEMM,ZTEMP(N))
          ZTEMP(N)=-ZTEMP(N)
        ENDDO
        IF (XAPPL.LT.TEMM) THEN
          XPOND=-1.0
        ELSE
          XPOND=AGINT(NTEMP,ZTEMP,YTEMP,-XAPPL)
          IPOND=IFIX(XPOND)
          IF (XPOND.GT.FLOAT(IPOND)) IPOND=IPOND+1
          XPOND=FLOAT(IPOND)
        ENDIF
      ENDIF
C
C  Convert results
C
      IF (INTYPE.NE.2) THEN
        IF (ACTIVE.LE.0.0.OR.XAPPL.LE.0.0) THEN
          XDEPS=-1.0
        ELSE
          XDEPS=X1*XAPPL
        ENDIF
      ENDIF
      IF (INTYPE.NE.3) THEN
        IF (ACTIVE.LE.0.0.OR.XAPPL.LE.0.0) THEN
          XDEPP=-1.0
        ELSE
          XDEPP=X3*XAPPL
        ENDIF
      ENDIF
      IF (INTYPE.NE.4) THEN
        IF (ACTIVE.LE.0.0.OR.XAPPL.LE.0.0) THEN
          XCONC=-1.0
        ELSE
          XCONC=X2*XAPPL/XDEEP
        ENDIF
      ENDIF
      RETURN
      END