C**AGCOV
C  Continuum Dynamics, Inc.
C  Version 2.02 06/01/01
C
      SUBROUTINE AGCOV(NUMC,COVV,COVS,COVD,INTYPE,COV,ESWTH,EMEAN)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGCOV
!MS$ATTRIBUTES REFERENCE :: NUMC
!MS$ATTRIBUTES REFERENCE :: COVV
!MS$ATTRIBUTES REFERENCE :: COVS
!MS$ATTRIBUTES REFERENCE :: COVD
!MS$ATTRIBUTES REFERENCE :: INTYPE
!MS$ATTRIBUTES REFERENCE :: COV
!MS$ATTRIBUTES REFERENCE :: ESWTH
!MS$ATTRIBUTES REFERENCE :: EMEAN
C
C  AGCOV computes the spray block statistics
C
C  NUMC   - Number of points in COV array
C  COVV   - COV array
C  COVS   - Swath width array (m)
C  COVD   - Mean deposition array (fraction applied)
C  INTYPE - Input type: 0 = COV known
C                       1 = Swath width known
C                       2 = Mean deposition known
C  COV    - Coefficient of variation
C  ESWTH  - Swath width (m)
C  EMEAN  - Mean deposition (fraction applied)
C
      DIMENSION COVV(2),COVS(2),COVD(2),TEMD(100)
C
C  COV known
C
      IF (INTYPE.EQ.0) THEN
        NMIN=1
10      IF (COVV(NMIN+1).LT.COVV(NMIN)) THEN
          NMIN=NMIN+1
          GO TO 10
        ENDIF
        CMAX=COVV(NMIN)
        NMAX=NUMC
        DO N=NMIN,NUMC
          IF (COVV(N).GT.CMAX) THEN
            CMAX=COVV(N)
            NMAX=N
          ENDIF
        ENDDO
        IF (COV.LE.COVV(NMIN).OR.COV.GT.COVV(NMAX)) THEN
          ESWTH=-1.0
          EMEAN=-1.0
        ELSE
          N=NMIN
20        N=N+1
          IF (COVV(N).LT.COV) THEN
            GO TO 20
          ELSE
            ESWTH=(COVS(N-1)*(COVV(N)-COV)+COVS(N)*(COV-COVV(N-1)))/
     $            (COVV(N)-COVV(N-1))
          ENDIF
          EMEAN=AGINT(NUMC,COVS,COVD,ESWTH)
        ENDIF
C
C  Swath width known
C
      ELSEIF (INTYPE.EQ.1) THEN
        IF (ESWTH.LT.COVS(1).OR.ESWTH.GT.COVS(NUMC)) THEN
          COV=-1.0
          EMEAN=-1.0
        ELSE
          COV=AGINT(NUMC,COVS,COVV,ESWTH)
          EMEAN=AGINT(NUMC,COVS,COVD,ESWTH)
        ENDIF
C
C  Mean deposition known
C
      ELSE
        IF (EMEAN.GT.COVD(1).OR.EMEAN.LT.COVD(NUMC)) THEN
          COV=-1.0
          ESWTH=-1.0
        ELSE
          DO N=1,NUMC
            TEMD(N)=-COVD(N)
          ENDDO
          COV=AGINT(NUMC,TEMD,COVV,-EMEAN)
          ESWTH=AGINT(NUMC,TEMD,COVS,-EMEAN)
        ENDIF
      ENDIF
      RETURN
      END