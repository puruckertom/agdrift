C**AGOVL
C  Continuum Dynamics, Inc.
C  Version 1.09 05/15/00
C
      SUBROUTINE AGOVL
C
C  AGOVL constructs the multiple swath deposition patterns
C
      INCLUDE 'AGCOMMON.INC'
C
      NVEC=NDEPR
      SFAC=0.5*(1+IBOOM)
      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      NSWTM=NSWTH
      IF (IBOOM.EQ.1) NSWTM=NSWTH+1
      DO NS=1,NSWTM
        DO N=1,NVEC
          Y=YDEPR(N)+(NS-SFAC)*SWATH
          IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
            Z=0.5*AGINT(NDEPS,YDEPS,ZDEPH,Y)
          ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
            Z=0.5*(AGINT(NDEPS,YDEPS,ZDEPS,Y)
     $        -AGINT(NDEPS,YDEPS,ZDEPH,Y))
          ELSE
            Z=AGINT(NDEPS,YDEPS,ZDEPS,Y)
          ENDIF
          ZDEPR(N)=ZDEPR(N)+Z
        ENDDO
      ENDDO
C
      IF (ISDTYP.EQ.1) THEN
        DMAX=0.0
        DO N=1,NVEC
          IF (ZDEPR(N).GT.DMAX) THEN
            DMAX=ZDEPR(N)
            JMAX=N
          ENDIF
        ENDDO
        DDISP=SDISP
        IF (DDISP.LT.ZDEPR(NVEC)) THEN
          SWDISP=-1.0
        ELSE
          JS=JMAX+1
          JE=NVEC
          DO J=JS,JE
            IF ((ZDEPR(J-1).LE.DDISP.AND.DDISP.LE.ZDEPR(J)).OR.
     $          (ZDEPR(J-1).GE.DDISP.AND.DDISP.GE.ZDEPR(J))) THEN
              YC=(YDEPR(J-1)*(ZDEPR(J)-DDISP)
     $           +YDEPR(J)*(DDISP-ZDEPR(J-1)))/(ZDEPR(J)-ZDEPR(J-1))
              GO TO 10
            ENDIF
          ENDDO
10        SWDISP=YC
          DO N=1,NVEC
            Y=YDEPR(N)+YC
            ZDEPT(N)=AGINT(NDEPR,YDEPR,ZDEPR,Y)
          ENDDO
          DO N=1,NVEC
            ZDEPR(N)=ZDEPT(N)
          ENDDO
        ENDIF
      ENDIF
C
      IF (JSMO.NE.0) THEN
        NAVE=16
        NB=0
        DMAX=0.0
        DO N=1,NVEC
          ZDEPT(N)=ZDEPR(N)
          DMAX=AMAX1(DMAX,ZDEPT(N))
          EMAX=AMIN1(0.05,0.5*DMAX)
          IF (YDEPR(N).GT.0.0.AND.NB.EQ.0) NB=1
          IF (ZDEPT(N).LT.EMAX.AND.NB.EQ.1) NB=N
        ENDDO
        IF (NB.GT.1) THEN
          NB=MAX0(NB,NAVE+1)
          NE=NVEC-NAVE
          NFLG=-1
          DO N=NB,NE
            NSTT=N-NAVE
            NEND=N+NAVE
            DAVE=0.0
            DO NN=NSTT,NEND
              IF (ZDEPR(NN).GT.0.0) DAVE=DAVE+ALOG10(ZDEPR(NN))
            ENDDO
            DAVE=DAVE/(NAVE+NAVE+1)
            NFLG=NFLG+1
            IF (NFLG.LT.NAVE.AND.ZDEPR(N).GT.0.0)
     $        DAVE=(NFLG*DAVE+(NAVE-NFLG)*ALOG10(ZDEPR(N)))/NAVE
            IF (ZDEPR(N).GT.0.0) ZDEPT(N)=10.0**DAVE
          ENDDO
          DO N=1,NE
            ZDEPR(N)=ZDEPT(N)
          ENDDO
          NDEPR=NE
        ENDIF
      ENDIF
      RETURN
      END