C**AGVRF
C  Continuum Dynamics, Inc.
C  Version 1.08 09/30/99
C
      SUBROUTINE AGVRF(TNEW,ANS)
C
C  AGVRF computes the continuous vertical flux pattern
C
C  TNEW   - Time
C  ANS    - Trajectory results array
C
      DIMENSION ANS(4,60),XV(3)
C
      INCLUDE 'AGCOMMON.INC'
C
C  ISW =  1  Active drop above the surface
C         0  Drop hits the surface and penetrates
C        -1  Four standard deviations below the surface and finish
C
      NSWTM=NSWTH
      IF (IBOOM.EQ.1) NSWTM=NSWTH+1
      IF (TNEW.GE.0.0) THEN
        DTE=TNEW-TOLD
        DO N=1,NVAR
          IF (ISW(N).NE.0) THEN
            XNFLX(1,N)=ANS(2,N)
            XNFLX(2,N)=YFLXV-ANS(1,N)
            XNFLX(3,N)=ANS(3,N)
            DSFLX(N)=AFRAC
          ELSE
            DO I=1,3
              XNFLX(I,N)=XNFLX(I,N)+DTE*DNFLX(I,N)
            ENDDO
          ENDIF
          DO NS=1,NSWTM
            ICDP=0
            IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
              IF (IHALF(N).EQ.1) ICDP=1
            ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
              IF (IHALF(N).EQ.0) ICDP=1
            ELSE
              ICDP=1
            ENDIF
            IF (IFLXV(N,NS).GE.0.AND.ICDP.EQ.1) THEN
              XV(1)=XNFLX(1,N)
              XV(2)=XNFLX(2,N)+(NS-1)*SWATH
              XV(3)=XNFLX(3,N)
              CALL AGDEP(XV,DNFLX(1,N),DTE,DSFLX(N),YFLXR,DFLXR,
     $                   NFLXR,TEMNF*CMASS(N),ZFLXR,ZFLXR,0,I)
              IF (I.EQ.0.AND.IFLXV(N,NS).EQ.0) IFLXV(N,NS)=-1
            ENDIF
            IF (ISW(N).LT.0.AND.IFLXV(N,NS).GT.0) IFLXV(N,NS)=0
          ENDDO
        ENDDO
C
C  Extend deposition for active drops below the surface
C
      ELSE
        TIMEE=TOLD
        TMAXE=10.0*TIMEE
        DTEE=DTE
10      TIMEE=TIMEE+DTEE
        L=0
        DO N=1,NVAR
          IF (ISW(N).EQ.0) THEN
            DO I=1,3
              XNFLX(I,N)=XNFLX(I,N)+DTEE*DNFLX(I,N)
            ENDDO
            DO NS=1,NSWTM
              ICDP=0
              IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
                IF (IHALF(N).EQ.1) ICDP=1
              ELSEIF (NS.EQ.NSWTM.AND.IBOOM.EQ.1) THEN
                IF (IHALF(N).EQ.0) ICDP=1
              ELSE
                ICDP=1
              ENDIF
              IF (IFLXV(N,NS).EQ.0.AND.ICDP.EQ.1) THEN
                L=L+1
                XV(1)=XNFLX(1,N)
                XV(2)=XNFLX(2,N)+(NS-1)*SWATH
                XV(3)=XNFLX(3,N)
                CALL AGDEP(XV,DNFLX(1,N),DTEE,DSFLX(N),YFLXR,DFLXR,
     $                     NFLXR,TEMNF*CMASS(N),ZFLXR,ZFLXR,0,I)
                IF (I.EQ.0) IFLXV(N,NS)=-1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        DTEE=1.1*DTEE
        IF (L.NE.0.AND.TIMEE.LT.TMAXE) GO TO 10
      ENDIF
      RETURN
      END