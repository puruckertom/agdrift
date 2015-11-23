C**AGSAV
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGSAV(XV,T)
C
C  AGSAV saves the current results for plotting
C
C  XV     - Array of results
C  T      - Current time
C
      DIMENSION XV(9,60),ANSW(4,60)
C
      INCLUDE 'AGCOMMON.INC'
C
      IF (T.GE.0.0) THEN
C
C  Save Y,Z,Spread,Volume Ratio
C  Save derivatives for continuous deposition
C
        DO N=1,NVAR
          IF (ISW(N).NE.0) THEN
            ANSW(1,N)=XV(2,N)
            ANSW(2,N)=XV(3,N)
            ANSW(3,N)=XV(7,N)
            ANSW(4,N)=(EDOV(N)/DIAM)**3
            DNDEP(1,N)=XV(5,N)
            DNDEP(2,N)=XV(6,N)
            DNDEP(3,N)=2.0*XV(8,N)
            TEM1=DNDEP(1,N)
            TEM2=DNDEP(2,N)
            DNFLX(1,N)=TEM2
            DNFLX(2,N)=-TEM1
            DNFLX(3,N)=DNDEP(3,N)
          ENDIF
C
C  Save deposition information for contour
C
          IF (ISW(N).LT.0.AND.IIDEP.GE.0) THEN
            IF (IIDEP.EQ.0) THEN
              VOLRN=1.0/DIAM**3
            ELSEIF (IIDEP.EQ.1) THEN
              VOLRN=CMASS(N)*AFRAC
            ELSEIF (IIDEP.EQ.2) THEN
              VOLRN=CMASS(N)*(1.0-VFRAC)
            ELSE
              VOLRN=CMASS(N)*(EDOV(N)/DIAM)**3
            ENDIF
            XPOSV(N,NNDRP)=XV(1,N)
            YPOSV(N,NNDRP)=XV(2,N)
            SPRDV(N,NNDRP)=SQRT(ABS(XV(7,N)))
            VOLRV(N,NNDRP)=VOLRN
          ENDIF
C
C  Increment discrete receptor deposition
C
          IF (ISW(N).NE.0.AND.IIDIS.EQ.1) THEN
            IF (IIDEP.EQ.0) THEN
              VOLRN=1.0/DIAM**3
            ELSEIF (IIDEP.EQ.1) THEN
              VOLRN=CMASS(N)*AFRAC
            ELSEIF (IIDEP.EQ.2) THEN
              VOLRN=CMASS(N)*(1.0-VFRAC)
            ELSE
              VOLRN=CMASS(N)*(EDOV(N)/DIAM)**3
            ENDIF
            IF (VOLRN.GT.0.0) THEN
              DO NR=1,NNDSR
                IF (NTDSR(NR,N).GE.1.AND.NTDSR(NR,N).LE.4) THEN
                  IF (XV(3,N).LE.ZZDSR(NR)) THEN
                    CALL AGDSR(NR,N,XV(1,N),VOLRN)
                    NTDSR(NR,N)=0
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C  Increment deposition
C
      CALL AGCON(T,ANSW)
C
C  Increment flux
C
      CALL AGVRF(T,ANSW)
C
      DO N=1,NVAR
        IF (ISW(N).LT.0) ISW(N)=0
      ENDDO
      TOLD=T
      RETURN
      END