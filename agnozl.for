C**AGNOZL
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGNOZL(NVAR,YV,S,BWIDTH,NNEW,AV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGNOZL
!MS$ATTRIBUTES REFERENCE :: NVAR
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: S
!MS$ATTRIBUTES REFERENCE :: BWIDTH
!MS$ATTRIBUTES REFERENCE :: NNEW
!MS$ATTRIBUTES REFERENCE :: AV
C
C  AGNOZL corrects the basic nozzle locations for boom length
C
C  NVAR   - Number of original nozzles
C  YV     - Original nozzle locations (m)
C  S      - Wing semispan or rotor radius (m)
C  BWIDTH - Boom width (%)
C  NNEW   - Number of corrected nozzles
C  AV     - Corrected nozzle locations (m)
C
      DIMENSION YV(2),AV(2)
C
      F=BWIDTH*S/100.0
      TEM=AMAX1(-YV(1),YV(NVAR))
      IF (ABS(F-TEM)/S.LT.0.01) THEN
        NNEW=NVAR
        DO I=1,NVAR
          AV(I)=YV(I)
        ENDDO
      ELSE
C
C  Correct nozzle positions
C
        IF (F.LT.TEM) THEN
          II=0
          DO I=1,NVAR
            IF (YV(I).GE.-F.AND.YV(I).LE.F) THEN
              II=II+1
              AV(II)=YV(I)
            ENDIF
          ENDDO
        ELSE
          DYN=YV(2)-YV(1)
          IN=IFIX((F+YV(1))/DYN)
          DYP=YV(NVAR)-YV(NVAR-1)
          IP=IFIX((F-YV(NVAR))/DYP)
          NN=NVAR+IN+IP
          IF (NN.GT.60) THEN
            NN=(NN-59)/2
            IN=IN-NN
            IP=IP-NN
          ENDIF
          II=0
          IF (IN.GT.0) THEN
            DO I=1,IN
              AV(I)=YV(1)-DYN*(IN-I+1)
            ENDDO
            II=IN
          ENDIF
          DO I=1,NVAR
            II=II+1
            AV(II)=YV(I)
          ENDDO
          IF (IP.GT.0) THEN
            DO I=1,IP
              AV(II+I)=YV(NVAR)+DYP*I
            ENDDO
            II=II+IP
          ENDIF
        ENDIF
        NNEW=II
C
C  Expand to fill to boom width desired
C
        FN=-F/AV(1)
        FP=F/AV(NNEW)
        DO I=1,NNEW
          IF (AV(I).LT.0.0) THEN
            AV(I)=FN*AV(I)
          ELSE
            AV(I)=FP*AV(I)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END