C**AGCHK
C  Continuum Dynamics, Inc.
C  Version 0.18 04/01/97
C
      SUBROUTINE AGCHK(X,XMIN,XMAX,ITIER,XXMIN,XXMAX,LFL,FAC,RV)
C
C  AGCHK checks limits on input values
C
C  X      - Input value
C  XMIN   - Minimum value
C  XMAX   - Maximum value
C  ITIER  - Tier (2 or 3)
C  XXMIN  - Absolute minimum value
C  XXMAX  - Absolute maximum value
C  LFL    - Error flag
C  FAC    - Units conversion factor
C  RV     - Value in user units
C
      DIMENSION RV(3)
C
      LFL=0
      RV(1)=FAC*X
      RV(2)=FAC*XMIN
      RV(3)=FAC*XMAX
      IF (X.LT.XMIN.OR.X.GT.XMAX) LFL=4-ITIER
      IF (LFL.EQ.1.AND.(X.LT.XXMIN.OR.X.GT.XXMAX)) THEN
        LFL=2
        RV(2)=FAC*XXMIN
        RV(3)=FAC*XXMAX
      ENDIF
      RETURN
      END