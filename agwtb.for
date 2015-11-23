C**AGWTB
C  Continuum Dynamics, Inc.
C  Version 1.04 09/18/97
C
      SUBROUTINE AGWTB(TMPR,RHUM,PRES,WETB)
C
C  AGWTB computes the wet bulb temperature depression
C
C  TMPR   - Temperature (deg C)
C  RHUM   - Relative humidity (%)
C  PRES   - Pressure (mb)
C  WETB   - Wet bulb temperature depression (deg C)
C
      TDRY=1.8*TMPR+32.0
      PAMB=14.7*PRES/1013.0
C
      PDRY=FPRES(TDRY)
      PSAT=0.01*RHUM*PDRY
      TMIN=0.0
      TMAX=TDRY
      ITER=0
C
10    TEMP=0.5*(TMIN+TMAX)
      ITER=ITER+1
      PTEM=FPRES(TEMP)
      PNEW=PTEM-(PAMB-PTEM)*(TDRY-TEMP)/(2800.0-1.3*TEMP)
      IF (ABS(PNEW-PSAT).LT.0.001.OR.ITER.GT.20) GO TO 20
      IF (PNEW.LT.PSAT) THEN
        TMIN=TEMP
      ELSE
        TMAX=TEMP
      END IF
      GO TO 10
C
20    WETB=(TDRY-TEMP)/1.8
      RETURN
      END
C**FPRES
      FUNCTION FPRES(TEMP)
C
C  Saturation pressure
C
      THETA=(TEMP+459.67)/1165.14
      OMT=1.0-THETA
      SUMT=-OMT*(7.691234564+OMT*(26.08023696+OMT*
     $  (168.1706546+OMT*(-64.23285504+OMT*118.9646225))))
      SUMT=SUMT/THETA/(1.0+OMT*(4.16711732+OMT*
     $  20.9750676))-OMT/(OMT*OMT*1.0E+09+6.0)
      FPRES=EXP(SUMT)*3208.235
      RETURN
      END