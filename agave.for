C**AGAVE
C  Continuum Dynamics, Inc.
C  Version 1.06 03/01/98
C
      SUBROUTINE AGAVE(NPTSD,YDV,DDV,NPTSI,YIV,DIV)
C
C  AGAVE computes the pond-integrated deposition profile
C
C  NPTSD  - Number of deposition points
C  YDV    - Deposition distance array (m)
C  DDV    - Deposition array (fraction applied)
C  NPTSI  - Number of pond-integrated deposition points
C  YIV    - Pond-integrated deposition distance array (m)
C  DIV    - Pond-integrated deposition array (fraction applied)
C
      DIMENSION YDV(2),DDV(2),YIV(2),DIV(2)
C
      DATA YAVE / 63.6 /
C
      DO N=1,NPTSD
        IF (YDV(N).LT.YDV(NPTSD)-YAVE) THEN
          NN=N
          XAVE=0.5*(YDV(NN+1)-YDV(NN))*(DDV(NN+1)+DDV(NN))
10        NN=NN+1
          IF (YDV(NN+1).LT.YDV(N)+YAVE) THEN
            XAVE=XAVE+0.5*(YDV(NN+1)-YDV(NN))*(DDV(NN+1)+DDV(NN))
            GO TO 10
          ELSE
            DD=AGINT(NPTSD,YDV,DDV,YDV(N)+YAVE)
            XAVE=XAVE+0.5*(YDV(N)+YAVE-YDV(NN))*(DD+DDV(NN))
          ENDIF
          YIV(N)=YDV(N)
          DIV(N)=XAVE/YAVE
        ELSE
          NPTSI=N-1
          RETURN
        ENDIF
      ENDDO
      RETURN
      END