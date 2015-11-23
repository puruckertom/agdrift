C**AGLIBR
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGLIBR(ITIER,IDEP,DWND,NPTS,YV,DV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGLIBR
!MS$ATTRIBUTES REFERENCE :: ITIER
!MS$ATTRIBUTES REFERENCE :: IDEP
!MS$ATTRIBUTES REFERENCE :: DWND
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: YV
!MS$ATTRIBUTES REFERENCE :: DV
C
C  AGBCPC conditions the library data
C
C  ITIER  - Tier (1,2,3)
C  IDEP   - Deposition flag: 0 = deposition; 1 = pond-integrated deposition
C  DWND   - Maximum distance downwind (Tier III only)
C  NPTS   - Number of points in deposition array
C  YV     - Deposition distance array (m)
C  DV     - Deposition array (fraction applied)
C
      DIMENSION YV(2),DV(2)
C
      COMMON /TEMP/ NTEMP,YTEMP(1620),ZTEMP(1620)
C
      IF (IDEP.EQ.1) THEN
        CALL AGAVE(NPTS,YV,DV,NTEMP,YTEMP,ZTEMP)
        NPTS=NTEMP
        DO N=1,NPTS
          YV(N)=YTEMP(N)
          DV(N)=ZTEMP(N)
        ENDDO
      ENDIF
      IF (ITIER.EQ.1.OR.ITIER.EQ.2) THEN
        NPTS=MIN0(NPTS,153)
      ELSE
        MPTS=DWND/2.0+1
        NPTS=MIN0(NPTS,MPTS)
      ENDIF
      RETURN
      END