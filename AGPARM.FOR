C**AGPARM
C  Continuum Dynamics, Inc.
C  AGDISP Version 8.11 08/05/04
C
      SUBROUTINE AGPARM(LFLAG,ICLS,ISRC,VMD,XRS,NPTS,DDV,XXV)
C      SUBROUTINE AGPARM(LFLAG,ICLS,ICVT,VMD,XRS,NPTS,DDV,XXV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGPARM
!MS$ATTRIBUTES REFERENCE :: LFLAG
!MS$ATTRIBUTES REFERENCE :: ICLS
!MS$ATTRIBUTES REFERENCE :: ISRC
!MS$ATTRIBUTES REFERENCE :: VMD
!MS$ATTRIBUTES REFERENCE :: XRS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DDV
!MS$ATTRIBUTES REFERENCE :: XXV
C
C  AGPARM reconstructs the drop size distribution
C
C  LFLAG  - Input type (0=spray quality; 1=fixed; 2=optimized)
C  ICLS   - Size class flag: -1 = no; 0-10 = class to use
C  ISRC   - PMS to Malvern conversion: 0 = no; 1 = yes
C  VMD    - Volume median diameter (micrometers)
C  XRS    - Relative span
C  NPTS   - Number of points in drop size distribution
C  DDV    - Drop size distribution array
C  XXV    - Volume fraction array
C
      DIMENSION DDV(2),XXV(2)
C
      CALL AGPARX(LFLAG,ICLS,ISRC,VMD,XRS,NPTS,DDV,XXV)
      RETURN
      END
C**AGPARX
      SUBROUTINE AGPARX(LFLAG,ICLS,ISRC,VMD,XRS,NPTS,DDV,XXV)
C
      DIMENSION DDV(2),XXV(2)
C
      IF (ISRC.EQ.1) THEN
        VMD=15.775+0.82122*VMD
        XRS=1.3207446-0.11238*XRS
      ENDIF
C
      IF (LFLAG.EQ.0) THEN
        CALL AGSQL(VMD,XRS,ICLS)
      ELSE
        ICLS=-1
        CALL AGDSRX(LFLAG,NPTS,DDV,XXV,VMD,XRS,XND10,XND90,XF141,XDP)
      ENDIF
      RETURN
      END
C**AGSQL
      SUBROUTINE AGSQL(VMD,XRS,ICLS)
C
C  AGSQL selects the spray quality adjustment
C
      DIMENSION D10V(11),VMDV(11)
C
      DATA D10V /  20.58,  30.39,  62.42,  76.25, 113.76,
     $            131.21, 156.55, 175.24, 209.03, 223.77,
     $            241.52 /
      DATA VMDV /  47.79,  81.52, 137.20, 179.59, 254.74,
     $            294.11, 340.86, 385.22, 439.34, 477.94,
     $            521.34 / 
C
      NVMD=11
      DO N=10,1,-1
        IF (VMD.LE.VMDV(N+1)) NVMD=N
      ENDDO
C
      D10=VMD*(1.0+XRS*FX(0.1)/5.126915)**2
      ND10=11
      DO N=10,1,-1
        IF (D10.LE.D10V(N+1)) ND10=N
      ENDDO
C
      ICLS=MIN0(NVMD,ND10)-1
      RETURN
      END