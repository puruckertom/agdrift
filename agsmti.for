C**AGSMTI
C  Continuum Dynamics, Inc.
C  Version 2.08 07/11/03
C
      SUBROUTINE AGSMTI(IAPPL,NUMD,DEPD,DEPV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSMTI
!MS$ATTRIBUTES REFERENCE :: IAPPL
!MS$ATTRIBUTES REFERENCE :: NUMD
!MS$ATTRIBUTES REFERENCE :: DEPD
!MS$ATTRIBUTES REFERENCE :: DEPV
C
C  AGSMTI transfers the Tier I deposition into AgDRIFT common blocks
C
C  IAPPL  - Application method (0 = aerial; 1 = ground; 2 = orchard airblast)
C  NUMD   - Number of points in deposition array
C  DEPD   - Downwind distance array (m)
C  DEPV   - Deposition array (fraction applied)
C
      DIMENSION DEPD(2),DEPV(2)
C
      INCLUDE 'AGSAMPLE.INC'
C
      COMMON /SSBL/ SSBLF,SSBLM,SSBLS,SSBLT
C
      IF (IAPPL.EQ.0) THEN
        BLKSIZ=365.764
      ELSE
        BLKSIZ=SSBLT
      ENDIF
      NDEPA=NUMD
      DO N=1,NDEPA
        YDEPA(N)=DEPD(N)
        ZDEPV(N,1,1)=DEPV(N)
      ENDDO
      RETURN
      END