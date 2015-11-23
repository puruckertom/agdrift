C**AGDROT
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGDROT(DDVEC)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDROT
!MS$ATTRIBUTES REFERENCE :: DDVEC
C
C  AGDROT recovers the discrete receptor result
C
C  DDVEC  - Discrete receptor deposition
C
      DIMENSION DDVEC(2)
C
      INCLUDE 'AGCOMMON.INC'
C
      IF (IIDIS.NE.1) RETURN
      DO N=1,NNDSR
        DDVEC(N)=DPDSR(N)
      ENDDO
      RETURN
      END