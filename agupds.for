C**AGUPDS
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGUPDS(UD,ND)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGUPDS
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: ND
C
C  AGUPDS updates the drop size distribution after AGKICK is rerun
C
C  UD     - USERDATA data structure
C  ND     - Drop size designation (0,1,2)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      INCLUDE 'AGCOMMON.INC'
C
      NDD=ND+1
      NDRP(NDD)=UD.DSD(NDD).NUMDROP
      DO N=1,NDRP(NDD)
        DIAMV(N,NDD)=UD.DSD(NDD).DIAM(N)
        DMASS(N,NDD)=UD.DSD(NDD).MASSFRAC(N)
      ENDDO
      RETURN
      END