C**AGRTRN
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGRTRN(XH,YV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGRTRN
!MS$ATTRIBUTES REFERENCE :: XH
!MS$ATTRIBUTES REFERENCE :: YV
C
C  AGRTRN performs the inverse transformation for spray block details
C
C  XH     - Horizontal plot distance (m)
C  YV     - Vertical plot distance (m)
C
      COMMON /RTRN/ CTH,STH
C
      XTEM=-XH*CTH+YV*STH
      YTEM=-XH*STH-YV*CTH
      XH=XTEM
      YV=YTEM
      RETURN
      END