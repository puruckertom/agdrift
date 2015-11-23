C**AGLIMS
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGLIMS(NPTS,DV,PV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGLIMS
!MS$ATTRIBUTES REFERENCE :: NPTS
!MS$ATTRIBUTES REFERENCE :: DV
!MS$ATTRIBUTES REFERENCE :: PV
C
C  AGLIMS finishes the initialization
C
C  NPTS   - Number of drop sizes
C  DV     - Drop size diameter array (micrometers)
C  PV     - Percentage completed array (%)
C
      DIMENSION DV(2),PV(2)
C
      CALL AGLIMX(NPTS,DV,PV)
      RETURN
      END
C**AGLIMX
      SUBROUTINE AGLIMX(NPTS,DV,PV)
C
      DIMENSION DV(2),PV(2),DSDV(36)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA DSDV /    8.00,    9.27,   10.75,   12.45,   14.43,   16.73,
     $              19.39,   22.49,   26.05,   30.21,   35.01,   40.57,
     $              47.03,   54.50,   63.16,   73.23,   84.85,   98.12,
     $             113.71,  131.73,  152.79,  177.84,  205.84,  238.45,
     $             276.48,  320.60,  372.18,  430.74,  498.91,  578.54,
     $             670.72,  777.39,  900.61, 1044.42, 1210.66, 1403.04 / 
C
C  Horizontal deposition planes
C
      YMIN=YDEPN
      YMAX=YDEPX
      NVEC=NDEPR
      DY=(YMAX-YMIN)/(NVEC-1)
      DO N=1,NVEC
        YDEPR(N)=YMIN+(N-1)*DY
        ZDEPR(N)=0.0
      ENDDO
      DDEPR=DY
      YMAX=YMAX+(NSWTH+2.5)*SWATH
      NVEC=(YMAX-YMIN)/DY+2
      NDEPS=NVEC
      DO N=1,NVEC
        YDEPS(N)=YMIN+(N-1)*DY
        ZDEPS(N)=0.0
        ZDEPT(N)=0.0
        ZDEPH(N)=0.0
        ZDEPI(N)=0.0
      ENDDO
C
C  Vertical flux planes
C
      YMIN=ZFLXN
      YMAX=ZFLXX
      NVEC=NFLXR
      DY=(YMAX-YMIN)/(NVEC-1)
      DO N=1,NVEC
        YFLXR(N)=YMIN+(N-1)*DY
        ZFLXR(N)=0.0
        ZFLXT(N)=0.0
      ENDDO
      DFLXR=DY
C
C  Transfer drop size distribution
C
      NPTS=0
      PNOZ=0
      DMAX=0.0
      DO ND=1,3
        IF (NZTYPE(ND).GT.0) THEN
          DO N=1,NDRP(ND)
            NPTS=NPTS+1
            DDIAMN(NPTS)=DIAMV(N,ND)
            DMAX=AMAX1(DMAX,DDIAMN(NPTS))
            DMASSN(NPTS)=DMASS(N,ND)
            NNOZLN(NPTS)=ND
            PNOZ=PNOZ+NZTYPE(ND)
            PV(NPTS)=PNOZ
          ENDDO
        ENDIF
      ENDDO
      NNDRPT=NPTS
      DO N=1,NPTS
        DV(N)=DDIAMN(N)
        PV(N)=100.0*PV(N)/PV(NPTS)
      ENDDO
C
      NDSD=0
10    NDSD=NDSD+1
      IF (NDSD.LE.36) THEN
        DSDC(NDSD)=DSDV(NDSD)
      ELSE
        DSDC(NDSD)=1.159*DSDC(NDSD-1)
      ENDIF
      IF (DSDC(NDSD).LT.DMAX.AND.NDSD.LT.75) GO TO 10
      DO N=1,NDSD
        DSSB(N)=0.0
        DSDW(N)=0.0
        DSVP(N)=0.0
        DSCP(N)=0.0
      ENDDO
C
C  Set initial values
C
      SWATH=SWATH/CTS
      SFAC=0.5*(1+IBOOM)
      IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
      YEDGE=YGRID+(1.0-SFAC)*SWATH
      YDRFT=0.0
      EFRAC=0.0
      ALEFT=0.0
C
C  Set canopy constants
C
      IF (LCANF.NE.0) THEN
        WINDHT=2.0*BOOMHT
        WINDSP=USK*ALOG((WINDHT+ZO)/ZO)
        TEM=1.0-DOC+ZOC
        ALPHAC=1.0/TEM/ALOG(TEM/ZOC)
        UOPN=WINDSP/ALOG((WINDHT/HCAN-DOC+ZOC)/ZOC)
        UCAN=UOPN*ALOG(TEM/ZOC)
        QQMX=0.845*UOPN*UOPN
        QQMC=QQMX/(HCAN*TEM)**2
        TEM=WINDDR*6.2831853/360.0
        CCW=COS(TEM)
        SCW=SIN(TEM)
      ENDIF
C
C  Set total accountancy arrays
C
      TATTV(1)=0.0
      TATTV(2)=0.1
      NATT=2
      DT=0.1
      FT=1.05
20    DT=DT*FT
      NATT=NATT+1
      TATTV(NATT)=TATTV(NATT-1)+DT
      IF (TATTV(NATT).LT.300.0.AND.NATT.LT.200) GO TO 20
      TATTV(NATT)=AMIN1(TATTV(NATT),300.0)
      DO N=1,NATT
        DO I=1,3
          TATFV(I,N)=0.0
        ENDDO
      ENDDO
C
      DO N=1,27
        TADDV(N)=2.0*(N-26)
      ENDDO
      NADD=27
      DD=2.0
      FD=1.05
30    DD=DD*FD
      NADD=NADD+1
      TADDV(NADD)=TADDV(NADD-1)+DD
      IF (TADDV(NADD).LT.300.0.AND.NADD.LT.200) GO TO 30
      TADDV(NADD)=AMIN1(TADDV(NADD),300.0)
      DO N=1,NADD
        DO I=1,3
          TADFV(I,N)=0.0
        ENDDO
      ENDDO
C
      DAHH=0.05
40    DAHH=2.0*DAHH
      NAHH=(BOOMHT+0.5*S)/DAHH+1
      IF (NAHH.GT.200) GO TO 40
      NAHH=MAX0(NAHH,2)
      DO N=1,NAHH
        TAHHV(N)=(N-1)*DAHH
        DO I=1,3
          TAHFV(I,N)=0.0
        ENDDO
      ENDDO
      RETURN
      END