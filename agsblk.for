C**AGSBLK
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGSBLK(UD,NUMSS,SGLD,SGLV,SGLH,IDTYPE,INTYPE,
     $                  XLENG,XDEEP,XACT,XAPPL,XDEPS,XDEPP,XCONC,
     $                  NSS,YSBL,ZSBL)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSBLK
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: NUMSS
!MS$ATTRIBUTES REFERENCE :: SGLD
!MS$ATTRIBUTES REFERENCE :: SGLV
!MS$ATTRIBUTES REFERENCE :: SGLH
!MS$ATTRIBUTES REFERENCE :: IDTYPE
!MS$ATTRIBUTES REFERENCE :: INTYPE
!MS$ATTRIBUTES REFERENCE :: XLENG
!MS$ATTRIBUTES REFERENCE :: XDEEP
!MS$ATTRIBUTES REFERENCE :: XACT
!MS$ATTRIBUTES REFERENCE :: XAPPL
!MS$ATTRIBUTES REFERENCE :: XDEPS
!MS$ATTRIBUTES REFERENCE :: XDEPP
!MS$ATTRIBUTES REFERENCE :: XCONC
!MS$ATTRIBUTES REFERENCE :: NSS
!MS$ATTRIBUTES REFERENCE :: YSBL
!MS$ATTRIBUTES REFERENCE :: ZSBL
C
C  AGSBLK performs the spray block assessment calculations
C
C  UD     - USERDATA data structure
C  NUMSS  - Number of points in single swath deposition array
C  SGLD   - Downwind distance array (m)
C  SGLV   - Single swath deposition array (fraction applied)
C  SGLH   - Upwind half boom single swath deposition array (fraction applied)
C  IDTYPE - Deposition type: 0 = Deposition
C                            1 = Pond-integrated deposition
C  INTYPE - Input type: 0 = Fraction applied known
C                       1 = g/ha known
C                       2 = lb/ac known
C                       3 = ng/L known
C  XLENG  - Length of pond (m)
C  XDEEP  - Pond depth (m)
C  XACT   - Active fraction (Tier I only)
C  XAPPL  - Deposition level (fraction applied)
C  XDEPS  - Deposition level (g/ha)
C  XDEPP  - Deposition level (lb/ac)
C  XCONC  - Concentration level (ng/L)
C  NSS    - Number of points in spray block assessment array
C  YSBL   - Spray block width (m)
C  ZSBL   - Buffer distance (m)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      DIMENSION SGLD(2),SGLV(2),SGLH(2),YSBL(2),ZSBL(2)
C
      COMMON /TEMP/ NTEMP,YTEMP(1620),ZTEMP(1620)
      COMMON /SBLK/ ZT(4900),ZL(4900)
      COMMON /TBLK/ YN(4900),ZN(4900)
      COMMON /SSBL/ SSBLF,SSBLM,SSBLS,SSBLT
C
      DATA JSMO / 1 /
C
C  Set block width parameters
C
      IF (UD.TIER.EQ.1) THEN
        IBOOM=0
        IF (UD.APPLMETH.EQ.0) THEN
          NSBL=20
          SWATH=18.2882
        ELSEIF (UD.APPLMETH.EQ.1) THEN
          NSBL=UD.GA.NUMSWATH
        ELSE
          NSBL=UD.OA.ENDTROW
        ENDIF
        ISDTYP=0
      ELSE
        IBOOM=UD.CTL.HALFBOOM
        NSBL=UD.CTL.NUMLINES
        IF (IBOOM.EQ.1) NSBL=NSBL+1
        IF (UD.CTL.SWTYPE.EQ.0) THEN
          SWATH=UD.CTL.SWATHWID
        ELSE
          SWATH=2.0*UD.AC.SEMISPAN*UD.CTL.SWATHWID
        ENDIF
        ISDTYP=UD.CTL.SDTYPE
        IF (ISDTYP.EQ.0) THEN
          SDISP=-UD.CTL.SDVALUE
        ELSEIF (ISDTYP.EQ.1) THEN
          SDISP=UD.CTL.SDVALUE
        ELSEIF (ISDTYP.EQ.2) THEN
          SDISP=-UD.CTL.SDVALUE/SWATH
        ELSE
          SDISP=0.5*(1-IBOOM)
        ENDIF
      ENDIF
      IF (UD.TIER.EQ.1.OR.UD.TIER.EQ.2) THEN
        DISTMX=304.81
      ELSE
        DISTMX=UD.CTL.MAXDWND
      ENDIF
C
C  Compute conversion factors
C
      IF (UD.TIER.EQ.1) THEN
        ACTIVE=XACT*UD.SM.FLOWRATE
      ELSE
        ACTIVE=UD.SM.ACFRAC*UD.SM.FLOWRATE*UD.SM.NONVGRAV
      ENDIF
      IF (IDTYPE.EQ.1) THEN
        IF (XLENG.LT.0.1.OR.XLENG.GT.DISTMX) ACTIVE=-1.0
        IF (XDEEP.LE.0.01.OR.XDEEP.GT.100.0) ACTIVE=-1.0
      ENDIF
      X1=1000.0*ACTIVE
      X2=100000.0*ACTIVE
      X3=X1/1120.66
C
C  Convert requests to fraction applied
C
      IF (INTYPE.EQ.0) THEN
        IF (ACTIVE.LE.0.0.OR.XAPPL.LE.0.0) THEN
          XDEPS=-1.0
          XDEPP=-1.0
        ELSE
          XDEPS=X1*XAPPL
          XDEPP=X3*XAPPL
        ENDIF
      ELSEIF (INTYPE.EQ.1) THEN
        IF (ACTIVE.LE.0.0.OR.XDEPS.LE.0.0) THEN
          XAPPL=-1.0
          XDEPP=-1.0
        ELSE
          XAPPL=XDEPS/X1
          XDEPP=X3*XAPPL
        ENDIF
      ELSEIF (INTYPE.EQ.2) THEN
        IF (ACTIVE.LE.0.0.OR.XDEPP.LE.0.0) THEN
          XAPPL=-1.0
          XDEPS=-1.0
        ELSE
          XAPPL=XDEPP/X3
          XDEPS=X1*XAPPL
        ENDIF
      ENDIF
      IF (IDTYPE.EQ.1) THEN
        IF (INTYPE.NE.3) THEN
          IF (ACTIVE.LE.0.0.OR.XAPPL.LE.0.0) THEN
            XCONC=-1.0
          ELSE
            XCONC=X2*XAPPL/XDEEP
          ENDIF
        ELSE
          IF (ACTIVE.LE.0.0.OR.XCONC.LE.0.0) THEN
            XAPPL=-1.0
            XDEPS=-1.0
            XDEPP=-1.0
          ELSE
            XAPPL=XCONC*XDEEP/X2
            XDEPS=X1*XAPPL
            XDEPP=X3*XAPPL
          ENDIF
        ENDIF
      ENDIF
C
C  Set up loop through number of swaths
C
      IF (UD.TIER.EQ.1) THEN
        IF (UD.APPLMETH.EQ.0) THEN
          NUMS=NUMSS
          SFAC=1.0
          IF (UD.DSD(1).BASICTYP.EQ.0) THEN
            SFACB=-0.5
          ELSEIF (UD.DSD(1).BASICTYP.EQ.1) THEN
            SFACB=0.0
          ELSE
            SFACB=0.5
          ENDIF
          DO N=1,NUMS
            YN(N)=SGLD(N)
          ENDDO
        ELSEIF (UD.APPLMETH.EQ.1) THEN
          ITYPE=UD.GA.BASICTYP
          XDWND=UD.CTL.MAXDWND
          ISWTH=UD.GA.NUMSWATH
          CALL AGGRNX(ITYPE,1,XDWND,ISWTH,-1,NUMS,YN,ZN)
        ELSE
          ITYPE=UD.OA.BASICTYP
          XDWND=UD.CTL.MAXDWND
          IBROW=UD.OA.BEGTROW
          IEROW=UD.OA.ENDTROW
          CALL AGORCX(ITYPE,1,XDWND,IBROW,IEROW,-1,NUMS,YN,ZN)
        ENDIF
        SDISP=0.0
      ELSE
        NUMS=NUMSS
        SFAC=0.5*(1+IBOOM)
        IF (ISDTYP.NE.1) SFAC=SFAC+SDISP
        DO N=1,NUMS
          YN(N)=SGLD(N)
        ENDDO
      ENDIF
      DO N=1,NUMS
        ZT(N)=0.0
      ENDDO
C
      NSS=0
      DO NS=1,NSBL
C
        IF (UD.APPLMETH.EQ.0) THEN
          IF (UD.TIER.EQ.1) THEN
            SSBLF=(NS-SFACB)*SWATH
          ELSE
            SSBLF=(NS-SFAC)*SWATH
          ENDIF
          SSBLM=NSBL*SWATH-SSBLF
          DO N=1,NUMS
            Y=YN(N)+(NS-SFAC)*SWATH
            IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
              Z=0.5*AGINT(NUMS,YN,SGLH,Y)
            ELSEIF (NS.EQ.NSBL.AND.IBOOM.EQ.1) THEN
              Z=0.5*(AGINT(NUMS,YN,SGLV,Y)-AGINT(NUMS,YN,SGLH,Y))
            ELSE
              Z=AGINT(NUMS,YN,SGLV,Y)
            ENDIF
            ZT(N)=ZT(N)+Z
          ENDDO
        ELSEIF (UD.APPLMETH.EQ.1) THEN
          ITYPE=UD.GA.BASICTYP
          XDWND=UD.CTL.MAXDWND
          ISWTH=UD.GA.NUMSWATH
          CALL AGGRNX(ITYPE,1,XDWND,ISWTH,-NS,NUMS,YN,ZN)
          DO N=1,NUMS
            ZT(N)=ZT(N)+ZN(N)
          ENDDO
        ELSE
          ITYPE=UD.OA.BASICTYP
          XDWND=UD.CTL.MAXDWND
          IBROW=UD.OA.BEGTROW
          IEROW=UD.OA.ENDTROW
          CALL AGORCX(ITYPE,1,XDWND,IBROW,IEROW,-NS,NUMS,YN,ZN)
          DO N=1,NUMS
            ZT(N)=ZT(N)+ZN(N)
          ENDDO
        ENDIF
C
        YC=0.0
        IF (ISDTYP.EQ.1) THEN
          DMAX=0.0
          DO N=1,NUMS
            IF (ZT(N).GT.DMAX) THEN
              DMAX=ZT(N)
              JMAX=N
            ENDIF
          ENDDO
          DDISP=SDISP
          IF (DDISP.GE.ZT(NUMS)) THEN
            JS=JMAX+1
            JE=NUMS
            DO J=JS,JE
              IF ((ZT(J-1).LE.DDISP.AND.DDISP.LE.ZT(J)).OR.
     $            (ZT(J-1).GE.DDISP.AND.DDISP.GE.ZT(J))) THEN
                YC=(YN(J-1)*(ZT(J)-DDISP)
     $             +YN(J)*(DDISP-ZT(J-1)))/(ZT(J)-ZT(J-1))
                GO TO 10
              ENDIF
            ENDDO
          ENDIF
        ENDIF
C
10      DO N=1,NUMS
          ZL(N)=ZT(N)
        ENDDO
        NL=NUMS-16
        IF (JSMO.NE.0) THEN
          NAVE=16
          NB=0
          DMAX=0.0
          DO N=1,NUMS
            DMAX=AMAX1(DMAX,ZT(N))
            EMAX=AMIN1(0.05,0.5*DMAX)
            IF (YN(N)+YC.GT.0.0.AND.NB.EQ.0) NB=1
            IF (ZT(N).LT.EMAX.AND.NB.EQ.1) NB=N
          ENDDO
          IF (NB.GT.1) THEN
            NB=MAX0(NB,NAVE+1)
            NE=NUMS-NAVE
            NFLG=-1
            DO N=NB,NE
              NSTT=N-NAVE
              NEND=N+NAVE
              DAVE=0.0
              DO NN=NSTT,NEND
                IF (ZT(NN).GT.0.0) DAVE=DAVE+ALOG10(ZT(NN))
              ENDDO
              DAVE=DAVE/(NAVE+NAVE+1)
              NFLG=NFLG+1
              IF (NFLG.LT.NAVE.AND.ZT(N).GT.0.0)
     $          DAVE=(NFLG*DAVE+(NAVE-NFLG)*ALOG10(ZT(N)))/NAVE
              IF (ZT(N).GT.0.0) ZL(N)=10.0**DAVE
            ENDDO
            NL=NE
          ENDIF
        ENDIF
C
C  Correct for pond-integrated deposition
C
        IF (IDTYPE.EQ.1) THEN
          CALL AGEXTD(NL,YN,ZL,XLENG,NNTEMP,YTEMP,ZTEMP)
          DO N=1,NL
            NN=N
            XAVE=0.5*(YTEMP(NN+1)-YTEMP(NN))*(ZTEMP(NN+1)+ZTEMP(NN))
20          NN=NN+1
            IF (NN.LT.NNTEMP) THEN
              IF (YTEMP(NN+1).LT.YN(N)+XLENG) THEN
                XAVE=XAVE+0.5*(YTEMP(NN+1)-YTEMP(NN))
     $                       *(ZTEMP(NN+1)+ZTEMP(NN))
                GO TO 20
              ELSE
                DD=AGINT(NNTEMP,YTEMP,ZTEMP,YN(N)+XLENG)
                XAVE=XAVE+0.5*(YN(N)+XLENG-YTEMP(NN))*(DD+ZTEMP(NN))
              ENDIF
            ENDIF
            ZL(N)=XAVE/AMAX1(XLENG,0.1)
          ENDDO
        ENDIF
C
C  Compute distance to fraction applied
C
        DO N=1,NL
          ZL(N)=-ZL(N)
        ENDDO
        XDD=AGINT(NL,ZL,YN,-XAPPL)
        IF (XDD.LT.DISTMX+SSBLM) THEN
          NSS=NSS+1
          YSBL(NSS)=SSBLF+YC
          ZSBL(NSS)=XDD-YC
        ENDIF
      ENDDO
      RETURN
      END