C**AGINIT
C  Continuum Dynamics, Inc.
C  Version 2.09 10/19/05
C
      SUBROUTINE AGINIT(UD,MAA)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGINIT
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: MAA
C
C  AGINIT sets up all default pointers for data input
C
C  UD     - USERDATA data structure
C  MAA    - Multiple application flag: -1,0=no; #=wind speed index
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      CALL AGINIX(UD,MAA)
      RETURN
      END
C**AGINIX
      SUBROUTINE AGINIX(UD,MAA)
C
      DIMENSION KSTV(5,7),PSTV(6),QSTV(6),BSTV(6)
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD /USERDATA/ UD
C
      INCLUDE 'AGCOMMON.INC'
      INCLUDE 'AGSAMPLE.INC'
C
      DATA TPI / 6.2831853 /
      DATA KSTV / 1,1,2,3,3,1,2,2,3,4,2,3,3,4,4,3,4,4,4,4,
     $            4,4,4,4,4,6,5,4,4,4,6,6,5,4,4 /
      DATA PSTV / 0.524, 0.373, 0.211, 0.0,-0.533,-3.175 /
      DATA QSTV / 2.207, 1.693, 1.309, 1.0, 0.734, 0.500 /
      DATA BSTV / 1.911, 1.393, 1.161, 1.0, 0.893, 0.786 /
C
C  Set all necessary default flags
C
      GRDMX=0.0
      NPRP=0
      DZBP=0.0
      PSBP=0.0
      PGBP=0.0
      JHEL=0
      QQMX=1.0
      SDISP=0.0
      JSMO=1
      LCANF=0
      HCAN=0.0
      CTU=1.0
      STU=0.0
      CTS=1.0
      STS=0.0
      IIDEP=-1
      IIDIS=-1
C
      IF (MAA.LT.0) THEN
        LFMAA=1
        LFMET=0
        LFMAC=0
      ELSEIF (MAA.EQ.0) THEN
        LFMAA=1
        LFMET=1
        LFMAC=0
      ELSE
        LFMAA=0
        LFMET=1
        LFMDR=MAA/100+1
        LFMAC=MAA-100*(LFMDR-1)
      ENDIF
C
C  Establish initial values based on data structure
C
      DO ND=1,3
        NDRP(ND)=UD.DSD(ND).NUMDROP
        DO N=1,NDRP(ND)
          DIAMV(N,ND)=UD.DSD(ND).DIAM(N)
          DMASS(N,ND)=UD.DSD(ND).MASSFRAC(N)
        ENDDO
        NZTYPE(ND)=0
      ENDDO
      ITRTYP=UD.TIER
      IF (ITRTYP.EQ.2) THEN
        IACTYP=UD.AC.BASICTYP+1
      ELSE
        IACTYP=UD.AC.WINGTYPE+2
      ENDIF
      CACNM=UD.AC.NAME
      LACNM=UD.AC.LNAME
      S=UD.AC.SEMISPAN
      UO=UD.AC.TYPSPEED
      WT=9.81*UD.AC.WEIGHT
      IF (UD.AC.WINGTYPE.EQ.3) THEN
        LMVEL=3
        DZBPD=UD.AC.BIPLSEP
        AS=UD.AC.PLANAREA
        TDOT=UD.AC.PROPRPM
        RPRPS=UD.AC.PROPRAD
        DZPRP=UD.AC.ENGVERT
        XPRPS=-UD.AC.ENGFWD
        NPRP=UD.AC.NUMENG
        IF (NPRP.EQ.1) THEN
          YPRPS(1)=0.0
        ELSEIF (NPRP.EQ.2) THEN
          YPRPS(1)=UD.AC.ENGHORIZ(1)
          YPRPS(2)=-UD.AC.ENGHORIZ(1)
        ELSE
          YPRPS(1)=UD.AC.ENGHORIZ(1)
          YPRPS(2)=-UD.AC.ENGHORIZ(1)
          YPRPS(3)=UD.AC.ENGHORIZ(2)
          YPRPS(4)=-UD.AC.ENGHORIZ(2)
        ENDIF
      ELSE
        LMVEL=4
        BDOT=UD.AC.PROPRPM
      ENDIF
C
      ISMKY=UD.SMOKEY
      IF (ITRTYP.EQ.3.AND.ISMKY.EQ.1) THEN
        ANGTU=UD.TRN.UPSLOPE
        IF (ANGTU.NE.0.0) THEN
          CTU=COS(ANGTU*TPI/360.0)
          STU=SIN(ANGTU*TPI/360.0)
        ENDIF
        ANGTS=UD.TRN.SIDESLOPE
        IF (ANGTS.NE.0.0) THEN
          CTS=COS(ANGTS*TPI/360.0)
          STS=SIN(ANGTS*TPI/360.0)
        ENDIF
      ENDIF
C
      NVAR=UD.NZ.NUMNOZ
      XOSMN=1.0E+10
      XOSMX=-1.0E+10
      ZOSMN=1.0E+10
      ZOSMX=-1.0E+10
      DO N=1,NVAR
        XOSMN=AMIN1(XOSMN,UD.NZ.POSFWD(N))
        XOSMX=AMAX1(XOSMX,UD.NZ.POSFWD(N))
        ZOSMN=AMIN1(ZOSMN,UD.NZ.POSVERT(N))
        ZOSMX=AMAX1(ZOSMX,UD.NZ.POSVERT(N))
      ENDDO
      BOOMHT=UD.CTL.HEIGHT
      BOOMVT=UD.AC.BOOMVERT
      BOOMFD=UD.AC.BOOMFWD
      WINGVT=UD.AC.WINGVERT
      DIST=BOOMHT-BOOMVT+WINGVT
      XOS=-BOOMFD-XOSMX
      IF (LMVEL.EQ.3) ZOSMN=AMIN1(ZOSMN,DZPRP-BOOMVT-RPRPS)
      HOSMN=DIST
      DO N=1,NVAR
        XS(1,N)=-BOOMFD-UD.NZ.POSFWD(N)
        XS(2,N)=UD.NZ.POSHORIZ(N)
        XS(3,N)=BOOMHT+UD.NZ.POSVERT(N)
        HOSMN=AMIN1(HOSMN,XS(3,N)*CTS-XS(2,N)*STS)
        XS(4,N)=-UO
        DO K=5,9
          XS(K,N)=0.0
        ENDDO
        NSD(N)=UD.NZ.NOZTYP(N)+1
        NZTYPE(NSD(N))=NZTYPE(NSD(N))+1
        IF (XS(2,N).LT.0.0) THEN
          IHALF(N)=1
        ELSE
          IHALF(N)=0
        ENDIF
      ENDDO
      IF (UD.CTL.SWTYPE.EQ.0) THEN
        SWATH=UD.CTL.SWATHWID
      ELSE
        SWATH=2.0*S*UD.CTL.SWATHWID
      ENDIF
      FLOW=0.001585*UO*UD.SM.FLOWRATE*SWATH
      IF (UD.SM.TYPE.EQ.0) THEN
        IF (UD.SM.BASICTYP.EQ.0) THEN
          DENF=0.92
          DENN=0.92
          ERATE=0.0
          LEVAP=0
        ELSE
          DENF=1.0
          DENN=1.0
          ERATE=84.76
          LEVAP=1
        ENDIF
      ELSE
        DENF=UD.SM.SPECGRAV
        DENN=UD.SM.NONVGRAV
        ERATE=UD.SM.EVAPRATE
        LEVAP=2
      ENDIF
      VFRAC=1.0-UD.SM.NVFRAC
      IF (ABS(VFRAC).LT.0.001) LEVAP=0
      AFRAC=UD.SM.ACFRAC
      WINDSP=UD.MET.WINDSPD
      ZO=UD.MET.SURFRUFF
      TEMPTR=UD.MET.TEMP
      RHUMTR=UD.MET.HUMIDITY
      NSWTH=UD.CTL.NUMLINES
      IBOOM=UD.CTL.HALFBOOM
      ISDTYP=UD.CTL.SDTYPE
      IF (ISDTYP.EQ.0) THEN
        SDISP=-UD.CTL.SDVALUE
      ELSEIF (ISDTYP.EQ.1) THEN
        SDISP=UD.CTL.SDVALUE
      ELSEIF (ISDTYP.EQ.2) THEN
        SDISP=-UD.CTL.SDVALUE/AMAX1(SWATH,1.0)
      ELSE
        SDISP=0.5*(1-IBOOM)
      ENDIF
      YFLXV=UD.CTL.FLXPLANE
C
      IF (ISDTYP.NE.1) THEN
        SWDISP=-SDISP*SWATH
      ELSE
        SWDISP=0.0
      ENDIF
C
      ITEM=MIN0(IFIX((WINDSP+1.0)/2.0)+1,5)
      KSTAB=UD.MET.INSOL+1
      LSTAB=KSTV(ITEM,KSTAB)
      PSTAB=PSTV(LSTAB)
      QSTAB=QSTV(LSTAB)
      BSTAB=BSTV(LSTAB)
C
C  Advanced settings
C
      IF (ITRTYP.EQ.2) THEN
        TMAX=600.0
        GDK=1.12
        WINDDR=-90.0
        WINDHT=2.0
        DRAG=0.1
        PROP=0.8
        PRTR=1013.0
        ZREF=0.0
        NDEPR=231
      ELSE
        TMAX=UD.CTL.MAXTIME
        GDK=2.0*UD.MET.VTXDECAY
        IF (LFMAA.EQ.1) THEN
          WINDDR=UD.MET.WINDDIR
        ELSE
          WINDDR=-90.0
        ENDIF
        WINDHT=UD.MET.WINDHGT
        DRAG=UD.AC.DRAG
        PROP=UD.AC.PROPEFF
        PRTR=UD.MET.PRESSURE
        ZREF=UD.TRN.ZREF
        NDEPR=0.5*(UD.CTL.MAXDWND+155.0)+1
      ENDIF
      YDEPX=2.0*(NDEPR-31)
      YGRID=YDEPX-95.0
      NGRID=NDEPR-46
C
C  Canopy settings
C
      IF (ISMKY.EQ.1) THEN
        IF (ITRTYP.EQ.2) THEN
          HCAN=UD.CAN.HEIGHT
          ZREF=AMAX1(ZREF,HCAN)
          IF (HCAN.GT.0.0) THEN
            LCANF=-1
            ZOC=UD.CAN.NDRUFF
            DOC=UD.CAN.NDDISP
          ENDIF
        ELSE
          LCANF=UD.CAN.TYPE
          IF (LCANF.NE.0) THEN
            HCAN=UD.CAN.HEIGHT
            ZOC=UD.CAN.NDRUFF
            DOC=UD.CAN.NDDISP
            IF (LCANF.EQ.3) THEN
              LCANF=-1
              ZREF=AMAX1(ZREF,HCAN)
            ELSE
              ESIZE=UD.CAN.ELESIZ
              TEMPC=UD.CAN.TEMP
              RHUMC=UD.CAN.HUMIDITY
              IF (LCANF.EQ.1) THEN
                STEMS=UD.CAN.STANDEN
                NCAN=UD.CAN.NUMENV
                CANIMN=1.0E+10
                CANIMX=-1.0E+10
                CANPMN=1.0E+10
                CANPMX=-1.0E+10
                DO N=1,NCAN
                  CANHV(N)=UD.CAN.ENVHGT(N)
                  CANIV(N)=UD.CAN.ENVDIA(N)
                  CANPV(N)=UD.CAN.ENVPOP(N)
                  CANIMN=AMIN1(CANIMN,CANIV(N))
                  CANIMX=AMAX1(CANIMX,CANIV(N))
                  CANPMN=AMIN1(CANPMN,CANPV(N))
                  CANPMX=AMAX1(CANPMX,CANPV(N))
                ENDDO
              ELSE
                LCANF=LCANF+UD.CAN.OPTYPE-1
                IF (LCANF.EQ.2) THEN
                  NCAN=UD.CAN.NUMLAI
                  ICANMN=0
                  DO N=1,NCAN
                    CANHV(N)=UD.CAN.LAIHGT(N)
                    CANIV(N)=UD.CAN.LAICUM(N)
                    IF (N.GT.1) THEN
                      IF (CANIV(N).GT.CANIV(N-1)) ICANMN=N
                    ENDIF
                  ENDDO
                ELSE
                  NCAN=1
                  CANHV(1)=UD.CAN.LIBHGT
                  CANIV(1)=UD.CAN.LIBLAI
                  BBULL=UD.CAN.LIBB
                  CBULL=UD.CAN.LIBC
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C  Multiple application assessment settings
C
      IF (LFMAA.EQ.0) THEN
        WINDSP=LFMAC
        WINDDR=-90.0+10.0*(LFMDR-1)
        TEMPTR=TEMPAA
        RHUMTR=RHUMAA
      ENDIF
C
C  Set ICV array for displaying initial data and testing limits
C
      DO N=1,83
        ICV(N)=0
      ENDDO
      TEM=0.1*(ITRTYP-1)
      DO ND=1,3
        IDKD(ND)=0
        IF (UD.DSD(ND).TYPE.EQ.1.AND.LFMAA.EQ.1) THEN
          DKDENF(ND)=UD.DK(ND).DENSITY
          DKSPD(ND)=UD.DK(ND).SPEED
          QFLOW=0.006*UD.SM.FLOWRATE*SWATH*UO/MAX0(NVAR,1)
          DKFLOW(ND)=UD.DK(ND).FLOW
          IF (ABS(1.0-DENF/DKDENF(ND)).GT.TEM) ICV(1+3*(ND-1))=10
          IF (ABS(1.0-UO/DKSPD(ND)).GT.TEM) ICV(2+3*(ND-1))=10
          IF (ABS(1.0-QFLOW/DKFLOW(ND)).GT.TEM) ICV(3+3*(ND-1))=10
        ELSEIF (UD.DSD(ND).TYPE.EQ.5.AND.LFMAA.EQ.1) THEN
          IDKD(ND)=1
          DKDENF(ND)=1.0
          DKSPD(ND)=UD.BK(ND).SPEED
          IF (ABS(1.0-DENF/DKDENF(ND)).GT.TEM) ICV(1+3*(ND-1))=10
          IF (ABS(1.0-UO/DKSPD(ND)).GT.TEM) ICV(2+3*(ND-1))=10
        ENDIF
      ENDDO
      ICV(10)=20*LFMAA
      ICV(11)=20*(ITRTYP-2)*LFMAA
      ICV(12)=20*LFMAA
      ICV(13)=20*(ITRTYP-2)*LFMAA
      ICV(14)=20*(ITRTYP-2)*LFMAA
      ICV(15)=20*(ITRTYP-2)*LFMAA
      ICV(16)=20*(ITRTYP-2)*LFMAA
      IF (LMVEL.EQ.3) THEN
        ICV(17)=21
        ICV(18)=23
        ICV(19)=40*(ITRTYP-2)*LFMAA
        ICV(20)=40*(ITRTYP-2)*LFMAA
        ICV(21)=40*(ITRTYP-2)*LFMAA
        ICV(22)=40*(ITRTYP-2)*LFMAA
        ICV(23)=40*(ITRTYP-2)*LFMAA
        ICV(24)=40*(ITRTYP-2)*LFMAA
        ICV(25)=40
        ICV(26)=45*(ITRTYP-2)*LFMAA
        IF (NPRP.EQ.4) ICV(27)=45*(ITRTYP-2)*LFMAA
      ELSE
        ICV(17)=30*(ITRTYP-2)*LFMAA
        ICV(18)=30
      ENDIF
      ICV(28)=15*(ITRTYP-2)*LFMAA
      ICV(29)=15*(ITRTYP-2)*LFMAA
      ICV(30)=28*(ITRTYP-2)*LFMAA
      ICV(31)=28*(ITRTYP-2)*LFMAA
      ICV(32)=28*LFMET
      ICV(33)=28*LFMET
      ICV(34)=60*LFMAA
      ICV(35)=60*LFMAA
      IF (LEVAP.EQ.2) THEN
        ICV(36)=60*LFMAA
        ICV(37)=60*LFMAA
        ICV(38)=60*LFMAA
      ENDIF
      ICV(39)=61*LFMAA
      ICV(40)=61*LFMAA
      ICV(41)=62*(ITRTYP-2)*LFMAA
      ICV(42)=62*(ITRTYP-2)*LFMAA
      ICV(43)=62*(ITRTYP-2)*LFMAA
      ICV(44)=62*(ITRTYP-2)*LFMAA
      DO ND=1,3
        JCARD=44+4*(ND-1)
        IF (NZTYPE(ND).GT.0) THEN
          ICV(JCARD+1)=64*LFMAA
          ICV(JCARD+2)=64*LFMAA
          ICV(JCARD+3)=64*LFMAA
          ICV(JCARD+4)=64
        ENDIF
      ENDDO
      ICV(57)=65*LFMAA
      ICV(58)=65*LFMAA
      ICV(59)=66*(ITRTYP-2)*LFMAA
      ICV(60)=66*LFMET
      ICV(61)=66*LFMET
      IF (ZREF.NE.0.0) ICV(62)=70*(ITRTYP-2)*LFMAA
      ICV(63)=72*(ITRTYP-2)*LFMAA*IBOOM
      IF (LFMAA.EQ.0) THEN
        ICV(64)=75
        ICV(65)=75
      ELSE
        ICV(64)=85
        ICV(65)=85
      ENDIF
      ICV(66)=86*LFMAA
      ICV(67)=90
      ICV(68)=95
      IF (ISMKY.EQ.1) THEN
        IF (LCANF.EQ.-1) THEN
          ICV(69)=100*LFMAA
          ICV(72)=110*LFMAA
          ICV(73)=110*LFMAA
        ELSE
          IF (LCANF.NE.0) THEN
            ICV(69)=110*LFMAA
            ICV(70)=110*LFMAA
            ICV(71)=110*LFMAA
            ICV(72)=110*LFMAA
            ICV(73)=110*LFMAA
            IF (LCANF.EQ.1) THEN
              ICV(74)=120*LFMAA
              ICV(75)=120*LFMAA
              ICV(76)=120*LFMAA
              ICV(77)=120*LFMAA
              ICV(78)=120*LFMAA
              ICV(79)=120*LFMAA
              ICV(80)=120*LFMAA
            ELSEIF (LCANF.EQ.2) THEN
              ICV(74)=125*LFMAA
              ICV(75)=125*LFMAA
              ICV(76)=125*LFMAA
              ICV(77)=125*LFMAA
              ICV(78)=125*LFMAA
            ELSEIF (LCANF.EQ.3) THEN
              ICV(74)=130*LFMAA
              ICV(75)=130*LFMAA
            ENDIF
          ENDIF
          IF (ANGTU.NE.0.0) ICV(81)=140*LFMAA
          IF (ANGTS.NE.0.0) THEN
            ICV(82)=140*LFMAA
            ICV(83)=140*LFMAA
          ENDIF
        ENDIF
      ENDIF
      ICV(84)=28*(ITRTYP-2)*LFMAA
      ICV(85)=200
C
      ICARD=0
      RETURN
      END