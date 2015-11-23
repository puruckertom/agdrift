C**AGTRAJ
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGTRAJ(UD,DROP,NTR,SV,AV,JAV)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGTRAJ
!MS$ATTRIBUTES REFERENCE :: UD
!MS$ATTRIBUTES REFERENCE :: DROP
!MS$ATTRIBUTES REFERENCE :: NTR
!MS$ATTRIBUTES REFERENCE :: SV
!MS$ATTRIBUTES REFERENCE :: AV
!MS$ATTRIBUTES REFERENCE :: JAV
C
C  AGTRAJ initializes the trajectory details toolbox calculation
C
C  UD     - USERDATA data structure
C  DROP   - Initial drop diameter (micrometers)
C  NTR    - Flag to inverse coordinate transform (0=no; 1=yes)
C  SV     - Scale limits (x,y,z) for this diameter
C  AV     - Initial position array (x,y,z) for all nozzles
C  JAV    - Position counter set to 1 = active
C
      CHARACTER*40 CHSTR
C
      INCLUDE 'AGDSTRUC.INC'
C
      RECORD / USERDATA / UD
C
      DIMENSION SV(2),AV(3,60),JAV(60),REALWD(3)
C
      INCLUDE 'AGCOMMON.INC'
      COMMON /TRAJ/ TSLIM(6),XOV(9,60),T,DMIN,ISTT,NTRAJ,NTRPL
C
C  Initialize all variables
C
      CALL AGINIX(UD,0)
10    CALL AGREAX(1,IER,IDK,IWR,REALWD,CHSTR,JCHSTR)
      IF (IER.NE.3) GO TO 10
C
C  Test drop size and limit it between reasonable minimum and maximum values
C
      DIAM=AMAX1(AMIN1(DROP,2000.0),5.0)
C
C  Set scale sizes
C
      TT=0.0
      YY=0.0
      DNEW=DIAM
      HH=BOOMHT
      DH=0.1*BOOMHT
      DO NN=1,10
        WTN=1.0
        DTAU=3.12E-06*DNEW*DNEW
20      WTO=WTN
        REYNO=0.0688*DNEW*WTO
        DTEM=DTAU/(1.0+0.197*REYNO**0.63+0.00026*REYNO**1.38)
        WTN=9.8*DTEM
        IF (ABS(WTN/WTO-1.0).GT.0.0001) GO TO 20
        DT=DH/WTN
        TT=TT+DT
        HH=AMAX1(HH-DH,ZO)
        BB=ALOG((HH+ZO)/ZO)
        YY=YY-BB*SCW*DT
        DNEW=DIAM*SQRT(ABS(1.0-ERATE*TT*DTEMP/DIAM/DIAM))
      ENDDO
      SV(1)=-2.0*BOOMHT
      SV(2)=BOOMHT
      SV(3)=-1.2*S
      SV(4)=1.2*S+YY
      SV(5)=0.0
      SV(6)=2.0*BOOMHT
C
      NTRPL=NTR
      IF (NTRPL.EQ.1) THEN
        DO N=1,6
          TSLIM(N)=0.0
        ENDDO
        DO I=1,2
          DO J=3,4
            DO K=5,6
              TI=SV(I)*CTU-(SV(J)*STS+SV(K)*CTS)*STU
              TJ=SV(J)*CTS-SV(K)*STS
              TK=SV(I)*STU+(SV(J)*STS+SV(K)*CTS)*CTU
              TSLIM(1)=AMIN1(TSLIM(1),TI)
              TSLIM(2)=AMAX1(TSLIM(2),TI)
              TSLIM(3)=AMIN1(TSLIM(3),TJ)
              TSLIM(4)=AMAX1(TSLIM(4),TJ)
              TSLIM(5)=AMIN1(TSLIM(5),TK)
              TSLIM(6)=AMAX1(TSLIM(6),TK)
            ENDDO
          ENDDO
        ENDDO
        DO N=1,6
          SV(N)=TSLIM(N)
        ENDDO
      ENDIF
C
C  Begin initialization
C
      DCUT=DIAM*(1.0-VFRAC)**0.33333
      XO=XOS
      DO N=1,NVAR
        ISW(N)=1
        DO K=1,9
          XOV(K,N)=XS(K,N)
        ENDDO
        XOV(2,N)=XS(3,N)*STS+XS(2,N)*CTS
        XOV(3,N)=XS(3,N)*CTS-XS(2,N)*STS
        EDOV(N)=DIAM
        EDNV(N)=DIAM
        CMASS(N)=1.0
      ENDDO
      DO N=1,NVOR
        G2PI(N)=G2PIS(N)
        YBAR(N)=ZBARS(N)*STS+YBARS(N)*CTS
        ZBAR(N)=ZBARS(N)*CTS-YBARS(N)*STS
        YBAL(N)=ZBALS(N)*STS+YBALS(N)*CTS
        ZBAL(N)=ZBALS(N)*CTS-YBALS(N)*STS
        GDKV(N)=1.0
      ENDDO
      IF (LMVEL.EQ.4) THEN
        WHEL=CHW
        YHEL=ZHELS*STS+YHELS*CTS
        ZHEL=ZHELS*CTS-YHELS*STS
      ENDIF
      IF (NPRP.GT.0) THEN
        DO N=1,NPRP
          XPRP(N)=XPRPS
          YPRP(N)=ZPRPS*STS+YPRPS(N)*CTS
          ZPRP(N)=ZPRPS*CTS-YPRPS(N)*STS
          RPRP(N)=RPRPS
          VPRP(N)=VPRPS
          CPXI(N)=CPXIS
        ENDDO
      ENDIF
C
C  Initialize integration
C
      ISWC=NVAR
      DO I=1,NVAR
        IF (NTRPL.EQ.1) THEN
          AV(1,I)=-XOV(1,I)*CTU-(XOV(2,I)*STS+XOV(3,I)*CTS)*STU
          AV(2,I)=XOV(2,I)*CTS-XOV(3,I)*STS
          AV(3,I)=-XOV(1,I)*STU+(XOV(2,I)*STS+XOV(3,I)*CTS)*CTU
        ELSE
          AV(1,I)=-XOV(1,I)
          AV(2,I)=XOV(2,I)
          AV(3,I)=XOV(3,I)
        ENDIF
        JAV(I)=1
      ENDDO
      NSTEP=0
      NTRAJ=1
C
      DT=0.0
      T=0.0
      DMIN=DIAM
      ISTT=0
      IF (NPRP.NE.0) THEN
        DO N=1,NPRP
          CPXI(N)=CPXI(N)+XO-XPRP(N)
        ENDDO
      ENDIF
      IF (LMVEL.EQ.4.AND.XO.GT.0.0) THEN
        FN=EXP(-AMIN1(CHF*XO,25.0))
        G2PI(1)=CHG*(1.0-FN)
        ZBAR(1)=HHEL-FHEL*(1.0-FN)
        ZBAL(1)=HHEL-FHEL*(1.0-FN)
        WHEL=CHW*SQRT(FN)
      ENDIF
      RETURN
      END