C**AGDROP
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGDROP(NNDROP)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGDROP
!MS$ATTRIBUTES REFERENCE :: NNDROP
C
C  AGDROP controls the solution for each drop equation set
C
C  NNDROP - Drop size number
C
      DIMENSION XV(9,60)
C
      INCLUDE 'AGCOMMON.INC'
C
C  Set for next drop diameter
C
      NNDRP=NNDROP
      NN=NNDROP
      XDTOT=0.0
      FDTOT=0.0
      DIAM=DDIAMN(NN)
      DCUT=DIAM*(1.0-VFRAC)**0.33333
      YMASS=DMASSN(NN)/NVAR
      XO=XOS
      DO N=1,NVAR
        IF (NSD(N).EQ.NNOZLN(NN)) THEN
          ISW(N)=1
        ELSE
          ISW(N)=0
        ENDIF
        DO K=1,9
          XV(K,N)=XS(K,N)
        ENDDO
        XV(2,N)=XS(3,N)*STS+XS(2,N)*CTS
        XV(3,N)=XS(3,N)*CTS-XS(2,N)*STS
        EDOV(N)=DIAM
        EDNV(N)=DIAM
        CMASS(N)=1.0
        IF (IIDIS.EQ.1) THEN
          DO NR=1,NNDSR
            NTDSR(NR,N)=ITDSR(NR)
          ENDDO
        ENDIF
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
      IDIAM=DIAM/20.0
      IF (20.0*IDIAM.LT.DIAM) IDIAM=IDIAM+1
      IDIAM=MIN0(IDIAM,100)
C
C  Set deposition and flux flags
C
      TEMND=DMASSN(NN)*SWATH*CTS*CTS/AFRAC/NVAR/5.01326
      DO N=1,NVAR
        IDEPV(N)=2*ISW(N)-1
        CNDEP(N)=0.0
        CSDEP(N)=1.0
      ENDDO
      TEMNF=DMASSN(NN)*SWATH*CTS*CTS/AFRAC/NVAR/5.01326
      DO N=1,NVAR
        DO NS=1,NSWTH
          IFLXV(N,NS)=2*ISW(N)-1
          CNFLX(N,NS)=0.0
          CSFLX(N,NS)=1.0
        ENDDO
      ENDDO
      TOLD=0.0
C
C  Integrate the equations to maximum time
C
      CALL AGEQN(XV)
C
C  Correct for mass conservation
C
      XDSUM=0.0
      DO N=2,NDEPS
        XDSUM=XDSUM+0.5*DDEPR*(ZDEPS(N)+ZDEPS(N-1))
      ENDDO
      IF (XDSUM.GT.0.0) THEN
        TEM=SWATH*CTS*XDTOT/XDSUM
        DO N=1,NDEPS
          ZDEPT(N)=ZDEPT(N)+TEM*ZDEPS(N)
          ZDEPS(N)=0.0
          ZDEPI(N)=ZDEPI(N)+TEM*ZDEPH(N)
          ZDEPH(N)=0.0
        ENDDO
      ENDIF
      FDSUM=0.0
      DO N=2,NFLXR
        FDSUM=FDSUM+0.5*DFLXR*(ZFLXR(N)+ZFLXR(N-1))
      ENDDO
      IF (FDSUM.GT.0.0) THEN
        TEM=SWATH*CTS*FDTOT/FDSUM
        DO N=1,NFLXR
          ZFLXT(N)=ZFLXT(N)+TEM*ZFLXR(N)
          ZFLXR(N)=0.0
        ENDDO
      ENDIF
      RETURN
      END