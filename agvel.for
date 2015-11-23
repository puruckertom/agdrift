C**AGVEL
C  Continuum Dynamics, Inc.
C  Version 2.09 10/19/05
C
      SUBROUTINE AGVEL(X,Y,Z,U,V,W)
C
C  AGVEL determines the mean velocity at a specified location
C
C  X      - X location
C  Y      - Y location
C  Z      - Z location
C  U      - U velocity
C  V      - V velocity
C  W      - W velocity
C
      INCLUDE 'AGCOMMON.INC'
C
      U=0.0
      V=0.0
      W=0.0
C
      IF (Z.LE.0.0) RETURN
      IF (NVOR.EQ.0) GO TO 10
      IF (X.GE.0.0) THEN
        DO N=1,NVOR
C
C  Quadrant 1 vortex
C
          R=AMAX1(0.01,SQRT(ABS((Y-YBAR(N))**2+(Z-ZBAR(N))**2)))
          B=G2PI(N)*GDKV(N)/AMAX1(R,RLIM)/R
          V=V-B*(Z-ZBAR(N))
          W=W+B*(Y-YBAR(N))
C
C  Quadrant 2 vortex
C
          R=AMAX1(0.01,SQRT(ABS((Y-YBAL(N))**2+(Z-ZBAL(N))**2)))
          B=G2PI(N)*GDKV(N)/AMAX1(R,RLIM)/R
          V=V+B*(Z-ZBAL(N))
          W=W-B*(Y-YBAL(N))
C
C  Quadrant 3 vortex
C
          R=AMAX1(0.01,SQRT(ABS((Y-YBAL(N))**2+(Z+ZBAL(N))**2)))
          B=G2PI(N)*GDKV(N)/AMAX1(R,RLIM)/R
          V=V-B*(Z+ZBAL(N))
          W=W+B*(Y-YBAL(N))
C
C  Quadrant 4 vortex
C
          R=AMAX1(0.01,SQRT(ABS((Y-YBAR(N))**2+(Z+ZBAR(N))**2)))
          B=G2PI(N)*GDKV(N)/AMAX1(R,RLIM)/R
          V=V+B*(Z+ZBAR(N))
          W=W-B*(Y-YBAR(N))
        ENDDO
      ENDIF
C
C  Helicopter rotor
C
      IF (LMVEL.EQ.4) THEN
        IF (X.GE.0.0) THEN
          IF (JHEL.EQ.0.AND.WHEL.GT.0.0) THEN
            YH=YHEL*CTS-ZHEL*STS
            ZH=YHEL*STS+ZHEL*CTS
            YY=Y*CTS-Z*STS
            ZZ=Y*STS+Z*CTS
            TEM=XO+UO*(ZH-ZZ)/WHEL
            B=SQRT(ABS((YY-YH)**2+(X-TEM)**2))
            IF (B.LT.RHEL) THEN
              U=U+WHEL*STU
              V=V-WHEL*CTU*STS
              W=W-WHEL*CTU*CTS
            ENDIF
          ENDIF
C
C  Helicopter upstream
C
        ELSE
          XXS=(Z-ZHEL)/FHEL
          XXE=X+XXS
          BS=XXS*XXS+(Y-YHEL)**2
          BE=XXE*XXE+(Y-YHEL)**2
          US=UO*RHEL*RHEL/BS
          UE=UO*RHEL*RHEL/BE
          U=U-US+UE+2.0*(US/BS-UE/BE)*(Y-YHEL)**2
          V=V-2.0*(US*XXS/BS-UE*XXE/BE)*(Y-YHEL)
        ENDIF
C
C  Propeller
C
      ELSE
        IF (NPRP.NE.0) THEN
          DO N=1,NPRP
            IF (X.GE.XPRP(N)) THEN
              R=SQRT(ABS((Y-YPRP(N))**2+(Z-ZPRP(N))**2))
              E=15.174*R/CPXI(N)
              UA=11.785*CPUR/CPXI(N)/(1.0+0.25*E*E)**2
              VA=5.894*CPUR*(1.0-0.25*E*E)/CPXI(N)**2/(1.0+0.25*E*E)**2
              VS=VPRP(N)/RPRP(N)
              IF (R.GT.RPRP(N)) VS=0.0
              U=U+UA
              V=V+VA*(Y-YPRP(N))+VS*(Z-ZPRP(N))
              W=W+VA*(Z-ZPRP(N))-VS*(Y-YPRP(N))
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C  Mean crosswind
C
10    IF (LMCRS.EQ.1) THEN
        IF (LCANF.EQ.0) THEN
          B=ALOG((Z+ZO)/ZO)-PSTAB
        ELSE
          IF (Z.LE.HCAN) THEN
            B=UCAN*EXP(ALPHAC*(Z/HCAN-1.0))
          ELSE
            B=UOPN*(ALOG((Z/HCAN-DOC+ZOC)/ZOC)-PSTAB)
          ENDIF
        ENDIF
        U=U+B*CCW
        V=V-B*SCW
      ENDIF
      RETURN
      END