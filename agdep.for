C**AGDEP
C  Continuum Dynamics, Inc.
C  Version 2.05 02/01/02
C
      SUBROUTINE AGDEP(AV,DV,DT,DMCV,YMN,DY,
     $                 NVEC,TEMND,ZVECS,ZVECH,IHF,IGK)
C
C  AGDEP computes the continuous deposition contribution
C
C  AV     - Current Y,Z,spread
C  DV     - V,W,spread
C  DT     - Time step
C  DMCV   - Current volume ratio
C  YMN    - Minimum Y location
C  DY     - Y increment
C  NVEC   - Number of Y points
C  TEMND  - Units normalization
C  ZVECS  - Results array for full deposition
C  ZVECH  - Results array for upwind deposition
C  IHF    - Half boom flag
C  IGK    - Activity flag
C
      DIMENSION AV(3),DV(3),ZVECS(2),ZVECH(2)
C
      IGK=0
      SNEW=SQRT(ABS(AV(3)))
      IF (SNEW.LE.0.25*ABS(AV(2))) RETURN
C
      XTEM=0.707107*AV(2)/SNEW
      TTEM=1.0/(1.0+0.47047*ABS(XTEM))
      ETEM=TTEM*(0.3480242+TTEM*(-0.0958798+TTEM*0.7478556))
     $     *EXP(-AMIN1(XTEM*XTEM,25.0))
      IF (XTEM.LT.0.0) ETEM=2.0-ETEM
      YNEW=AV(1)
      ZNEW=ABS(AV(2))
      IS=MAX0(IFIX((YNEW-4.0*SNEW-YMN)/DY)-1,1)
      IE=MIN0(IFIX((YNEW+4.0*SNEW-YMN)/DY)+1,NVEC)
      DO I=IS,IE
        Y=YMN+(I-1)*DY
        YTEM=EXP(-AMIN1(0.5*((Y-YNEW)/SNEW)**2,25.0))
        ZTEM=EXP(-AMIN1(0.5*(ZNEW/SNEW)**2,25.0))
        DMDT1=-0.5*YTEM*ETEM*DV(3)/SNEW/AV(3)
        DMDT2B=0.5*YTEM*ETEM*(Y-YNEW)**2*DV(3)/AV(3)/SNEW/AV(3)
        DMDT3A=-0.79788456*YTEM*ZTEM*DV(2)/AV(3)
        DMDT3B=0.39894228*YTEM*ZTEM*ZNEW*DV(3)/AV(3)/AV(3)
        DMDT=DMDT1+DMDT2B+AMAX1(DMDT3A,0.0)+DMDT3B
        IF (DMDT.GT.0.0) THEN
          ZVECS(I)=ZVECS(I)+DMDT*DT*TEMND*DMCV
          IF (IHF.EQ.1) ZVECH(I)=ZVECH(I)+DMDT*DT*TEMND*DMCV
        ENDIF
      ENDDO
      IGK=1
      RETURN
      END