C**AGKRR
C  Continuum Dynamics, Inc.
C  Version 1.08 03/01/00
C
      SUBROUTINE AGKRR(NPTS,DKV,CKV,DNV,XNV,PSAVE,IER)
C
C  AGKRR reconstructs the Rosin-Rammler drop size distribution
C
C  NPTS   - Number of user-defined drop sizes
C  DKV    - User-defined drop size distribution
C  CKV    - User-defined cumulative volume fraction
C  DNV    - Drop size distribution
C  XNV    - Volume fraction array
C  PSAVE  - Total cumulative volume fraction
C  IER    - Error flag
C
      DIMENSION DKV(2),CKV(2),DNV(2),XNV(2)
C
      DO N=1,NPTS
        CKV(N)=AMIN1(1.0,AMAX1(0.0,CKV(N)))
      ENDDO
      NS=0
10    NS=NS+1
      IF (CKV(NS).LT.0.00001) GO TO 10
      NE=NPTS+1
20    NE=NE-1
      IF (CKV(NE).GT.0.99999) GO TO 20
      IF (NE-NS+1.LT.2) THEN
        IER=4
        RETURN
      ENDIF
C
C  Compute least squares line through data
C
      SUMN=0.0
      SUMX=0.0
      SUMY=0.0
      SUMXX=0.0
      SUMXY=0.0
      DO N=NS,NE
        Y=ALOG(-ALOG(1.0-CKV(N)))
        X=ALOG(DKV(N))
        SUMN=SUMN+1.0
        SUMX=SUMX+X
        SUMY=SUMY+Y
        SUMXX=SUMXX+X*X
        SUMXY=SUMXY+X*Y
      ENDDO
      AA=(SUMXY-SUMX*SUMY/SUMN)/(SUMXX-SUMX*SUMX/SUMN)
      BB=(SUMY-AA*SUMX)/SUMN
      X=EXP(-BB/AA)
C
C  Construct profile with given drop sizes
C
      PSAVE=0.0
      DO N=1,32
        Q=1.0-EXP(-(DNV(N)/X)**AA)
        XNV(N)=Q-PSAVE
        PSAVE=Q
      ENDDO
      IF (PSAVE.LT.1.0) THEN
        DO N=1,32
          XNV(N)=XNV(N)/PSAVE
        ENDDO
      ENDIF
      RETURN
      END