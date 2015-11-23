C**AGSEND
C  Continuum Dynamics, Inc.
C  Version 2.00 04/15/01
C
      SUBROUTINE AGSEND(NXPTS,NYPTS,XGRDV,YGRDV,DGRDV,CONV,DMAX)
!MS$ATTRIBUTES DLLEXPORT,STDCALL :: AGSEND
!MS$ATTRIBUTES REFERENCE :: NXPTS
!MS$ATTRIBUTES REFERENCE :: NYPTS
!MS$ATTRIBUTES REFERENCE :: XGRDV
!MS$ATTRIBUTES REFERENCE :: YGRDV
!MS$ATTRIBUTES REFERENCE :: DGRDV
!MS$ATTRIBUTES REFERENCE :: CONV
!MS$ATTRIBUTES REFERENCE :: DMAX
C
C  AGSEND completes the spray block details solution
C
C  NXPTS  - Number of grid points in X direction
C  NYPTS  - Number of grid points in Y direction
C  XGRDV  - X grid point array (m)
C  YGRDV  - Y grid point array (m)
C  DGRDV  - Deposition array (in user units)
C  CONV   - Contour values array (in user units)
C  DMAX   - Maximum deposition (in user units)
C
      DIMENSION XGRDV(NXPTS),YGRDV(NYPTS),DGRDV(NXPTS,NYPTS)
      DIMENSION CONV(5),CONN(5),DEPV(1000)
C
      INCLUDE 'AGCOMMON.INC'
C
      DATA CONN / 0.1 , 0.01 , 0.001 , 0.0 , 0.0 /
C
C  Build grid spacing
C
      DO NX=1,NXPTS
        XGRDV(NX)=XGMIN+(NX-1)*DGSD
      ENDDO
      DO NY=1,NYPTS
        YGRDV(NY)=YGMIN+(NY-1)*DGSD
      ENDDO
      DO NX=1,NXPTS
        DO NY=1,NYPTS
          DGRDV(NX,NY)=0.0
        ENDDO
      ENDDO
C
C  Construct cross flight line deposition pattern
C
      DO N=1,NVAR
        DO NN=1,NNDRPT
          VOLNN=VOLRV(N,NN)
          YY=YPOSV(N,NN)
          SY=AMAX1(SPRDV(N,NN),1.0)
          XX=XPOSV(N,NN)
          IF (VOLNN.GT.0.0) THEN
            DO J=1,NNDEP
              DEPV(J)=0.0
            ENDDO
            JB=MAX0(IFIX((YY-4.0*SY-YYDEP)/DDDEP)-1,1)
            JE=MIN0(IFIX((YY+4.0*SY-YYDEP)/DDDEP)+1,NNDEP)
            IF (JB.LE.JE) THEN
              DO J=JB,JE
                Y=YYDEP+(J-1)*DDDEP
                YEXP=EXP(-AMIN1(0.5*((Y-YY)/SY)**2,25.0))
                DEPV(J)=FFDEP*DMASSN(NN)*VOLNN*YEXP/SY/NVAR/2.50663
              ENDDO
              YB=YYDEP+(JB-1)*DDDEP
              YE=YYDEP+(JE-1)*DDDEP
C
C  Correct for along flight line and all flight lines
C
              SX=SY  !AMAX1(SY,16.0)
              ST=1.414214*SX
              DO NS=1,NSFLT
                YTEM=0.0
                IF (NS.EQ.1.AND.IBOOM.EQ.1) THEN
                  IF (IHALF(N).EQ.1) YTEM=1.0
                ELSEIF (NS.EQ.NSFLT.AND.IBOOM.EQ.1) THEN
                  IF (IHALF(N).EQ.0) YTEM=1.0
                ELSE
                  YTEM=1.0
                ENDIF
                HFLEN=0.5*(XSEND(NS)-XSBEG(NS))
                XC=0.5*(XSEND(NS)+XSBEG(NS))+XX
                XB=XSBEG(NS)+XX
                XE=XSEND(NS)+XX
                IB=MAX0(IFIX((XB-4.0*SX-XGMIN)/DGSD)-1,1)
                IE=MIN0(IFIX((XE+4.0*SX-XGMIN)/DGSD)+1,NXPTS)
                JB=MAX0(IFIX((YB+YSFLT(NS)-YGMIN)/DGSD)-1,1)
                JE=MIN0(IFIX((YE+YSFLT(NS)-YGMIN)/DGSD)+1,NYPTS)
                IF (IB.LE.IE.AND.JB.LE.JE) THEN
                  DO I=IB,IE
                    X=XGRDV(I)-XC
                    XERF=0.5*(AGUNF(NERF,YERF,DERF,ERFV,(HFLEN-X)/ST)
     $                       +AGUNF(NERF,YERF,DERF,ERFV,(HFLEN+X)/ST))
                    DO J=JB,JE
                      DGRDV(I,J)=DGRDV(I,J)+YTEM*XERF*
     $                  AGUNF(NNDEP,YYDEP,DDDEP,DEPV,YGRDV(J)-YSFLT(NS))
                    ENDDO
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C  Check maximum value and contour values
C
      DMAX=0.0
      DO NX=1,NXPTS
        DO NY=1,NYPTS
          DMAX=AMAX1(DMAX,DGRDV(NX,NY))
        ENDDO
      ENDDO
      DO N=1,5
        IF (NCON.EQ.0) THEN
          CONV(N)=VCON(N)
        ELSE
          CONV(N)=CONN(N)*DMAX
        ENDIF
      ENDDO
      RETURN
      END