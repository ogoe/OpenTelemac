!                    ****************************
                     SUBROUTINE ERROR_COMPUTATION
!                    ****************************
!
     &(F,MESH,FINEMESH,NELMAX,NPOIN,CORRESP,RLEVELS,NLEVEL,
     & IKLE,FINEIKLE)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!Brief   Error computation
!+       Computes the error between the numerical solution for a field
!+       F and an analytical solution, which is defined here by the user
!+       The error is computed on the finest mesh FINEMESH
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| F              |<--| FIELD FOR ERROR COMPUTATION
!| FINEMESH       |<--| FINE MESH ON WHICH ERRORS ARE COMPUTED
!| MESH           |<--| COARSER MESH CONTAINING THE SIMULATION RESULTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)       :: NPOIN
      DOUBLE PRECISION, INTENT(IN)       :: F(NPOIN)
      TYPE(BIEF_MESH) , INTENT(IN)       :: MESH
      TYPE(BIEF_MESH) , INTENT(IN)       :: FINEMESH
      INTEGER         , INTENT(IN)       :: NELMAX
      INTEGER         , INTENT(IN)       :: RLEVELS
      INTEGER         , INTENT(IN)       :: NLEVEL
      INTEGER         , INTENT(IN)       :: CORRESP(NELMAX,RLEVELS)
!     WARNING, THERE SHOULD BE NDP INSTEAD OF 3
      INTEGER         , INTENT(IN)       :: IKLE(NELMAX*3)
      INTEGER         , INTENT(IN)       :: FINEIKLE(NELMAX*3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,IERR,IPOIN,JPOIN1,JPOIN2,JPOIN3,IELEM,JELEM
!
      DOUBLE PRECISION PI,L,DETM,ERRL1,ERRL2,ERRLINF
      DOUBLE PRECISION X1,Y1,F1,X2,Y2,F2,X3,Y3,F3,XM,YM,A,B,C
!
      DOUBLE PRECISION, ALLOCATABLE :: ANALYTICAL(:)
      DOUBLE PRECISION, ALLOCATABLE :: NUMERICAL(:)
      INTEGER FID
!
      INTRINSIC ABS,MAX,SQRT
!
!-----------------------------------------------------------------------
!
      PI = ACOS(-1.D0)
!
!-----------------------------------------------------------------------
!
!     ALLOCATE ARRAYS TO STORE THE ANALYTICAL SOLUTION VALUES
!     AND THE NUMERICAL SOLUTION VALUES
!
      ALLOCATE(ANALYTICAL(FINEMESH%NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"ERROR_COMPUTATION: ANALYTICAL")
      ALLOCATE(NUMERICAL(FINEMESH%NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,"ERROR_COMPUTATION: NUMERICAL")
      DO IPOIN= 1,FINEMESH%NPOIN
        ANALYTICAL(IPOIN) = 0.D0
        NUMERICAL (IPOIN) = 0.D0
      ENDDO
!
!=======================================================================
! COMPUTE THE NUMERICAL SOLUTION ON THE FINE MESH
!=======================================================================
!
!     LOOP ON ALL THE FINE ELEMENTS
      DO IELEM=1,FINEMESH%NELEM
!       FIND THE ELEMENT OF THE COARSE MESH TO WHICH THE FINE
!       ELEMENT BELONGS
        JELEM = IELEM
        DO K=RLEVELS,NLEVEL,-1
          JELEM = CORRESP(JELEM,K)
        ENDDO
!       GET THE INDEX OF EACH POINT OF THE COARSE ELEMENT
        JPOIN1 = IKLE(              JELEM)
        JPOIN2 = IKLE(  MESH%NELMAX+JELEM)
        JPOIN3 = IKLE(2*MESH%NELMAX+JELEM)
!       GET THE COORDINATES OF EACH POINT OF THE COARSE ELEMENT
        X1 = MESH%X%R(JPOIN1)
        X2 = MESH%X%R(JPOIN2)
        X3 = MESH%X%R(JPOIN3)
        Y1 = MESH%Y%R(JPOIN1)
        Y2 = MESH%Y%R(JPOIN2)
        Y3 = MESH%Y%R(JPOIN3)
!       GET THE VALUE OF THE FUNCTION AT EACH POINT OF THE COARSE ELEMENT
        F1 = F(JPOIN1)
        F2 = F(JPOIN2)
        F3 = F(JPOIN3)
!       LOOP OVER THE POINTS OF THE FINE ELEMENT
        DO I=1,3
          IPOIN = FINEIKLE((I-1)*FINEMESH%NELMAX + IELEM)
!         TRY TO AVOID COMPUTING THINGS TOO MANY TIMES
!          IF (NUMERICAL(IPOIN).EQ.0.D0) THEN
!           GET THE CORRDINATES OF THE POINT ON THE FINE MESH
            XM    = FINEMESH%X%R(IPOIN)
            YM    = FINEMESH%Y%R(IPOIN)
!           SOLVE A LINEAR SYSTEM TO COMPUTE COEFFICIENTS A AND B
            DETM  = ABS((X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1))
            A     = ABS((XM-X2)*(Y3-Y2) - (X3-X2)*(YM-Y2))/DETM
            B     = ABS((XM-X1)*(Y3-Y1) - (X3-X1)*(YM-Y1))/DETM
            C     = ABS((XM-X1)*(Y2-Y1) - (X2-X1)*(YM-Y1))/DETM
            NUMERICAL(IPOIN) = A*F1 + B*F2 + C*F3
!          ENDIF
        ENDDO
      ENDDO
!
!=======================================================================
! COMPUTE THE ANALYTICAL SOLUTION ON THE FINE MESH
!=======================================================================
!
!     L IS USER DEFINED FOR THE ERROR COMPUTATION
!
      L = 200.D0
!
      DO I=1,FINEMESH%NPOIN
        ANALYTICAL(I) = SIN(2.D0*PI/L*(FINEMESH%X%R(I)-10.D0))
     &                * SIN(2.D0*PI/L*(FINEMESH%Y%R(I)-10.D0))
      ENDDO
!
!=======================================================================
! COMPUTES THE ERROR COMPARED TO AN ANALYTICAL SOLUTION
!=======================================================================
!
!  INITIALISE
      ERRL1=0.D0
      ERRL2=0.D0
      ERRLINF=0.D0
!
      DO I=1,FINEMESH%NPOIN
        ERRL1=ERRL1+ABS(ANALYTICAL(I)-NUMERICAL(I))
        ERRL2=ERRL2+(ANALYTICAL(I)-NUMERICAL(I))**2
        ERRLINF=MAX(ERRLINF,
     &          ABS(ANALYTICAL(I)-NUMERICAL(I)))
      ENDDO
!
      WRITE(LU,*)'---------------------------------------------------'
      WRITE(LU,*)'ERROR FOR REFINEMENT LEVEL',NLEVEL
      WRITE(LU,*)'---------------------------------------------------'
      ERRL1=ERRL1/FINEMESH%NPOIN
      ERRL2=SQRT(ERRL2/FINEMESH%NPOIN)
      ERRLINF=ERRLINF
      WRITE(LU,*)'ERROR NORM L1 IS',ERRL1
      WRITE(LU,*)'ERROR NORM L2 IS',ERRL2
      WRITE(LU,*)'ERROR NORM LINF IS',ERRLINF
      WRITE(LU,*)'NPOIN FOR NUMERICAL RESOLUTION IS',NPOIN
      WRITE(LU,*)'NPOIN FOR ERROR COMPUTATION IS',FINEMESH%NPOIN
!     WRITE THE ERRORS IN A FILE
      CALL GET_FREE_ID(FID)
#if defined NAGFOR
!     Manually going to the end of the file
      OPEN(UNIT=FID,FILE='ERRORS.DAT',FORM='FORMATTED',
     &     STATUS='UNKNOWN')
      REWIND(FID)
      IF(NLEVEL.GT.1) THEN
        READ(FID,*)
        DO I=1,NLEVEL-1
          READ(FID,*)
        ENDDO
      ENDIF
#else
      OPEN(UNIT=FID,FILE='ERRORS.DAT',FORM='FORMATTED',
     &     STATUS='UNKNOWN',ACCESS='APPEND')
#endif
      IF(NLEVEL.EQ.1) THEN
        WRITE(FID, *) "#NPOIN ", "ERRL2 ", "ERRL1 ", "ERRLINF "
      ENDIF
      WRITE(FID, *) NPOIN, ERRL2, ERRL1, ERRLINF
      CLOSE(FID)
!
      ERRL1=LOG10(ERRL1)
      ERRL2=LOG10(ERRL2)
      ERRLINF=LOG10(ERRLINF)
      WRITE(LU,*)'LOG_10(ERRL1) =',ERRL1
      WRITE(LU,*)'LOG_10(ERRL2) =',ERRL2
      WRITE(LU,*)'LOG_10(ERRLINF) =',ERRLINF
      WRITE(LU,*)'---------------------------------------------------'
!
!=======================================================================
!
      RETURN
      END
