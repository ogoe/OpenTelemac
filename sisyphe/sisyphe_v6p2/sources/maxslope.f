!                    *******************
                     SUBROUTINE MAXSLOPE
!                    *******************
!
     &(SLOPE,ZF,ZR,XEL,YEL,NELEM,NELMAX,NPOIN,IKLE,EVOL,UNSV2D,MESH)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COLLAPSE OF SAND WITH A SLOPE GREATER THAN A
!+                STABILITY CRITERION.
!
!history  J-M HERVOUET (LNH)
!+        16/11/2007
!+        V5P8
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EVOL           |<->| WORK ARRAY, THEN EVOLUTION DUE TO SLIDE
!| IKLE           |-->| CONNECTIVITY TABLE
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SLOPE          |-->| MAXIMUM SLOPE IN DEGREES
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
!| XEL,YEL        |-->| MESH COORDINATES PER ELEMENT
!| ZF             |<->| BOTTOM THAT WILL BE MODIFIED
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE INTERFACE_SISYPHE, EX_MAXSLOPE => MAXSLOPE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
!
      DOUBLE PRECISION, INTENT(IN   ) :: SLOPE
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: EVOL
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,L,ZC,DEUXSURF,TANSL
!
      INTRINSIC SQRT,MIN,MAX,TAN
!
!-----------------------------------------------------------------------
!
      TANSL=TAN(3.141592653589D0*SLOPE/180.D0)
!
!     INITIALISES THE RIGHT-HAND SIDE EVOL TO ZERO
!
      CALL CPSTVC(UNSV2D,EVOL)
      CALL OS('X=0     ',X=EVOL)
!
!     LOOP ON ELEMENTS
!
      DO IELEM=1,NELEM
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
!
        X2=XEL(IELEM,2)
        X3=XEL(IELEM,3)
        Y2=YEL(IELEM,2)
        Y3=YEL(IELEM,3)
        Z2=ZF(I2)-ZF(I1)
        Z3=ZF(I3)-ZF(I1)
!
!       TWICE THE TRIANGLE AREA
!
        DEUXSURF=X2*Y3-X3*Y2
!
!       AVERAGE BOTTOM IN THE ELEMENT
!
        ZC=(ZF(I1)+ZF(I2)+ZF(I3))/3.D0
!
!       COMPONENTS OF BOTTOM GRADIENT
!
        A=(Z2*Y3-Z3*Y2)/DEUXSURF
        B=(Z3*X2-Z2*X3)/DEUXSURF
!
!       CORRECTING FACTOR ON SLOPE
!
        L=MIN(1.D0,TANSL/MAX(SQRT(A**2+B**2),1.D-8))
!
!       L LIMITED DUE TO NON-ERODABLE BEDS : ZF MUST NOT GO BELOW ZR
!
        IF(ZF(I1).GT.ZC) L=MAX(L,(ZR(I1)-ZC)/MAX(ZF(I1)-ZC,1.D-8))
        IF(ZF(I2).GT.ZC) L=MAX(L,(ZR(I2)-ZC)/MAX(ZF(I2)-ZC,1.D-8))
        IF(ZF(I3).GT.ZC) L=MAX(L,(ZR(I3)-ZC)/MAX(ZF(I3)-ZC,1.D-8))
!
!       BUILDS THE RIGHT-HAND SIDE
!
        EVOL%R(I1)=EVOL%R(I1)+(1.D0-L)*(ZC-ZF(I1))*DEUXSURF/6.D0
        EVOL%R(I2)=EVOL%R(I2)+(1.D0-L)*(ZC-ZF(I2))*DEUXSURF/6.D0
        EVOL%R(I3)=EVOL%R(I3)+(1.D0-L)*(ZC-ZF(I3))*DEUXSURF/6.D0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FINAL RESOLUTION
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(EVOL,2,MESH)
      ENDIF
!
      DO I=1,NPOIN
        EVOL%R(I)=EVOL%R(I)*UNSV2D%R(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
