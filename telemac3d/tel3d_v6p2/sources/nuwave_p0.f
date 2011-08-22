!                    ********************
                     SUBROUTINE NUWAVE_P0
!                    ********************
!
     &(NUWAVE,DM1,Z,DZ,IKLE,NPOIN2,NPLAN,NELMAX,NELEM2,XMUL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    FOR WAVE EQUATION, COMPUTES THE PSEUDO-VISCOSITY
!+                AS A PIECE-WISE CONSTANT VARIABLE.
!+
!+            THE COMPUTATION IS DONE SO THAT THE SUM ON THE
!+                VERTICAL OF FLUXES DUE TO GRADIENT OF FREE SURFACE
!+                COMPUTED IN VC04PP GIVES THE SAME RESULT
!+               (SEE VCGRADP WITH SPECAD=.TRUE.).
!
!history  J-M HERVOUET (LNHE)
!+        22/06/06
!+        V6P0
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
!| DM1            |-->| D**-1 (SEE BOOK)
!| DZ             |<->| DELTA(Z) : HEIGHT OF PRISMS
!| IKLE           |-->| CONNECTIVITY TABLE IN 2D MESH
!| NELEM2         |-->| NUMBER OF 2D ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF 2D ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NUWAVE         |<->| PSEUDO-VISCOSITY IN WAVE EQUATION
!| XMUL           |-->| MULTIPLICATIVE CONSTANT
!| Z              |-->| ELEVATION OF REAL 3D MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN2,NPLAN,NELMAX,NELEM2
      INTEGER,          INTENT(IN)    :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: NUWAVE(NELEM2),DZ(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: DM1(NPOIN2,NPLAN),XMUL
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE,I1,I2,I3,IELEM
      DOUBLE PRECISION F1,F2,F3,G1,G2,G3,XSUR12
!
!***********************************************************************
!
      XSUR12=XMUL/12.D0
!
!     COMPUTES DELTA(Z) FOR ALL THE LEVELS : IN DZ
!
      CALL OV('X=Y-Z   ',DZ,Z(1,2),Z(1,1),0.D0,NPOIN2*(NPLAN-1))
!
!     SUMS AVERAGE DZ*DM1 FOR EACH LEVEL
!     AVERAGE DZ*DM1 = INTEGRAL OF DZ*DM1 ON THE TRIANGLE
!     DIVIDED BY THE TRIANGLE AREA
!
      CALL OV('X=C     ',NUWAVE,NUWAVE,NUWAVE,0.D0,NELEM2)
!
      DO IETAGE=1,NPLAN-1
        DO IELEM=1,NELEM2
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
!       FUNCTION F : DZ
!       FUNCTION G : AVERAGE DM1 FOR EACH VERTICAL
        F1=DZ(I1,IETAGE)
        F2=DZ(I2,IETAGE)
        F3=DZ(I3,IETAGE)
        G1=0.5D0*(DM1(I1,IETAGE)+DM1(I1,IETAGE+1))
        G2=0.5D0*(DM1(I2,IETAGE)+DM1(I2,IETAGE+1))
        G3=0.5D0*(DM1(I3,IETAGE)+DM1(I3,IETAGE+1))
        NUWAVE(IELEM)=NUWAVE(IELEM)+
     &   (F3*(G1+G2)+2*(F1*G1+F2*G2+F3*G3)+F2*(G1+G3)+F1*(G2+G3))*XSUR12
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
