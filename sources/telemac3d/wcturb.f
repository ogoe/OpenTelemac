!                    *****************
                     SUBROUTINE WCTURB
!                    *****************
!
     &(WC, WCHU0, U, V, W, HN, RUGOF, LISRUF, TRAV1, TRAV2, TRAV3,
     & S, MESH3, IELM3, NPOIN2, NPLAN, TURBA, TURBB, MSK, MASKEL,
     & UETCAR)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    MODELS THE INFLUENCE OF TURBULENCE ON THE
!+                SETTLING VELOCITY:
!code
!+                         (1+A*G)
!+                  WC= WC*________
!+                         (1+B*G*G)
!
!history  C LE NORMANT
!+        01/08/97
!+        V5P4
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  R. KOMANN (BAW), C. SEEGERS, upload by P. TASSI
!+        15/12/2015
!+        V7P1
!+   Correction of parallel issue: call to PARCOM missed
!+   (needed also for GRADF)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HN             |-->| WATER DEPTH
!| IELM3          |-->| TYPE OF ELEMENT
!| LISRUF         |-->| TURBULENCE MODEL FOR BOTTOM
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3          |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| RUGOF          |-->| BOTTOM FRICTION COEFFICIENT
!| S              |-->| VOID STRUCTURE
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| TURBA          |-->| FLOCULATION COEFFICIENT
!| TURBB          |-->| COEFFICIENT RELATIVE TO FLOC DESTRUCTION
!| U              |-->| COMPONENT OF VELOCITY
!| UETCAR         |-->| USTAR**2 FOR BOTTOM
!| V              |-->| COMPONENT OF VELOCITY
!| W              |-->| COMPONENT OF VELOCITY
!| WC             |<->| SEDIMENT SETTLING VELOCITY
!| WCHU0          |-->| CONSTANT SEDIMENT SETTLING VELOCITY (M/S)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_WCTURB => WCTURB
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN2,NPLAN,IELM3,LISRUF
      DOUBLE PRECISION, INTENT(IN)  :: WCHU0,TURBA,TURBB
      LOGICAL, INTENT(IN)           :: MSK
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WC,TRAV1,TRAV2,TRAV3
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,S,HN,RUGOF,UETCAR,U,V,W
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION C
!
!----------------------------------------------------------------------
!
!     COMPUTES THE VELOCITY GRADIENTS
!
      CALL VECTOR(TRAV1,'=','GRADF          Z',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV2,'=','GRADF          Z',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV3,'=','MASBAS          ',IELM3,1.D0,S,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRAV1, 2, MESH3)
        CALL PARCOM(TRAV2, 2, MESH3)
        CALL PARCOM(TRAV3, 2, MESH3)
      ENDIF
!
!  DU/DZ IS TRAV1; DV/DZ IS TRAV2
!
      CALL OS('X=Y/Z   ',TRAV1,TRAV1,TRAV3,C,2,0.D0,1.D-10)
      CALL OS('X=Y/Z   ',TRAV2,TRAV2,TRAV3,C,2,0.D0,1.D-10)
!
      CALL OS('X=XY    ',X=TRAV1,Y=TRAV1)
      CALL OS('X=XY    ',X=TRAV2,Y=TRAV2)
!
      CALL OS('X=X+Y   ',X=TRAV1,Y=TRAV2)
      CALL OS('X=SQR(Y)',X=TRAV2,Y=TRAV1)
!
!----------------------------------------------------------------------
!
!                           AUBORF * U_B * DU/DZ
!     COMPUTES  G  =  SQRT( --------------------- ) : TRAV3
!                             NU
!
      CALL CALCG(TRAV2%R,TRAV3%R,U%R,V%R,UETCAR%R,NPOIN2,NPLAN)
!
!----------------------------------------------------------------------
!
!     COMPUTES 1 + A G  : TRAV1
!
      CALL OS('X=CY    ',X=TRAV1,Y=TRAV3,C=TURBA)
      CALL OS('X=C     ',X=TRAV2,C=1.D0)
      CALL OS('X=X+Y   ',X=TRAV1,Y=TRAV2)
!
!     COMPUTES 1 + B G G  : TRAV2
!
      CALL OS('X=XY    ',X=TRAV3,Y=TRAV3)
      CALL OS('X=X+CY  ',X=TRAV2,Y=TRAV3,C=TURBB)
!
!     COMPUTES WC (...WITH WCHU0-BASE...)
!
      CALL OS('X=Y/Z   ',TRAV1,TRAV1,TRAV2,C,2,0.D0,1.D-10)
      CALL OS('X=CY    ',X=WC,Y=TRAV1,C=WCHU0)
!
!=======================================================================
!
      RETURN
      END
