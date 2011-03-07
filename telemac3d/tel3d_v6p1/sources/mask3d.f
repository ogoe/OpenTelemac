!                    *****************
                     SUBROUTINE MASK3D
!                    *****************
!
     &(IFABOR3D,MASKEL,MASKPT,MASKBR,
     & X,Y,ZF,ZFE,H,HMIN,AT,LT,ITRA01,NELBO3,
     & NELMA2,NELEM2,NPOIN2,NPTFR,NPLAN,NETAGE,IELM3,MESH2D)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  JMH (LNHE)     ; J-M JANIN (LNH)    ; F LEPEINTRE (LNH)
!+        21/10/08
!+        V5P9
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
!| AT             |-->| TEMPS DU PAS DE TEMPS
!| H              |-->| HAUTEUR D'EAU
!| HMIN           |-->| HAUTEUR D'EAU MINIMALE ACCEPTABLE
!| IELM3          |-->| TYPE DE DISCRETISATION 3D
!| IFABOR3D       |<--| TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
!| ITRA01         |-->| TABLEAUX DE TRAVAIL D'ENTIERS
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| MASKBR         |---|
!| MASKEL         |<--| MASQUAGE DES ELEMENTS
!| MASKPT         |-->| MASQUAGE DES POINTS
!| MESH2D         |---|
!| NELBO3         |---|
!| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
!| NELMA2         |-->| NOMBRE MAXIMAL D'ELEMENTS EN 2D
!| NETAGE         |-->| NPLAN - 1
!| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
!| ZF             |-->| COTES DU FOND
!| ZFE            |-->| COTE DU FOND PAR ELEMENT.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM2, NPOIN2, NETAGE, NPLAN
      INTEGER, INTENT(IN)             :: NELMA2, NPTFR
      INTEGER, INTENT(INOUT)          :: IFABOR3D(NELEM2,5,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM2,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKBR(NPTFR,NETAGE)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2), Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2), H(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZFE(NELEM2)
      INTEGER, INTENT(IN)             :: NELBO3(NPTFR,NETAGE)
      INTEGER, INTENT(INOUT)          :: ITRA01(NELEM2)
      INTEGER, INTENT(IN)             :: LT, IELM3
      DOUBLE PRECISION, INTENT(IN)    :: HMIN, AT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: MASKPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IETAGE,IADR,IPLAN
!
!***********************************************************************
!
! INITIALISES MASKEL IF MASK!
!  MASKS TIDAL FLATS (MASKBD)
!  MASKS A USER-DEFINED AREA (MASKOB)
!  FILLS IN MASKPT (MASKTO)
!=======================================================================
!
      CALL OV('X=C     ',MASKEL,MASKEL,MASKEL,1.D0,NELEM2)
!
      CALL MASKBD(MASKEL,ZFE,ZF,H,HMIN,MESH2D%IKLE%I,MESH2D%IFABOR%I,
     &            ITRA01,NELEM2,NPOIN2)
!
      CALL MASKOB(MASKEL,X,Y,MESH2D%IKLE%I,NELEM2,NELMA2,NPOIN2,AT,LT)
!
      CALL MASKTO(MASKEL,MASKPT,IFABOR3D,MESH2D%IKLE%I,
     &            MESH2D%IFABOR%I,MESH2D%ELTSEG%I,MESH2D%NSEG,
     &            NELEM2,NPOIN2,IELM3,MESH2D)
!
!=======================================================================
!     COPIES MASKEL ON HIGHER LEVELS
!=======================================================================
!
      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            MASKEL(IELEM2,IETAGE) = MASKEL(IELEM2,1)
          ENDDO
        ENDDO
      ENDIF
!
!=======================================================================
!     COPIES MASKPT ON HIGHER LEVELS
!=======================================================================
!
      DO IPLAN=2,NPLAN
        IADR=(IPLAN-1)*NPOIN2
        CALL OV('X=Y     ',MASKPT%R(IADR+1:IADR+NPOIN2),
     &                     MASKPT%R,MASKPT%R,0.D0,NPOIN2)
      ENDDO
!
!=======================================================================
!     INITIALISES MASKBR
!=======================================================================
!
      CALL OV ( 'X=C     ',MASKBR,MASKBR,MASKBR,1.D0,NPTFR*NETAGE)
!
!     AND SETS IT WITH THE NEIGHBOURING MASKEL
!
      CALL OVBD ('X=Y     ',MASKBR,MASKEL,MASKEL,
     &           0.D0,NELBO3,NPTFR*NETAGE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END