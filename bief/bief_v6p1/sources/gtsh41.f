!                    *****************
                     SUBROUTINE GTSH41
!                    *****************
!
     &(U,V,WS,X,Y,SHP,SHZ,ELT,ETA,IKLE2,INDIC,NLOC,NPOIN2,NELEM2,NPLAN,
     & LV,MSK,MASKEL)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FIXES THE BARYCENTRIC COORDINATES OF ALL THE MESH
!+                NODES IN THE ELEMENT TOWARDS WHICH POINTS THE
!+                CHARACTERISTIC CURVE, FOR THE TELEMAC-3D PRISMS AND
!+                BEFORE TRACING BACK IN TIME THE CHARACTERISTIC
!+                CURVES.
!
!history  J-M JANIN (LNH)
!+        21/08/2008
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
!| ELT            |<--| NUMEROS DES ELEMENTS CHOISIS POUR CHAQUE
!|                |   | NOEUD.
!| ETA            |---| 
!| IKLE2          |---| 
!| INDIC          |---| 
!| LV             |---| 
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NELEM2         |---| 
!| NLOC           |---| 
!| NPLAN          |---| 
!| NPOIN2         |---| 
!| SHP            |<--| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
!|                |   | LEURS ELEMENTS "ELT" ASSOCIES.
!| SHZ            |---| 
!| U,V            |-->| COMPOSANTES DE LA VITESSE
!| WS             |---| 
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPOIN2,NELEM2,NPLAN,LV
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
      INTEGER, INTENT(INOUT) :: INDIC(NPOIN2),NLOC(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: WS(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2),Y(NPOIN2),MASKEL(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZ(NPOIN2,NPLAN)
!
      LOGICAL, INTENT(IN) :: MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,IPOIN,IELEM,I1,I2,I3
!
!***********************************************************************
!
!     LOOP ON ALL POINTS
!
      DO IPLAN = 1,NPLAN
        DO IELEM=1,NELEM2
          I1=IKLE2(IELEM,1)
          I2=IKLE2(IELEM,2)
          I3=IKLE2(IELEM,3)
          ELT(I1,IPLAN)=IELEM
          ELT(I2,IPLAN)=IELEM
          ELT(I3,IPLAN)=IELEM
          SHP(1,I1,IPLAN)=1.D0
          SHP(2,I1,IPLAN)=0.D0
          SHP(3,I1,IPLAN)=0.D0
          SHP(1,I2,IPLAN)=0.D0
          SHP(2,I2,IPLAN)=1.D0
          SHP(3,I2,IPLAN)=0.D0
          SHP(1,I3,IPLAN)=0.D0
          SHP(2,I3,IPLAN)=0.D0
          SHP(3,I3,IPLAN)=1.D0
        ENDDO
      ENDDO
!
!     ON THE VERTICAL, IT IS DONE DEPENDING ON THE VERTICAL VELOCITY
!
      DO IPLAN = 1,NPLAN
        DO IPOIN=1,NPOIN2
          IF((WS(IPOIN,IPLAN).GT.0.D0.AND.IPLAN.NE.1).OR.
     &                                              IPLAN.EQ.NPLAN) THEN
            ETA(IPOIN,IPLAN) = IPLAN-1
            SHZ(IPOIN,IPLAN) = 1.D0
          ELSE
            ETA(IPOIN,IPLAN) = IPLAN
            SHZ(IPOIN,IPLAN) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END