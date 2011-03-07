!                    *****************
                     SUBROUTINE MASSED
!                    *****************
!
     & (MASSE, TA, X, Y, Z, IVIDE, EPAI, CONC, HDEP,
     & SURFAC, TRAV1, TRA02, S, IKLE2, MESH3, IELM3,
     & NPLAN, NELEM2, NELEM3, NPOIN2, NPOIN3, NTRAC, NVBIL,
     & NPFMAX, NCOUCH, NPF, TASSE, GIBSON, RHOS, CFDEP, MSK, MASKEL)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    PERFORMS INITIAL RELATIVE MASS BALANCE FOR
!+                THE SEDIMENT.
!
!history  C.LE NORMANT(LNH)
!+        26/08/92
!+        V5P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!| CONC           |-->| CONCENTRATIONS DES COUCHES DU FOND VASEUX
!| EPAI           |-->| TAILLE DES MAILLES DU FOND EN
!|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
!| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
!| HDEP           |-->| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!| IELM3          |---| 
!| IKLE2          |-->| TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
!| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS SECS
!| MASSE          |---| 
!| MESH3          |---| 
!| MSK            |---| 
!| NCOUCH         |---| 
!| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
!| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
!| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!|                |   | DISCRETISANT LE FOND VASEUX
!| NPLAN          |-->| NOMBRE DE PLANS SUR LA VERTICALE
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPOIN3         |-->| NOMBRE DE POINTS 3D
!| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
!| NVBIL          |---| 
!| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
!| S              |---| 
!| SURFAC         |-->| SURFACES DES ELEMENTS.
!| TA             |-->| CONCENTRATION DU SEDIMENT EN SUSPENSION
!| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!| TRA02          |---| 
!| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPFMAX, NELEM2, NELEM3, NPOIN2, NPOIN3
      INTEGER, INTENT(IN)    :: IELM3, NPLAN, NCOUCH
      INTEGER, INTENT(IN)    :: NTRAC, NVBIL
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3), NPF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSE
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3) ,Y(NPOIN3) ,Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2), CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(IN)      :: TA   ! CALLED AS TA%ADR(NTRAC)%
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRAV1, S
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3
      DOUBLE PRECISION, INTENT(IN)    :: RHOS, CFDEP
      LOGICAL, INTENT(IN)             :: MSK, TASSE, GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER L1, L2, L3, IPOIN, IPF, IELEM2
      DOUBLE PRECISION MASSE1, MASSE6
!
!=======================================================================
!
! MASS OF MUDDY DEPOSITS ON THE RIGID BED (MASSE6)
!
!=======================================================================
!
      CALL OV('X=CY    ', TRA02 , HDEP , Z , CFDEP , NPOIN2)
!
      IF(TASSE) THEN
        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          DO IPF=1,NCOUCH
            TRA02(IPOIN)=TRA02(IPOIN)+CONC(IPF)*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ELSEIF(GIBSON) THEN
        DO IPOIN=1,NPOIN2
          DO IPF=1,NPF(IPOIN)-1
            TRA02(IPOIN)=TRA02(IPOIN)+RHOS*EPAI(IPF,IPOIN)
          ENDDO
        ENDDO
      ENDIF
      MASSE6=0.D0
      DO IELEM2=1,NELEM2
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        MASSE6=MASSE6+SURFAC(IELEM2)
     &                            *(TRA02(L1)+TRA02(L2)+TRA02(L3))/3.D0
      ENDDO
      WRITE(LU,*)
     &      'MASSE INITIALE DES DEPOTS VASEUX :                       ',
     &                MASSE6
!
!=======================================================================
!
! TOTAL MASS OF SEDIMENT IN THE DOMAIN (MASSE1)
!
!=======================================================================
!
      MASSE1=MASSE+MASSE6
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)
     &  'MASSE TOTALE DE SEDIMENTS DANS LE DOMAINE:               ',
     &  MASSE1
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)
     &  'TOTAL MASS OF SEDIMENTS IN THE DOMAIN:                   ',
     &  MASSE1
      ENDIF
!
!=======================================================================
!
      RETURN
      END