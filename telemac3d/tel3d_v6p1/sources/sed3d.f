!                    ****************
                     SUBROUTINE SED3D
!                    ****************
!
     &(MASSE1,WC,TA,EPAI,HDEP,CONC,FLUER,PDEPOT,TRA02,
     & NELEM2,NPOIN2,NPOIN3,NPFMAX,NCOUCH,
     & NPF,DT,TASSE,GIBSON,RHOS,CFDEP,VOLU2D)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RELATIVE MASS BALANCE FOR THE
!+                SEDIMENT DURING A TIMESTEP.
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
!history  J-M HERVOUET (LNHE)
!+        18/03/2011
!+        V6P1
!+   Call to massed replaces the old (and duplicated) formula.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!| CONC           |-->| CONCENTRATION DES COUCHES DU FOND
!| DT             |-->| PAS DE TEMPS
!| EPAI           |-->| TAILLE DES MAILLES DU FOND EN
!|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+IVIDE))
!| FLUER          |-->| FLUX D'EROSION EN CHAQUE POINT 2D
!| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
!| HDEP           |-->| HAUTEUR DES DEPOTS FRAIS (COUCHE TAMPON)
!| IKLE2          |-->| TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
!| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| MASSE1         |<->| MASSE DU SEDIMENT EN SUSPENSION
!| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
!| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
!| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!|                |   | DISCRETISANT LE FOND VASEUX(MODELE DE GIBSON
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPOIN3         |-->| NOMBRE DE POINTS 3D
!| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
!| NVBIL          |---|
!| PDEPOT         |-->| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
!| SURFAC         |-->| SURFACES DES ELEMENTS.
!| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!| TRA02          |---|
!| U,V,W          |-->| VITESSE AU PAS DE TEMPS PRESENT
!| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
!| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
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
      INTEGER, INTENT(IN) :: NPFMAX,NCOUCH,NELEM2,NPOIN2,NPOIN3
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: MASSE1
      DOUBLE PRECISION, INTENT(IN) :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3),VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HDEP(NPOIN2),FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: PDEPOT(NPOIN2),CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,RHOS,CFDEP
!
      LOGICAL, INTENT(IN)             :: TASSE , GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION MASSE3,MASSE4,MASSE5,FLUX
!
      INTEGER I
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
!
! COMPUTES THE MASS OF ERODED SEDIMENTS (MASSE3)
! DURING THE TIMESTEP
!
!=======================================================================
!
      FLUX=0.D0
!
      DO I=1,NPOIN2
        FLUX=FLUX+FLUER(I)*VOLU2D(I)
      ENDDO
!
      MASSE3=FLUX*DT
      IF(NCSIZE.GT.1) MASSE3=P_DSUM(MASSE3)
!
!=======================================================================
!
! COMPUTES THE MASS OF DEPOSITED SEDIMENTS (MASSE4) DURING THE TIMESTEP
!
!=======================================================================
!
      FLUX=0.D0
!
      DO I=1,NPOIN2
        FLUX=FLUX-PDEPOT(I)*WC(I)*TA(I)*VOLU2D(I)
      ENDDO
!
      MASSE4=FLUX*DT
      IF(NCSIZE.GT.1) MASSE4=P_DSUM(MASSE4)
!
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT EXCHANGED (MASSE5)
! BETWEEN THE MUDDY BED AND THE FLUID DURING THE TIMESTEP
!
!=======================================================================
!
      MASSE5=MASSE4-MASSE3
      IF(MASSE5.LE.1.D-8) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'MASSE NETTE DE SEDIMENT ERODE        : ',-MASSE5
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NET MASS OF ERODED SEDIMENT          : ',-MASSE5
        ENDIF
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'MASSE NETTE DE SEDIMENTS DEPOSES     : ',MASSE5
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NET MASS OF DEPOSIT                  : ',MASSE5
        ENDIF
      ENDIF
!
!=======================================================================
!
! COMPUTES THE MASS OF MUDDY DEPOSITS ON THE RIGID BOTTOM AND PRINTS IT
!
!=======================================================================
!
      CALL MASSED(MASSE1,EPAI,CONC,HDEP,TRA02,NPOIN2,NPFMAX,NCOUCH,
     &            NPF,TASSE,GIBSON,RHOS,CFDEP,VOLU2D)
!
!=======================================================================
!
      RETURN
      END
