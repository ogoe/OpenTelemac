!                    ****************
                     SUBROUTINE SED3D
!                    ****************
!
     & (MASSE1,U,V,W,WC,TA,X,Y,Z,
     &  IVIDE,EPAI,HDEP,CONC,FLUER,PDEPOT,SURFAC,TRA01,TRA02,
     &  IKLE2,NELEM2,NPOIN2,NPOIN3,NTRAC,NVBIL,NPFMAX,NCOUCH,
     &  NPF,LT,AT,DT,INFO,TASSE,GIBSON,RHOS,CFDEP)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
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
!| INFO           |-->| LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
!| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| MASSE1         |<->| MASSE DU SEDIMENTEN SUSPENSION
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPFMAX, NCOUCH, NELEM2, NPOIN2
      INTEGER, INTENT(IN) :: NPOIN3, NTRAC, LT, NVBIL
!
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASSE1
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN3), V(NPOIN3), W(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: PDEPOT(NPOIN2), CONC(NCOUCH)
!
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3), TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,AT,RHOS,CFDEP
!
      LOGICAL, INTENT(IN)             :: INFO , TASSE , GIBSON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION MASSE3, MASSE4, MASSE5, MASSE6
      DOUBLE PRECISION FLUX, MTOTAL
!
      INTEGER IPTFR, IPLAN, L1, L2, L3, IPOIN, IPF, IELEM2
!
      INTRINSIC SQRT
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
      DO IELEM2=1,NELEM2
!
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        FLUX=FLUX+SURFAC(IELEM2)*(FLUER(L1)+FLUER(L2)+FLUER(L3))/3.D0
!
       ENDDO
!
       MASSE3=FLUX*DT
!
!=======================================================================
!
! COMPUTES THE MASS OF DEPOSITED SEDIMENTS (MASSE4) DURING THE TIMESTEP
!
!=======================================================================
!
      FLUX=0.D0
!
      DO IELEM2=1,NELEM2
!
        L1=IKLE2(IELEM2,1)
        L2=IKLE2(IELEM2,2)
        L3=IKLE2(IELEM2,3)
        FLUX=FLUX-SURFAC(IELEM2)*(PDEPOT(L1)*WC(L1)*TA(L1)+
     &                            PDEPOT(L2)*WC(L2)*TA(L2)+
     &                            PDEPOT(L3)*WC(L3)*TA(L3))/3.D0
      ENDDO
!
      MASSE4=FLUX*DT
!
!            IF (INFO) WRITE(LU,*)
!     &      'MASSE DE SEDIMENTS DEPOSEE AU COURS DU PAS DE TEMPS:     '
!     &                MASSE4
!
!=======================================================================
!
! COMPUTES THE MASS OF SEDIMENT EXCHANGED (MASSE5)
! BETWEEN THE MUDDY BED AND THE FLUID DURING THE TIMESTEP
!
!=======================================================================
!
! MASSE5=MASSE4-MASSE3
!
      MASSE5=MASSE4-MASSE3
      IF(INFO) THEN
        IF(MASSE5.LE.1.D-8) THEN
              WRITE(LU,*)
     &      'MASSE NETTE DE SEDIMENTS QUI PART EN SUSPENSION:         ',
     &             -MASSE5
        ELSE
              WRITE(LU,*)
     &      'MASSE NETTE DE SEDIMENTS QUI SE DEPOSENT       :         ',
     &              MASSE5
        ENDIF
      ENDIF
!
!=======================================================================
!
! COMPUTES THE MASS OF MUDDY DEPOSITS ON THE RIGID BOTTOM (MASSE6)
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
      ELSEIF (GIBSON) THEN
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
      IF (INFO) WRITE(LU,*)
     &      'MASSE TOTALE DES DEPOTS VASEUX:                          ',
     &                MASSE6
!
!=======================================================================
!
! TOTAL MASS OF SEDIMENTS IN THE DOMAIN (MTOTAL)
!
!=======================================================================
!
      MTOTAL=MASSE1+MASSE6
            IF(INFO) WRITE(LU,*)
     &      'MASSE TOTALE DE SEDIMENTS DANS LE DOMAINE:               ',
     &                MTOTAL
!
!
!=======================================================================
!
      RETURN
      END SUBROUTINE SED3D