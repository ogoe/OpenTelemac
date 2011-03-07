!                    *****************
                     SUBROUTINE FLUHYD
!                    *****************
!
     &(NS,NT,NSEG,NPTFR,NUBO,G,DT,X,Y,AIRS,NU,AIRE,
     & UA,ZF,VNOIN,CE,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     & KDDL,HBOR,UBOR,VBOR,FLUENT,FLUSORT,NORDRE,CMI,JMI,
     & DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     & DPX,DPY,IVIS,CVIS,FLUHBTEMP,BETA,DSZ,AIRST,HC,FLUXTEMP,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FLUXES AT TIME N.
!
!history  INRIA
!+        
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
!| AIRE           |-->| AIRES DES TRIANGLES
!| AIRS           |-->| AIRES DES CELLULES
!| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
!| BETA           |---| COEFFICIENT EXTRAPOLATION POUR ORDRE 2
!| CE             |<--| FLUX   +  TERMES DIFFUSION
!| CFLWTD         |-->| NOMBRE DE CFL
!| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
!| CVIS           |-->| COEFFICIENT DE DIFFUSION DES VITESSES
!| DJX,DJY        |---| GRADIENTS PAR TRIANGLES
!| DSZ            |-->| VARIATIONS DE Z POUR ORDRE 2
!| DT             |<->| PAS DE TEMPS
!| DTHAUT         |-->| UTILISE POUR CONDITION CFL
!| DX,DY          |---| GRADIENTS PAR NOEUDS
!| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
!| FLUHBTEMP      |<--| FLUX BORD POUR TRACEUR
!| FLUXTEMP       |<--| FLUX POUR TRACEUR
!| G              |-->| CONSTANTE DE GRAVITE
!| HBOR           |-->| VALEURS IMPOSEES DE H
!| HC             |<--| H RECONSTRUIT ORDRE 2   CORRIGE
!| IVIS           |-->| OPTION DIFFUSION DES VITESSES
!| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!|                |   | POINT MILIEU DE L'INTERFACE
!| KDDL           |-->| CONVENTION POUR LES POINTS LIBRES
!| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
!| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NORDRE         |-->| ORDRE DU SCHEMA
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
!| NT             |-->| NOMBRE D'ELEMENTS DU MAILLAGE
!| NTRAC          |---| 
!| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
!| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| UBOR           |-->| VALEURS IMPOSEES DE U
!| VBOR           |-->| VALEURS IMPOSEES DE V
!| VNOIN          |-->| NORMALE A L'INTERFACE
!|                |   | (2 PREMIERES COMPOSANTES) ET
!|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
!| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
!| ZF             |-->| COTES DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUHYD => FLUHYD
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NT,NSEG,NPTFR,KDIR,KNEU,KDDL,NORDRE
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6),NU(NT,3)
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),JMI(*),IVIS,NTRAC
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: HBOR(NPTFR),G,CFLWTD,DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN) :: UBOR(NPTFR),VBOR(NPTFR),CMI(2,*)
      DOUBLE PRECISION, INTENT(IN) :: AIRST(2,*),CVIS
      DOUBLE PRECISION, INTENT(IN) :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA,DT,HC(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(3,NS),FLUENT,FLUSORT
      DOUBLE PRECISION, INTENT(IN) :: UA(3,NS),ZF(NS),VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: DSZ(2,*),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,*),DY(3,*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUXTEMP,FLUHBTEMP,FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,IVAR
!
!-----------------------------------------------------------------------
!
!     EXPLICIT RESOLUTION
!
      DO IS=1,NS
        DO IVAR=1,3
          CE(IVAR,IS) = 0.D0
        ENDDO
      ENDDO
!
!   COMPUTES GRADIENTS AT NODES AS WELL AS DIFFUSION TERMS
!
      IF(NORDRE.EQ.2.OR.IVIS.EQ.1) CALL GRADNOD(NS,NT,NU,AIRE,AIRS,
     &                        UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF)
!
      CALL FLUCIN(NS,NSEG,NUBO,G,X,Y,CFLWTD,DT,UA,ZF,VNOIN,CE,NORDRE,
     &            CMI,JMI,DJX,DJY,DX,DY,BETA,DSZ,AIRS,
     &            AIRST,HC,FLUXTEMP,NPTFR,NBOR,XNEBOR,YNEBOR,NTRAC)
!
!        BOUNDARY CONDITIONS TREATMENT
!
      CALL CDL(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &         G,HBOR,UBOR,VBOR,UA,CE,FLUENT,FLUSORT,FLBOR,
     &         DTHAUT,DT,CFLWTD,FLUHBTEMP,NTRAC)
!
!-----------------------------------------------------------------------
!
      RETURN
      END