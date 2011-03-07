!                    **************
                     SUBROUTINE MAJ
!                    **************
!
     &(NS,NSEG,NPTFR,G,DT,AIRS,
     & H,QU,QV,UA,CE,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,SMH,KFROT,CF)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE HYDRO SOLUTIONS AT TIME N+1
!+                USING AN EXPLICIT SCHEME.
!
!history  INRIA
!+        
!+        V5P4
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
!| AIRS           |-->| AIRES DES CELLULES
!| CE             |-->| FLUX
!| CF             |-->| COEFFICIENT DE FROTTEMENT
!| DT             |-->| PAS DE TEMPS
!| G              |-->| CONSTANTE DE GRAVITE
!| H              |-->| HAUTEURS D'EAU AU TEMPS N
!| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
!| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
!| QU,QV          |-->| COMPOSANTES DU DEBIT AU TEMPS N
!| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
!| UA             |<--| H, QU, QV  AU TEMPS N+1
!| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_MAJ => MAJ
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,NSEG,NPTFR,KNEU,KFROT
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: H(NS),QU(NS),QV(NS),SMH(NS)
      DOUBLE PRECISION, INTENT(IN) :: CE(3,NS),CF(NS),AIRS(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,DT
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
!
      DOUBLE PRECISION USAIS
!
      INTRINSIC ABS
!
!-----------------------------------------------------------------------
!
! EXPLICIT RESOLUTION
!
!     UPDATES THE PHYSICAL SOLUTION
!
      DO IS =1,NS
!
        USAIS = DT/AIRS(IS)
        UA(1,IS)  = H(IS) + USAIS*(CE(1,IS)+SMH(IS))
        UA(2,IS)  = QU(IS) + USAIS*CE(2,IS)
        UA(3,IS)  = QV(IS) + USAIS*CE(3,IS)
!
      ENDDO
!
!     PROJECTS ON THE SLIPPING BOUNDARY CONDITIONS
!
      CALL CDLPROJ(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,UA)
!
      DO IS =1,NS
        IF(UA(1,IS).LE.1.D-12) UA(1,IS)=0.D0
        IF(ABS(UA(2,IS)).LE.1.D-12) UA(2,IS)=0.D0
        IF(ABS(UA(3,IS)).LE.1.D-12) UA(3,IS)=0.D0
      ENDDO
!
      IF(KFROT.NE.0) CALL FRICTION(NS,G,DT,UA,H,QU,QV,CF)
!
!-----------------------------------------------------------------------
!
      RETURN
      END