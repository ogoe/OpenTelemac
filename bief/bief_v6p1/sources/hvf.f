!                    **************
                     SUBROUTINE HVF
!                    **************
!
     &(H,HN,FXMAT,UNSV2D,DT,FXBOR,SMH,YASMH,NSEG,NPOIN,NPTFR,GLOSEG,
     & SIZGLO,NBOR,OPTSOU,T7,MESH,MSK)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AN INTERMEDIATE DEPTH IF THERE ARE
!+                SUB-ITERATIONS.
!
!history  CHI-TUAN PHAM (LNHE)
!+        09/02/2009
!+        V5P9
!+   JMH : SEQUENCE IF(MSK) : AVOIDS NEGATIVE DEPTHS 
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
!| DT             |-->| PAS DE TEMPS.
!| FXBOR          |-->| FLUX AU BORD (DEFINI SUR TOUT LE DOMAINE
!|                |   | ET ASSEMBLE EN PARALLELE)
!| FXMAT          |-->| MATRICE DE STOCKAGE DES FLUX.
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!| H              |<--| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!| HN             |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N.
!| MESH           |---| 
!| MSK            |-->| MSK : IF YES, MASKING OF DRY ELEMENTS
!| NBOR           |-->| TABLEAU D'INDICES DE NOEUDS SUR LE BORD.
!| NPOIN          |-->| NOMBRE DE NOEUDS DANS LE MAILLAGE.
!| NPTFR          |-->| NOMBRE DE NOEUDS SUR LA FRONTIERE.
!| NSEG           |-->| NOMBRE DE SEGMENTS DANS LE MAILLAGE.
!| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
!|                |   | 1: NORMAL  2: DIRAC
!|                |   | SEE PROPAG IN TELEMAC-2D
!| SIZGLO         |---| 
!| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE.
!| T7             |---| 
!| UNSV2D         |---| 
!| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_HVF => HVF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,OPTSOU,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG*2)
      LOGICAL, INTENT(IN)             :: YASMH,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,N
!
!-----------------------------------------------------------------------
!
      DO I = 1,NPOIN
        H(I) = HN(I)
      ENDDO
!
!     SOURCES TERMS
!
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*SMH(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*UNSV2D(I)*SMH(I)
          ENDDO
        ENDIF
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO I = 1,NPOIN
          T7%R(I) = 0.D0
        ENDDO
        DO I = 1,NSEG
          T7%R(GLOSEG(I,1))=T7%R(GLOSEG(I,1))
     &                     -DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          T7%R(GLOSEG(I,2))=T7%R(GLOSEG(I,2))
     &                     +DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
        CALL PARCOM(T7,2,MESH)
        DO I = 1,NPOIN
          H(I) = H(I) + T7%R(I)
        ENDDO
      ELSE
        DO I = 1,NSEG
          H(GLOSEG(I,1))=H(GLOSEG(I,1))-DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          H(GLOSEG(I,2))=H(GLOSEG(I,2))+DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
      ENDIF
!
!     ON THE BOUNDARIES : BOUNDARY FLUX TERMS
!
      DO I=1,NPTFR
        N=NBOR(I)
        H(N) = H(N) - DT*UNSV2D(N)*FXBOR(N)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     WHEN NEGATIVE DEPTHS APPEAR WHILE COMPUTING H, THE PREVIOUS
!     VALUE OF H IS KEPT
!
      IF(MSK) THEN
        DO I = 1,NPOIN
          IF(H(I).LT.0.D0) H(I) = MAX(1.D-2,HN(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END