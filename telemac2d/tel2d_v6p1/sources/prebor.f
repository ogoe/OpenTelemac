!                    *****************
                     SUBROUTINE PREBOR
!                    *****************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,HN,T,NBOR,KP1BOR,NPOIN,NPTFR,
     & NTRAC,DEBLIQ,FINLIQ,NFRLIQ)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE BOUNDARY CONDITIONS FOR TREATMENT BY THOMPSON.
!+     UBOR, VBOR, HBOR, TBOR ARE INITIALISED HERE WITH VALUES AT TIME N
!+     AFTER BORD, THESE ARRAYS THEREFORE CONTAIN EITHER THE VALUE AT
!+     TIME N OR THE IMPOSED VALUE.
!+
!+
!+            STORES H IN A TEMPORARY ARRAY TO SAVE ITS VALUE AT
!+     THE BOUNDARY AT TIME N (MODIFIED IN BORD).
!
!history  E DAVID (LHF)
!+        03/09/2007
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
!| DEBLIQ         |-->| TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
!| FINLIQ         |-->| TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
!| H              |-->| HAUTEUR AU TEMPS N
!| HBOR           |<--| HAUTEUR IMPOSEE.
!| HN             |-->| HAUTEUR DE PROPAGATION (OPTION H-U)
!| KP1BOR         |-->| NUMERO DU POINT FRONTIERE SUIVANT
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NTRAC          |-->| NOMBRE DE TRACEURS
!| T              |-->| TRACEUR AU TEMPS N
!| TBOR           |<--| TRACEUR IMPOSE AU BORD
!| U,V            |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
!| UBOR           |<--| VITESSE U IMPOSEE.
!| VBOR           |<--| VITESSE V IMPOSEE.
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
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NFRLIQ,NTRAC
      INTEGER, INTENT(IN)             :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),KP1BOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,IFRLIQ,ITRAC
!
      DOUBLE PRECISION C
!
      LOGICAL DEP
!
!-----------------------------------------------------------------------
!
!  LOOP ON THE LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
!
      DO 10 IFRLIQ = 1 , NFRLIQ
!
        DEP = .FALSE.
        K = DEBLIQ(IFRLIQ)
11      CONTINUE
        UBOR(K)=U(NBOR(K))
        VBOR(K)=V(NBOR(K))
        HBOR(K)=H(NBOR(K))
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K)=T%ADR(ITRAC)%P%R(NBOR(K))
          ENDDO
        ENDIF
        IF(K.EQ.FINLIQ(IFRLIQ).AND.DEP) THEN
          GO TO 12
        ELSE
          DEP=.TRUE.
          K = KP1BOR(K,1)
          GO TO 11
        ENDIF
12      CONTINUE
!
10    CONTINUE
!
      ENDIF
!
      CALL OV( 'X=Y     ' , HN , H , H , C , NPOIN )
!
!-----------------------------------------------------------------------
!
      RETURN
      END