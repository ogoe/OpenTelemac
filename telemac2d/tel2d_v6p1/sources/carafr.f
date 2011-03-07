!                    *****************
                     SUBROUTINE CARAFR
!                    *****************
!
     & ( U,V,H,T,UCONV,VCONV,X , Y , SHP ,
     &   SURDET , DT , IKLE , IFABOR , ELT ,
     &   NBOR , NELBOR , NULONE , IELM , NELEM , NELMAX ,
     &   NPOIN , NDP , NPTFR ,
     &   MSK , MASKEL , MASKPT , NPT , LISPFR, NTRAC ,
     &   HBTIL , UBTIL , VBTIL , TBTIL , ZBTIL , ZF, T5  )
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE ADVECTION EQUATIONS BY THE METHOD OF
!+                CHARACTERISTICS, FOR A NUMBER OF FUNCTIONS AND ON AN
!+                ENSEMBLE OF FIXED BOUNDARY POINTS: LISPFR(NPT).
!
!history  E. DAVID (LHF)
!+        05/09/2008
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
!| DT             |-->| PAS DE TEMPS
!| ELT            |---| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!|                |   | CARACTERISTIQUES.
!| H              |---|
!| HBTIL,UBTIL    |---| ..
!|                |   | DE H,U,V,T
!| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
!|                |   | 21 : QUADRANGLE P1
!|                |   | 41 : PRISME DE TEL3D
!| IFABOR         |-->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
!|                |   | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
!| LISPFR         |-->| LISTE DES POINTS FRONTIERES A TRAITER
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
!| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
!|                |   | =1. : NORMAL   =0. : POINT MASQUE.
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
!| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT 2D.
!| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AU BORD.
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
!| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU MAILLAGE.
!| NPT            |-->| NOMBRE DE POINTS FRONTIERES A TRAITER
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NTRAC          |---|
!| NULONE         |-->| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR.
!| SHP            |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!|                |   | COURBES CARACTERISTIQUES.
!| SURDET         |-->| 1/DETERMINANT POUR LES ELEMENTS 2D.
!| T              |---|
!| T5             |---|
!| TBTIL          |---|
!| U              |-->| VARIABLES A L'ETAPE N .
!| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
!| V              |---|
!| VBTIL          |---|
!| X,Y            |-->| COORDONNEES DU MAILLAGE .
!| ZBTIL          |---|
!| ZF             |---|
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
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,NDP,NPTFR,IELM,NPT,NTRAC
      INTEGER, INTENT(IN) :: LISPFR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,NDP),IFABOR(NELMAX,*)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT)          :: ELT(NPOIN)
      LOGICAL, INTENT(IN)             :: MSK
      DOUBLE PRECISION, INTENT(INOUT) :: HBTIL(NPTFR),UBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBTIL(NPTFR),T5(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN),VCONV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBTIL
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSP(1),IFR,IPT,NRK,ITRAC
!
      DOUBLE PRECISION XCONV(1),YCONV(1),DX(1),DY(1)
!
!-----------------------------------------------------------------------
!
! NUMBER OF SUB TIME STEPS OF RUNGE-KUTTA BY PROCESSED ELEMENT
!
      NRK = 3
!
      IF(IELM.EQ.11) THEN
!
!    P1 TRIANGLES
!    ============
!
!      CALLS THE SUBROUTINE UPWINDING THE CURVES OF CHARATERISTICS
!
        DO IFR=1,NPT
          IPT=NBOR(LISPFR(IFR))
          XCONV(1) = X(IPT)
          YCONV(1) = Y(IPT)
          CALL CHAR11(UCONV,VCONV,DT,NRK , X , Y , IKLE , IFABOR ,
     &                XCONV,YCONV,DX,DY , SHP(1,IPT) ,
     &                ELT(IPT) , NSP , 1 , NPOIN , NELEM , NELMAX ,
     &                SURDET , -1 ,T5)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'CARAFR : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'CARAFR : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATION AT THE FOOT OF THE CHARACTERISTICS
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        HBTIL(LISPFR(IFR)) =
     &         H(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + H(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + H(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        UBTIL(LISPFR(IFR)) =
     &         U(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + U(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + U(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        VBTIL(LISPFR(IFR)) =
     &         V(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + V(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + V(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        ZBTIL(LISPFR(IFR)) =
     &         ZF(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + ZF(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + ZF(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
              TBTIL%ADR(ITRAC)%P%R(LISPFR(IFR)) =
     &        T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),3)) * SHP(3,IPT)
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END