!                    *****************
                     SUBROUTINE MXPTEL
!                    *****************
!
     &(MXPTVS,MXELVS,IKLES,IELM,NPOIN,NELEM,NDP,IPOBO,LISTIN)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM NUMBER OF POINTS AND ELEMENTS
!+                NEIGHBOURING A POINT FOR A GIVEN TRIANGULAR MESH.
!
!note     ALLOCATES ITRAV HERE, INTERNALLY.
!+         IT'S A LOCAL WORKING VARIABLE ANYWAY.
!+         COULD ALSO PASS THE ELEMENT TYPE IN ARGUMENT TO TREAT
!+         THE 3D SPECIFICALLY.
!
!history  J-M HERVOUET (LNH)
!+        24/08/95
!+        V5P1
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
!| IELM           |---| 
!| IKLES          |-->| TABLE DE CONNECTIVITE (DU FORMAT SELAFIN)
!| IPOBO          |-->| TABLEEAU QUI VAUT 0 POUR LES POINTS INTERIEURS
!|                |   | ET NON NUL POUR LES POINTS DE BORD.
!|                |   | POUR SE PLACER SUR LES ENREGISTREMENTS DES
!| LISTIN         |-->| LOGIQUE : IMPRESSION DE MXELVS ET MXPTVS
!| MXELVS         |<--| NOMBRE MAXIMUM D'ELEMENTS VOISINS.
!| MXPTVS         |<--| NOMBRE MAXIMUM DE POINTS VOISINS.
!| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT.
!| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT) :: MXPTVS,MXELVS
      INTEGER, INTENT(IN)    :: IELM,NDP,NPOIN,NELEM
      INTEGER, INTENT(IN)    :: IKLES(NDP,NELEM),IPOBO(NPOIN)
      LOGICAL, INTENT(IN)    :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                       ITRAV(NPOIN): AUTOMATIC ARRAY
      INTEGER I,J,IELEM,ITRAV(NPOIN)
!
!-----------------------------------------------------------------------
!
! 1) INITIALISES THE NUMBER OF NEIGHBOURING ELEMENTS TO 0:
!
      DO 10 I = 1 , NPOIN
        ITRAV(I) = 0
10    CONTINUE
!
! 2) COUNTS THE NUMBER OF NEIGHBOURING ELEMENTS PER ASSEMBLY OPERATION:
!
      DO 22 J = 1, NDP
        DO 20 IELEM = 1 , NELEM
          ITRAV(IKLES(J,IELEM)) = ITRAV(IKLES(J,IELEM)) + 1
20      CONTINUE
22    CONTINUE
!
! 3) LOOKS FOR THE MAXIMUM :
!
      MXELVS = ITRAV(1)
      DO 30 I = 2 , NPOIN
        MXELVS = MAX(MXELVS,ITRAV(I))
30    CONTINUE
!
! 4) NUMBER OF NEIGHBOURING POINTS: NEED TO ADD 1 TO THIS NUMBER
!                                   FOR BOUNDARY NODES.
!    SIMULTANEOUSLY LOOKS FOR THE MAXIMUM
!
      IF (IELM.EQ.31) THEN
        CALL MXPTEL31(NELEM,NPOIN,MXELVS,IKLES,MXPTVS)
      ELSE
        MXPTVS = MXELVS
        DO 40 I = 1 , NPOIN
          IF(IPOBO(I).NE.0) MXPTVS = MAX(MXPTVS,ITRAV(I)+1)
40      CONTINUE
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        IF(LNG.EQ.1) WRITE(LU,97) MXELVS,MXPTVS
        IF(LNG.EQ.2) WRITE(LU,98) MXELVS,MXPTVS
      ENDIF
97    FORMAT(1X,'MXPTEL (BIEF) : NOMBRE MAXIMUM D''ELEMENTS VOISINS D''
     &UN POINT : ',1I3,/,1X,
     &          '                NOMBRE MAXIMUM DE POINTS VOISINS D''UN
     &POINT : ',1I3)
98    FORMAT(1X,'MXPTEL (BIEF) : MAXIMUM NUMBER OF ELEMENTS AROUND A POI
     &NT: ',1I3,/,1X,
     &          '                MAXIMUM NUMBER OF POINTS AROUND A POINT
     &: ',1I3)
!
!-----------------------------------------------------------------------
!
      RETURN
      END