!                    *****************
                     SUBROUTINE ELEB3D
!                    *****************
!
     &(IKLE3 , NBOR , KP1BOR , NELBOR, IKLBOR, NULONE,
     & NELEM2, NPOIN2, NPLAN, NETAGE, NPTFR )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE 3D MESH.
!+
!+            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!+                       TO ELEBD.
!+
!+            OUTPUT: ARRAYS COMPLETE IN 3D.
!
!history  J-M HERVOUET (LNHE)
!+        23/06/2008
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
!| IKLBOR         |<--| TABLE DE CONNECTIVITE ELEMENTS DE BORD
!| IKLE3          |<--| CORRESPONDANCE LOCALE - GLOBALE EN 3D
!| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!| NBOR           |-->| CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
!|                |   | ET LA NUMEROTATION GLOBALE (2D)
!| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
!| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
!| NETAGE         |-->| NPLAN - 1
!| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
!| NULONE         |<--| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!|                |   | NUMEROTATION LOCALE 3D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEB3D => ELEB3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2, NPOIN2, NPLAN, NETAGE, NPTFR
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,NETAGE,6)
      INTEGER, INTENT(INOUT) :: IKLBOR(NPTFR,NETAGE,4)
      INTEGER, INTENT(INOUT) :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(INOUT) :: NELBOR(NPTFR*NETAGE), NBOR(NPTFR*NPLAN)
      INTEGER, INTENT(INOUT) :: KP1BOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,IPOIN
      INTEGER IETAGE,IPTFR
!
!***********************************************************************
!
! CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR3 ,
! CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS AND 3D LOCAL NUMBERS --> NULONE
!
! COMPUTES NELBO3
!
!     LATERAL BOUNDARIES
!
      DO IETAGE = 1,NETAGE
        DO IPTFR = 1,NPTFR
          IKLBOR(IPTFR,IETAGE,1) =        IPTFR  + (IETAGE-1)*NPTFR
          IKLBOR(IPTFR,IETAGE,2) = KP1BOR(IPTFR) + (IETAGE-1)*NPTFR
          IKLBOR(IPTFR,IETAGE,3) = IKLBOR(IPTFR,IETAGE,2) + NPTFR
          IKLBOR(IPTFR,IETAGE,4) = IKLBOR(IPTFR,IETAGE,1) + NPTFR
          IPOIN = NBOR(IPTFR)
          NBOR(IPTFR +(IETAGE-1)*NPTFR)=IPOIN+(IETAGE-1)*NPOIN2
          IELEM = NELBOR(IPTFR)
          IF(IELEM.GT.0) THEN
            NELBOR(IPTFR+(IETAGE-1)*NPTFR)=IELEM+(IETAGE-1)*NELEM2
            IF(IPOIN.EQ.IKLE3(IELEM,1,1)) THEN
              NULONE(IPTFR,IETAGE,1) = 1
              NULONE(IPTFR,IETAGE,2) = 2
              NULONE(IPTFR,IETAGE,3) = 5
              NULONE(IPTFR,IETAGE,4) = 4
            ELSEIF(IPOIN.EQ.IKLE3(IELEM,1,2)) THEN
              NULONE(IPTFR,IETAGE,1) = 2
              NULONE(IPTFR,IETAGE,2) = 3
              NULONE(IPTFR,IETAGE,3) = 6
              NULONE(IPTFR,IETAGE,4) = 5
            ELSEIF(IPOIN.EQ.IKLE3(IELEM,1,3)) THEN
              NULONE(IPTFR,IETAGE,1) = 3
              NULONE(IPTFR,IETAGE,2) = 1
              NULONE(IPTFR,IETAGE,3) = 4
              NULONE(IPTFR,IETAGE,4) = 6
            ELSE
              IF(LNG.EQ.1) WRITE(LU,101) IPOIN
              IF(LNG.EQ.2) WRITE(LU,102) IPOIN
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSEIF(NCSIZE.GT.1) THEN
            NULONE(IPTFR,IETAGE,1) = 0
            NULONE(IPTFR,IETAGE,2) = 0
            NULONE(IPTFR,IETAGE,3) = 0
            NULONE(IPTFR,IETAGE,4) = 0
            NELBOR(IPTFR+(IETAGE-1)*NPTFR)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,101) IPOIN
            IF(LNG.EQ.2) WRITE(LU,102) IPOIN
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDDO
!
!     COMPLETES NBOR IN VIEW OF 2D VALUES
!
      DO IPTFR = 1,NPTFR
         NBOR(IPTFR +(NPLAN-1)*NPTFR) = NBOR(IPTFR) + NETAGE*NPOIN2
      END DO
!
!-----------------------------------------------------------------------
!
101   FORMAT(' ELEB3D : PROBLEME A LA CONSTRUCTION DE NULONE, IPOIN =',
     &  I6)
102   FORMAT(' ELEB3D: PROBLEM WHEN BUILDING NULONE, IPOIN =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END