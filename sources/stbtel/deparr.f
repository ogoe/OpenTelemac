!                       *****************
                        SUBROUTINE DEPARR
!                       *****************
!
     &(IKLE,NDEPAR,LGVEC)
!
!***********************************************************************
! STBTEL    V5P2                                   28/08/89
!***********************************************************************
!
!brief    DETECTION OF BACKWARD DEPENDENCIES
!
!history  J-M HERVOUET (LNH)
!+        28/08/89
!+        V5P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE      |<--| GLOBAL NUMBERING OF ALL NODES BY ELEMENT
!| NDEPAR    |<--| NUMBER OF BACKWARD DEPENDENCIES
!| MESH      |-->| TYPE OF ELEMENTS IN THE MESH
!| NDP       |-->| NUMBER OF NODE PER ELEMENT
!| NPOIN     |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NELEM     |-->| TOTAL NUMBER OF ELEMENTS IN THE MESH
!| NPMAX     |-->| EFFECTIVE DIMENSION OF ARRAYS X AND Y
!|           |   | (NPMAX = NPOIN + 0.1*NELEM)
!| NELMAX    |-->| EFFECTIVE DIMENSION OF THE ARRAYS DEALING WITH
!|           |   | THE ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER LGVEC , MESH , NDP , NDEPAR , IELEM , NELEM , K
      INTEGER NPMAX , NPOIN , NELMAX
      INTEGER I1 , I2 , I3 , J1 , J2 , J3 , IEL1
      INTEGER IKLE(NELMAX,4)
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!=======================================================================
!
      NDEPAR = 0
      DO 20 IELEM = 1,NELEM
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         DO 30 K = 2,LGVEC
            IEL1 = MOD(NELEM+IELEM-K,NELEM) + 1
            J1 = IKLE(IEL1,1)
            J2 = IKLE(IEL1,2)
            J3 = IKLE(IEL1,3)
            IF (I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3.OR.
     &          I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3.OR.
     &          I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3) NDEPAR = NDEPAR + 1
30       CONTINUE
20    CONTINUE
!
!=======================================================================
!
      RETURN
      END