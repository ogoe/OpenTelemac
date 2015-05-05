!                    ********************************
                     SUBROUTINE GET_NODES_PER_ELEMENT
!                    ********************************
!
     &(TYP_ELT, NDP)
!
!***********************************************************************
! BIEF   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE EXTENSION FOR NAMING FILES IN PARALLEL
!+
!
!history  J-M HERVOUET (LNHE)
!+        11/07/2008
!+        V5P9
!+  
!
!history  J-M HERVOUET (LNHE)
!+        22/11/2012
!+        V6P3
!+   USE BIEF removed, IIPID and IPID changed into I.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| N              |-->| TOTAL NUMBER OF PROCESSORS
!| I              |-->| RANK OF THE PROCESSOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      INTEGER, INTENT(IN) :: TYP_ELT
      INTEGER, INTENT(INOUT) :: NDP
!
!-----------------------------------------------------------------------
!
      NDP = 0
      SELECT CASE(TYP_ELT)
      CASE(TYPE_NULL)
        NDP = 0
      CASE(POINT_ELT_TYPE)
        NDP = 1
      CASE(POINT_BND_ELT_TYPE)
        NDP = 1
      CASE(TRIANGLE_ELT_TYPE)
        NDP = 3
      CASE(QUADRANGLE_ELT_TYPE)
        NDP = 4
      CASE(TETRAHEDRON_ELT_TYPE)
        NDP = 4
      CASE(PRISM_ELT_TYPE)
        NDP = 6
      CASE(SPLIT_PRISM_ELT_TYPE)
        NDP = 7
      CASE(EDGE_BND_ELT_TYPE)
        NDP = 2
      CASE(TRIANGLE_BND_ELT_TYPE)
        NDP = 3
      CASE(QUADRANGLE_BND_ELT_TYPE)
        NDP = 4
      CASE(TRIANGLE_3D_BND_ELT_TYPE)
        NDP = 3
      CASE DEFAULT
        IF(LNG.EQ.1) WRITE(LU,*) 'UNKNWOWN ELEMENT TYPE ',TYP_ELT
        IF(LNG.EQ.2)WRITE(LU,*) 'TYPE D ELEMENT INCONNU ',TYP_ELT
        CALL PLANTE(1)
      END SELECT
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE 

