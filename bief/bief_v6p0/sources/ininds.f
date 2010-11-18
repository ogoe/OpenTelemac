C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE COMMON NODES, WHICH IS USED BY
!>                FUNCTIONS NBPTS, NBMPTS, NBFEL, AND NBPEL.
!>  @code
!> INDEX 1 OF NDS : NUMBER OF DEGREES OF FREEDOM IN THE MESH FOR THIS ELEMENT
!> INDEX 2 OF NDS : NUMBER OF SEGMENTS IN THE MESH FOR THIS ELEMENT
!> INDEX 3 OF NDS : NUMBER OF POINTS PER ELEMENT
!> INDEX 4 OF NDS : NUMBER OF FACES PER ELEMENT (EX 3 FOR A TRIANGLE)
!> INDEX 5 OF NDS : MAXIMUM NUMBER OF DEGREES OF FREEDOM IN THE MESH
!>                  FOR THIS ELEMENT
!> INDEX 6 OF NDS : NUMBER OF SEGMENTS FOR THIS ELEMENT
!>
!> FUNCTIONS IN BIEF:
!>
!> INDEX 1 OF NDS : NBPTS
!> INDEX 2 OF NDS : NBSEG
!> INDEX 3 OF NDS : NBPEL
!> INDEX 4 OF NDS : NBFEL
!> INDEX 5 OF NDS : NBMPTS
!> INDEX 6 OF NDS : NBSEGEL
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NELEB, NELEM, NELMAX, NPLAN, NPMAX, NPOIN, NPTFR, NPTFX, NSEGBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> NODES : NDS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 24/10/2008
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18; REGINA NEBAUER; LAM MINH PHUONG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NELEB
!></td><td>--></td><td>NOMBRE D'ELEMENTS DE BORD 3D NON STRUCTURE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE SOMMETS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE SOMMETS DU MAILLAGE 2D
!>                  (SAUF TETRAEDRES OU C'EST LE NOMBRE TOTAL)
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE SOMMETS DE FRONTIERE
!>    </td></tr>
!>          <tr><td>NPTFX
!></td><td>--></td><td>NOMBRE MAXIMUM DE SOMMETS DE FRONTIERE
!>    </td></tr>
!>          <tr><td>NSEGBOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ININDS
     &(NPOIN,NPTFR,NELEM,NPMAX,NPTFX,NELMAX,NPLAN,NSEGBOR,NELEB)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NELEB          |-->| NOMBRE D'ELEMENTS DE BORD 3D NON STRUCTURE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPLAN          |---| 
C| NPMAX          |-->| NOMBRE MAXIMUM DE SOMMETS DU MAILLAGE
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE 2D
C|                |   | (SAUF TETRAEDRES OU C'EST LE NOMBRE TOTAL)
C| NPTFR          |-->| NOMBRE DE SOMMETS DE FRONTIERE
C| NPTFX          |-->| NOMBRE MAXIMUM DE SOMMETS DE FRONTIERE
C| NSEGBOR        |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER , INTENT(IN) :: NPOIN
      INTEGER , INTENT(IN) :: NPTFR
      INTEGER , INTENT(IN) :: NELEM
      INTEGER , INTENT(IN) :: NPMAX
      INTEGER , INTENT(IN) :: NPTFX
      INTEGER , INTENT(IN) :: NELMAX
      INTEGER , INTENT(IN) :: NPLAN
      INTEGER , INTENT(IN) :: NSEGBOR
      INTEGER , INTENT(IN), OPTIONAL :: NELEB
C
      INTEGER NDS(0:81,7)
C
      COMMON/NODES/NDS
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE COMMON NODES
C
C     THE VALUES WHICH WILL NOT BE USED ARE NOT INITIALISED
C
C     0) P0 ELEMENT IN DIMENSION 1
C
C                 NSEGBOR (OVER-DIMENSIONING)
      NDS(00,1) = NPTFR
C     NDS(00,2) = ?????
      NDS(00,3) = 1
C     NDS(00,4) = ?????
      NDS(00,5) = NPTFX
C     NDS(00,6) = ?????
C
C-----------------------------------------------------------------------
C
C     1) P1 ELEMENT IN DIMENSION 1
C
      NDS(01,1) = NPTFR
C     NDS(01,2) = ?????
      NDS(01,3) = 2
C     NDS(01,4) = ?????
      NDS(01,5) = NPTFX
C     NDS(01,6) = ?????
C
C-----------------------------------------------------------------------
C
C     2) QUADRATIC ELEMENT IN DIMENSION 1
C
C                 NPTFR+NSEGBOR (OVER-DIMENSIONING)
      NDS(02,1) = 2*NPTFR
C     NDS(02,2) = ?????
      NDS(02,3) = 3
C     NDS(02,4) = ?????
      NDS(02,5) = 2*NPTFX
C     NDS(02,6) = ?????
C
C     NOTE : THE QUADRATIC BOUNDARY POINT OF SEGMENT K WILL HAVE
C            THE NUMBER K+NPTFR, THUS WE HAVE HERE TO DECLARE
C            NDS(02,1) AS 2*NPTFR, THOUGH IN PARALLEL THERE MAY
C            EXISTING POINT K WITHOUT SEGMENT K IN THE DOMAIN
C
C
C-----------------------------------------------------------------------
C
C     10) P0 ELEMENT ON TRIANGLES
C
      NDS(10,1) = NELEM
C     NDS(10,2) = ?????
      NDS(10,3) = 1
      NDS(10,4) = 3
      NDS(10,5) = NELMAX
C     NDS(10,6) = ??????
C
C-----------------------------------------------------------------------
C
C     11) P1 ELEMENT ON TRIANGLES
C
      NDS(11,1) = NPOIN
      NDS(11,2) = (3*NELEM+NSEGBOR)/2
      NDS(11,3) = 3
      NDS(11,4) = 3
      NDS(11,5) = NPMAX
      NDS(11,6) = 3
C
C-----------------------------------------------------------------------
C
C     12) QUASI-BUBBLE ELEMENT ON TRIANGLES
C
      NDS(12,1) = NPOIN+NELEM
      NDS(12,2) = (9*NELEM+NSEGBOR)/2
      NDS(12,3) = 4
      NDS(12,4) = 3
      NDS(12,5) = NPMAX+NELMAX
      NDS(12,6) = 6
C
C-----------------------------------------------------------------------
C
C     13) QUADRATIC ELEMENT ON TRIANGLES
C
C     NUMBER OF DDL = NUMBER OF NODES + NUMBER OF SEGMENTS OF P1
      NDS(13,1) = NDS(11,1)+NDS(11,2)
C     TOTAL NUMBER OF SEGMENTS (3 PER LINEAR SEGMENT +
C                               6 INTERIOR SEGMENTS FOR EACH ELEMENT)
      NDS(13,2) = 6*NELEM+3*NDS(11,2)
      NDS(13,3) = 6
      NDS(13,4) = 3
      NDS(13,5) = NPMAX+(3*NELMAX+NSEGBOR)/2
      NDS(13,6) = 15
C
C-----------------------------------------------------------------------
C
C     14) P1-ISO P1 ELEMENT ON TRIANGLES
C
C     NUMBER OF DDL = NUMBER OF NODES + NUMBER OF SEGMENTS OF P1
      NDS(14,1) = NDS(11,1)+NDS(11,2)
C     TOTAL NUMBER OF SEGMENTS (3 PER LINEAR SEGMENT +
C                               3 INTERIOR SEGMENTS FOR EACH ELEMENT)
      NDS(14,2) = 3*NELEM+3*NDS(11,2)
      NDS(14,3) = 6
      NDS(14,4) = 3
      NDS(14,5) = NPMAX+(3*NELMAX+NSEGBOR)/2
C     3 LINEAR SEGMENTS FOLLOWED BY THE 9 P1-ISO P1
      NDS(14,6) = 12
C
C-----------------------------------------------------------------------
C
C     20) P0 ELEMENT ON QUADRILATERALS (3D: SPECIAL CASE)
C
      NDS(20,1) = NELEM
C     NDS(20,2) = ?????
      NDS(20,3) = 1
      NDS(20,4) = 4
      NDS(20,5) = NELMAX
C     NDS(20,6) = ??????
C     LATERAL SIDES OF 3D PRISM MESHES
C     IF(NPLAN.GE.2) THEN
C       NDS(20,1) = NPTFR*(NPLAN-1)
C       NDS(20,5) = NPTFX*(NPLAN-1)
C     ENDIF
C
C-----------------------------------------------------------------------
C
C     21) P1 ELEMENT ON QUADRILATERALS (3D: SPECIAL CASE)
C
      NDS(21,1) = NPOIN
C     NDS(21,2) = ?????
      NDS(21,3) = 4
      NDS(21,4) = 4
      NDS(21,5) = NPMAX
C     NDS(21,6) = ?????
C     LATERAL SIDES OF 3D PRISM MESHES
C     IF(NPLAN.GE.2) THEN
C       NDS(21,1) = NPTFR*NPLAN
C       NDS(21,5) = NPTFX*NPLAN
C     ENDIF
C
C-----------------------------------------------------------------------
C
C     THREE-DIMENSIONAL ELEMENTS (ONLY FOR 3D MESHES)
C
C
C     30) T0 ELEMENT ON TETRAHEDRONS
C
      NDS(30,1) = NELEM
C     NDS(30,2) = ?????
      NDS(30,3) = 1
      NDS(30,4) = 4
      NDS(30,5) = NELMAX
C     NDS(30,6) = ?????
C
C-----------------------------------------------------------------------
C
C     31) T1 ELEMENT ON TETRAHEDRONS
C
CC 3D MESH
      NDS(31,1) = NPOIN
C     NDS(31,2) = ?????
      NDS(31,3) = 4
      NDS(31,4) = 4
      NDS(31,5) = NPMAX
C     NDS(31,6) = ?????
C
C-----------------------------------------------------------------------
C
C     IF(NPLAN.GT.1) : AVOIDS ERASING WHAT HAS BEEN DONE BY A PREVIOUS
C                      CALL BY TELEMAC-3D WHEN COUPLING WITH SISYPHE
C
C
      IF(NPLAN.GT.1) THEN
C
C     40) P0 ELEMENT ON PRISMS
C
      NDS(40,1) = NELEM*(NPLAN-1)
C     NDS(40,2) = ??????????
      NDS(40,3) = 1
      NDS(40,4) = 5
      NDS(40,5) = NELMAX*(NPLAN-1)
      NDS(40,6) = 15
C
C-----------------------------------------------------------------------
C
C     41) P1 ELEMENT ON PRISMS
C
      NDS(41,1) = NPOIN*NPLAN
      NDS(41,2) = NDS(11,2)*(3*NPLAN-2)+NPOIN*(NPLAN-1)
      NDS(41,3) = 6
      NDS(41,4) = 5
      NDS(41,5) = NPMAX*NPLAN
      NDS(41,6) = 15
C
C-----------------------------------------------------------------------
C
C     50) PRISMS SPLIT IN T0 TETRAHEDRONS
C
      NDS(50,1) = NELEM*(NPLAN-1)*3
C     NDS(50,2) = ?????
      NDS(50,3) = 1
      NDS(50,4) = 4
      NDS(50,5) = NELMAX*(NPLAN-1)*3
C     NDS(50,6) = ?????
C
C-----------------------------------------------------------------------
C
C     51) PRISMS SPLIT IN T1 TETRAHEDRONS
C
      NDS(51,1) = NPOIN*NPLAN
C     NDS(51,2) = ?????
      NDS(51,3) = 4
      NDS(51,4) = 4
      NDS(51,5) = NPMAX*NPLAN
C     NDS(51,6) = ?????
C
C-----------------------------------------------------------------------
C
C     60) P0 TRIANGLES ON LATERAL SIDE OF 3D PRISM MESHES
C         (PRISM MESH SPLIT IN TETRAHEDRONS)
C
      NDS(60,1) = 2*NPTFR*(NPLAN-1)
C     NDS(60,2) = ?????
      NDS(60,3) = 1
      NDS(60,4) = 3
      NDS(60,5) = 2*NPTFX*(NPLAN-1)
C     NDS(60,6) = ??????
C
C-----------------------------------------------------------------------
C
C     61) P1 TRIANGLES ON LATERAL SIDE OF 3D PRISM MESHES
C         (PRISM MESH SPLIT IN TETRAHEDRONS)
C
      NDS(61,1) = NPTFR*NPLAN
C     NDS(61,2) = ?????????
      NDS(61,3) = 3
      NDS(61,4) = 3
      NDS(61,5) = NPTFX*NPLAN
      NDS(61,6) = 3
C
C-----------------------------------------------------------------------
C
C     70) Q0 QUADRILATERALS ON LATERAL SIDE OF 3D PRISM MESHES
C
      NDS(70,1) = NPTFR*(NPLAN-1)
C     NDS(70,2) = ?????
      NDS(70,3) = 1
      NDS(70,4) = 4
      NDS(70,5) = NPTFX*(NPLAN-1)
C     NDS(70,6) = ??????
C
C-----------------------------------------------------------------------
C
C     71) Q1 QUADRILATERALS ON LATERAL SIDE OF 3D PRISM MESHES
C
      NDS(71,1) = NPTFR*NPLAN
C     NDS(71,2) = ?????????
      NDS(71,3) = 4
      NDS(71,4) = 4
      NDS(71,5) = NPTFX*NPLAN
C     NDS(71,6) = ???????????
C
C     CORRESPONDS TO : IF(NPLAN.GT.1) THEN
      ENDIF
C
C-----------------------------------------------------------------------
C
C     80) P0 BOUNDARY TRIANGLES FOR TETRAHEDRONS IN AN UNSTRUCTURED 3D
C         MESH
C
      NDS(80,1) = NPTFR
C     NDS(80,2) = ?????
      NDS(80,3) = 1
      NDS(80,4) = 3
      NDS(80,5) = 3*NELEB
C     NDS(80,6) = ??????
C
C-----------------------------------------------------------------------
C
C     81) P1 BOUNDARY TRIANGLES FOR TETRAHEDRONS IN AN UNSTRUCTURED 3D
C         MESH
C
      NDS(81,1) = NPTFR
C     NDS(81,2) = ?????????
      NDS(81,3) = 3
      NDS(81,4) = 3
      NDS(81,5) = NPTFR
      NDS(81,6) = 3
C
C-----------------------------------------------------------------------

      RETURN
      END
C
C#######################################################################
C