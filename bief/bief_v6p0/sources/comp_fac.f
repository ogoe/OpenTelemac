C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLETES THE ARRAY FAC FOR QUADRATIC POINTS
!>                AT THE INTERFACE BETWEEN 2 SUBDOMAINS.
!><br>            FAC%R(1:NPOIN) IS FILLED IN PARINI.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, FAC, IFABOR, NELEM, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_COMP_FAC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 24/10/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
!>    </td></tr>
!>          <tr><td>FAC
!></td><td><-></td><td>COEFFICIENT FOR COMPUTING DOT PRODUCTS IN //
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>-2 MEANS INTERFACE WITH ANOTHER SUB-DOMAIN
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NUMBER OF ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COMP_FAC
     &(ELTSEG,IFABOR,NELEM,NPOIN,FAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |-->| GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
C| FAC            |<->| COEFFICIENT FOR COMPUTING DOT PRODUCTS IN //
C| IFABOR         |-->| -2 MEANS INTERFACE WITH ANOTHER SUB-DOMAIN
C| NELEM          |-->| NUMBER OF ELEMENTS
C| NPOIN          |-->| NUMBER OF POINTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_COMP_FAC => COMP_FAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM,NPOIN
      INTEGER, INTENT(IN)    :: IFABOR(NELEM,3),ELTSEG(NELEM,3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
        IF(IFABOR(IELEM,1).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,1))=0.5D0
        IF(IFABOR(IELEM,2).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,2))=0.5D0
        IF(IFABOR(IELEM,3).EQ.-2) FAC%R(NPOIN+ELTSEG(IELEM,3))=0.5D0
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C