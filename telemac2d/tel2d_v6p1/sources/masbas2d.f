C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VARIOUS VOLUMES OF 2D BASIS AND THE INVERSE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, MASKEL, MESH, MSK, S, T1, UNSV2D, V2DPAR, VOLU2D
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 14/06/2006
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE OF ELEMENT (11 FOR LINEAR)
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>ARRAY OF MASKS, PER ELEMENT
!>    </td></tr>
!>          <tr><td>MESH
!></td><td><-></td><td>MESH
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>IF YES, THERE IS MASKING, MASKEL IS TO BE USED
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>EMPTY BIEF_OBJ STRUCTURE
!>    </td></tr>
!>          <tr><td>T1
!></td><td><-></td><td>BIEF_OBJ STRUCTURE FOR LOCAL WORK
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td><-></td><td>INVERSE OF INTEGRAL OF TEST FUNCTIONS
!>                  WITHOUT MASKING
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td><-></td><td>AS VOLU2D IF NOT PARALLEL
!>                  IN PARALLEL COMPLETED WITH OTHER SUBDOMAINS
!>    </td></tr>
!>          <tr><td>VOLU2D
!></td><td><-></td><td>INTEGRAL OF TEST FUNCTIONS, WITH MASKING
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MASBAS2D
     &(VOLU2D,V2DPAR,UNSV2D,IELM,MESH,MSK,MASKEL,T1,S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE OF ELEMENT (11 FOR LINEAR)
C| MASKEL         |-->| ARRAY OF MASKS, PER ELEMENT
C| MESH           |<->| MESH
C| MSK            |-->| IF YES, THERE IS MASKING, MASKEL IS TO BE USED
C| S             |-->| EMPTY BIEF_OBJ STRUCTURE
C| T1             |<->| BIEF_OBJ STRUCTURE FOR LOCAL WORK
C| UNSV2D         |<->| INVERSE OF INTEGRAL OF TEST FUNCTIONS
C|                |   | WITHOUT MASKING
C| V2DPAR         |<->| AS VOLU2D IF NOT PARALLEL
C|                |   | IN PARALLEL COMPLETED WITH OTHER SUBDOMAINS
C| VOLU2D         |<->| INTEGRAL OF TEST FUNCTIONS, WITH MASKING
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: IELM
      LOGICAL, INTENT(IN)            :: MSK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: VOLU2D,V2DPAR,UNSV2D,T1
      TYPE(BIEF_OBJ) , INTENT(IN)    :: MASKEL,S
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     VOLU2D : VOLUME WITH POSSIBLE MASKING
C
      CALL VECTOR(VOLU2D,'=','MASBAS          ',IELM,1.D0,
     &            S,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     V2DPAR : LIKE VOLU2D BUT IN PARALLEL VALUES COMPLETED AT
C              INTERFACES BETWEEN SUBDOMAINS
C
      CALL OS('X=Y     ',X=V2DPAR,Y=VOLU2D)
      IF(NCSIZE.GT.1) CALL PARCOM(V2DPAR,2,MESH)
C
C     INVERSE OF VOLUMES (DONE WITHOUT MASKING), THERE SHOULD BE
C     NO DIVISION BY ZERO, UNLESS ELEMENT WITH NO AREA
C
      IF(MSK) THEN
        CALL VECTOR(T1,'=','MASBAS          ',IELM,1.D0,
     &              S,S,S,S,S,S,MESH,.FALSE.,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
        CALL OS('X=1/Y   ',X=UNSV2D,Y=T1)
      ELSE
        CALL OS('X=1/Y   ',X=UNSV2D,Y=V2DPAR)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C