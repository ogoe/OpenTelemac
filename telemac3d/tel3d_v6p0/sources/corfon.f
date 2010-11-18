C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOTTOM TOPOGRAPHY.
!><br>            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!><br>           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!>         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> LISFON, MASKEL, MATR2D, MESH2D, MSK, NPOIN2, PRIVE, S, ST1, ST2, SZF, T1, T2, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, K, MAS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FILTER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 25/11/97
!> </td><td> J.M. JANIN  (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>(S)T1,2
!></td><td><-></td><td>WORK ARRAYS (ST1, ST2 THEIR STRUCTURES
!>    </td></tr>
!>          <tr><td>(S)ZF
!></td><td><-></td><td>BOTTOM.(IF SZF: STRUCTURE)
!>    </td></tr>
!>          <tr><td>AMESH2
!></td><td>--></td><td>BLOCK OF DOUBLE PRECISION ARRAYS
!>    </td></tr>
!>          <tr><td>IMESH2
!></td><td>--></td><td>BLOCK OF INTEGER ARRAYS
!>    </td></tr>
!>          <tr><td>LISFON
!></td><td>--></td><td>NUMBER OF SMOOTHINGS REQUIRED
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASK OF ELEMENTS
!>    </td></tr>
!>          <tr><td>MATR
!></td><td><-></td><td>WORK MATRIX
!>    </td></tr>
!>          <tr><td>MATR2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>IF YES, THERE ARE MASKED ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF 2D POINTS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF PRIVATE ARRAYS
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ST1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ST2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>MESH COORDINATES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORFON
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| (S)T1,2        |<->| WORK ARRAYS (ST1, ST2 THEIR STRUCTURES
C| (S)ZF          |<->| BOTTOM.(IF SZF: STRUCTURE)
C| AMESH2         |-->| BLOCK OF DOUBLE PRECISION ARRAYS
C| IMESH2         |-->| BLOCK OF INTEGER ARRAYS
C| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
C| MASKEL         |-->| MASK OF ELEMENTS
C| MATR           |<->| WORK MATRIX
C| MATR2D         |---| 
C| MESH2D         |---| 
C| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
C| NPOIN2         |-->| NUMBER OF 2D POINTS
C| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS
C| S             |---| 
C| ST1            |---| 
C| ST2            |---| 
C| SZF            |---| 
C| T2             |---| 
C| X,Y            |-->| MESH COORDINATES
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
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,I
      LOGICAL MAS
!
!***********************************************************************
!
C     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
         MAS = .TRUE.
!
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)

      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CORFON
C
C#######################################################################
C