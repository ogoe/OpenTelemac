C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!>                VARIATION IN SPACE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!>  @code
!>  EXAMPLE:
!>      NCOUCHES(J) = 10
!>      ES(J,1) = 1.D0
!>      ES(J,2) = 1.D0
!>      ES(J,3) = 1.D0
!>      ES(J,4) = 1.D0
!>      ES(J,5) = 1.D0
!>      ES(J,6) = 1.D0
!>      ES(J,7) = 1.D0
!>      ES(J,8) = 1.D0
!>      ES(J,9) = 1.D0
!>        DO I = 1, NSICLA
!>          DO K = 1, NCOUCHES(J)
!>          AVAIL(J,K,I) = AVA0(I)
!>          ENDDO
!>        ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NCOUCHES
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::AVA0 AVA0@endlink, 
!> @link DECLARATIONS_SISYPHE::AVAIL AVAIL@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_AVAI()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 2002
!> </td><td> MATTHIEU GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AVAIL
!></td><td><--</td><td>SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
!>                  AVAIL(NPOIN,10,NSICLA)
!>    </td></tr>
!>          <tr><td>ES
!></td><td><--</td><td>THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10)
!>    </td></tr>
!>          <tr><td>NCOUCHES
!></td><td>--></td><td>NUMBER OF LAYER FOR EACH POINT
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF NODES
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>--></td><td>NUMBER OF SIZE-CLASSES OF BED MATERIAL
!>                  (LESS THAN 10)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE INIT_COMPO
     &(NCOUCHES)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AVAIL          |<--| SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
C|                |   | AVAIL(NPOIN,10,NSICLA)
C| ES             |<--| THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10)
C| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
C| NPOIN          |-->| NUMBER OF NODES
C| NSICLA         |-->| NUMBER OF SIZE-CLASSES OF BED MATERIAL
C|                |   | (LESS THAN 10)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I , J
C
C-----------------------------------------------------------------------
C
      DO J=1,NPOIN
C
C       BY DEFAULT : UNIFORM BED COMPOSITION
C
          NCOUCHES(J) = 1
          DO I = 1, NSICLA
            AVAIL(J,1,I) = AVA0(I)
            AVAIL(J,2,I) = AVA0(I)
          ENDDO
C
C  TO BE FILLED BY THE USER
C      NCOUCHES(J) = 10
C      ES(J,1) = 1.D0
C      ES(J,2) = 1.D0
C      ES(J,3) = 1.D0
C      ES(J,4) = 1.D0
C      ES(J,5) = 1.D0
C      ES(J,6) = 1.D0
C      ES(J,7) = 1.D0
C      ES(J,8) = 1.D0
C      ES(J,9) = 1.D0
C        DO I = 1, NSICLA
C          DO K = 1, NCOUCHES(J)
C          AVAIL(J,K,I) = AVA0(I)
C          ENDDO
C        ENDDO
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