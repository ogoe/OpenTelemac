C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOTTOM TOPOGRAPHY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::AM1 AM1@endlink, 
!> @link DECLARATIONS_TOMAWAC::LISFON LISFON@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST2 ST2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SW1 SW1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SZF SZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FILTER(), OV()
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
!> </td><td> 12/01/2001
!> </td><td> OPTIMER (02 98 44 24 51)
!> </td><td> TOMAWAC/COWADIS MERGE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> F. MARCOS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>LISFON
!></td><td>--></td><td>NUMBER OF BOTTOM SMOOTHINGS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>T1,2
!></td><td><-></td><td>WORKING TABLES
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>WORKING TABLE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td><-></td><td>MESH COORDINATES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>BOTTOM
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORFON
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| LISFON         |-->| NUMBER OF BOTTOM SMOOTHINGS
C| NPOIN2         |-->| NUMBER OF POINTS IN THE MESH
C| T1,2           |<->| WORKING TABLES
C| W1             |<->| WORKING TABLE
C| X,Y            |<->| MESH COORDINATES
C| ZF             |<->| BOTTOM
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C
C-----------------------------------------------------------------------
C
C  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
C
      IF(LISFON.GT.0) THEN
C
C       W1 ( EX MASKEL) SET TO 1
        CALL OV('X=C     ', SW1%R, ST1%R, ST2%R, 1.D0, NELEM2)
C
        CALL FILTER(SZF,.TRUE.,ST1,ST2,AM1,'MATMAS          ',
     &          1.D0,ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,SW1,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TOMAWAC) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TOMAWAC) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ELSE
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TOMAWAC): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TOMAWAC): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C