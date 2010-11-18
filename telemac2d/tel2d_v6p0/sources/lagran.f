C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES FIRST AND FINAL TIMESTEPS FOR THE LAGRANGIAN DRIFTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!>  @code
!>  EXAMPLE :
!>
!>      DO 10 ILAG=1,NLAG
!>         DEBLAG(ILAG) = 1
!>         FINLAG(ILAG) = 299
!> 10   CONTINUE
!>  @endcode

!>  @warning  TWO DRIFTS CANNOT COMPLETE IN THE SAME TIMESTEP (ONLY THE 1ST WILL BE WRITTEN TO FILE)

!>  @warning  THE RESULTS MUST BE SAVED BETWEEN TWO DRIFT COMPUTATION ENDS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBLAG, FINLAG, NLAG
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ILAG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBLAG
!></td><td><--</td><td>TIME STEP AT THE BEGINNING
!>    </td></tr>
!>          <tr><td>FINLAG
!></td><td><--</td><td>TIME STEP AT THE END
!>    </td></tr>
!>          <tr><td>NLAG
!></td><td>--></td><td>NUMBER OF LAGRANGIAN DRIFTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LAGRAN
     &(NLAG,DEBLAG,FINLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBLAG         |<--| TIME STEP AT THE BEGINNING
C| FINLAG         |<--| TIME STEP AT THE END
C| NLAG           |-->| NUMBER OF LAGRANGIAN DRIFTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NLAG
      INTEGER, INTENT(INOUT) :: DEBLAG(NLAG) , FINLAG(NLAG)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ILAG
C
C-----------------------------------------------------------------------
C
C   INITIALISES THE FIRST AND FINAL TIMESTEPS FOR THE COMPUTATION OF
C   LAGRANGIAN DRIFT - BY DEFAULT NOTHING IS DONE
C
C-----------------------------------------------------------------------
C
C     THIS WARNING AND THE CALL TO PLANTE MUST BE REMOVED IF
C     SOMETHING IS IMPLEMENTED BY THE USER BELOW
C
      IF(LNG.EQ.1) WRITE(LU,20)
      IF(LNG.EQ.2) WRITE(LU,21)
20    FORMAT(1X,'ATTENTION, VOUS APPELEZ LE SOUS-PROGRAMME LAGRAN',/,1X,
     &          'DE LA BIBLIOTHEQUE.   COMME VOUS CALCULEZ UN OU',/,1X,
     &          'PLUSIEURS CHAMPS DE DERIVES LAGRANGIENNES, VOUS',/,1X,
     &          'DEVEZ RAPATRIER "LAGRAN" DANS VOTRE FORTRAN, ET',/,1X,
     &          'LE COMPLETER',/////)
21    FORMAT(1X,'ATTENTION, YOU CALL SUBROUTINE LAGRAN OF THE LIBRARY.',
     &     /,1X,'AS YOU COMPUTE ONE OR MORE FIELDS OF LAGRANGIAN',/,1X,
     &          'DRIFTS, YOU NEED TO COPY THIS SUBROUTINE IN YOUR',/,1X,
     &          'OWN FORTRAN FILE AND COMPLETE IT.',/////)
C
      CALL PLANTE(1)
C
C-----------------------------------------------------------------------
C
C  EXAMPLE :
C
C      DO 10 ILAG=1,NLAG
C         DEBLAG(ILAG) = 1
C         FINLAG(ILAG) = 299
C 10   CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C