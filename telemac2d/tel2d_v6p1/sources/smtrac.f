C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE SECOND MEMBER FOR THE TRACER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DIMT, DT, ISCE, ITRAC, MAXSCE, MAXTRA, NPOIN, NREJET, SMH, SMTR, TSCE2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!>      <td><center> 5.8                                       </center>
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>DIMT
!></td><td>--></td><td>DIMENSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS HYDRO
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS SOURCES
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NREJET
!></td><td>--></td><td>NOMBRE DE SOURCES/PUITS
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERMES SOURCES DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SMTR
!></td><td>---</td><td>TERMES SOURCES DU TRACEUR
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td>--></td><td>VALEURS DU TRACEUR AUX SOURCES
!>    </td></tr>
!>          <tr><td>TSCE2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SMTRAC
     &(NPOIN,DIMT,AT,DT,SMTR,SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| DIMT           |-->| DIMENSION DU TRACEUR
C| DT             |-->| PAS DE TEMPS HYDRO
C| ISCE           |-->| NUMEROS GLOBAUX DES POINTS SOURCES
C| ITRAC          |---| 
C| MAXSCE         |---| 
C| MAXTRA         |---| 
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NREJET         |-->| NOMBRE DE SOURCES/PUITS
C| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
C| SMTR           |---| TERMES SOURCES DU TRACEUR
C| TSCE           |-->| VALEURS DU TRACEUR AUX SOURCES
C| TSCE2          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NREJET,ISCE(*),DIMT,ITRAC
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT,SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: SMTR(DIMT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IS
C
C-----------------------------------------------------------------------
C
      IF(NREJET.NE.0) THEN
        DO I=1,NREJET
          IS =ISCE(I)
          SMTR(IS) = SMTR(IS) + DT*SMH(IS) * TSCE2(I,ITRAC)
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C