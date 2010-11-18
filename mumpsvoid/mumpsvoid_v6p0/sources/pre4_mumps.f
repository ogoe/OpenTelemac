C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLS THE DIRECT SOLVER MUMPS.
!>                IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CVB1, CVB2, DAB1, DAB2, DAB3, DAB4, GLOSEGB, INFOGR, NPOIN, NSEGB, TYPEXT, XAB1, XAB2, XAB3, XAB4, XX1, XX2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

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
!>  <tr>
!>    <td><center> 5.9                                    </center></td>
!>    <td> 14/10/2009                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 20/11/2006                                              </td>
!>    <td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03                  </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CVB1,CVB2
!></td><td>--></td><td>SECONDS MEMBRES
!>    </td></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
!>                  MATRICES
!>    </td></tr>
!>          <tr><td>DAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!>    </td></tr>
!>          <tr><td>GLOSEGB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES
!>    </td></tr>
!>          <tr><td>NSEGB
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XX1,XX2
!></td><td><--</td><td>SOLUTIONS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE PRE4_MUMPS
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,INFOGR,TYPEXT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CVB1,CVB2      |-->| SECONDS MEMBRES
C| DA,XA          |-->| DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
C|                |   | MATRICES
C| DAB1           |---| 
C| DAB2           |---| 
C| DAB3           |---| 
C| DAB4           |---| 
C| GLOSEG         |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
C| GLOSEGB        |---| 
C| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
C| NPOIN          |-->| NOMBRE D'INCONNUES
C| NSEGB          |-->| NOMBRE DE SEGMENTS
C| TYPEXT         |---| 
C| XAB1           |---| 
C| XAB2           |---| 
C| XAB3           |---| 
C| XAB4           |---| 
C| XX1,XX2        |<--| SOLUTIONS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NSEGB
      INTEGER, INTENT(IN) :: GLOSEGB(NSEGB*2)
      LOGICAL, INTENT(IN) :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEGB),XAB2(NSEGB)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEGB),XAB4(NSEGB)
      DOUBLE PRECISION, INTENT(INOUT) :: XX1(NPOIN),XX2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CVB1(NPOIN),CVB2(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
C

      IF(LNG.EQ.1) WRITE(LU,2018)
      IF(LNG.EQ.2) WRITE(LU,2019)
2018  FORMAT(1X,'MUMPS NON INSTALLE SUR CE SYSTEME,',/,1X,
     &     'CHOISIR UNE AUTRE METHODE',///)
2019  FORMAT(1X,'MUMPS NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP


C
C-----------------------------------------------------------------------
C

C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C