C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLS THE DIRECT SOLVER MUMPS
!>                IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DA, GLOSEG, INFOGR, LT, MAXSEG, NPOIN, NSEGB, RHS, TYPEXT, XA, XINC
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
!>    <td><center> 5.7                                    </center></td>
!>    <td> 02/11/2009                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 14/10/2009                                              </td>
!>    <td> F. ZAOUI / C. DENIS (LNHE/SINETICS)                     </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>DIAGONALE ET TERMES EXTRA-DIAGONAUX DE LA MATRICE
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES
!>    </td></tr>
!>          <tr><td>NSEGB
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>RHS
!></td><td>--></td><td>SECOND MEMBRE
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XINC
!></td><td><--</td><td>SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE SOLVE_MUMPS
     &(NPOIN,NSEGB,GLOSEG,MAXSEG,DA,XA,XINC,RHS,INFOGR,TYPEXT,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DA,XA          |-->| DIAGONALE ET TERMES EXTRA-DIAGONAUX DE LA MATRICE
C| GLOSEG         |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
C| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
C| LT             |---| 
C| MAXSEG         |---| 
C| NPOIN          |-->| NOMBRE D'INCONNUES
C| NSEGB          |-->| NOMBRE DE SEGMENTS
C| RHS            |-->| SECOND MEMBRE
C| TYPEXT         |---| 
C| XINC           |<--| SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C ARGUMENTS
      INTEGER, INTENT(IN)             :: NPOIN,NSEGB,MAXSEG
      INTEGER, INTENT(IN)             :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(IN)             :: LT
      LOGICAL, INTENT(IN)             :: INFOGR
      DOUBLE PRECISION, INTENT(INOUT) :: XA(*),RHS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XINC(NPOIN),DA(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
      COMMON/INFO/LNG,LU
      INTEGER LNG,LU


      IF(LNG.EQ.1) WRITE(LU,2018)
      IF(LNG.EQ.2) WRITE(LU,2019)
2018  FORMAT(1X,'MUMPS NON INSTALLE SUR CE SYSTEME,',/,1X,
     &     'CHOISIR UNE AUTRE METHODE',///)
2019  FORMAT(1X,'MUMPS NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP


      END
C
C#######################################################################
C