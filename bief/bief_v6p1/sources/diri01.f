C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TAKES INTO ACCOUNT POINTS OF TYPE DIRICHLET IN A SYSTEM
!>                OF LINEAR EQUATIONS WITH SYMMETRICAL MATRIX.
!><br>            IN THE EQUATIONS FOR POINTS NOT OF TYPE DIRICHLET :
!>                DIRICHLET VALUES ARE REMOVED.
!><br>            IN THE EQUATIONS FOR POINTS OF TYPE DIRICHLET :
!>                DEFINES AN EQUATION FIXING THE IMPOSED VALUE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THIS SUBROUTINE IS NOT PROTECTED AGAINST DIAGONAL EQUAL
!>            TO 0 ON DIRICHLET POINTS; IT WILL THEN SET AN EQUATION
!>            0 X = 0 ON SUCH POINTS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FBOR, KDIR, LIMDIR, MASKPT, MESH, MSK, S, SM, WORK1, WORK2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, IELMFB, IELMSM, OLDDIA, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIRI01
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DIRAUX(), MATVEC(), OM(), OS(), OSDBIF(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIFF3D(), DIRICH(), PREDIV()

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
!> </td><td> 07/08/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td><-></td><td>VALEURS A L'ETAPE N+1 ET INITIALISATION
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES DES POINTS DIRICHLET.
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES CONDITIONS DE DIRICHLET
!>    </td></tr>
!>          <tr><td>LIMDIR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES .
!>                  SI LIMDIR(K) = KDIR LE KIEME POINT DE BORD
!>                  EST DE TYPE DIRICHLET.
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>S
!></td><td><-></td><td>MATRICE DU SYSTEME
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>SECOND MEMBRE DU SYSTEME.
!>    </td></tr>
!>          <tr><td>WORK1,2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>WORK2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRI01
     &(F, S, SM ,FBOR,LIMDIR,WORK1,WORK2,MESH,KDIR,MSK,MASKPT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |<->| VALEURS A L'ETAPE N+1 ET INITIALISATION
C| FBOR           |-->| CONDITIONS AUX LIMITES DES POINTS DIRICHLET.
C| KDIR           |-->| CONVENTION POUR LES CONDITIONS DE DIRICHLET
C| LIMDIR         |-->| TYPES DE CONDITIONS AUX LIMITES .
C|                |   | SI LIMDIR(K) = KDIR LE KIEME POINT DE BORD
C|                |   | EST DE TYPE DIRICHLET.
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| S             |<->| MATRICE DU SYSTEME
C| SM             |-->| SECOND MEMBRE DU SYSTEME.
C| WORK1,2        |-->| TABLEAUX DE TRAVAIL.
C| WORK2          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DIRI01 => DIRI01
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,S,SM,WORK1,WORK2
      TYPE(BIEF_OBJ), INTENT(IN)    :: FBOR,MASKPT
      INTEGER, INTENT(IN) :: LIMDIR(*), KDIR
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL, INTENT(IN) :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C,Z(1)
C
      INTEGER IELMSM,IELMFB
C
      CHARACTER*1 OLDDIA
C
C----------------------------------------------------------------------
C
C  DEPLOYS THE MESH STRUCTURE
C
C----------------------------------------------------------------------
C
C  BUILDS AN ARRAY WITH 0S EVERYWHERE EXCEPT AT DIRICHLET POINTS
C  FOR WHICH THE VALUE IS TAKEN FROM FBOR
C  FBOR MUST BE 0 WHEN THE POINT IS NOT OF TYPE DIRICHLET
C
      CALL CPSTVC(SM,WORK1)
C
      IELMSM=SM%ELM
      IELMFB=FBOR%ELM
      IF(IELMSM.EQ.IELMFB) THEN
        CALL MATVEC( 'X=AY    ' ,WORK2,S,FBOR,C, MESH )
      ELSE
        CALL OS( 'X=0     ' , X=WORK1 )
        CALL OSDBIF( 'X=Y     ' ,WORK1,FBOR,LIMDIR,KDIR,MESH)
        CALL MATVEC( 'X=AY    ' ,WORK2,S,WORK1,C, MESH )
      ENDIF
C
C----------------------------------------------------------------------
C
C  THE PRODUCT S WORK1 IS DEDUCTED FROM THE SECOND MEMBER.
C  IT MEANS THAT THE VALUES AT DIRICHLET POINTS ARE NO LONGER
C  UNKNOWNS IN THE EQUATIONS FOR THE OTHER POINTS.
C
      CALL OS( 'X=X-Y   ' , X=SM , Y=WORK2 )
C
C----------------------------------------------------------------------
C
C  BUILDS AN ARRAY WITH 1S EVERYWHERE EXCEPT AT DIRICHLET POINTS
C  FOR WHICH IT'S 0
C
C  WHAT'S MORE, AN EQUATION OF THE FORM DS(N) * X = DS(N) * FBOR
C  (WILL GIVE X=FBOR) IS SET IN THE MATRIX FOR DIRICHLET POINTS;
C  AND F IS INITIALISED TO ITS KNOWN VALUE.
C  THIS ASSUMES THAT DS(N) IS NOT 0
C
      CALL DIRAUX(SM,S%D,FBOR,WORK2,F,LIMDIR,KDIR,MESH )
C
C  MASKING : FOR THE POINTS OF MASKED ELEMENTS THE EQUATION X=0
C            IS SET FOR THE DIAGONAL COEFFICIENT PRES
C
      IF(MSK) THEN
        CALL OV( 'X=XY    ', SM%R   ,MASKPT%R ,Z,C,   SM%DIM1)
        CALL OV( 'X=XY    ', F%R    ,MASKPT%R ,Z,C,    F%DIM1)
        CALL OV( 'X=XY    ', WORK2%R,MASKPT%R ,Z,C,WORK2%DIM1)
      ENDIF
C
C----------------------------------------------------------------------
C
C  WORK2 * S * WORK2 :
C  ERASES THE LINES AND COLUMNS IN S WHICH CORRESPOND TO DIRICHLET
C  POINTS
C  DOES NOT ALTER THE DIAGONAL BY DECLARING IT 0 HERE
C
      OLDDIA=S%TYPDIA
      S%TYPDIA='0'
      CALL OM( 'M=DMD   ' , S , S , WORK2 , C , MESH )
      S%TYPDIA=OLDDIA
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C