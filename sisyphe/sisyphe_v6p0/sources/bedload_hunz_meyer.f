C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       HUNZIKER BEDLOAD FORMULATION (1995)
!><br>           (ADAPTED FROM MEYER-PETER FORMULATION).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  **-1.5 AND **1.5 SHOULD BE OPTIMISED (JMH)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, ACP, AHUNZI, DENS, DM, GRAV, HIDING, MU, NPOIN, QSC, TETAP, TOB, UNLADM, XMVE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C1, C2, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_HUNZ_MEYER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_FORMULA()

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
!> </td><td>
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/10/2003
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> **/01/2002
!> </td><td> BUI MINH DUC
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AHUNZI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_HUNZ_MEYER !
     &  (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV, DM, AC,
     &    TETAP, AHUNZI, ACP, HIDING, QSC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| ACP            |---| 
C| AHUNZI         |---| 
C| DENS           |---| 
C| DM             |---| 
C| GRAV           |---| 
C| HIDING         |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| TETAP          |---| 
C| TOB            |---| 
C| UNLADM         |---| 
C| XMVE           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &          EX_BEDLOAD_HUNZ_MEYER => BEDLOAD_HUNZ_MEYER
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, MU, ACLADM, UNLADM
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DENS, XMVE, GRAV, DM, AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP, AHUNZI ! WORK ARRAY T1, T2
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP           ! WORK ARRAY T3
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: HIDING, QSC


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: C1, C2

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ************************************* !
      ! I - ADIMENSIONAL SKIN STRESS          !
      ! ************************************* !
      C1 = 1.D0/(DENS*XMVE*GRAV*DM)
      CALL OS('X=CYZ   ', X=TETAP, Y=TOB, Z=MU, C=C1)
!
C     CHANGED BY JMH ON 28/10/2009 AFTER MODIFICATIONS BY
C     REBEKKA KOPMANN TRANSMITTED BY JACEK JANKOWSKI
C     CALL OS('X=+(Y,C)', X=TETAP , Y=TETAP, C= 1.D-06 )
      CALL OS('X=+(Y,C)', X=TETAP , Y=TETAP, C= 1.D-02 )
C
      CALL OS('X=Y**C  ', X=AHUNZI, Y=TETAP, C=-1.5D0  )
      CALL OS('X=CX    ', X=AHUNZI, C= 0.011D0)
      CALL OS('X=X+C   ', X=AHUNZI, C=-0.3D0  )
!
C RK COMMENT:
C BEWARE: AHUNZI CAN BECOME SO LARGE THAT THE HIDING FACTOR BECOMES
C INFINITE; HUNZIKER HIMSELF SUGGESTS THAT IT BE CAPPED TO 2.3.
C THIS INITIALLY ADOPTS A LIMIT OF APPROXIMATELY 10.
C (WHICH IS APPARENT IN TETAP BEING SET TO VALUES .GE. 0.01)
!
C     REMARK BY JMH: I WOULD STRONGLY RECOMMEND A SINGLE LOOP
C                    WITH THE WHOLE FORMULA, INSTEAD OF PILING
C                    UP CALLS TO OS
!
      DO I = 1, NPOIN
        HIDING%R(I) = (DM/ACLADM%R(I))**(-AHUNZI%R(I))
      ENDDO

      ! ************************************************* !
      ! IV - CORRECTS THE ADIMENSIONAL CRITICAL STRESS    !
      ! ************************************************* !
      CALL OS('X=Y/Z   ', X=ACP, Y=UNLADM, Z=ACLADM)
      CALL OS('X=Y**C  ', X=ACP, Y=ACP   , C=0.33D0)
      CALL OS('X=CX    ', X=ACP, C=AC)

      ! ********************* !
      ! V - TRANSPORT RATE    !
      ! ********************* !
      CALL OS('X=Y-Z   ', X=QSC, Y=TETAP , Z=ACP )
      CALL OS('X=+(Y,C)', X=QSC, Y=QSC   , C=0.D0)
      CALL OS('X=XY    ', X=QSC, Y=HIDING)
      CALL OS('X=Y**C  ', X=QSC, Y=QSC   , C=1.5D0)
      C2 = 5.D0*SQRT(GRAV*DENS*DM**3)
      CALL OS('X=CX    ', X=QSC, C=C2)

!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C