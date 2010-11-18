C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE PHYSICAL PARAMETERS TO START
!>                AN ADJOINT COMPUTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALIRE, NRES, TROUVE
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV1 CV1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV2 CV2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV3 CV3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HH HH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HIT1 HIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NVARCL NVARCL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NVARRES NVARRES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PP PP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QQ QQ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RR RR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXRES TEXRES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UIT1 UIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UU UU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARCL VARCL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARCLA VARCLA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VIT1 VIT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VV VV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::W W@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AT1, HIST, I, ITER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_SUITE(), LITENR(), MESURES(), OS()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 24/04/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALIRE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIN_ADJ
     &(ALIRE,NRES,TROUVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALIRE          |---| 
C| NRES           |---| 
C| TROUVE         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: ALIRE(*),NRES
      INTEGER, INTENT(INOUT) :: TROUVE(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ITER
      DOUBLE PRECISION HIST(1),AT1
C
C-----------------------------------------------------------------------
C
C     CONDIN FOR ADJOINT PROBLEM: T=T(N+1) : P=0,Q=0,R=0
C
      CALL OS( 'X=C     ',PP,PP,PP,0.D0)
      CALL OS( 'X=C     ',QQ,QQ,QQ,0.D0)
      CALL OS( 'X=C     ',RR,RR,RR,0.D0)
C
C     JUST IN CASE CV1,.. IS WRITTEN IN THE RESULT FILE
C
      CALL OS( 'X=C     ',CV1,CV1,CV1,0.D0)
      CALL OS( 'X=C     ',CV2,CV2,CV2,0.D0)
      CALL OS( 'X=C     ',CV3,CV3,CV3,0.D0)
C
C     READS THE LAST TIME IN THE TELEMAC RESULT FILE (NRES)
C     INITIALISES U,V AND H
C
      REWIND NRES
C
      CALL BIEF_SUITE(VARSOR,VARCL,ITER,NRES,'SERAFIN ',
     &           HIST,0,NPOIN,AT,TEXTE,VARCLA,
     &           NVARCL,TROUVE,ALIRE,LISTIN,.TRUE.,MAXVAR)
C
C     GIVES MEASUREMENTS HD,UD AND VD AT THE LAST TIME STEP
C     (ITER AND AT GIVEN BY THE PREVIOUS CALL TO SUITE)
C
      CALL MESURES(ITER,AT)
C     INITIALISES HH, UU, VV
C
      CALL OS( 'X=Y     ' , HH   , H , H , 0.D0 )
      CALL OS( 'X=Y     ' , UU   , U , U , 0.D0 )
      CALL OS( 'X=Y     ' , VV   , V , V , 0.D0 )
      CALL OS( 'X=C     ' , HIT1 , HIT1 , HIT1 , 0.D0 )
      CALL OS( 'X=C     ' , UIT1 , UIT1 , UIT1 , 0.D0 )
      CALL OS( 'X=C     ' , VIT1 , VIT1 , VIT1 , 0.D0 )
C
C     READS TELEMAC2D RESULTS (RESULT FILE - UNIT NRES)
C     THIS IS TO HAVE UN AT THE LAST TIME STEP INTO U.
C
C     BEWARE : ASSUMES THAT NVARRES HAS ALREADY BEEN COMPUTED
C
      DO I=1,2*(NVARRES+1)
        BACKSPACE NRES
      ENDDO
      CALL LITENR(VARSOR,VARCL,NRES,'STD',HIST,0,NPOIN,AT1,TEXTE,
     &           TEXRES,NVARRES,VARCLA,0,TROUVE,ALIRE,W,.FALSE.,MAXVAR)
C
C-----------------------------------------------------------------------
C
      AT = AT + DT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C