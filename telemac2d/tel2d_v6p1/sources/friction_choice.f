C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MAIN SUBROUTINE FOR FRICTION COMPUTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, FRICTION_DEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FRICTION_PASS, KARMAN
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CFBOR CFBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHBORD CHBORD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DP DP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FRICTB FRICTB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IKLE IKLE@endlink,  
!> @link DECLARATIONS_TELEMAC2D::KFROT KFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFRO_B KFRO_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDDP LINDDP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDNER LINDNER@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDSP LINDSP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISRUG LISRUG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEF NDEF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEFMA NDEFMA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEF_B NDEF_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NELEM NELEM@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NELMAX NELMAX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NKFROT NKFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::S S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SB SB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SP SP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, VK
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FRICTION_CHOICE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FRICTION_BUBBLE(), FRICTION_INIT(), FRICTION_QUAD(), FRICTION_UNIF(), FRICTION_ZONES(), OS(), PLANTE(), STRCHE()
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
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/04/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FRICTION_PASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_CHOICE
     &(FRICTION_PASS,KARMAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FRICTION_PASS  |---| 
C| KARMAN         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_FRICTION_CHOICE => FRICTION_CHOICE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN) :: FRICTION_PASS
      DOUBLE PRECISION, INTENT(IN) :: KARMAN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: VK = 1.D-6
!
!======================================================================!
!======================================================================!
C                               PROGRAMME                              !
!======================================================================!
!======================================================================!
!
C INITIALIZATION
! --------------
!
      IF(FRICTION_PASS == 0) THEN
!
C ZONES INITIALIZATION
! --------------------
!
         IF (FRICTB) THEN
            CALL FRICTION_INIT
            CALL STRCHE
C FH : FOR QUASI-BUBBLE
C FH : 2004/03/01
CJAJ FOR QUADRATIC ELEMENTS
! =>
            IF (CF%ELM /= H%ELM) THEN
              IF(CF%ELM==12 .AND. H%ELM==11) THEN
                CALL FRICTION_BUBBLE
     &              (IKLE, NPOIN, NELEM, NELMAX, LINDNER, NKFROT,
     &               CHESTR, NDEFMA, LINDDP, LINDSP)
              ELSEIF (CF%ELM==13 .AND. H%ELM==11) THEN
                CALL FRICTION_QUAD
     &              (IKLE%I, NPOIN, NELEM, NELMAX, LINDNER, NKFROT,
     &               CHESTR, NDEFMA, LINDDP, LINDSP)
C                WRITE (LU,*)
C     &           'FRICTION_CHOICE::QUADRATIC ELEMENTS NOT IMPLEMENTED.'
C                CALL PLANTE(1)
              ELSE
                WRITE (LU,*)
     &           'FRICTION_CHOICE::DISCRETISATION NOT IMPLEMENTED.'
                WRITE (LU,*)
     &           'CF%ELM, H%ELM: ',CF%ELM, H%ELM
                CALL PLANTE(1)
              ENDIF
            ENDIF
! <=
CJAJ FOR QUADRATIC ELEMENTS
C FH : 2004/03/01
C FH : FOR QUASI-BUBBLE

!
!        UNIFORM CASE
!        ------------
!
         ELSE

            ! CHESTR FOR BOUNDARY CONDITIONS INITIALIZATION
            ! -----------------------------------------------
            IF(LISRUG.EQ.2) THEN
              IF(KFROTL.EQ.1) THEN
                DO I = 1, NPTFR
                  CHBORD%R(I) = CHESTR%R(MESH%NBOR%I(I))
                ENDDO
              ELSE
!               JMH 21/12/2010
!               BOUNDARY CONDITIONS FILE DATA IF ANY SUPERSEDE
!               THE KEY-WORD ROUGHNESS COEFFICIENT OF BOUNDARIES              
                IF(P_DOTS(CHBORD,CHBORD,MESH).EQ.0.D0) THEN
                  CALL OS('X=C     ', X=CHBORD, C=SB)
                ENDIF
              ENDIF
            ENDIF

            ! TYPE OF FRICTION LAW FOR EACH NODE
            ! ----------------------------------
            
            DO I=1, CF%DIM1
              NKFROT%I(I) = KFROT
            ENDDO
!
         ENDIF
!
C     COMPUTATION
!     -----------
!
      ELSE
!
         ! FRICTION BY ZONES
         ! -----------------
         IF (FRICTB) THEN
C FH : FOR QUASI-BUBBLE
C FH : 2004/03/01
CJAJ FOR QUADRATIC ELEMENTS
! =>
            IF (CF%ELM /= H%ELM) THEN
              IF (CF%ELM==12 .AND. H%ELM==11) THEN
                CALL FRICTION_BUBBLE
     &              (IKLE, NPOIN, NELEM, NELMAX, LINDNER, NKFROT,
     &               CHESTR, NDEFMA, LINDDP, LINDSP)
              ELSE IF (CF%ELM==13 .AND. H%ELM==11) THEN
                CALL FRICTION_QUAD
     &              (IKLE%I, NPOIN, NELEM, NELMAX, LINDNER, NKFROT,
     &               CHESTR, NDEFMA, LINDDP, LINDSP)
              ELSE
                WRITE (LU,*)
     &           'FRICTION_CHOICE::DISCRETISATION NOT IMPLEMENTED.'
                WRITE (LU,*)
     &           'CF%ELM, H%ELM: ',CF%ELM, H%ELM
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDIF
!
            CALL FRICTION_ZONES
     &           (MESH, H, U, V, S, CHESTR, CHBORD, NKFROT, NDEFMA,
     &            LINDDP, LINDSP, KFRO_B, NDEF_B, ITURB, LISRUG,
     &            LINDNER, VK, KARMAN, GRAV, T1, T2, CF, CFBOR)
! <=
CJAJ FOR QUADRATIC ELEMENTS
C FH : 2004/03/01
C FH : FOR QUASI-BUBBLE
!
         ! UNIFORM FRICTION
         ! ----------------
         ELSE
            CALL FRICTION_UNIF
     &           (MESH,H,U,V,CHESTR,S,KFROT,KFROTL,ITURB, LISRUG,
     &            LINDNER, SB, NDEF, DP, SP, VK, KARMAN, GRAV, T1,
     &            T2, CHBORD, CF, CFBOR)
!
         ENDIF
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C
