!                        ************************
                          SUBROUTINE PAROIMOBILE
!                        ************************
!
     &  (KPARMOB, NPMAX, NPART, PI, TEMPS, VXMOB, VZMOB)
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! KPARMOB   ! -->  ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
! ! VXMOB,    ! <--  ! WALL VELOCITY COMPONENTS                       !
! ! VZMOB                                                             !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : impose une vitesse a une ou plusieurs parois
! FUNCTION : computes the velocity of one or several walls
!
! PROGRAMMES APPELANT : MOUVEMENT
! CALLED BY
!
! PROGRAMMES APPELES  : -
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER I, NPMAX, NPART
!
      DOUBLE PRECISION TEMPS, PERIODE, AMPVIT, PI
!
      INTEGER KPARMOB (NPMAX)
!
      DOUBLE PRECISION VXMOB (NPMAX), VZMOB (NPMAX)
!
      PERIODE = 2.390D0
      AMPVIT  = 0.103D0*2.D0*PI/PERIODE
!
      DO 445 I=1,NPART
!
        VXMOB (I) = 0.
        VZMOB (I) = 0.
!
        IF (KPARMOB(I).EQ.1) THEN
            VXMOB (I) = AMPVIT*COS(2.D0*PI*TEMPS/PERIODE)
            VZMOB (I) = 0.
        ENDIF
!
 445  CONTINUE
!
      RETURN
      END