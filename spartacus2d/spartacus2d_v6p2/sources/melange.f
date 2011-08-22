!                        ********************
                          SUBROUTINE MELANGE
!                        ********************
!
     &(NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
!
!----------------------------------------------------------------------
!                              MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !               MEANING                          !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
! ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
! ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
! ! LM        ! <--  ! MIXING LENGTH                                  !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
! !___________!______!________________________________________________!
!
! MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
!
!----------------------------------------------------------------------
!
! SPARTACUS2D V5P9
! D. Violeau           & R. Issa
! +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28
! LNHE - 2008
!
! FONCTION : definition (par l'utilisateur) de la longueur de melange
! FUNCTION : Mixing length defined by the user
!
! PROGRAMMES APPELANT : INITIAL , VISCTURB
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
      INTEGER NPMAX, NPART
      INTEGER I
!
      DOUBLE PRECISION DR , DELTA
!
      INTEGER KPAR (NPMAX)
!
      DOUBLE PRECISION LM(NPMAX)
      DOUBLE PRECISION X (NPMAX)
      DOUBLE PRECISION Z (NPMAX)
!
! Definition de la longueur de melange
!=====================================
! Mixing length definition
!=========================
!
      DO 407 I=1,NPART
        LM(I)=DR
        LM(I)=MAX(LM(I),1.0D-5)
 407  CONTINUE
!
      RETURN
      END