!                        *******************
                          SUBROUTINE PRIVEE
!                        *******************
!
     &(NPMAX, NPART, EPS, MASS, NUT, P ,
     & PRIV , RHO  , S  , TKE , VX , VZ,
     & X    , Z                        )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! EPS       ! -->  ! DISSIPATION RATE                               !
! ! MASS      ! -->  ! PARTICLE MASS                                  !
! ! NUT       ! -->  ! EDDY VISCOSITY                                 !
! ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
! ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
! ! P         ! -->  ! PRESSURE                                       !
! ! PRIV      ! <--  ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
! ! RHO       ! -->  ! DENSITY                                        !
! ! S         ! -->  ! RATE OF STRAIN                                 !
! ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
! ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            !
! ! X, Z      ! -->  ! PARTICLE POSITION                              !
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
! FONCTION : calcule la valeur d une variable de sortie definie par l utilisateur
! FUNCTION : computes a printout variable defined by the user
!
! PROGRAMMES APPELANT : SPARTACUS2D
! CALLED BY
!
! PROGRAMMES APPELES  : -
! CALLED PROGRAMS
!
!----------------------------------------------------------------------
!
! Mode d emploi
!--------------
! Afin de visualiser une nouvelle variable de sortie, l utilisateur
! est invite a decommenter les lignes de calcul suivantes
!
! User guide
!-----------
! In order to visualize a new printout variable, the user has to
! suppress the following comment characters
!
!-----------------------------------------------------------------------
!
! Variables
!==========
!
      IMPLICIT NONE
!
      INTEGER NPMAX , NPART , I
!
      DOUBLE PRECISION X  (NPMAX), Z   (NPMAX), P   (NPMAX)
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), TKE (NPMAX)
      DOUBLE PRECISION RHO(NPMAX), MASS(NPMAX), EPS (NPMAX)
      DOUBLE PRECISION NUT(NPMAX), S   (NPMAX), PRIV(NPMAX)
!
!-----------------------------------------------------------
! Partie a modifier par l utilisateur
!-----------------------------------------------------------
! Part to be modified by the user
!-----------------------------------------------------------
!
      DO I=1,NPART
        PRIV (I) = RHO (I) * MASS (I)
      ENDDO
!
!------------------------------------------------------------
! Fin de la partie a modifier par l utilisateur
!------------------------------------------------------------
! End of the part to be modified by the user
!------------------------------------------------------------
!
      RETURN
      END