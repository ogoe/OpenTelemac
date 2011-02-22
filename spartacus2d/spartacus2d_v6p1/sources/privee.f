C                        *******************
                          SUBROUTINE PRIVEE
C                        *******************
C
     .(NPMAX, NPART, EPS, MASS, NUT, P ,
     . PRIV , RHO  , S  , TKE , VX , VZ,
     . X    , Z                        )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! EPS       ! -->  ! DISSIPATION RATE                               !
C ! MASS      ! -->  ! PARTICLE MASS                                  !
C ! NUT       ! -->  ! EDDY VISCOSITY                                 !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! P         ! -->  ! PRESSURE                                       !
C ! PRIV      ! <--  ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
C ! RHO       ! -->  ! DENSITY                                        !
C ! S         ! -->  ! RATE OF STRAIN                                 !
C ! TKE       ! -->  ! TURBULENT KINETIC ENERGY                       !
C ! VX, VZ    ! -->  ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : calcule la valeur d une variable de sortie definie par l utilisateur
C FUNCTION : computes a printout variable defined by the user
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY           
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS     
C
C----------------------------------------------------------------------
C
C Mode d emploi
C--------------
C Afin de visualiser une nouvelle variable de sortie, l utilisateur
C est invite a decommenter les lignes de calcul suivantes
C
C User guide
C-----------
C In order to visualize a new printout variable, the user has to
C suppress the following comment characters
C
C-----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX , NPART , I
C
      DOUBLE PRECISION X  (NPMAX), Z   (NPMAX), P   (NPMAX)
      DOUBLE PRECISION VX (NPMAX), VZ  (NPMAX), TKE (NPMAX)
      DOUBLE PRECISION RHO(NPMAX), MASS(NPMAX), EPS (NPMAX)
      DOUBLE PRECISION NUT(NPMAX), S   (NPMAX), PRIV(NPMAX)
C
C-----------------------------------------------------------
C Partie a modifier par l utilisateur
C-----------------------------------------------------------
C Part to be modified by the user
C-----------------------------------------------------------
C 
      DO I=1,NPART
        PRIV (I) = RHO (I) * MASS (I)
      ENDDO
C
C------------------------------------------------------------
C Fin de la partie a modifier par l utilisateur
C------------------------------------------------------------
C End of the part to be modified by the user
C------------------------------------------------------------     
C
      RETURN
      END
