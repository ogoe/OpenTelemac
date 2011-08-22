!                     ********************
                       SUBROUTINE LECINIT
!                     ********************
!
     &  (NPARB , NPARF  , NPART, NPMAX, NBMAX, KENT,
     &   KFLUID, KPARMOB, KPAR , DR   , THETA, X   ,
     &   Z                                         )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                 MEANING                        !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
! ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    ! <--  ! FLUID TYPE                                     !
! ! KPAR      ! <--  ! PARTICLE  TYPE                                 !
! ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
! ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
! ! X, Z      ! <--  ! PARTICLE POSITION                              !
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
! FONCTION : lit le fichier de geometrie initiale
! FUNCTION : reads the initial geometry file
!
! PROGRAMMES APPELANT : INITIAL
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
      INTEGER NPMAX, NPARF, NPART, NPARB
      INTEGER NBMAX, I    , J
!
      DOUBLE PRECISION DR  , TEMPO
!
      INTEGER KPAR(NPMAX), KFLUID (NPMAX)
      INTEGER KENT(NPMAX), KPARMOB(NPMAX)
!
      DOUBLE PRECISION X(NPMAX), Z(NPMAX), THETA(NBMAX)
!
! Lecture du fichier de geometrie initiale
!=========================================
! Initial geometry file reading
! =============================
!
      OPEN (77,FILE='FORT.77',STATUS='old')
!
      READ (77,544) NPARF
      READ (77,544) NPARB
      READ (77,544) NPART
      READ (77,543) DR
!
      DO 678 I=1,NPART
        READ (77,545) J, X   (I), Z     (I), TEMPO  ,
     &                   KPAR(I), KFLUID(I), KENT(I), KPARMOB(I)
        IF (KPAR(I).EQ.2) THEN
          THETA(I-NPARF)=TEMPO
        ENDIF
 678  CONTINUE
!
      CLOSE (77)
!
 543  FORMAT(D16.8)
 544  FORMAT(I6)
 545  FORMAT(I6,3(D16.8),4(I1))
!
      RETURN
      END