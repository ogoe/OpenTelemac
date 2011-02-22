C                     ********************
                       SUBROUTINE LECINIT
C                     ********************
C
     .  (NPARB , NPARF  , NPART, NPMAX, NBMAX, KENT, 
     .   KFLUID, KPARMOB, KPAR , DR   , THETA, X   ,
     .   Z                                         )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                 MEANING                        !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DR        ! <--  ! INITIAL INTERPARTICLE SPACING                  !
C ! KENT      ! <--  ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    ! <--  ! FLUID TYPE                                     !
C ! KPAR      ! <--  ! PARTICLE  TYPE                                 !
C ! KPARMOB   ! <--  ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! NPARB     ! <--  ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     ! <--  ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--  ! TOTAL PARTICLE NUMBER                          !
C ! NBMAX     ! -->  ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! THETA     ! <--  ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
C ! X, Z      ! <--  ! PARTICLE POSITION                              !
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
C FONCTION : lit le fichier de geometrie initiale
C FUNCTION : reads the initial geometry file
C
C PROGRAMMES APPELANT : INITIAL
C CALLED BY          
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS     
C 
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX, NPARF, NPART, NPARB
      INTEGER NBMAX, I    , J    
C
      DOUBLE PRECISION DR  , TEMPO
C
      INTEGER KPAR(NPMAX), KFLUID (NPMAX)
      INTEGER KENT(NPMAX), KPARMOB(NPMAX) 
C
      DOUBLE PRECISION X(NPMAX), Z(NPMAX), THETA(NBMAX)
C
C Lecture du fichier de geometrie initiale
C=========================================
C Initial geometry file reading
C =============================
C
      OPEN (77,FILE='FORT.77',STATUS='old')
C
      READ (77,544) NPARF
      READ (77,544) NPARB
      READ (77,544) NPART
      READ (77,543) DR
C
      DO 678 I=1,NPART
        READ (77,545) J, X   (I), Z     (I), TEMPO  ,
     .                   KPAR(I), KFLUID(I), KENT(I), KPARMOB(I)
        IF (KPAR(I).EQ.2) THEN
          THETA(I-NPARF)=TEMPO
        ENDIF
 678  CONTINUE
C
      CLOSE (77)
C
 543  FORMAT(D16.8)
 544  FORMAT(I6)
 545  FORMAT(I6,3(D16.8),4(I1))
C 
      RETURN
      END
