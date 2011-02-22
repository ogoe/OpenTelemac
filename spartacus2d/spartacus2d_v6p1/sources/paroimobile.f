C                        ************************
                          SUBROUTINE PAROIMOBILE
C                        ************************
C
     .  (KPARMOB, NPMAX, NPART, PI, TEMPS, VXMOB, VZMOB)
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! KPARMOB   ! -->  ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! TEMPS     ! -->  ! PHYSICAL TIME                                  !
C ! VXMOB,    ! <--  ! WALL VELOCITY COMPONENTS                       ! 
C ! VZMOB                                                             !
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
C FONCTION : impose une vitesse a une ou plusieurs parois
C FUNCTION : computes the velocity of one or several walls
C
C PROGRAMMES APPELANT : MOUVEMENT
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
      INTEGER I, NPMAX, NPART
C
      DOUBLE PRECISION TEMPS, PERIODE, AMPVIT, PI
C
      INTEGER KPARMOB (NPMAX)
C    
      DOUBLE PRECISION VXMOB (NPMAX), VZMOB (NPMAX)
C
      PERIODE = 2.390D0
      AMPVIT  = 0.103D0*2.D0*PI/PERIODE

      DO 445 I=1,NPART
C
        VXMOB (I) = 0.
        VZMOB (I) = 0.
C
        IF (KPARMOB(I).EQ.1) THEN
            VXMOB (I) = AMPVIT*COS(2.D0*PI*TEMPS/PERIODE)
            VZMOB (I) = 0.
        ENDIF
C
 445  CONTINUE
C
      RETURN
      END
