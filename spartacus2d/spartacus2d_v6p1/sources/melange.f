C                        ********************
                          SUBROUTINE MELANGE
C                        ********************
C
     .(NPART , NPMAX , DR , LM , X , Z , DELTA , KPAR)
C
C----------------------------------------------------------------------
C                              MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !               MEANING                          !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! DELTA     ! -->  ! EDGE PARTICLE DISTANCE TO REAL WALLS           !
C ! DR        ! -->  ! INITIAL INTERPARTICLE SPACING                  !
C ! KPAR      ! -->  ! PARTICLE  TYPE                                 !
C ! LM        ! <--  ! MIXING LENGTH                                  !
C ! NPART     ! -->  ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! X, Z      ! -->  ! PARTICLE POSITION                              !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)     
C     
C----------------------------------------------------------------------
C     
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C     
C FONCTION : definition (par l'utilisateur) de la longueur de melange
C FUNCTION : Mixing length defined by the user
C
C PROGRAMMES APPELANT : INITIAL , VISCTURB
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
      INTEGER NPMAX, NPART
      INTEGER I
C     
      DOUBLE PRECISION DR , DELTA
C
      INTEGER KPAR (NPMAX)
C
      DOUBLE PRECISION LM(NPMAX)
      DOUBLE PRECISION X (NPMAX)
      DOUBLE PRECISION Z (NPMAX)            
C
C Definition de la longueur de melange
C=====================================
C Mixing length definition
C=========================
C
      DO 407 I=1,NPART
        LM(I)=DR
        LM(I)=MAX(LM(I),1.0D-5)                      
 407  CONTINUE
C
      RETURN
      END
