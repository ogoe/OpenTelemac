!                        ******************
                          SUBROUTINE ECRIT
!                        ******************
!
     &  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     &   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     &   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   ,
     &   P     , PRIV , RHO    , S     , TEMPS, THETA ,
     &   TKE   , VX   , VZ     , X     , Z            )
!
!----------------------------------------------------------------------
!                         MAIN VARIABLES
! .________________.___._______________________________________________
! !           !      !                                                !
! !   NAME    ! MODE !                MEANING                         !
! !___________!______!________________________________________________!
! !           !      !                                                !
! ! CT        ! <--> ! TECPLOT CURRENT ZONE NUMBER                    !
! ! EPS       !  --> ! DISSIPATION RATE                               !
! ! IT        !  --> ! INDEX OF THE CURRENT TIME STEP                 !
! ! KENT      !  --> ! CONDITION TYPE AT FLUID BOUNDARIES             !
! ! KFLUID    !  --> ! FLUID TYPE                                     !
! ! KPAR      !  --> ! PARTICLE  TYPE                                 !
! ! KSORTP    !  --> ! LOGICAL FOR POSITION PRINTOUT                  !
! ! KSORTR    !  --> ! LOGICAL FOR FIELD PRINTOUT                     !
! ! KVISU     !  --> ! CHOICE INDEX FOR THE POSTPROCESSOR             !
! ! KPARMOB   !  --> ! MOVING WALL OR EDGE PARTICLE TYPE              !
! ! MASS      !  --> ! PARTICLE MASS                                  !
! ! NBMAX     !  --> ! MAXIMUM NUMBER OF EDGE PARTICLES               !
! ! NPARB     !  --> ! NUMBER OF EDGE PARTICLES                       !
! ! NPARF     !  --> ! NUMBER OF FLUID PARTICLES                      !
! ! NPART     !  --> ! TOTAL PARTICLE NUMBER                          !
! ! NPMAX     !  --> ! MAXIMUM PARTICLE NUMBER                        !
! ! NT        !  --> ! NUMBER OF TIME STEPS                           !
! ! NUT       !  --> ! EDDY VISCOSITY                                 !
! ! P         !  --> ! PRESSURE                                       !
! ! PRIV      !  <-- ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
! ! RHO       !  --> ! DENSITY                                        !
! ! S         !  --> ! RATE OF STRAIN                                 !
! ! TEMPS     !  --> ! PHYSICAL TIME                                  !
! ! THETA     !  --> ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
! ! TKE       !  --> ! TURBULENT KINETIC ENERGY                       !
! ! VX, VZ    !  --> ! VELOCITY COMPONENTS                            !
! ! X, Z      !  --> ! PARTICLE POSITION                              !
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
! FONCTION : ecrit les positions des particules et les grandeurs
!            physiques dans les fichiers sorties, ainsi
!            que les parametres du fichier suite a la fin du calcul
! FUNCTION : writes particle positions and particle fields in printout
!            files ; writes also the file in order to continue the
!            calculation
!
! PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPART, NPMAX, NT   , NPARF
      INTEGER NPARB, NBMAX, KVISU
      INTEGER IT   , I    , CT
!
      LOGICAL KSORTR, KSORTP
!
      DOUBLE PRECISION X   (NPMAX), Z      (NPMAX)
      DOUBLE PRECISION VX  (NPMAX), VZ     (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), RHO    (NPMAX)
      DOUBLE PRECISION P   (NPMAX), TKE    (NPMAX)
      DOUBLE PRECISION EPS (NPMAX), NUT    (NPMAX)
      DOUBLE PRECISION S   (NPMAX), THETA  (NBMAX)
      DOUBLE PRECISION PRIV(NPMAX)
!
      INTEGER KENT  (NPMAX), KPAR   (NPMAX)
      INTEGER KFLUID(NPMAX), KPARMOB(NPMAX)
!
      DOUBLE PRECISION TEMPS, TEMPO
!
      CHARACTER*7  VARZONE
      CHARACTER*8  VARZONE1
      CHARACTER*21 TEXTE1
      CHARACTER*3  TEXTE2
      CHARACTER*15 TEXTE3
      CHARACTER*2  COMMENT
!
      VARZONE   = 'ZONE I='
      VARZONE1  = ' F=POINT'
      TEXTE1    = 'TEXT X=1 Y=1  T="T = '
      TEXTE2    = ' s"'
      TEXTE3    = ', CS=FRAME, ZN='
      COMMENT   = '# '
!
! Ecritures sorties
!==================
! Output writing
!===============
!
      IF (KVISU.EQ.1) THEN
!
! Format Rubens
!--------------
! Rubens format
!--------------
!
        IF (KSORTP) THEN
!
! Fichier positions
!..................
! Position file
!..............
!
          WRITE (47,903) 'TEMPS =',TEMPS
!
          DO 216 I=1,NPART
            WRITE(47,233) REAL(I), X(I), Z(I)
 216      CONTINUE
!
        ENDIF
!
      ELSE
!
! Format Tecplot
!---------------
! Tecplot format
!---------------
!
        CT = CT+1
!
        IF (KSORTP) THEN
!
! Fichier positions
!..................
! Position file
!..............
!
          WRITE(47,135) VARZONE,NPART,VARZONE1
          DO 234 I=1,NPART
            WRITE(47,133) X(I), Z(I)
 234      CONTINUE
          WRITE(47,132) TEXTE1, TEMPS, TEXTE2,
     &                  TEXTE3, CT
!
        ENDIF
!
        IF (KSORTR) THEN
!
! Fichier champs
!...............
! Field file
!...........
!
          WRITE(48,135) VARZONE,NPART,VARZONE1
          DO 237 I=1,NPART
            WRITE(48,833) X   (I), Z   (I), VX    (I),
     &                    VZ  (I), RHO (I), P     (I),
     &                    TKE (I), EPS (I), NUT   (I),
     &                    S   (I), KPAR(I), KFLUID(I),
     &                    PRIV(I)
 237      CONTINUE
          WRITE(48,132) TEXTE1, TEMPS, TEXTE2,
     &                  TEXTE3, CT
!
        ENDIF
!
      ENDIF
!
! Fichier suite de calcul
!========================
! Continued calculation file
!===========================
!
      IF (IT.EQ.NT) THEN
!
        WRITE(49,137) NPARF
        WRITE(49,137) NPARB
        WRITE(49,137) NPART
        WRITE(49,134) TEMPS
        WRITE(49,137) CT
!
        DO 238 I=1,NPART
          IF (I.GE.NPARF+1.AND.I.LE.NPARF+NPARB) THEN
            TEMPO=THETA(I-NPARF)
          ELSE
            TEMPO=0.D0
          ENDIF
          WRITE(49,933) MASS  (I), X   (I), Z      (I),
     &                  VX    (I), VZ  (I), RHO    (I),
     &                  P     (I), TKE (I), EPS    (I),
     &                  NUT   (I), TEMPO  , KPAR   (I),
     &                  KFLUID(I), KENT(I), KPARMOB(I)
 238    CONTINUE
!
      ENDIF
!
!----------------------------------------------------
!
 132  FORMAT(A21,D16.8,A3,A15,I5)
 133  FORMAT(2(' ',D16.8))
 233  FORMAT(7(' ',F16.8),3(I6))
 134  FORMAT(D16.8)
 135  FORMAT(A7,I6,A8)
 136  FORMAT(A2,I8)
 137  FORMAT(I6)
 833  FORMAT(10(' ',D16.8),2(I6),(' ',D16.8))
 933  FORMAT(11(D16.8),4(I6))
 903  FORMAT(A,F16.8)
!
!
      RETURN
      END