C                        ******************
                          SUBROUTINE ECRIT
C                        ******************
C 
     .  (NBMAX , NPARB, NPARF  , NPART , NPMAX, NT    ,
     .   CT    , IT   , KENT   , KFLUID, KPAR , KSORTP,
     .   KSORTR, KVISU, KPARMOB, EPS   , MASS , NUT   , 
     .   P     , PRIV , RHO    , S     , TEMPS, THETA , 
     .   TKE   , VX   , VZ     , X     , Z            )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! CT        ! <--> ! TECPLOT CURRENT ZONE NUMBER                    !
C ! EPS       !  --> ! DISSIPATION RATE                               !
C ! IT        !  --> ! INDEX OF THE CURRENT TIME STEP                 !
C ! KENT      !  --> ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    !  --> ! FLUID TYPE                                     !
C ! KPAR      !  --> ! PARTICLE  TYPE                                 !
C ! KSORTP    !  --> ! LOGICAL FOR POSITION PRINTOUT                  !
C ! KSORTR    !  --> ! LOGICAL FOR FIELD PRINTOUT                     !
C ! KVISU     !  --> ! CHOICE INDEX FOR THE POSTPROCESSOR             !
C ! KPARMOB   !  --> ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! MASS      !  --> ! PARTICLE MASS                                  !
C ! NBMAX     !  --> ! MAXIMUM NUMBER OF EDGE PARTICLES               !
C ! NPARB     !  --> ! NUMBER OF EDGE PARTICLES                       !
C ! NPARF     !  --> ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     !  --> ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     !  --> ! MAXIMUM PARTICLE NUMBER                        !
C ! NT        !  --> ! NUMBER OF TIME STEPS                           !
C ! NUT       !  --> ! EDDY VISCOSITY                                 !
C ! P         !  --> ! PRESSURE                                       !
C ! PRIV      !  <-- ! PIVATE PRINTOUT VARIABLE FOR THE USER          !
C ! RHO       !  --> ! DENSITY                                        !
C ! S         !  --> ! RATE OF STRAIN                                 !
C ! TEMPS     !  --> ! PHYSICAL TIME                                  !
C ! THETA     !  --> ! ANGLE BETWEEN WALL NORMAL VECTOR AND X-AXIS    !
C ! TKE       !  --> ! TURBULENT KINETIC ENERGY                       !
C ! VX, VZ    !  --> ! VELOCITY COMPONENTS                            ! 
C ! X, Z      !  --> ! PARTICLE POSITION                              !
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
C FONCTION : ecrit les positions des particules et les grandeurs
C            physiques dans les fichiers sorties, ainsi
C            que les parametres du fichier suite a la fin du calcul
C FUNCTION : writes particle positions and particle fields in printout 
C            files ; writes also the file in order to continue the 
C            calculation 
C
C PROGRAMMES APPELANT : SPARTACUS2D
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
      INTEGER NPART, NPMAX, NT   , NPARF 
      INTEGER NPARB, NBMAX, KVISU
      INTEGER IT   , I    , CT
C
      LOGICAL KSORTR, KSORTP
C
      DOUBLE PRECISION X   (NPMAX), Z      (NPMAX)   
      DOUBLE PRECISION VX  (NPMAX), VZ     (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), RHO    (NPMAX) 
      DOUBLE PRECISION P   (NPMAX), TKE    (NPMAX)
      DOUBLE PRECISION EPS (NPMAX), NUT    (NPMAX)
      DOUBLE PRECISION S   (NPMAX), THETA  (NBMAX)
      DOUBLE PRECISION PRIV(NPMAX) 
C            
      INTEGER KENT  (NPMAX), KPAR   (NPMAX) 
      INTEGER KFLUID(NPMAX), KPARMOB(NPMAX)
C
      DOUBLE PRECISION TEMPS, TEMPO
C
      CHARACTER*7  VARZONE
      CHARACTER*8  VARZONE1
      CHARACTER*21 TEXTE1
      CHARACTER*3  TEXTE2
      CHARACTER*15 TEXTE3
      CHARACTER*2  COMMENT
C
      VARZONE   = 'ZONE I='
      VARZONE1  = ' F=POINT'
      TEXTE1    = 'TEXT X=1 Y=1  T="T = '
      TEXTE2    = ' s"'
      TEXTE3    = ', CS=FRAME, ZN='
      COMMENT   = '# '
C
C Ecritures sorties
C==================
C Output writing
C===============
C 
      IF (KVISU.EQ.1) THEN
C
C Format Rubens
C--------------
C Rubens format
C--------------
C
        IF (KSORTP) THEN
C
C Fichier positions
C..................
C Position file
C..............
C
          WRITE (47,903) 'TEMPS =',TEMPS
C	
          DO 216 I=1,NPART
            WRITE(47,233) REAL(I), X(I), Z(I)
 216      CONTINUE
C
        ENDIF
C
      ELSE
C
C Format Tecplot
C---------------
C Tecplot format
C---------------
C
        CT = CT+1
C
        IF (KSORTP) THEN
C
C Fichier positions
C..................
C Position file
C..............
C
          WRITE(47,135) VARZONE,NPART,VARZONE1
          DO 234 I=1,NPART
            WRITE(47,133) X(I), Z(I)
 234      CONTINUE
          WRITE(47,132) TEXTE1, TEMPS, TEXTE2,
     .                  TEXTE3, CT
C
        ENDIF
C
        IF (KSORTR) THEN
C
C Fichier champs
C...............
C Field file
C...........
C
          WRITE(48,135) VARZONE,NPART,VARZONE1
          DO 237 I=1,NPART
            WRITE(48,833) X   (I), Z   (I), VX    (I), 
     .                    VZ  (I), RHO (I), P     (I), 
     .                    TKE (I), EPS (I), NUT   (I),
     .                    S   (I), KPAR(I), KFLUID(I),
     .                    PRIV(I)
 237      CONTINUE
          WRITE(48,132) TEXTE1, TEMPS, TEXTE2,
     .                  TEXTE3, CT
C
        ENDIF
C
      ENDIF
C
C Fichier suite de calcul
C========================
C Continued calculation file
C===========================
C
      IF (IT.EQ.NT) THEN
C 
        WRITE(49,137) NPARF
        WRITE(49,137) NPARB
        WRITE(49,137) NPART
        WRITE(49,134) TEMPS
        WRITE(49,137) CT
C
        DO 238 I=1,NPART
          IF (I.GE.NPARF+1.AND.I.LE.NPARF+NPARB) THEN
            TEMPO=THETA(I-NPARF)
          ELSE
            TEMPO=0.D0
          ENDIF
          WRITE(49,933) MASS  (I), X   (I), Z      (I),
     .                  VX    (I), VZ  (I), RHO    (I),
     .                  P     (I), TKE (I), EPS    (I),
     .                  NUT   (I), TEMPO  , KPAR   (I), 
     .                  KFLUID(I), KENT(I), KPARMOB(I)
 238    CONTINUE
C
      ENDIF
C
C----------------------------------------------------
C
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
 

      RETURN
      END
