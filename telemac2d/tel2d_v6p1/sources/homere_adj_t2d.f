!                    *************************
                     SUBROUTINE HOMERE_ADJ_T2D
!                    *************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    1) ACQUIRES THE DATA NECESSARY TO DEFINE THE POINTERS:
!+                STEERING FILE + GEOMETRY FILE (PARTIALLY ONLY).
!+
!+            2) CALIBRATION LOOP.
!
!history  A LEOPARDI (UNINA)
!+        19/09/2000
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!     TYPE INTEGER:
!
      INTEGER I,NPARAM,ITER
      INTEGER IH,IU,IV,NVAR
      INTEGER TROUVE(MAXVAR+10)
      INTEGER NLAGR,ILAGR,NPOINRES
!
!     TYPE REAL:
!
      DOUBLE PRECISION ROX,JCOUT,JR,JCOUTN
      DOUBLE PRECISION R02,R03
      DOUBLE PRECISION C
      DOUBLE PRECISION HIST(1)
      DOUBLE PRECISION JSTEP0,JCOUT1,JCOUT2,JCOUT3
      DOUBLE PRECISION ERRH,ERRU,ERRV,AT1
!
!     TYPE LOGICAL
!
      LOGICAL RSTART
!
      CHARACTER(LEN=72) :: TITFIC
!
!-----------------------------------------------------------------------
!  VARIABLES TO READ :
!  0 : DISCARD    1 : READ  (SEE SUBROUTINE NOMVAR)
!
      INTEGER ALIRRES(MAXVAR)
!
!     ALIRRES READS U,V,H IN TELEMAC RESULTS FILE
!
      DATA ALIRRES /1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!-----------------------------------------------------------------------
! 2:  INIT PIT
!-----------------------------------------------------------------------
!
!     PRINTS HEADER LINES TO LISTING
!
      WRITE(LU,92)
92    FORMAT(/////,
     &14X,'   AAAAA    DDDD       JJ        TTTTTTT  22222  DDDD ',/,
     &14X,'   A   A    D   D      JJ           T         2  D   D',/,
     &14X,'   AAAAA    D   D      JJ   IIII    T     22222  D   D',/,
     &14X,'   A   A    D   D      JJ           T     2      D   D',/,
     &14X,'   A   A    DDDD    JJJJJ           T     22222  DDDD ',/,
     &14X,'                                                      ',/,
     &14X,'               VERSION 6.0   FORTRAN 90               ',/,
     &14X,/////)
!
! ALLOCATES VECTORS, MATRICES AND BLOCKS
!
      CALL POINT_ADJ_T2D
!
!     WILL BE DONE AGAIN IN TELEMAC2D, BUT SEEMS NECESSARY HERE
!     TO GET NZONE
!
      IF(DEFZON) CALL DEF_ZONES
      IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE ZONES : ',NZONE
      IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF ZONES: ',NZONE
      IF(NZONE.GT.NPOIN) THEN
        IF(LNG.EQ.2) WRITE(LU,*) 'ERREUR: PLUS DE ZONES QUE DE POINTS'
        IF(LNG.EQ.2) WRITE(LU,*) 'ERROR: MORE ZONES THAN POINTS'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     NUMBER OF PARAMETERS TO BE ESTIMATED
!
!     TO MODIFY IF FRICTION + SOMETHING ELSE NEEDS ESTIMATING
      NPARAM = NPOIN
      IF(NZONE.GT.0) NPARAM = NZONE
!
!-----------------------------------------------------------------------
!
!     INITIALISATIONS AND OPTIONS FOR THE IDENTIFICATION OF PARAMETERS
!
!     INITIALISES SORLEOA AND SORIMPA
      DO I=1,MAXVAR
         SORLEOA(I)=.FALSE.
         SORIMPA(I)=.FALSE.
      ENDDO
!
      IF(INCLU2(ESTIME,'DEBUG')) THEN
!        CV1, CV2, CV3 (SECOND MEMBERS OF THE ADJOINT SYSTEMS)
         SORLEOA(20)=.TRUE.
         SORLEOA(21)=.TRUE.
         SORLEOA(22)=.TRUE.
!        OUTPUT OF ADJOINT VARIABLES PP, QQ, RR
         SORLEOA(23)=.TRUE.
         SORLEOA(24)=.TRUE.
         SORLEOA(25)=.TRUE.
      ELSE
!        OUTPUT OF BOTTOM TOPOGRAPHY (6) AND FRICTION (19)
         SORLEOA(6)=.TRUE.
         SORLEOA(19)=.TRUE.
      ENDIF
!
      CALL OS('X=C     ',PRIVE,PRIVE,PRIVE,0.D0)
!
!     IDENTIFICATION METHOD: OPTID = 1 SIMPLE GRADIENT
!                            OPTID = 2 CONJ. GRADIENT
!                            OPTID = 3 LAGRANGE INTERPOLATION FOR RHO
!
      WRITE(LU,*) 'OPTID = ',OPTID
      IF(OPTID.EQ.0) THEN
         WRITE(LU,*) 'PLAN D''EXPERIENCE'
      ELSEIF(OPTID.EQ.1) THEN
         WRITE(LU,*) 'GRADIENT METHOD'
      ELSEIF(OPTID.EQ.2) THEN
         WRITE(LU,*) 'CONJUGATE GRADIENT METHOD'
      ELSEIF(OPTID.EQ.3) THEN
         WRITE(LU,*) 'LAGRANGE INTERPOLATION'
      ELSE
         WRITE(LU,*) 'WRONG OPTION FOR COMPUTATION OF RHO'
         CALL PLANTE(1)
         STOP
      ENDIF
!
      IF(OPTID.EQ.3) THEN
        NLAGR=3
      ELSE
        NLAGR=1
      ENDIF
!
!======================================================================
!     INIT ADJOINT
!======================================================================
!
      JSTEP0=1.D0
      JR=0.D0
      JCOUTN = 8000000.D0
      NITERA = 0
!
      JCOUT=0.D0
      RSTART=.TRUE.
!
      IF(OPTID.NE.0) THEN
        CALL OV('X=C     ',GRADJ%R  , GRADJ%R  , GRADJ%R  ,0.D0,NPARAM)
        CALL OV('X=C     ',GRADJN%R , GRADJN%R , GRADJN%R ,0.D0,NPARAM)
      ENDIF
!
!======================================================================
!      FILE MANAGEMENT
!======================================================================
!
! HEADER FOR ASCII OUTPUT (FORMAT SCOPT)
!
       IF(T2D_FILES(T2DRFO)%NAME.NE.' ') THEN
         WRITE(T2D_FILES(T2DRFO)%LU,300) TITCAS
300      FORMAT('''',A,'''',1I2)
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'PARAMETER ESTIMATION'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'IDENTIFICATION OF FRICTION'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'ITERATION'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'COST'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'ERROR ON H (M)'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'ERROR ON U (M/S)'
         WRITE(T2D_FILES(T2DRFO)%LU,300) 'ERROR ON V (M/S)'
         IF(DEFZON) THEN
           DO I=1,NZONE
             WRITE(T2D_FILES(T2DRFO)%LU,300) 'FRICTION ZONE ',I
           ENDDO
         ELSE
           WRITE(T2D_FILES(T2DRFO)%LU,300) 'FRICTION POINT 1'
         ENDIF
       ENDIF
!
! ****************************************************************************
! LOOP OF CALIBRATION
! ****************************************************************************
!
!     INITIAL STRICKLERS ARE SET IN STRCHE
!     (IF NOT MODIFIED ALL VALUES AT FFON)
!
!     LINES BELOW WILL BE REPEATED WHEN NITERA = 1 IN TELEMAC2D (SEE CALL TO FONSTR)
!     THEY ARE KEPT HERE TO INITIALISE SETSTR2
!
      CALL OS('X=C     ',X=CHESTR,C=FFON)
      CALL STRCHE
      CALL INITSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN,T1)
      CALL ASSIGNSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN)
!
95    CONTINUE
!
!
!------------------------------------------------------------------------------
!
!     LIST OF TESTS : READS THE COEFFICIENTS
!
!     SKIPS A COMMENTED LINE
      IF(OPTID.EQ.0) READ(T2D_FILES(T2DFO1)%LU,*)
500   CONTINUE
!
!------------------------------------------------------------------------------
!
!     NITERA : NUMBER OF ITERATIONS
!
      NITERA = NITERA + 1
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '------------------'
      WRITE(LU,*) 'ITERATION : ',NITERA
      WRITE(LU,*) '------------------'
      WRITE(LU,*) ' '
!
! TO PRESERVE THE VALUES OF STRICKLERS'.
!
      CALL OS( 'X=Y     ' ,X=SETSTR2 , Y=SETSTR )
!
! READS THE NEW STRICKLERS IN A FILE IF OPTID=0
!
      IF(OPTID.EQ.0) THEN
        READ(T2D_FILES(T2DFO1)%LU,*,END=999) ITER,
     &       (SETSTR%R(I),I=1,NPARAM)
        IF(ITER.NE.NITERA) THEN
          IF(LNG.EQ.1) WRITE(LU,*)'PB. DANS LE PLAN D''EXPERIENCE ITER=',ITER
          IF(LNG.EQ.2) WRITE(LU,*)'PB. IN LIST OF TESTS AT ITER=',ITER
          STOP
        ENDIF
        CALL ASSIGNSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN)
      ENDIF
!
! *** LOOP FOR LAGRANGE INTERPOLATION ***
!
      DO ILAGR=1,NLAGR
!
         IF(OPTID.EQ.3) THEN
           WRITE(LU,*) ' '
           WRITE(LU,*) '------------------'
           IF(LNG.EQ.1) WRITE(LU,*) 'SOUS-ITERATION : ',ILAGR
           IF(LNG.EQ.2) WRITE(LU,*) 'SUB-ITERATION : ',ILAGR
           WRITE(LU,*) '------------------'
           WRITE(LU,*) ' '
         ENDIF
!
!-----------------------------------------------------------------------
!
         IF(LNG.EQ.1) WRITE(LU,100)
         IF(LNG.EQ.2) WRITE(LU,101)
         WRITE(LU,102)
100      FORMAT(/////,1X,'LISTING DE TELEMAC-2D ',78('-'))
101      FORMAT(/////,1X,'LISTING OF TELEMAC-2D ',78('-'))
102      FORMAT(/////,
     &14X,'TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &14X,'  T    E      L      E      MM MM  A   A  C    ',/,
     &14X,'  T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &14X,'  T    E      L      E      M   M  A   A  C    ',/,
     &14X,'  T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &14X,'                                               ',/,
     &14X,'        2D    VERSION 6.0     FORTRAN 90       ',/,
     &14X,'                                               ',/,
     &14X,'DIRECT MODE DIRECT MODE DIRECT MODE DIRECT MODE',/,
     &14X,/////)
!
      ADJO=.FALSE.
      CALL TELEMAC2D(PASS= -1,ATDEP=0.D0,NITER=0,CODE='       ')
!
!  /* TEMPORAL LOOP (COMPUTES THE COST FUNCTION) */
!
      JCOUT=0.D0
!
! SKIPS GEOMETRY
!
      REWIND T2D_FILES(T2DRES)%LU
!
      CALL SKIPGEO(T2D_FILES(T2DRES)%LU,TITFIC,NPOINRES,NVARRES,TEXRES)
      IF(NPOINRES.NE.NPOIN) THEN
        WRITE(LU,*) 'ERROR: NPOINRES DIFFERENT FROM NPOIN'
        WRITE(LU,*) 'NPOINRES = ',NPOINRES
        WRITE(LU,*) 'NPOIN    = ',NPOIN
        CALL PLANTE(1)
        STOP
      ENDIF
!
! SKIPS INITIAL CONDITION
!
      IF(OUTINI) THEN
       CALL LITENR(VARSOR,VARCL,T2D_FILES(T2DRES)%LU,'STD',HIST,0,
     &             NPOIN,AT1,TEXTE,TEXRES,NVARRES,VARCLA,0,TROUVE,
     &             ALIRRES,W,.FALSE.,MAXVAR)
      ENDIF
!
      ERRH=0.D0
      ERRU=0.D0
      ERRV=0.D0
      IH=0
      IU=0
      IV=0
!
      DO LT=1,NIT
!
!     IN STEADY STATE ONLY THE LAST TIMESTEP IS CONSIDERED
!
      IF(     INCLU2(ESTIME,'PERMANENT')
     &    .OR.INCLU2(ESTIME,'STEADY'   )  ) THEN
!
        IF(LT.EQ.1) THEN
          REWIND T2D_FILES(T2DRES)%LU
          CALL BIEF_SUITE(VARSOR,VARCL,ITER,T2D_FILES(T2DRES)%LU,
     &                    T2D_FILES(T2DRES)%FMT,
     &                    HIST,0,NPOIN,AT1,TEXTE,VARCLA,
     &                    NVARCL,TROUVE,ALIRRES,LISTIN,.TRUE.,MAXVAR)
!         GETTING MEASUREMENTS AND WEIGHTS AT THE SAME TIME.
!         HERE ALPHA1, ALPHA2 AND ALPHA3 ARE ALSO SET.
!         ITER OF LAST RECORD GIVEN BY THE CALL TO SUITE
          CALL MESURES(ITER,AT1)
        ENDIF
!
      ELSE
!
!  READS TELEMAC2D RESULTS (RESULTS FILE - UNIT NRES)
!
        ITER=LT
        IF(OUTINI) ITER=ITER+1
!
        CALL LITENR(VARSOR,VARCL,T2D_FILES(T2DRES)%LU,'STD',HIST,0,
     &              NPOIN,AT1,TEXTE,TEXRES,NVARRES,VARCLA,0,TROUVE,
     &             ALIRRES,W,.FALSE.,MAXVAR)
!
!       GETTING MEASUREMENTS AND WEIGHTS AT THE SAME TIME
!
!       HERE ALPHA1, ALPHA2 AND ALPHA3 ARE ALSO SET.
        CALL MESURES(ITER,AT1)
!
      ENDIF
!
!       COMPUTES THE COST FUNCTION :
!
        CALL COST_FUNCTION(JCOUT,OPTCOST,'FCT')
!
!       COMPUTES THE DIFFERENCES BETWEEN MEASUREMENTS AND COMPUTED VALUES
!
        CALL ERRMAX(H,HD,C,I)
        IF(ERRH.LT.C) THEN
          ERRH=C
          IH=I
        ENDIF
        CALL ERRMAX(U,UD,C,I)
        IF(ERRU.LT.C) THEN
          ERRU=C
          IU=I
        ENDIF
        CALL ERRMAX(V,VD,C,I)
        IF(ERRV.LT.C) THEN
          ERRV=C
          IU=I
        ENDIF
!
! END OF TEMPORAL LOOP (COST FUNCTION COMPUTED)
!
      ENDDO
!
      IF(NITERA.EQ.1.AND.ILAGR.EQ.1) THEN
        IF(JCOUT.GT.0.D0) THEN
          JSTEP0=JCOUT
        ELSE
          JSTEP0=1.D0
        ENDIF
      ENDIF
!
      IF(LNG.EQ.1) THEN
!
      WRITE(LU,*)'FONCTION COUT =',JCOUT,' VALEUR INITIALE:',JSTEP0
      WRITE(LU,*)'ERREUR MAXIMUM SUR H =',ERRH
      WRITE(LU,*)'ERREUR MAXIMUM SUR U =',ERRU
      WRITE(LU,*)'ERREUR MAXIMUM SUR V =',ERRV
!
      ELSEIF(LNG.EQ.2) THEN
!
      WRITE(LU,*) 'COST FUNCTION =',JCOUT,' INITIAL VALUE :',JSTEP0
      WRITE(LU,*) 'MAX ERROR ON H =',ERRH
      WRITE(LU,*) 'MAX ERROR ON U =',ERRU
      WRITE(LU,*) 'MAX ERROR ON V =',ERRV
!
      ENDIF
!
      IF(ILAGR.EQ.1) THEN
        JR = JCOUT/JSTEP0
        IF(LNG.EQ.1) WRITE(LU,*) 'FONCTION COUT RELATIVE : ',JR
        IF(LNG.EQ.2) WRITE(LU,*) 'RELATIVE COST FUNCTION: ',JR
        IF(T2D_FILES(T2DRFO)%NAME(1:1).NE.' ') THEN
          IF(DEFZON) THEN
            WRITE(T2D_FILES(T2DRFO)%LU,*) NITERA,JR,ERRH,ERRU,ERRV,
     &                    (SETSTR%R(I),I=1,NZONE)
          ELSE
            WRITE(T2D_FILES(T2DRFO)%LU,*) NITERA,JR,ERRH,ERRU,ERRV,
     &                     SETSTR%R(1)
          ENDIF
        ENDIF
      ENDIF
!
!
!
      IF(OPTID.EQ.0) GO TO 500
!
!
!
!  TEST, DECISIONAL STEP & ADJOINT SYSTEM (ONLY FOR ILAGR=1)
!
!  TEST: TWO CRITERIA
!
      IF (ILAGR.EQ.1) THEN
!
!     DECISIONAL STEP :
!
      IF(      JR.LE.TOLEST(4).OR.
     &      (ERRH.LE.TOLEST(1).AND.
     &       ERRU.LE.TOLEST(2).AND.
     &       ERRV.LE.TOLEST(3))       ) THEN
!
         IF(LISTIN) THEN
           IF(LNG.EQ.1) WRITE(LU,395) NITERA
           IF(LNG.EQ.2) WRITE(LU,396) NITERA
         ENDIF
!
395      FORMAT(/,1X,'------------------------------------------',/
     &           ,1X,'    SOLUTION TROUVEE EN ',1I3,' ITERATIONS',/
     &           ,1X,'------------------------------------------')
396      FORMAT(/,1X,'-----------------------------------------',/
     &           ,1X,'    SOLUTION FOUND IN ',1I3,' ITERATIONS',/
     &           ,1X,'-----------------------------------------')
         WRITE(LU,*) 'GRADIENT OF ZONE 1 : ',GRADJ%R(1)
         WRITE(LU,*) 'STRICKLER OF POINT 10 : ',CHESTR%R(10)
         GO TO 999
!
      ELSEIF (NITERA.GT.MAXEST) THEN
!
         IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'PAS DE CONVERGENCE EN ',NITERA,' ITERATIONS'
         WRITE(LU,*) 'STRICKLER DU POINT 10 : ',CHESTR%R(10)
         WRITE(LU,398) MAXEST,JCOUT
398      FORMAT(1X,'SOLUTION NON TROUVEE APRES ',1I6,1X,
     &           'ITERATIONS',/,1X,
     &           'PRECISION  :',G16.7,1X,'JCOUTN/JCOUT1 :',G16.7)
         ELSEIF(LNG.EQ.2) THEN
         WRITE(LU,*) 'NO CONVERGENCE AFTER ',NITERA,' ITERATIONS'
         WRITE(LU,*) 'STRICKLER OF POINT 10 : ',CHESTR%R(10)
         WRITE(LU,399) MAXEST,JCOUT
399      FORMAT(1X,'SOLUTION NOT FOUND AFTER ',1I6,1X,
     &           'ITERATIONS',/,1X,
     &           'PRECISION  :',G16.7,1X,'JCOUTN/JCOUT1 :',G16.7)
         ENDIF
         GO TO 999
!
      ELSEIF (JCOUT.GT.JCOUTN.AND..NOT.RSTART) THEN
!
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'LA FONCTION COUT AUGMENTE : STOP'
           WRITE(LU,*) 'STRICKLER DU POINT 10 : ',CHESTR%R(10)
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*) 'COST FUNCTION INCREASES : STOP'
           WRITE(LU,*) 'STRICKLER OF POINT 10 : ',CHESTR%R(10)
         ENDIF
!        GO TO 999
!
      ENDIF
!
! ADJOINT SYSTEM
!
         IF(LNG.EQ.1) WRITE(LU,403)
         IF(LNG.EQ.2) WRITE(LU,404)
         WRITE(LU,405)
403      FORMAT(/////,1X,'LISTING D" ESTIMATION',82(1H-))
404      FORMAT(/////,1X,'LISTING OF ESTIMATION',82(1H-))
405      FORMAT(/////,
     &14X,'TTTTT  EEEEE  L      EEEEE  M   M  AAAAA  CCCCC',/,
     &14X,'  T    E      L      E      MM MM  A   A  C    ',/,
     &14X,'  T    EEE    L      EEE    M M M  AAAAA  C    ',/,
     &14X,'  T    E      L      E      M   M  A   A  C    ',/,
     &14X,'  T    EEEEE  LLLLL  EEEEE  M   M  A   A  CCCCC',/,
     &14X,'                                               ',/,
     &14X,'        2D    VERSION 6.0     FORTRAN 90       ',/,
     &14X,'                                               ',/,
     &14X,' ADJOINT MODE ADJOINT MODE ADJOINT MODE ADJOINT',/,
     &14X,/////)
!
!        INITIALISES THE GRADIENT WHICH WILL BE COMPUTED
!        BY PROPAG_ADJ
!
         CALL OV('X=C     ',GRADJ%R,GRADJ%R,GRADJ%R,0.D0,NPARAM)
!
!        SERIES OF ADJOINT SYSTEMS
!
         ADJO=.TRUE.
         CALL TELEMAC2D(PASS= -1,ATDEP=0.D0,
     &                  NITER=0,CODE='       ')
!
         IF(NZONE.GT.0) THEN
           DO I=1,NZONE
             WRITE(LU,*) 'GRADJ(',I,')= ',GRADJ%R(I)
!            'TEMPORARY' STOP
           ENDDO
         ENDIF
!
! END OF: IF (ILAGR.EQ.1)
      ENDIF
!
!
      IF(ILAGR.EQ.1) THEN
!
!       GRADIENT METHOD: COMPUTES RHO AND DIRECTION
!       JCOUT1 IS JCOUT FOR RHO=0
        JCOUT1=JCOUT
        CALL METGRA(ROX,ESTIME,GRADJ,GRADJN,JCOUT1,DESC,NPARAM,OPTID,
     &              RSTART,R02,R03)
        IF(OPTID.EQ.3) THEN
          CALL NEWSTR(SETSTR,SETSTR2,DESC,ROX,RSTART,NPARAM,
     &                ESTIME,KFROT)
          CALL ASSIGNSTR(CHESTR ,SETSTR,ZONE%I,NZONE,NPOIN)
        ENDIF
      ELSEIF (ILAGR.EQ.2) THEN
! JCOUT2 IS JCOUT FOR RHO=ROX
         JCOUT2=JCOUT
         CALL NEWSTR(SETSTR,SETSTR2,DESC,0.5D0*ROX,RSTART,NPARAM,
     &               ESTIME,KFROT)
         CALL ASSIGNSTR(CHESTR ,SETSTR,ZONE%I,NZONE,NPOIN)
      ELSEIF (ILAGR.EQ.3) THEN
! JCOUT3 IS JCOUT FOR RHO=1/2 ROX
         JCOUT3=JCOUT
!
      ENDIF
!
! *** END OF LAGRANGIAN LOOP ***
      ENDDO
!
!  CASE OF A NEW ITERATION:
!
!  COMPUTES THE NEW VALUE OF ROX (IF LAGRANGE)
      IF(OPTID.EQ.3) CALL INTERPOL(ROX,R02,R03,JCOUT1,JCOUT2,JCOUT3)
!
      WRITE(LU,*) 'ITERATION = ',NITERA
      WRITE(LU,*) 'STRICKLERS = ',SETSTR%R(1)
      WRITE(LU,*) 'J         = ',JCOUT1
      WRITE(LU,*) 'JR        = ',JR
!
!  COMPUTES THE NEW SET OF COEFFICIENTS
!
      CALL NEWSTR(SETSTR,SETSTR2,DESC,ROX,RSTART,NPARAM,ESTIME,KFROT)
      CALL ASSIGNSTR(CHESTR ,SETSTR,ZONE%I,NZONE,NPOIN)
!
!  COST FUNCTION AND GRADIENT AT PREVIOUS ITERATION
!
      JCOUTN=JCOUT1
      CALL OV( 'X=Y     ' , GRADJN%R , GRADJ%R, GRADJ%R , C , NPARAM )
!
      GOTO 95
!
! ****************************************************************************
!  END OF LOOP OF CALIBRATION
! ****************************************************************************
!
999   CONTINUE
!
! PRINTS INFORMATION ABOUT THE LAST ITERATION
!
      WRITE(LU,*) 'ITERATION = ',NITERA
      WRITE(LU,*) 'STRICKLERS = ',SETSTR%R(1)
      WRITE(LU,*) 'J         = ',JCOUT1
      WRITE(LU,*) 'JR        = ',JR
!
!-----------------------------------------------------------------------
!
!     WRITES THE RESULTING GEOMETRY FILE, WHICH WILL ALSO HAVE
!     FRICTION (IN DEBUG MODE, PRINTS OUT THE ADJOINT VARIABLES
!     INSTEAD, SEE IN TELEMAC-2D)
!
      IF(.NOT.INCLU2(ESTIME,'DEBUG')) THEN
!
        CALL ECRGEO(MESH%X%R,MESH%Y%R,MESH%NPOIN,MESH%NBOR%I,
     &              T2D_FILES(T2DRBI)%LU,NVAR,TEXTE,VARCLA,NVARCL,
     &              TITCAS,SORLEOA,MAXVAR,MESH%IKLE%I,
     &              MESH%NELEM,MESH%NPTFR,3,MARDAT,MARTIM,
     &              NCSIZE,NPTIR,MESH%KNOLG%I,I3=I_ORIG,I4=J_ORIG)
        CALL BIEF_DESIMP('SERAFIN ',VARSOR,
     &                   HIST,0,NPOIN,T2D_FILES(T2DRBI)%LU,
     &                   'STD',0.D0,0,LISPRD,LEOPRD,
     &                   SORLEOA,SORIMPA,MAXVAR,TEXTE,0,0)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
