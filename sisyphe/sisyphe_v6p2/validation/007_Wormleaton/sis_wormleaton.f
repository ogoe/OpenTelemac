C                       *****************
C      
                        SUBROUTINE LAYER 
C                       ****************
C
     &(ZFCL_W,NLAYER,AVAI,ZR,ZF,ESTRAT,ELAY,MASBAS,
     & ACLADM,PASSE,NSICLA,NPOIN,ELAY0,TETA,VOLTOT,
     & ES,AVAIL,CONST_ALAYER,DTS,ESTRATNEW,NLAYNEW)
C
C***********************************************************************
C SISYPHE VERSION 5.9                       MATTHIEU GONZALES DE LINARES
C                                                2002       
C
C  
C COPYRIGHT EDF   
C***********************************************************************
C
C                                    
C     FONCTION  : CALCULATION OF AVAI FOR EACH CLASS AND EACH LAYER                                     
C                 NEW STRATUM THICKNESS ESTRAT 
C
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |
C |    ZFCL_W      | -->| EVOLUTION FOR EACH SEDIMENT CLASS
C |    NLAYER      |<-- | NUMBER OF LAYER FOR EACH POINT
C |    NLAYMAX     |<-- | MAX OF NLAYER
C |    ELAY        |<-- | ACTIVE LAYER THICKNESS FOR EACH POINT
C |    ESTRAT      |<-- | ACTIVE STRATUM THICKNESS FOR EACH POINT
C |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPHE
C PROGRAMMES APPELES : 
C***********************************************************************

      USE BIEF
!TBZ********************************************************************
!TBZ    2009-07-30 T.BRUDY-ZIPPELIUS
      USE DECLARATIONS_TELEMAC2D, ONLY:  LISPRD      
      USE DECLARATIONS_TELEMAC2D, ONLY:  LT      
!TBZ   end of changes		               
!TBZ********************************************************************
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W, ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASBAS, ACLADM
      INTEGER,          INTENT(IN)    :: PASSE, NSICLA, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DTS, TETA
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER 
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER, AVAI, ESTRAT, ELAY
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: ES(10,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(10,NSICLA,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLTOT(10),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C-----------------------------------------------------------------------
C
      INTEGER          I,J,K
      DOUBLE PRECISION EVOL,HEIGH,TEST1,TEST2,AEVOL,ZERO
C
C-----------------------------------------------------------------------
C TEST DE PRECISION 
      ZERO=1.D-2
C      
      DO I = 1, NSICLA  
        VOLTOT(I) = 0.D0
      ENDDO
C
      DO 99 J=1,NPOIN

         IF(.NOT.CONST_ALAYER) ELAY0 = 3.D0 * ACLADM%R(J)
C
         NLAYNEW(J) = NLAYER%I(J)
         EVOL  = 0.D0
         AEVOL = 0.D0
         HEIGH = ZF%R(J)-ZR%R(J)
         DO I=1,NSICLA
           EVOL = EVOL + ZFCL_W%ADR(I)%P%R(J)
           AEVOL = AEVOL + ABS(ZFCL_W%ADR(I)%P%R(J))
         ENDDO
C
         IF(AEVOL/DTS.LT.1.D-8) GOTO 99
C
         IF(NLAYER%I(J).GT.1) THEN
CV 02/04/2008 DEBUT MODIF 
C           IF(EVOL.GT.ZERO) THEN
C            DEPOT NORMAL
C             DO I=1,NSICLA
C               AVAIL(2,I,J) = (  AVAIL(1,I,J) * EVOL
C     &                         + AVAIL(2,I,J) * ESTRAT%R(J) )
C     &                        /( EVOL + ESTRAT%R(J) )
C               AVAIL(1,I,J) = (  AVAIL(1,I,J) * ( ELAY0 - EVOL )
C     &                         + ZFCL_W%ADR(I)%P%R(J) 
C     &                        )/ ELAY0  
C             ENDDO
C             ELAY%R(J) = ELAY0
C             ESTRATNEW(J) = ESTRAT%R(J) + EVOL
C           ENDIF
C
C           IF(EVOL.GT.ZERO) THEN
          IF(EVOL.GT.0.D0) THEN
C             DEPOT NORMAL
             IF(EVOL.LE.ELAY0) THEN
                DO I=1,NSICLA
                    AVAIL(2,I,J) = (  AVAIL(1,I,J) * EVOL
     &                         + AVAIL(2,I,J) * ESTRAT%R(J) )
     &                        /( EVOL + ESTRAT%R(J) )
                    AVAIL(1,I,J) = (  AVAIL(1,I,J) * ( ELAY0 - EVOL )
     &                         + ZFCL_W%ADR(I)%P%R(J) 
     &                        )/ ELAY0  
                ENDDO
             ELSE
                DO I=1,NSICLA
                    AVAIL(2,I,J) = (  AVAIL(1,I,J) * ELAY0
     &                         + AVAIL(2,I,J) * ESTRAT%R(J) )
                    AVAIL(2,I,J) = (AVAIL(2,I,J)+ ZFCL_W%ADR(I)%P%R(J)
     &                            *(EVOL-ELAY0)/EVOL)     
     &                            /( EVOL + ESTRAT%R(J) )
                    AVAIL(1,I,J) = ZFCL_W%ADR(I)%P%R(J)/ EVOL
                ENDDO             
             ENDIF    
             ELAY%R(J) = ELAY0
             ESTRATNEW(J) = ESTRAT%R(J) + EVOL
           ENDIF
C
C TEST D'ARRET SI EROSION SUPÉRIEURE A EPAISSEUR DE LA COUCHE ACTIVE !
C           
           IF(EVOL.LT.-ELAY0) THEN
               PRINT*,'EROSION TROP FORTE AU NOEUD J=',J
               PRINT*,'DIMINUER PAS DE TEMPS OU AUGMENTER ELAY0'
               PRINT*,'EVOL=', EVOL, 'ELAY0=',ELAY0 
               CALL PLANTE(1)
               STOP
C TRY ALSO ?????   
C              EVOL = -ELAY0
C             ZFCL_W%ADR(I)%P%R(J)= - ZFCL_W%ADR(I)%P%R(J)* ELAY0/EVOL  
C ??????
            ENDIF   
C FIN MODIF CV (02/04/2008)           
C           IF(EVOL.LE.ZERO) THEN
          IF(EVOL.LE.0.D0) THEN
C               
            ! EROSION AND WE HAVE TO DESTROY A STRATUM
            ! ----------------------------------------
            IF (-EVOL>ESTRAT%R(J)) THEN
!
               ! USUAL CASE
               ! ----------
               IF (NLAYER%I(J).GT.2) THEN
!
                  DO I=1,NSICLA
                     AVAIL(1,I,J) = (  AVAIL(1,I,J)*ELAY0
     &                               + ZFCL_W%ADR(I)%P%R(J)
     &                               + AVAIL(2,I,J)*ESTRAT%R(J)
     &                               - AVAIL(3,I,J)*(EVOL+ESTRAT%R(J))
     &                              )/ ELAY0
                     AVAIL(2,I,J) = AVAIL(3,I,J)
!
                     IF(TETA.EQ.0.D0.OR.PASSE==2) THEN
                        DO K=3,MIN(9,NLAYER%I(J))
                           AVAIL(K,I,J) = AVAIL(K+1,I,J)
                        ENDDO
                        AVAIL(K+1,I,J) = 0.D0
                     ENDIF
!
                  ENDDO
!
                  ELAY%R(J) = ELAY0
                  NLAYNEW(J) = NLAYER%I(J) - 1
                  ESTRATNEW(J) = ESTRAT%R(J) + EVOL + ES(3,J)
!
                  IF(TETA.EQ.0.D0.OR.PASSE.EQ.2) THEN
                    DO K=3,MIN(9,NLAYER%I(J))
                      ES(K,J) = ES(K+1,J)
                    ENDDO
                  ENDIF
!
! ONLY ONE LAYER LEFT
! -------------------
               ELSE
!
                  DO I=1,NSICLA
                     IF (HEIGH>0.D0) THEN
                        AVAIL(1,I,J) = (  AVAIL(1,I,J)*ELAY0
     &                                  + ZFCL_W%ADR(I)%P%R(J)
     &                                  + ESTRAT%R(J)*AVAIL(2,I,J)
     &                                 )/(ELAY%R(J)+EVOL+ESTRAT%R(J)) 
                     ELSE
                        AVAIL(1,I,J) = 0.D0
                     ENDIF
!
                     AVAIL(2,I,J) = 0.D0 
!  CORRECTION BORIS GLANDER (BAW) : 16/11/2007
!  THESE 3 LINES PUT AFTER THE ENDDO                        
!                    NLAYNEW(J) = NLAYER%I(J) - 1
!                    ELAY%R(J) = HEIGH
!                    ESTRATNEW(J) = 0.D0
                  ENDDO
                  NLAYNEW(J) = NLAYER%I(J) - 1
                  ELAY%R(J) = HEIGH
                  ESTRATNEW(J) = 0.D0                   
               ENDIF
!
! EROSION NORMALE
! ---------------
!
            ELSE
               DO I=1,NSICLA
                  AVAIL(1,I,J) = (  AVAIL(1,I,J) * ELAY0
     &                            + ZFCL_W%ADR(I)%P%R(J)
     &                            - EVOL*AVAIL(2,I,J)
     &                           )/ELAY0
               ENDDO
!
               ELAY%R(J) = ELAY0
               ESTRATNEW(J) = ESTRAT%R(J) + EVOL
!
            ENDIF
!
         ENDIF 
!
! THERE WAS ONLY ONE LAYER
! ------------------------
!
         ELSE
!
! IT IS NOW BIG ENOUGH TO MAKE TWO LAYERS
! ---------------------------------------
!
            IF(HEIGH.GT.ELAY0) THEN
               NLAYNEW(J) = 2
               ESTRATNEW(J) = HEIGH - ELAY0
               ELAY%R(J) = ELAY0
               DO I=1,NSICLA
                 AVAIL(2,I,J) = AVAIL(1,I,J)
                 AVAIL(1,I,J) = (AVAIL(1,I,J) * (ELAY0-EVOL)
     &                        + ZFCL_W%ADR(I)%P%R(J) )/ELAY0
               ENDDO        
!
! IF THERE REMAINS ONLY ONE LAYER
! -------------------------------
!
            ELSE
              DO I=1,NSICLA        
                  IF(HEIGH.GT.0.D0) THEN
                     AVAIL(1,I,J) = (  AVAIL(1,I,J)*ELAY%R(J)
     &                               + ZFCL_W%ADR(I)%P%R(J)
     &                              )/ (ELAY%R(J)+EVOL)
                  ELSE
                    AVAIL(1,I,J) = 0.D0
                  ENDIF
                  AVAIL(2,I,J) = 0.D0
              ENDDO
              ELAY%R(J) = HEIGH
              ESTRATNEW(J) = 0.D0
              NLAYNEW(J) = 1
           ENDIF
         ENDIF
!
! GRAPHICAL OUTPUT
! ----------------
!
      IF(TETA.EQ.0.D0.OR.PASSE.EQ.2) THEN
         ! ON N'A PAS BESOIN DE FAIRE TOUT CA A L'ETAPE DE PREDICTION 
         ! CHANGES ARE TAKEN INTO ACCOUNT
         NLAYER%I(J) = NLAYNEW(J)
         ESTRAT%R(J) = ESTRATNEW(J)
         ES(1,J) = ELAY%R(J)
         IF (NLAYER%I(J)>1) ES(2,J) = ESTRAT%R(J)
!
         DO K=1,2
            DO I=1,NSICLA
!            TESTE QUE AVAIL EST BIEN ENTRE 0 ET 1
!CV: RELAXER LE TEST !
           IF((AVAIL(K,I,J)-1.D0)>ZERO.OR.AVAIL(K,I,J)<-ZERO) THEN   
!TBZ********************************************************************
!TBZ    2009-07-30 T.BRUDY-ZIPPELIUS
!TBZ    output limited to listing period
             IF (MOD(LT,LISPRD+1).EQ.0) THEN 
cgl              WRITE(LU,*) 'PROBLEME FRACTION PAS ACCEPTABLE'
! 
cgl              WRITE(LU,*) 'COUCHE K=',K,'CLASSE I=',I,'POINT J=',J,
cgl     *             'AVAIL=' ,AVAIL(K,I,J), 'ZEITSCHRITT =', LT
!             CALL PLANTE(1)
!             STOP     
             ENDIF
!TBZ   end of changes		               
!TBZ********************************************************************
		 ENDIF
            AVAI%ADR(I+(K-1)*NSICLA)%P%R(J) = AVAIL(K,I,J)
            ENDDO
         ENDDO
!
         TEST1 = 0.D0
         TEST2 = 0.D0          
!
         DO I=1,NSICLA
           DO K=1,NLAYER%I(J)
!            CALCUL DU VOLUME TOTAL DE SEDIMENTS DANS LE DOMAINE            
             VOLTOT(I) = VOLTOT(I) + ES(K,J)*AVAIL(K,I,J)*MASBAS%R(J)
           ENDDO
           TEST1 = TEST1 + AVAIL(1,I,J)
           TEST2 = TEST2 + AVAIL(2,I,J)
         ENDDO
! 
!        TESTE QUE LA SOMME DES AVAIL VAUT BIEN 1
!
         IF((TEST1.GT.ZERO.AND.(TEST1-1.D0)**2>ZERO).OR.
     *      (TEST2.GT.ZERO.AND.(TEST2-1.D0)**2>ZERO)) THEN
!TBZ********************************************************************
!TBZ    2009-07-30 T.BRUDY-ZIPPELIUS
!TBZ    output limited to listing period
         IF (MOD(LT,LISPRD+1).EQ.0) THEN 
cgl         WRITE(LU,*) ' PROBLEME AVAIL: N,TEST1,TEST2,T',J,TEST1,TEST2,LT
         ENDIF
	   ENDIF
!TBZ********************************************************************

!
      ENDIF
!      
99    CONTINUE    
!
!     ADDED BY JMH 03/04/2008
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C 
      RETURN
      END

C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################

C                        C                         ********************
                          SUBROUTINE INIT_AVAI 
C                         ********************
C
C
C***********************************************************************
C SISYPHE VERSION 5.3               
C
C                            BUI MINH DUC  2002  
C                            initial fraction distribution
C                            for nonuniform bed materials 
C
C                            Matthieu GONZALES DE LINARES 2002/2003
C
C                                                
C COPYRIGHT EDF-BAW-IFH   
C***********************************************************************
C
C     FONCTION  : INITIAL FRACTION DISTRIBUTION AND LAYER THICKNESS
C
C     PHILOSOPHIE : INIT_COMPO PERMET DE DEFINIR LA STRATIFICATION
C     CORRESPONDANT A LA VARIATION DE COMPOSITION INITIALE DU FOND,
C     AINSI NCOUCHES CORRESPOND AU NOMBRE DE COUCHES INITIALES REELLES
C                   INIT_AVAI CORRIGE ET COMPLETE CETTE STRATIFICATION
C     EN CAS DE PROBLEMES, ET RAJOUTE LA COUCHE ACTIVE, AINSI NLAYER
C     CORRESPOND AU NOMBRE DE COUCHES UTILISEES DANS LE CALCUL
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |    ZF          | -->|  BOTTOM
C |    ZR          | -->|  RIGID BOTTOM
C |    ELAY0       | -->|  WANTED ACTIVE LAYER THICKNESS
C |    NLAYER      |<-- |  NUMBER OF LAYER FOR EACH POINT
C |    ELAY        |<-- |  ACTIVE LAYER THICKNESS FOR EACH POINT
C |    ESTRAT      |<-- |  ACTIVE STRATUM THICKNESS FOR EACH POINT
C |    AVAIL       |<-- |  SEDIMENT FRACTION FOR EACH LAYER, CLASS, NODE
C |    ES          |<-- |  THICKNESS FOR EACH LAYER AND NODE
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPHE
C PROGRAMMES APPELES : INIT_COMPO
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE,EX_INIT_AVAI=> INIT_AVAI
C
      IMPLICIT NONE
C      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
      INTEGER I,J,K,NMAXI
C     TABLEAU AUTOMATIQUE !!!!!
C     INTEGER NCOUCHES(NPOIN)  REMPLACE PAR IT1%I
C     
      DOUBLE PRECISION HAUTSED
      LOGICAL :: TRACE = .TRUE.  !jaj set to .false. -> no debug messages
      LOGICAL :: ESOUTMARK(NOMBLAY)
C
C-----------------------------------------------------------------------
C
      NMAXI = 0
      DO I=1,NSICLA
        VOLTOT(I) = 0.D0
      ENDDO
C
C     USER SET THE INITIAL NUMBER OF LAYERS, THEIR THICKNESS 
C     AND THEIR COMPOSITION
      CALL INIT_COMPO(IT1%I)     
C
C !jaj if ESOUT values for a layer have not been read from a file
C      they remain initialised to -1.0D0 (see point_sisyphe).
C      -> In this case, for a given layer K: 
C      (1) the ES field initialised in init_compo (hopefully correctly) 
C      should not be overwritten by unphys. ESOUT initialisation values
C      (2) Instead, ESOUT gets the ES values provided in init_compo       
C      -> take care in init_compo!

      ESOUTMARK=.TRUE.
      IF (NOMPRE.NE.' ') THEN 
        DO K=1,NOMBLAY
          IF ( MAXVAL(ESOUT%ADR(K)%P%R)<=-1.0D0 ) ESOUTMARK(K)=.FALSE. 
        END DO 
      ENDIF 
C
C !jaj
      IF (NOMPRE.NE.' '.AND.TRACE) THEN
        WRITE (LU,*) ''
        WRITE (LU,*) 'FIELD AND RESTART VALUES AFTER INIT_COMPO' 
        DO K = 1,NOMBLAY
          WRITE(LU,*) ' '
          WRITE(LU,*) 'LAYER, ESOUTMARK: ',K,ESOUTMARK(K)
          WRITE(LU,*) 'MIN MAX ES:   ',MINVAL(ES(K,:)), 
     &                                 MAXVAL(ES(K,:))
          WRITE(LU,*) 'MIN MAX ESOUT:',MINVAL(ESOUT%ADR(K)%P%R), 
     &                                 MAXVAL(ESOUT%ADR(K)%P%R)
          DO I=1,NSICLA
            WRITE(LU,*) 'CLASS: ',I
            WRITE(LU,*) 'MIN MAX AVAIL: ',MINVAL(AVAIL(K,I,:)), 
     &                                    MAXVAL(AVAIL(K,I,:))
            WRITE(LU,*) 'MIN MAX AVAI:  ',
     &           MINVAL(AVAI%ADR(I+(K-1)*NSICLA)%P%R), 
     &           MAXVAL(AVAI%ADR(I+(K-1)*NSICLA)%P%R)
          END DO
        END DO
      ENDIF
C
      DO 45 J=1,NPOIN
C
C        10 IS THE MAXIMUM NUMBER OF LAYERS ALLOWED
         NLAYER%I(J) = IT1%I(J)
         IF(NLAYER%I(J)>10) THEN
           IF(LNG.EQ.1) WRITE(LU,1800)         
           IF(LNG.EQ.2) WRITE(LU,1815)
           CALL PLANTE(1)
           STOP
         ENDIF
C 
C !jaj in the case of a restart, fill the allocated ES field 
C      with the values from the restart file only if actually read 
C      or initialise with the user values from init_compo
C      we use the whole allocated range 
C
        IF (NOMPRE.NE.' '.AND.ESOUTMARK(K)) THEN
          DO K = 1,NOMBLAY
            ES(K,J) = ESOUT%ADR(K)%P%R(J) ! restart, if read....
          END DO 
          
!RK       Der erste Layer darf nicht groesser sein, als der in der cas-Datei angegebene Wert
!         Das kommt vor, wenn beim Einlesen aus der Restartdatei ein double mit real belegt wird.
!         z.b.   0.100000000 -> 0.10000000014567 
 
          IF(ELAY0.LT.ES(1,J)) ES(1,J) = ELAY0
!RK
         
        ELSE
          DO K = 1,NOMBLAY
            ESOUT%ADR(K)%P%R(J) = ES(K,J) ! initialisation with ES, blind 
          END DO
        ENDIF 
C         
C        THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
C        IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
C        IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
         HAUTSED = 0.D0
         DO K=1,IT1%I(J)
         IF (HAUTSED + ES(K,J) >= ZF%R(J) - ZR%R(J)) THEN
           ES(K,J) = ZF%R(J) - ZR%R(J) -  HAUTSED 
!RK           hier wird ES belegt für den Fall, dass die letzte Schicht in der init_compo keine Schichtdicke bekommen hat.
!RK           Dies wurde in der Originalroutine nicht noch mal auf ESOUT kopiert...
           ESOUT%ADR(K)%P%R(J)=ES(K,J)
!RK           
             NLAYER%I(J) = K
           HAUTSED = HAUTSED + ES(K,J)
           GOTO 144
         ENDIF
         HAUTSED = HAUTSED + ES(K,J)
         ENDDO
C
144     CONTINUE
C
C       THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
C       THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
        IF(HAUTSED < (ZF%R(J)-ZR%R(J)) ) THEN
        ES(NLAYER%I(J),J) = ES(NLAYER%I(J),J) +ZF%R(J)-ZR%R(J)-HAUTSED
      ENDIF
C
cgl   Folgendes ist eigentlich nur eine Überprüfung auf sinnvolle Eingaben in der init_compo
cgl   und der cas-Datei. Somit sind Tests nur bei einem "Kaltstart" sinnvoll.
cgl   Bei einem Warmstart führt die real -> double Wandlung zu Genauigkeitsproblemen. 
cgl   deshalb:   IF (NOMPRE.EQ.' ') THEN

      IF (NOMPRE.EQ.' ') THEN

       IF(NLAYER%I(J)>1) THEN
C        On suppose que ELAY0 est plus petit que la premiere strate.
C        A rajouter : le cas ou ELAY0 est plus grand
         IF(ELAY0>ES(1,J)) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) 'COUCHE ACTIVE TROP GROSSE / STRATIFICATION'
           ENDIF
           IF(LNG.EQ.2) THEN
             WRITE(LU,*) ' ACTIVE LAYER TOO BIG / STRATIFICATION '
             WRITE(LU,*) 'J: ',j,' ELAY0: ',elay0,' ES(1,J): ',ES(1,J)
           ENDIF
           CALL PLANTE(1)
           STOP
         ENDIF
       ENDIF
           
      ENDIF
cgl

C     ON SEPARE LA PREMIERE STRATE EN ACTIVE LAYER + ACTIVE STRATUM   
      IF(ELAY0<ES(1,J)) THEN 
       print*,'elay0: ',elay0,' j: ',j,' es(1,j): ',es(1,j)
         NLAYER%I(J) = NLAYER%I(J) + 1
         IF(NLAYER%I(J)>10) THEN
            IF(LNG.EQ.1) WRITE(LU,1800)         
            IF(LNG.EQ.2) WRITE(LU,1815)
            CALL PLANTE(1)
           ENDIF
C          IL FAUT DECALER LES INDICES POUR ES ET AVAIL           
         IF(NLAYER%I(J)>2) THEN 
            DO K=NLAYER%I(J),3,-1
              ES(K,J) = ES(K-1,J)
            ENDDO
         ENDIF
            ES(2,J) = ES(1,J) - ELAY0
            ES(1,J) = ELAY0
           DO I=1,NSICLA
             DO K=NLAYER%I(J),2,-1
               AVAIL(K,I,J) = AVAIL(K-1,I,J)
             ENDDO
           ENDDO
      ENDIF

        ELAY%R(J) = ES(1,J)
C   
        IF(NLAYER%I(J)>1) THEN
          ESTRAT%R(J) = ES(2,J)           
!RK initialising of ESTRAT, if nlayer<1
        ELSE
          ESTRAT%R(J) = 0.D0
!RK and initialising of ES, if nlayer<1
          DO K=2,NOMBLAY
            ES(K,J) = 0.D0
          END DO
!RK end
        ENDIF
C
        IF(NLAYER%I(J)>NMAXI) NMAXI = NLAYER%I(J)
C
C        ON REMPLIT DE ZERO LES AVAIL INUTILES, CA VAUT MIEUX
         IF (NLAYER%I(J)<10) THEN
           DO I = 1, NSICLA
             DO K = NLAYER%I(J)+1,10
               AVAIL(K,I,J) = 0.D0
             ENDDO
           ENDDO
         ENDIF
C
C       LA SUITE DE CALCUL NE PEUT MARCHER QUE EN BICOUCHE (OU MONO)
C       ON MET DANS AVAIL (UTILISE DANS LES CALCULS) LES VALEURS LUES
C       DANS LE FICHIER DU CALCUL PRECEDENT SEDIMENTOLOGIQUE
C       !jaj we apply the whole allocated range
C
        IF (NOMPRE.NE.' ')  THEN
          IF (NLAYER%I(J)>NLAYMAX) THEN !jaj 2->NLAYMAX
            WRITE(LU,*) 'COMPUTATION CONTINUED : ONLY WITH ',
     &                  NLAYMAX,' LAYERS' !see declarations_sisyphe
            CALL PLANTE(1)
          ENDIF
!       print*,'init_avai nomblay',nomblay
! hier kommt nur 2 an. Warum?!?
          DO K = 1,NOMBLAY !jaj 2->NOMBLAY
            DO I=1,NSICLA
!RK Dies ist der Restart-Fall
! fuer die 2. Schicht K=2 soll beim Restart aus der Einschwemmdatei die
! Belegung der 1. Schicht genommen werden
! Diese Anweisung muss wieder raus, beim "normalen" Restart
             IF(K.EQ.2)
     &  AVAI%ADR(I+(K-1)*NSICLA)%P%R(J)=AVAI%ADR(I+(1-1)*NSICLA)%P%R(J)
!RK End
              AVAIL(K,I,J) = AVAI%ADR(I+(K-1)*NSICLA)%P%R(J) ! restart
            END DO
          END DO
        ELSE
          DO K = 1,NOMBLAY !jaj 2->NOMBLAY
            DO I=1,NSICLA
              AVAI%ADR(I+(K-1)*NSICLA)%P%R(J) = AVAIL(K,I,J) ! initialisation, blind
            END DO
          END DO
        ENDIF 
C
C       CALCUL DU VOLUME TOTAL DE SEDIMENTS DE CHAQUE CLASSE
        DO I=1,NSICLA
           DO K=1,NLAYER%I(J)
C NOTE JMH : J'IMAGINE QUE ES N'EST JAMAIS NEGATIF
C            DONC QU'EST-CE QUE C'EST QUE CE TEST QUI NE SERT A RIEN ???
!          IF(ES(K,J) > 0.D0) THEN
             VOLTOT(I) = VOLTOT(I) + ES(K,J)*AVAIL(K,I,J)*VOLU2D%R(J)
!          ENDIF
           ENDDO
        ENDDO
C     
45    CONTINUE
C 
C !jaj just a small security message 
C
      IF (NOMBLAY<NMAXI) THEN  
        WRITE(LU,*) 'BEWARE: NOMBLAY<NMAXI IN INIT_AVAI'
      ENDIF
C
C     ADDED BY JMH 15/11/2007
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
C
      WRITE(LU,*) 'MAXIMUM INITIAL NUMBER OF LAYERS :',NMAXI
      DO I=1,NSICLA
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'VOLUME TOTAL DE LA CLASSE ',I ,' :',VOLTOT(I)
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'TOTAL VOLUME OF CLASS ',I ,' :',VOLTOT(I)
        ENDIF
      ENDDO
C      
1800  FORMAT(1X,'IL Y A PLUS DE 10 COUCHES DANS LA STRATIFICATION')
1815  FORMAT(1X,'THERE ARE MORE THAN 10 LAYERS IN THE STRATIFICATION')
C
      IF (TRACE) THEN 
        WRITE (LU,*) ''
        WRITE (LU,*) 'FIELD VALUES AT THE END OF INIT_AVAI' 
        DO K = 1,NOMBLAY
          WRITE(LU,*) ' '
          WRITE(LU,*) 'LAYER, ESOUTMARK: ',K,ESOUTMARK(K)
          WRITE(LU,*) 'MIN MAX ES:   ',MINVAL(ES(K,:)), 
     &                                 MAXVAL(ES(K,:))
          WRITE(LU,*) 'MIN MAX ESOUT:',MINVAL(ESOUT%ADR(K)%P%R), 
     &                                 MAXVAL(ESOUT%ADR(K)%P%R)
          DO I=1,NSICLA
            WRITE(LU,*) 'CLASS: ',I
            WRITE(LU,*) 'MIN MAX AVAIL: ',MINVAL(AVAIL(K,I,:)), 
     &                                    MAXVAL(AVAIL(K,I,:))
            WRITE(LU,*) 'MIN MAX AVAI:  ',
     &           MINVAL(AVAI%ADR(I+(K-1)*NSICLA)%P%R), 
     &           MAXVAL(AVAI%ADR(I+(K-1)*NSICLA)%P%R)
          END DO
        END DO
      ENDIF
      
      
c       DO  J=1,NPOIN
c
c                                                                   
c                ! 1te Unterschicht---------                        
c                    AVAIL(2,1,J) = 0.13D0           ! Fraktion 1   Oberwasser Isar
c                    AVAIL(2,2,J) = 0.26D0           ! Fraktion 2   Oberwasser Isar
c                    AVAIL(2,3,J) = 0.39D0           ! Fraktion 3   Oberwasser Isar
c                    AVAIL(2,4,J) = 0.14D0           ! Fraktion 4   Oberwasser Isar
c                    AVAIL(2,5,J) = 0.08D0           ! Fraktion 5   Oberwasser Isar
c                                                                   
c                ! 2te Unterschicht---------                        
c                    AVAIL(3,1,J) = 0.13D0           ! Fraktion 1   Oberwasser Isar
c                    AVAIL(3,2,J) = 0.26D0           ! Fraktion 2   Oberwasser Isar
c                    AVAIL(3,3,J) = 0.39D0           ! Fraktion 3   Oberwasser Isar
c                    AVAIL(3,4,J) = 0.14D0           ! Fraktion 4   Oberwasser Isar
c                    AVAIL(3,5,J) = 0.08D0           ! Fraktion 5   Oberwasse 
c       END DO

      RETURN
      END

C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
                          

C                         ********************* 
                          SUBROUTINE INIT_COMPO (NCOUCHES)
C                         *********************
C
C***********************************************************************
C SISYPHE VERSION 5.3
C                             Matthieu GONZALES DE LINARES 2002
C
C                                                
C COPYRIGHT EDF-BAW-IFH   
C***********************************************************************
C
C     FONCTION  : DISTRIBUTION DES CLASSES
C                 % PAR COUCHE, STRATIFICATION 
C     SUBROUTINE A REMPLIR PAR l'UTILISATEUR
C
C 
C     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION, 
C                 VARIATION IN SPACE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                |    |  
C |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
C |    AVAIL(10,NSICLA,NPOIN)
C |    ES          |<-- |  THICKNESS FOR EACH LAYER AND NODE
C |    ES(10,NPOIN)
C |    NCOUCHES     |--> |  NUMBER OF LAYER FOR EACH POINT
C |    NSICLA      |--> |  NUMBER OF SIZE-CLASSES OF BED MATERIAL
C                           (LESS THAN 10)
C |    NPOIN       |--> |  NUMBER OF NODES
C |________________|____|______________________________________________
C MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : INIT_AVAI 
C PROGRAMMES APPELES : NONE
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE  
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER I , J  , K
C 
C-----------------------------------------------------------------------
C
       DO 45 J=1,NPOIN
C
C       BY DEFAULT : UNIFORM BED COMPOSITION
C
cgl          NCOUCHES(J) = 1
cgl          DO I = 1, NSICLA
cgl            AVAIL(1,I,J) = AVA0(I)
cgl            AVAIL(2,I,J) = AVA0(I)
cgl          ENDDO
C  

cgl        print*,'RS INIT_COMPO: NCOUCHES(',J,')=',NCOUCHES(J) 
     
C  TO BE FILLED BY THE USER

c         K=layer
c         I=Fraktion
c         J=Knoten
c
c sis_cas:
c            INITIAL FRACTION FOR PARTICULAR SIZE CLASS       =
c            0.16; 0.16; 0.36; 0.32; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0


          NCOUCHES(J) =  3
          ES(1,J)     =  0.20D0    !  Schichtdicke Activlayer sollte mit dem Wert in der sis.cas-Datei übereinstimmen
          ES(2,J)     =  1.00D0    !  Schichtdicke Stratum (erste Unterschicht) 
c         ES(3,J)     =  x.xxD0     !  wird keine Angabe gemacht  geht sie bis zum rgid Bed

!      Wormleaton    0.16; 0.16; 0.36; 0.33; 0.00; 0.00; 0.00; 0.00; 0.00; 0.00;  

        ! active Layer ------------                        
            AVAIL(1,1,J) = 0.16D0           ! Fraktion 1
            AVAIL(1,2,J) = 0.16D0           ! Fraktion 2
            AVAIL(1,3,J) = 0.36D0           ! Fraktion 3
            AVAIL(1,4,J) = 0.32D0           ! Fraktion 4
            AVAIL(1,5,J) = 0.00D0           ! Fraktion 5
c           AVAIL(1,6,J) = 0.00D0           ! Fraktion 6 
c           AVAIL(1,7,J) = 0.00D0           ! Fraktion 7
c						AVAIL(1,8,J) = 0.00D0           ! Fraktion 8
c						AVAIL(1,9,J) = 0.00D0           ! Fraktion 9
c						AVAIL(1,10,J) = 0.00D0          ! Fraktion 10
						                                           
        ! 1te Unterschicht---------                     
            AVAIL(2,1,J) = 0.16D0           ! Fraktion 1
            AVAIL(2,2,J) = 0.16D0           ! Fraktion 2
            AVAIL(2,3,J) = 0.36D0           ! Fraktion 3
            AVAIL(2,4,J) = 0.32D0           ! Fraktion 4
            AVAIL(2,5,J) = 0.00D0           ! Fraktion 5
c           AVAIL(2,6,J) = 0.00D0           ! Fraktion 6
c           AVAIL(2,7,J) = 0.00D0           ! Fraktion 7
c						AVAIL(1,8,J) = 0.00D0           ! Fraktion 8
c						AVAIL(1,9,J) = 0.00D0           ! Fraktion 9
c						AVAIL(1,10,J) = 0.00D0          ! Fraktion 10
						                                           
        ! 2te Unterschicht---------                     
            AVAIL(3,1,J) = 0.16D0           ! Fraktion 1
            AVAIL(3,2,J) = 0.16D0           ! Fraktion 2
            AVAIL(3,3,J) = 0.36D0           ! Fraktion 3
            AVAIL(3,4,J) = 0.32D0           ! Fraktion 4
c                                         AVAIL(3,5,J) = 0.00D0           ! Fraktion 5
c						AVAIL(3,6,J) = 0.00D0           ! Fraktion 6
c		 				AVAIL(3,7,J) = 0.00D0           ! Fraktion 7
c	 					AVAIL(1,8,J) = 0.00D0           ! Fraktion 8
c						AVAIL(1,9,J) = 0.00D0           ! Fraktion 9
c						AVAIL(1,10,J) = 0.00D0          ! Fraktion 10
						
45     CONTINUE 

       RETURN
       END SUBROUTINE INIT_COMPO

C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
 
C                       *****************
                        SUBROUTINE NOEROD
C                       *****************
C
     * (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
C
C***********************************************************************
C SISYPHE VERSION 5.1                             C. LENORMANT
C                                                
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
C***********************************************************************
C
C     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
C
C
C     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
C     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR 
C     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.  
C
C     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   H            | -->| WATER DEPTH
C |   ZF           | -->| BED LEVEL
C |   ZR           |<-- | RIGID BED LEVEL
C |   Z            | -->| FREE SURFACE 
C |   X,Y          | -->| 2D COORDINATES
C |   NPOIN        | -->| NUMBER OF 2D POINTS
C |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
C |   NLISS        |<-->| NUMBER OF SMOOTHINGS
C |________________|____|______________________________________________
C MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA)
C-----------------------------------------------------------------------
C
      USE BIEF
      use declarations_sisyphe, only : mesh, chestr
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS 
C
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)    
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
C
C-----------------------------------------------------------------------
      INTEGER I
C--------------------
C RIGID BEDS POSITION
C---------------------
C
C       DEFAULT VALUE:       ZR=ZF-100 
C
!TBZ********************************************************************
!TBZ    2009-07-13 T.BRUDY-ZIPPELIUS
!TBZ    Rigid bed read from previous sed. comp. file
!TBZ                                                              
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-0.45D0,NPOIN)                                                    
!TBZ
!TBZ    end of changes
!TBZ********************************************************************


        DO i=1,npoin         
            IF(       CHESTR%R(i) .GT. 0.0049) THEN        
                  ZR(I) = ZF(I)
            END IF 
         
        END DO


C------------------
C SMOOTHING OPTION
C------------------
C       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
C                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
C
        NLISS = 0        
C
C
      RETURN
      END SUBROUTINE NOEROD

C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################
C########################################################################################

      ! ************************* ! 
        SUBROUTINE BEDLOAD_EFFPNT ! (_IMP_) 
      ! ************************* ! 
 
     & (MASKEL,LIQBOR,S,ZF,U2D,V2D,UCMOY,NPOIN,NPTFR,IELMT,KENT, 
     &  BETA,PI,MSK,MESH,DZFDX,DZFDY,TETA,CTETA,STETA,T1, 
     &  COEF,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2, 
     &  TOB,XMVS,XMVE,DM,GRAV,UNSV2D) 
 
C**********************************************************************C 
C SISYPHE VERSION 5.1  11/09/1995  E. PELTIER                          C 
C SISYPHE VERSION 5.1  11/09/1995  C. LENORMANT                        C 
C SISYPHE VERSION 5.1  11/09/1995  J.-M. HERVOUET                      C 
C**********************************************************************C 
 
             ! ========================================= ! 
             ! Calcul des parametres de l'effet de pente ! 
             ! ========================================= ! 
 
 
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT 
C**********************************************************************C 
C                                                                      C 
C                 SSSS I   SSSS Y   Y PPPP  H   H EEEEE                C 
C                S     I  S      Y Y  P   P H   H E                    C 
C                 SSS  I   SSS    Y   PPPP  HHHHH EEEE                 C 
C                    S I      S   Y   P     H   H E                    C 
C                SSSS  I  SSSS    Y   P     H   H EEEEE                C 
C                                                                      C 
C----------------------------------------------------------------------C 
C                             ARGUMENTS                                C 
C .____________.____.__________________________________________________C 
C |    NOM     |MODE|                    ROLE                          C 
C |____________|____|__________________________________________________C 
C |____________|____|__________________________________________________C 
C                                                                      C 
C                =>  Can't be change                                   C 
C                <=> Can be change                                     C 
C                <=  Must be set                                       C  
C ---------------------------------------------------------------------C 
!                                                                      ! 
! CALLED BY BEDLOAD_INIT                                               ! 
!                                                                      ! 
! CALL      ------                                                     ! 
!                                                                      ! 
!======================================================================! 
!======================================================================! 
!                    DECLARATION DES TYPES ET DIMENSIONS               ! 
!======================================================================! 
!======================================================================! 
 
      ! 1/ MODULES 
      ! ---------- 
      USE INTERFACE_SISYPHE,EX_BEDLOAD_EFFPNT => BEDLOAD_EFFPNT 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
 
 
      ! 2/ GLOBAL VARIABLES 
      ! ------------------- 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, U2D,V2D, UCMOY, TOB 
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT 
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA 
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2 
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DM 
      LOGICAL,          INTENT(IN)    :: MSK 
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETA,CTETA,STETA,T1 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, CALFA, SALFA 
 
 
      ! 3/ LOCAL VARIABLES 
      ! ------------------ 
      INTEGER          :: I, K 
      DOUBLE PRECISION :: C,ALPHA,ZETA,PSI,PHI,C1,CALPHA,SALPHA,AA,BB
      DOUBLE PRECISION :: CPSI,SPSI,DZF,TANPHI,CZETA,SZETA,SURBETA2 
      DOUBLE PRECISION :: NORM ,TT1 
! 
!======================================================================! 
!======================================================================! 
!                               PROGRAMME                              ! 
!======================================================================! 
!======================================================================! 
! 
!     DETERMINATION DE COS ET SIN TETA 
!     TETA = ANGLE DE L'ECOULEMENT PAR RAPPORT A AXE X 
!
      DO I=1,NPOIN
        IF(UCMOY%R(I).GE.1.D-12) THEN
          CTETA%R(I)=U2D%R(I)/UCMOY%R(I)
          STETA%R(I)=V2D%R(I)/UCMOY%R(I)
        ELSE
          CTETA%R(I)=1.D0
          STETA%R(I)=0.D0
        ENDIF
      ENDDO
! 
!----------------------------------------------------------------------  
! 
!     CALCUL DE LA PENTE  : D(ZF)/DX ET D(ZF)/DY (VALEURS NODALES) 
! 
      CALL VECTOR(DZFDX, '=', 'GRADF          X',IELMT,1.D0,ZF,S,S, 
     *            S,S,S,MESH,MSK,MASKEL) 
      CALL VECTOR(DZFDY, '=', 'GRADF          Y',IELMT,1.D0,ZF,S,S, 
     *            S,S,S,MESH,MSK,MASKEL) 
C 
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DZFDX,2,MESH)
        CALL PARCOM(DZFDY,2,MESH)
      ENDIF 
C 
      CALL OS('X=XY    ',X=DZFDX,Y=UNSV2D)  
      CALL OS('X=XY    ',X=DZFDY,Y=UNSV2D)    
! 
!====================================================================== 
!
!     CALCUL DE L'ANGLE DU TRANSPORT SOLIDE ALFA = TETA + DEVIATION 
!
!     1 : KOCH ET FLOKSTRA
!
      IF(DEVIA==1) THEN 
! 
      C = 2.D0*(XMVS-XMVE)*GRAV*DM/3.D0
      DO I=1,NPOIN
!RK        TT1=C/MAX(TOB%R(I),1.D-10)
        TT1 = BETA
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM 
      ENDDO 
! 
!     2 : TALMON ET AL. JHR 1995 33(4) 
! 
      ELSEIF(DEVIA==2) THEN 
! 
      SURBETA2=1.D0/BETA2
      C = (XMVS-XMVE)*GRAV*DM*SURBETA2**2
      DO I=1,NPOIN
        TT1=SQRT(C/MAX(TOB%R(I),1.D-10))
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM 
      ENDDO 
!  
      ENDIF 
! 
!====================================================================== 
! 
!     CALCUL DE COEF POUR LA PRISE EN COMPTE DE L'EFFET DE PENTE 
!     SUR L'AMPLITUDE DU TRANSPORT SOLIDE                         
! 
!     METHODE 1 (EMPIRICAL METHOD) 
! 
      IF(SLOPEFF==1) THEN 
! 
        DO I=1,NPOIN
          COEF%R(I)=MAX(0.D0,
     *    1.D0-BETA*(DZFDX%R(I)*CTETA%R(I)+DZFDY%R(I)*STETA%R(I)) )
        ENDDO
! 
!     METHODE 2 : SOULSBY 1997 DYNAMICS OF MARINE SANDS p107-108 
!
      ELSEIF(SLOPEFF.EQ.2) THEN 
C 
        TANPHI = TAN(PHISED*PI/180.D0) 
C 
        DO I=1,NPOIN 
C
C         COSINUS ET SINUS DE LA DIRECTION DE LA PENTE
          DZF=SQRT(DZFDX%R(I)**2+DZFDY%R(I)**2)
          IF(DZF.GT.1.D-12) THEN
            CALPHA=DZFDX%R(I)/DZF
            SALPHA=DZFDY%R(I)/DZF
          ELSE
            CALPHA=1.D0
            SALPHA=0.D0
          ENDIF
C 
C         ZETA ANGLE QUE FAIT LA PENTE AVEC HORIZONTALE (BETA DE SOULSBY) 
          ZETA=ATAN(DZF)
          CZETA=COS(ZETA)
          SZETA=SIN(ZETA)    
C   
C         PSI ANGLE DU COURANT PAR RAPPORT A LA DIRECTION DE LA PENTE
C         PSI=TETA%R(I)-ALPHA
          CPSI=CTETA%R(I)*CALPHA+STETA%R(I)*SALPHA
          SPSI=STETA%R(I)*CALPHA-CTETA%R(I)*SALPHA  
          C1=(CZETA*TANPHI)**2-(SPSI*SZETA)**2 
          COEF%R(I)=MAX((CPSI*SZETA+SQRT(MAX(C1,0.D0)))/TANPHI,0.D0) 
          COEF%R(I)=MAX(COEF%R(I),0.D0) 
C 
        ENDDO 
! 
      ENDIF 
!
! ********************************************************************* ! 
!     V - TRAITEMENT DES POINTS FRONTIERES A DEBIT IMPOSE               ! 
!      PAS DE MODIFICATION DU QS QUAND IL EST DETERMINE PAR UTILISATEUR !  
! ********************************************************************* !
! 
      DO K = 1 , NPTFR 
         IF (LIQBOR%I(K) == KENT) COEF%R(MESH%NBOR%I(K)) = 1.D0 
!                           R.K. mai 2007 
!                           KSORT = 4
         IF (LIQBOR%I(K) == 4) COEF%R(MESH%NBOR%I(K)) = 1.D0 
      ENDDO 
! 
!====================================================================== 
!====================================================================== 
! 
      RETURN 
      END SUBROUTINE BEDLOAD_EFFPNT
