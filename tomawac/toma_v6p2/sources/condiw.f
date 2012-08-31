!                    *****************
                     SUBROUTINE CONDIW
!                    *****************
!
     &( AT, LT , DPI, TC1, TC2, NPC , TV1, TV2, NPV, TM1, TM2 , NPM ,
     &  NVHMA  , NVCOU, PART , U_TEL, V_TEL , H_TEL )
!
!***********************************************************************
! TOMAWAC   V6P1                                   10/06/2011
!***********************************************************************
!
!brief    INITIALISES THE ARRAYS WITH PHYSICAL PARAMETERS.
!
!history  F.MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+
!
!history
!+        25/08/2000
!+        V5P0
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Modification for direct coupling with TELEMAC
!
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |<--| COMPUTATION TIME
!| DPI            |---| 2*PI
!| H_TEL          |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NPC            |-->| NUMBER OF POINTS OF THE CURRENT FILE
!| NPM            |-->| NUMBER OF POINTS OF THE WATER HEIGHT FILE
!| NPV            |-->| NUMBER OF POINTS OF THE WIND FILE
!| NVCOU          |---| NUMBER OF VARIABLES OF THE FORMATTED CURRENT FILE
!| NVHMA          |<--| N.OF VARIABLES OF THE FORMATTED WATER LEVEL FILE
!| PART           |-->| FLAG FOR DIRECT COUPLING WITH TELEMAC
!| TC1            |<--| TIME T1 IN THE CURRENT FILE
!| TC2            |<--| TIME T2 IN THE CURRENT FILE
!| TM1            |<--| TIME T1 IN THE WATER LEVEL FILE
!| TM2            |<--| TIME T2 IN THE WATER LEVEL FILE
!| TV1            |<--| TIME T1 IN THE WIND FILE
!| TV2            |<--| TIME T2 IN THE WIND FILE
!| U_TEL          |-->| X-AXIS TELEMAC CURRENT SPEED
!| V_TEL          |-->| Y-AXIS TELEMAC CURRENT SPEED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_CONDIW=> CONDIW
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      DOUBLE PRECISION AT , DPI, TC1, TC2, TV1, TV2, TM1, TM2
      INTEGER          NPC,NPV,NPM,LT,NP0,NP1,NP2,NP3,NP4,NP5
      INTEGER          IPLAN, IFREQ , NVHMA, NVCOU, IBID
!
      CHARACTER*7   CHDON
!
!GM V6P1 - COUPLING WITH TELEMAC
      INTEGER,           INTENT(IN)      :: PART
      TYPE(BIEF_OBJ),    INTENT(IN)      :: U_TEL,V_TEL,H_TEL
!GM Fin
!
!***********************************************************************
!
      AT  = 0.D0
      NP0 = NPOIN3_G+1
      NP1 = 2*NPOIN3_G
      NP2 = NP1+1
      NP3 = 3*NPOIN3_G
      NP4 = NP3+1
      NP5 = 4*NPOIN3_G
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TIDAL CURRENT AND WATER LEVEL
!
      IF (MAREE) THEN
        IF(LNG.EQ.1) THEN
          CHDON='COURANT'
        ELSE
          CHDON='CURRENT'
        ENDIF
!       READS IN THE TIDAL CURRRENT
        IF((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
          WRITE(LU,*) 'FICHIER ANALYTIQUE POUR COURANT'
          CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2    , AT  , DDC  , LT         )
          WRITE(LU,*) ' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT DE MAREE'
            WRITE(LU,*)
     &      'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
            WRITE(LU,*)
     &      '==> LE COURANT DE MAREE EST INITIALISE DANS ANAMAR'
          ELSE
            WRITE(LU,*)'USE OF TIDAL CURRENT VELOCITIES'
            WRITE(LU,*)'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL CURRENT VELOCITIES IN ANAMAR'
          ENDIF
        ELSE
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
!           READS IN THE TIDAL CURRENTS FROM FORMATTED DATA FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACCOF)%LU , BINCOU, NBOR , NPTFR,
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN3_G, CHDON, NVCOU)
          ELSE
!           READS IN THE TIDAL CURRENT FROM BINARY FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACCOB)%LU , BINCOU, NBOR , NPTFR,
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN3_G, CHDON, NVCOU )
          ENDIF
        ENDIF
!
!       READS IN THE TIDAL WATER LEVEL
!
        IF((WAC_FILES(WACMAF)%NAME(1:1).EQ.' ').AND.
     &                       (WAC_FILES(WACMAB)%NAME(1:1).EQ.' ')) THEN
          IF((WAC_FILES(WACCOF)%NAME.NE.' ').OR.
     &       (WAC_FILES(WACCOB)%NAME.NE.' ')) THEN
            CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2,   AT  , DDC , LT    )
          ENDIF
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
     &      '==> LA HAUTEUR DE LA MAREE EST INITIALISEE DANS ANAMAR'
          ELSE
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL WATER LEVEL IN ANAMAR'
          ENDIF
        ELSE
          IF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
            WRITE(LU,*) 'LECTURE COURANT DANS FICHIER LECHAM MAF'
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACMAF)%LU, BINMAR, NBOR  ,
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
     &        TRA01(1:NPOIN3_G)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN3_G, IDHMA ,
     &        NVHMA )
           ELSE
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACMAB)%LU , BINMAR, NBOR  ,
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
     &        TRA01(1:NPOIN3_G)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN3_G, IDHMA ,
     &        NVHMA )
           ENDIF
        ENDIF
        CALL OV('X=X+Y   ',SDEPTH%R,STRA31%R,ST1%R,0.D0,NPOIN2)
      ENDIF
!
!   INITIALISES THE CURRENT
!   AND READS IN A TELEMAC VARIABLE (OPTIONAL)
!
      IF ((COUSTA).OR.(DONTEL)) THEN
        IF ((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
          IF(COUSTA) THEN
             CALL ANACOS
     &      ( SUC%R, SVC%R, MESH%X%R, MESH%Y%R, NPOIN2)
             WRITE(LU,*)' '
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT'
               WRITE(LU,*)
     &         'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
               WRITE(LU,*)'==> LE COURANT EST INITIALISE DANS ANACOS'
             ELSE
               WRITE(LU,*)'USE OF CURRENT VELOCITIES'
               WRITE(LU,*)
     &         'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
               WRITE(LU,*)
     &         '==> INITIALISATION OF CURRENT VELOCITIES IN ANACOS'
             ENDIF
          ELSE
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'RELECTURE D''UNE VARIABLE TELEMAC IMPOSSIBLE'
             ELSE
               WRITE(LU,*)' READING OF A TELEMAC DATA IMPOSSIBLE '
             ENDIF
             CALL PLANTE(0)
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            CHDON='COURANT'
          ELSE
            CHDON='CURRENT'
          ENDIF
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACCOF)%LU   , BINCOU ,
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN3_G , CHDON)
          ELSE
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACCOB)%LU   , BINCOU ,
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN3_G , CHDON)
          ENDIF
        ENDIF
        DZHDT = 0.D0
      ENDIF
!
!GM V6P1 - DIRECT COUPLING WITH TELEMAC
      IF(PART.EQ.0) THEN
        CALL OS('X=Y     ',X=SDEPTH,Y=H_TEL)
        CALL OV('X=Y     ',SUC%R,U_TEL%R,U_TEL%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',SVC%R,V_TEL%R,V_TEL%R,0.D0,NPOIN2)
      ENDIF
!GM Fin
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WIND
!
      IF (VENT) THEN
        IF(LNG.EQ.1) THEN
          CHDON='VENT   '
        ELSE
          CHDON='WIND   '
        ENDIF
!
        IF ((WAC_FILES(WACVEF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACVEB)%NAME(1:1).EQ.' ')) THEN
          CALL ANAVEN
     &   ( SUV%R, SVV%R, MESH%X%R, MESH%Y%R ,
     &     NPOIN2,AT,DDC,VX_CTE,VY_CTE)
          WRITE(LU,*)' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN VENT'
            WRITE(LU,*)'MAIS PAS DE FICHIER DE VENT'
            WRITE(LU,*)'==> LE VENT EST INITIALISE DANS ANAVEN'
          ELSE
            WRITE(LU,*)'USE OF WIND VELOCITIES'
            WRITE(LU,*)'BUT NO WIND FILE '
            WRITE(LU,*)'==> INITIALISATION OF WIND VELOCITIES IN ANAVEN'
          ENDIF
        ELSE
          IF (WAC_FILES(WACVEF)%NAME(1:1).NE.' ') THEN
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACVEF)%LU   , BINVEN ,
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN3_G ,CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACVEF)%LU , BINVEN, NBOR , NPTFR,
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN3_G, CHDON, IBID )
           ENDIF
          ELSE
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
     &        NPOIN2 , WAC_FILES(WACVEB)%LU   , BINVEN ,
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN3_G),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN3_G , CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
     &        NPOIN2, WAC_FILES(WACVEB)%LU, BINVEN, NBOR , NPTFR,
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
     &        TRA01(1:NPOIN3_G) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN3_G, CHDON, IBID )
           ENDIF
          ENDIF
         ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES TETA
!     BY DEFAULT THE DIRECTIONS OF PROPAGATION ARE EVENLY DISTRIBUTED
!
      DO IPLAN = 1,NPLAN+1
         TETA(IPLAN) = (IPLAN-1)*DPI/NPLAN
      ENDDO
!
!-----------------------------------------------------------------------
!
!
!     INITIALISES FREQ AND DFREQ, THE FREQUENCIES OF PROPAGATION
!     ARE DISTRIBUTED USING AN EXPONENTIAL LAW
!
      DO IFREQ = 1,NF
        FREQ(IFREQ) = F1*RAISF**(IFREQ-1)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISES F
!
      CALL SPEINI
     &  ( SF%R  , TRA01(1:NF)   , TRA01(NP0:NP1),
     &    SUV%R , SVV%R      , SFR%R  , STETA%R  , GRAVIT,
     &    FREMAX   , FETCH , SIGMAA, SIGMAB , GAMMA  , FPIC  , HM0   ,
     &    ALPHIL   , TETA1 , SPRED1, TETA2  , SPRED2 , XLAMDA, NPOIN2,
     &    NPLAN    , NF    , INISPE, E2FMIN , DEPTH  , FRABI  )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
