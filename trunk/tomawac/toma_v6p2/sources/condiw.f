!                    *****************
                     SUBROUTINE CONDIW
!                    *****************
!
     &( AT, LT , TC1, TC2, TV1, TV2, TM1, TM2 , 
     &  NVHMA  , NVCOU, PART , U_TEL, V_TEL , H_TEL )
!
!***********************************************************************
! TOMAWAC   V6P3                                   10/06/2011
!***********************************************************************
!
!brief    INITIALISES THE ARRAYS WITH PHYSICAL PARAMETERS.
!
!history  F.MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/01/2013
!+        V6P3
!+   Calls modified.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |<--| COMPUTATION TIME
!| H_TEL          |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: PART,LT
      INTEGER, INTENT(INOUT)          :: NVHMA,NVCOU
      DOUBLE PRECISION, INTENT(INOUT) :: AT,TC1,TC2,TV1,TV2,TM1,TM2
      TYPE(BIEF_OBJ), INTENT(IN)      :: U_TEL,V_TEL,H_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IBID,UL
      CHARACTER(LEN=7) CHDON
!
!-----------------------------------------------------------------------
!
      AT  = 0.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE TIDAL CURRENT AND WATER LEVEL
!
      IF(MAREE) THEN
!
        IF(LNG.EQ.1) THEN
          CHDON='COURANT'
        ELSE
          CHDON='CURRENT'
        ENDIF
!
!       READS IN THE TIDAL CURRRENT AND (OPTIONAL) DEPTH
!
!       JMH 16/11/2012 : formatted file suppressed     
!
        IF(WAC_FILES(WACCOB)%NAME(1:1).EQ.' ') THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT DE MAREE'
            WRITE(LU,*)
     &      'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
            WRITE(LU,*)
     &      '==> LE COURANT DE MAREE EST INITIALISE DANS ANAMAR'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'USE OF TIDAL CURRENT VELOCITIES'
            WRITE(LU,*)'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL CURRENT VELOCITIES IN ANAMAR'
          ENDIF
          WRITE(LU,*) ' '
          CALL ANAMAR(SUC%R,SVC%R,STRA31%R,SZM1%R,
     &                SZM2%R,SDZHDT%R,MESH%X%R,MESH%Y%R ,
     &                NPOIN2,AT,DDC,LT)
        ELSE
!         READS IN THE TIDAL CURRENT FROM BINARY FILE
          CALL LECDOI(SUC%R,SVC%R,MESH%X%R,MESH%Y%R,
     &                NPOIN2,WAC_FILES(WACCOB)%LU,BINCOU,NBOR,NPTFR,
     &                AT,DDC,TC1,TC2,SUC1%R,SVC1%R,SUC2%R,SVC2%R,
     &                INDIC,CHDON,NVCOU)
        ENDIF
!
!       READS IN THE TIDAL WATER LEVEL
!
        IF(WAC_FILES(WACMAF)%NAME(1:1).EQ.' '.AND.
     &     WAC_FILES(WACMAB)%NAME(1:1).EQ.' ') THEN
          IF(WAC_FILES(WACCOF)%NAME.NE.' '.OR.
     &       WAC_FILES(WACCOB)%NAME.NE.' ') THEN
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
          IF(WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
            WRITE(LU,*) 'LECTURE COURANT DANS FICHIER LECHAM MAF'
            UL=WAC_FILES(WACMAF)%LU
          ELSE
            WRITE(LU,*) 'LECTURE COURANT DANS FICHIER LECHAM MAB'
            UL=WAC_FILES(WACMAB)%LU
          ENDIF
          CALL LECHAM(STRA31%R,SDZHDT%R,MESH%X%R,MESH%Y%R,
     &                NPOIN2,UL,BINMAR,NBOR,NPTFR,AT,DDC,TM1,TM2, 
     &                SZM1%R,SZM2%R,INDIM,IDHMA,NVHMA)
        ENDIF
        CALL OV('X=X+Y   ',SDEPTH%R,STRA31%R,ST1%R,0.D0,NPOIN2)
!
      ENDIF
!
!     INITIALISES THE CURRENT
!
      IF(COUSTA) THEN
        IF(WAC_FILES(WACCOF)%NAME(1:1).EQ.' '.AND.
     &     WAC_FILES(WACCOB)%NAME(1:1).EQ.' ') THEN
          IF(COUSTA) THEN
            CALL ANACOS(SUC%R,SVC%R,MESH%X%R,MESH%Y%R,NPOIN2)
            WRITE(LU,*)' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT'
              WRITE(LU,*)
     &        'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
              WRITE(LU,*)'==> LE COURANT EST INITIALISE DANS ANACOS'
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*)'USE OF CURRENT VELOCITIES'
              WRITE(LU,*)
     &        'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
              WRITE(LU,*)
     &        '==> INITIALISATION OF CURRENT VELOCITIES IN ANACOS'
            ENDIF
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*)'RELECTURE D''UNE VARIABLE TELEMAC IMPOSSIBLE'
            ELSE
              WRITE(LU,*)' READING OF A TELEMAC DATA IMPOSSIBLE '
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          IF(WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
            UL=WAC_FILES(WACCOF)%LU
          ELSE
            UL=WAC_FILES(WACCOB)%LU
          ENDIF
!         HERE DEPTH READ AS THIRD VARIABLE
          CALL LECDON(SUC%R,'VITESSE U       M/S             ',
     &                      'VELOCITY U      M/S             ',2,    
     &                SVC%R,'VITESSE V       M/S             ',
     &                      'VELOCITY V      M/S             ',2, 
     &                SDEPTH%R,'HAUTEUR D''EAU   M               ',
     &                         'WATER DEPTH     M               ',1, 
     &                MESH%X%R,MESH%Y%R,NPOIN2,UL,BINCOU,NBOR,NPTFR,
     &                NPTT,INDIC,'COURANT')
        ENDIF
        DZHDT = 0.D0
      ENDIF
!
      IF(PART.EQ.0) THEN
        CALL OS('X=Y     ',X=SDEPTH,Y=H_TEL)
        CALL OV('X=Y     ',SUC%R,U_TEL%R,U_TEL%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',SVC%R,V_TEL%R,V_TEL%R,0.D0,NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WIND
!
      IF(VENT) THEN
        IF(LNG.EQ.1) THEN
          CHDON='VENT   '
        ELSEIF(LNG.EQ.2) THEN
          CHDON='WIND   '
        ENDIF
!
        IF(WAC_FILES(WACVEF)%NAME(1:1).EQ.' '.AND.
     &     WAC_FILES(WACVEB)%NAME(1:1).EQ.' '     ) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN VENT'
            WRITE(LU,*)'MAIS PAS DE FICHIER DE VENT'
            WRITE(LU,*)'==> LE VENT EST INITIALISE DANS ANAVEN'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'USE OF WIND VELOCITIES'
            WRITE(LU,*)'BUT NO WIND FILE '
            WRITE(LU,*)'==> INITIALISATION OF WIND VELOCITIES IN ANAVEN'
          ENDIF
          WRITE(LU,*) ' '
          CALL ANAVEN(SUV%R,SVV%R,MESH%X%R,MESH%Y%R,
     &                NPOIN2,AT,DDC,VX_CTE,VY_CTE)
        ELSE
          IF(WAC_FILES(WACVEF)%NAME(1:1).NE.' ') THEN
            UL=WAC_FILES(WACVEF)%LU
          ELSE
            UL=WAC_FILES(WACVEB)%LU
          ENDIF
          IF(VENSTA) THEN
            CALL LECDON(SUV%R,'VENT X          M/S             ',
     &                        'WIND ALONG X    M/S             ',2,    
     &                  SVV%R,'VENT Y          M/S             ',
     &                        'WIND ALONG Y    M/S             ',2, 
     &                  SVV%R,'????????????????????????????????',
     &                        '????????????????????????????????',0, 
     &                  MESH%X%R,MESH%Y%R,NPOIN2,UL,BINVEN,NBOR,NPTFR,
     &                  NPTT,INDIC,'WIND   ')
          ELSE
            CALL LECDOI(SUV%R,SVV%R,MESH%X%R,MESH%Y%R,
     &                  NPOIN2,UL,BINVEN,NBOR,NPTFR,
     &                  AT,DDC,TV1,TV2,SUV1%R,SVV1%R,SUV2%R,SVV2%R,
     &                  INDIV,CHDON,IBID)
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES F
!
      CALL SPEINI
     &  ( SF%R  , TRA01(1:NF)   , TRA01(NF+1:NF+NPLAN),
     &    SUV%R , SVV%R      , SFR%R  , STETA%R  , GRAVIT,
     &    FREMAX   , FETCH , SIGMAA, SIGMAB , GAMMA  , FPIC  , HM0   ,
     &    ALPHIL   , TETA1 , SPRED1, TETA2  , SPRED2 , XLAMDA, NPOIN2,
     &    NPLAN    , NF    , INISPE, E2FMIN , DEPTH  , FRABI  )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
