!                    *****************
                     SUBROUTINE CONDIW
!                    *****************
!
     &( AT, LT , TC1, TC2, TV1, TV2, TM1, TM2 ,
     &  NVHMA,NVCOU,NVWIN, PART , U_TEL, V_TEL , H_TEL )
!
!***********************************************************************
! TOMAWAC   V7P1
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history T FOUQUET (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to initialise spectrum when no wind
!+       and speini =1,3,5
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |<--| COMPUTATION TIME
!| H_TEL          |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NVCOU          |---| NUMBER OF VARIABLES OF THE CURRENT FILE
!| NVHMA          |<--| N.OF VARIABLES OF THE WATER LEVEL FILE
!| NVWIN          |<--| N.OF VARIABLES OF THE WIND FILE
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: PART,LT
      INTEGER, INTENT(INOUT)          :: NVHMA,NVCOU,NVWIN
      DOUBLE PRECISION, INTENT(INOUT) :: AT,TC1,TC2,TV1,TV2,TM1,TM2
      TYPE(BIEF_OBJ), INTENT(IN)      :: U_TEL,V_TEL,H_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER UL
      CHARACTER(LEN=8) ::FFORMAT
      LOGICAL TROUVE(3)
!
!-----------------------------------------------------------------------
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE TIDAL CURRENT AND WATER LEVEL
!
      TROUVE(1)=.FALSE.
      TROUVE(2)=.FALSE.
      TROUVE(3)=.FALSE.
!
      IF(MAREE) THEN
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
          CALL ANAMAR(UC,VC,TRA31,ZM1,ZM2,DZHDT,X,Y,
     &                NPOIN2,AT,DDC,LT)
        ELSE
!
!         READS IN THE CURRENTS FROM BINARY FILE, AND POSSIBLY THE DEPTH
!
          CALL LECDOI(UC,NAMEU,    'VELOCITY U      M/S             ',2,
     &                VC,NAMEV,    'VELOCITY V      M/S             ',2,
     &                DEPTH,NAMEH, 'WATER DEPTH     M               ',1,
     &                X, Y, NPOIN2, WAC_FILES(WACCOB)%LU,
     &                WAC_FILES(WACCOB)%FMT, NBOR, NPTFR,
     &                AT,DDC,TC1,TC2,UC1,UC2,VC1,VC2,
     &                ZM1,ZM2,INDIC,'COURANT',NVCOU,TEXCOB,TROUVE,
     &                UNITCOB,PHASCOB)
!         IF DEPTH READ
          IF(TROUVE(3)) THEN
            CALL OV('X=Y-Z   ',DZHDT,ZM2,ZM1,0.D0,NPOIN2)
            CALL OV('X=CX    ',DZHDT,DZHDT,DZHDT,1.D0/(TC2-TC1),NPOIN2)
          ELSE
            CALL OV('X=C     ',DZHDT,DZHDT,DZHDT,0.D0,NPOIN2)
          ENDIF
!
        ENDIF
!
!       READS IN THE TIDAL WATER LEVEL IF NOT FOUND IN CURRENT FILE
!
        IF(.NOT.TROUVE(3)) THEN
        IF(WAC_FILES(WACMAF)%NAME(1:1).EQ.' '.AND.
     &     WAC_FILES(WACMAB)%NAME(1:1).EQ.' ') THEN
          IF(WAC_FILES(WACCOF)%NAME.NE.' '.OR.
     &       WAC_FILES(WACCOB)%NAME.NE.' ') THEN
            CALL ANAMAR ( UC, VC, TRA31, ZM1, ZM2 , DZHDT, X, Y,
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
            IF(LNG.EQ.1) WRITE(LU,*) 'LECTURE HAUTEUR DANS FICHIER MAF'
            IF(LNG.EQ.2) WRITE(LU,*) 'READING DEPTH IN FILE MAF'
            UL=WAC_FILES(WACMAF)%LU
            FFORMAT=WAC_FILES(WACMAF)%FMT
          ELSE
            IF(LNG.EQ.1) WRITE(LU,*) 'LECTURE HAUTEUR DANS FICHIER MAB'
            IF(LNG.EQ.1) WRITE(LU,*) 'READING DEPTH IN FILE MAB'
            UL=WAC_FILES(WACMAB)%LU
            FFORMAT=WAC_FILES(WACMAB)%FMT
          ENDIF
          CALL LECDOI(UC, NAMEU, 'VELOCITY U      M/S             ',0,
     &                VC, NAMEV, 'VELOCITY V      M/S             ',0,
     &                DEPTH,NAMEH, 'WATER DEPTH     M               ',2,
     &                X, Y, NPOIN2, UL, FFORMAT, NBOR, NPTFR,
     &                AT, DDC, TM1, TM2, UC1, UC2, VC1, VC2,
     &                ZM1,  ZM2, INDIM, 'HAUTEUR', NVHMA, TEXMAB,TROUVE,
     &                UNITMAB,PHASMAB)
          CALL OV('X=Y-Z   ',DZHDT, ZM2, ZM1, 0.D0, NPOIN2)
          CALL OV('X=CX    ',DZHDT, DZHDT, DZHDT, 1.D0/(TM2-TM1),NPOIN2)
        ENDIF
!
      ENDIF
      ENDIF
!
!     INITIALISES THE CONSTANT CURRENT
!
      IF(COUSTA) THEN
        IF(WAC_FILES(WACCOF)%NAME(1:1).EQ.' '.AND.
     &     WAC_FILES(WACCOB)%NAME(1:1).EQ.' ') THEN
           CALL ANACOS(UC, VC, X, Y, NPOIN2)
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
          IF(WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
            UL=WAC_FILES(WACCOF)%LU
            FFORMAT=WAC_FILES(WACCOF)%FMT
          ELSE
            UL=WAC_FILES(WACCOB)%LU
            FFORMAT=WAC_FILES(WACCOB)%FMT
          ENDIF
!         HERE DEPTH POSSIBLY READ AS THIRD VARIABLE
          CALL LECDON(UC,NAMEU, 'VELOCITY U      M/S             ',2,
     &                VC,NAMEV, 'VELOCITY V      M/S             ',2,
     &                DEPTH,NAMEH, 'WATER DEPTH     M               ',1,
     &                X, Y, NPOIN2, UL, FFORMAT, NBOR, NPTFR,
     &                NPTT, INDIC, 'COURANT', TEXCOB, TROUVE)
        ENDIF
        CALL OV('X=C     ',DZHDT,DZHDT,DZHDT,0.D0,NPOIN2)
      ENDIF
!
      IF(PART.EQ.0) THEN
        CALL OS('X=Y     ',X=SDEPTH,Y=H_TEL)
        CALL OV('X=Y     ',UC, U_TEL%R, U_TEL%R, 0.D0, NPOIN2)
        CALL OV('X=Y     ',VC, V_TEL%R, V_TEL%R, 0.D0, NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WIND
!
      IF(VENT) THEN
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
          CALL ANAVEN(UV, VV, X, Y, NPOIN2,AT,DDC,VX_CTE,VY_CTE)
        ELSE
          IF(WAC_FILES(WACVEF)%NAME(1:1).NE.' ') THEN
            UL=WAC_FILES(WACVEF)%LU
            FFORMAT=WAC_FILES(WACVEF)%FMT
          ELSE
            UL=WAC_FILES(WACVEB)%LU
            FFORMAT=WAC_FILES(WACVEB)%FMT
          ENDIF
          IF(VENSTA) THEN
            CALL LECDON(UV, NAMEWX,'WIND ALONG X    M/S             ',2,
     &                  VV, NAMEWY,'WIND ALONG Y    M/S             ',2,
     &                  VV,'????????????????????????????????',
     &                        '????????????????????????????????',0,
     &                  X, Y, NPOIN2,UL, FFORMAT, NBOR, NPTFR,
     &                  NPTT,INDIV,'WIND   ',TEXVEB,TROUVE)
          ELSE
            CALL LECDOI(UV, NAMEWX,'WIND ALONG X    M/S             ',2,
     &                  VV, NAMEWY,'WIND ALONG Y    M/S             ',2,
     &                  VV, '????????????????????????????????',
     &                        '????????????????????????????????',0,
     &                  X, Y, NPOIN2, UL, FFORMAT, NBOR, NPTFR,
     &                  AT,DDC,TV1,TV2, UV1, UV2, VV1, VV2,
     &                  VV1, VV2, INDIV, 'VENT   ', NVWIN, TEXVEB,
     &                  TROUVE, UNITVEB, PHASVEB)
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISES F
!
      IF((INISPE.EQ.1.OR.INISPE.EQ.3.OR.INISPE.EQ.5).AND..NOT.VENT) THEN
!       SPECTRUM is NULL
        CALL SPEINI(F, TRA01(1:NF), TRA01(NF+1:NF+NPLAN),
     &              UV, VV, FREQ, TETA, GRAVIT,
     &              FREMAX, FETCH, SIGMAA, SIGMAB, GAMMA, FPIC, HM0,
     &              ALPHIL, TETA1, SPRED1, TETA2, SPRED2 ,XLAMDA,NPOIN2,
     &              NPLAN, NF, 0, E2FMIN, DEPTH, FRABI)
      ELSE
        CALL SPEINI(F, TRA01(1:NF), TRA01(NF+1:NF+NPLAN),
     &              UV, VV, FREQ, TETA, GRAVIT,
     &              FREMAX, FETCH, SIGMAA, SIGMAB, GAMMA, FPIC, HM0,
     &              ALPHIL,TETA1,SPRED1,TETA2,SPRED2,XLAMDA,NPOIN2,
     &              NPLAN, NF, INISPE, E2FMIN, DEPTH, FRABI)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
