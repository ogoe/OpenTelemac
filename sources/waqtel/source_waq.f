!                    **********************
                     SUBROUTINE SOURCE_WAQ
!                    **********************
!
     &(NPOIN,TEXP,TIMP,TN,NTRAC,WAQPROCESS,RAYEFF,IND_T,HPROP,
     & U,V,CF,T1,T2,T3,T4,PATMOS,LISTIN,GRAV,ZF,DEBUG)
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/09/2014
!***********************************************************************
!
!brief    GIVES CONTRIBUTION OF WAQ PROCESSES TO SOURCE TERMS
!+                FOR THE TRACER.
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+   CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DT             |-->| TIME STEP
!| HPROP          |-->| PROPAGATION DEPTH
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| WAQPROCESS     |-->| WATER QUALITY PROCESS
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY : DEMBEN,PHOTO,RESP,FORMK2,
     &                               O2SATU,K1,K22,K44,WATTEMP
      USE INTERFACE_WAQTEL, EX_SOURCE_WAQ => SOURCE_WAQ
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER       , INTENT(IN)    :: NPOIN,WAQPROCESS,DEBUG
      INTEGER       , INTENT(IN)    :: NTRAC
      INTEGER       , INTENT(INOUT) :: IND_T
      LOGICAL       , INTENT(IN)    :: LISTIN
      DOUBLE PRECISION,INTENT(IN  ) :: GRAV
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN,HPROP,CF,U,V,PATMOS,ZF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TEXP,TIMP,RAYEFF,T1,T2,T3,T4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!-----------------------------------------------------------------------
!
!     IMPLICIT SOURCE TERMS (DEPENDING ON THE LAW CHOSEN)
!
!       DO ITRAC=1,NTRAC
!         IF(LOITRAC(ITRAC).EQ.0) THEN
!           YASMI(ITRAC)=.FALSE.
!         ELSEIF(LOITRAC(ITRAC).EQ.1) THEN
!           YASMI(ITRAC)=.TRUE.
!           CALL OS('X=CY    ',X=TIMP%ADR(ITRAC)%P,Y=HPROP,
!      &            C=-2.3D0/COEF1TRAC(ITRAC)/3600.D0)
!         ELSE
!           IF(LNG.EQ.1) WRITE(LU,*) 'DIFSOU : LOI NON PROGRAMMEE'
!           IF(LNG.EQ.2) WRITE(LU,*) 'DIFSOU : LAW NOT IMPLEMENTED'
!         ENDIF
!       ENDDO
!
!
!-----------------------------------------------------------------------
!
!     TAKES THE SOURCES OF TRACER INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
!
      SELECT CASE(WAQPROCESS)
!       O2 MODULE
        CASE(1)
          CALL CALCS_O2(NPOIN,WATTEMP,O2SATU,DEMBEN,FORMK2,K1,K44,
     &              K22,PHOTO,RESP,TN,TEXP,NTRAC,GRAV,HPROP,U,V,ZF)
!       BIOMASS MODULE
        CASE(2)
          CALL CALCS_BIOMASS(NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,
     &                      HPROP,T1,T2,DEBUG)
!       EUTRO MODULE
        CASE(3)
          CALL CALCS_EUTRO(NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,
     &                      HPROP,T1,T2,T3,U,V,DEBUG)
!       MICROPOL MODULE
        CASE(4)
          CALL CALCS_MICROPOL(NPOIN,NTRAC,TN,TEXP,
     &                                HPROP,CF,U,V,T1,T2,T3,T4)
!       THERMIC MODULE
        CASE(5)
           CALL CALCS_THERMIC(NPOIN,TN,TEXP,HPROP,PATMOS,IND_T,LISTIN)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,10)WAQPROCESS
          ELSE
            WRITE(LU,20)WAQPROCESS
          ENDIF
          CALL PLANTE(1)
          STOP
!
      END SELECT
!
10    FORMAT(1X,'SOURCE_WAQ: MODULE WAQ INCONNU : ',I4)
20    FORMAT(1X,'SOURCE_WAQ: UNKNOWN WAQ MODULE : ',I4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
