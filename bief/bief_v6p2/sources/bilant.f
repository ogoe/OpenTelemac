!                    *****************
                     SUBROUTINE BILANT
!                    *****************
!
     &(H,WORK2,WORK3,DT,LT,NIT,INFO,
     & T,AGGLOT,MASSOU,MASTR0,MASTR2,MASTEN,
     & MASTOU,MSK,MASKEL,MESH,
     & FLBOR,NUMLIQ,NFRLIQ,NPTFR,NAMETRAC,FLBORTRA)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MASS BALANCE FOR THE TRACER.
!
!history  J-M HERVOUET (LNHE)     ; C MOULIN (LNH)
!+        10/06/08
!+        V5P9
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
!| AGGLOT         |-->| MASS-LUMPING ON TRACER
!| DT             |-->| TIME-STEP
!| FLBOR          |-->| WATER FLUXES AT BOUNDARIES
!| FLBORTRA       |-->| TRACER FLUXES AT BOUNDARIES
!| H              |-->| DEPTH AT TIME N+1.
!| INFO           |-->| LOGICAL, IF YES, PRINTING INFORMATION ON LISTING
!| LT,NIT         |-->| TIME STEP NUMBER, TOTAL NUMBER OF STEPS.
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASSOU         |<--| MASS OF TRACER BROUGTH BY SOURCE TERM
!| MASTEN         |<--| WATER MASS ENTERED THROUGH BOUNDARIES
!| MASTOU         |<--| WATER MASS CREATED BY SOURCE TERMS
!| MASTR0         |<--| INITIAL TRACER MASS
!| MASTR2         |<--| CURRENT TRACER MASS
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NAMETRAC       |-->| NAMES OF TRACERS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| T              |-->| TRACER AT TIME T(N+1)
!| WORK2          |<->| WORK ARRAY
!| WORK3          |<->| WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BILANT => BILANT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: LT,NIT,NFRLIQ,NPTFR
      INTEGER, INTENT(IN)            :: NUMLIQ(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)   :: DT,MASSOU,AGGLOT
      LOGICAL, INTENT(IN)            :: INFO,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: WORK2,WORK3
      TYPE(BIEF_OBJ), INTENT(IN)     :: H,T,MASKEL,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)     :: FLBORTRA
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      DOUBLE PRECISION, INTENT(INOUT):: MASTR0,MASTR2,MASTEN,MASTOU
      CHARACTER(LEN=32), INTENT(IN)  :: NAMETRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
      INTEGER I,IFRLIQ,IELMT,IELMH
!
      DOUBLE PRECISION ERREUT,PERDUE,FLUXT,MASBOR,RELATI,DENOM,MASTR1
!     300 IS HERE MAXFRO, THE MAXIMUM NUMBER OF LIQUID BOUNDARIES
      DOUBLE PRECISION FLT_BOUND(300)
!
      INTRINSIC ABS,MAX
!
!-----------------------------------------------------------------------
!
      IELMT = T%ELM
      IELMH = H%ELM
!
!-----------------------------------------------------------------------
!
!  COMPATIBLE COMPUTATION OF THE TRACER QUANTITY AT TIME N+1:
!  TAKES MASS-LUMPING INTO ACCOUNT BUT REQUIRES AGGLOC=AGGLOT
!
      IF(LT.NE.0) MASTR1 = MASTR2
!
      CALL VECTOR(WORK2,'=','MASVEC          ',IELMT,
     &            1.D0-AGGLOT,T,T,T,T,T,T,MESH,MSK,MASKEL)
!     H IS GIVEN HERE FOR A DUMMY STRUCTURE
      CALL VECTOR(WORK3,'=','MASBAS          ',IELMT,
     &                 AGGLOT,H,H,H,H,H,H,MESH,MSK,MASKEL)
!
      CALL OS('X=X+YZ  ',X=WORK2,Y=WORK3,Z=T)
!
      MASTR2 = DOTS(WORK2,H)
      IF(NCSIZE.GT.1) MASTR2=P_DSUM(MASTR2)
!
      IF(LT.EQ.0) THEN
        MASTR0 = MASTR2
        MASTR1 = MASTR2
        MASTEN = 0.D0
        MASTOU = 0.D0
      ENDIF
!
!=======================================================================
!
!   COMPUTES THE FLUXES (MISSES THE DIFFUSION FLUX,... INVESTIGATE)
!
!=======================================================================
!
      FLUXT=0.D0
!
      IF(LT.GT.0.AND.NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          FLT_BOUND(IFRLIQ)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
!           NOTE: COULD DEFINE FLUX_BOUNDARIES BETWEEN 0 AND NFRLIQ
            IFRLIQ=NUMLIQ(I)
            IF(IFRLIQ.GT.0) THEN
!             FLBORTRA MUST NOT BE ASSEMBLED IN PARALLEL MODE
              FLT_BOUND(IFRLIQ)=FLT_BOUND(IFRLIQ)+FLBORTRA%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO IFRLIQ=1,NFRLIQ
            FLT_BOUND(IFRLIQ)=P_DSUM(FLT_BOUND(IFRLIQ))
          ENDDO
        ENDIF
        DO IFRLIQ=1,NFRLIQ
          FLUXT=FLUXT+FLT_BOUND(IFRLIQ)
        ENDDO
      ENDIF
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE LIQUID BOUNDARIES
!
      MASTEN = MASTEN - FLUXT * DT
      MASTOU = MASTOU + MASSOU
!
!=======================================================================
!
!     COMPUTES THE TRACER FLUXES THRU THE WALLS (FLUX LAW)
!
!     TEMPORARY, TO BE CODED UP
      MASBOR = 0.D0
!
!=======================================================================
!
!     COMPUTES THE ERROR ON THE MASS FOR THIS TIMESTEP
!
      ERREUT = MASTR1 + MASSOU - MASTR2 - DT*FLUXT
!
!=======================================================================
!
!     PRINTOUTS :
!
      IF(INFO) THEN
!
!-----------------------------------------------------------------------
!
!     PRINTOUTS FOR THE TRACER
!
        WRITE(LU,*)
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '                      BILAN DE QUANTITE DE ',
     &    TRIM(NAMETRAC(1:16)),' (UNITE : ',TRIM(NAMETRAC(17:32)),')'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) '                           BALANCE OF ',
     &    TRIM(NAMETRAC(1:16)),' (UNIT: ',TRIM(NAMETRAC(17:32)),')'
        ENDIF
!
        IF(LT.EQ.0) THEN
!
          IF(LNG.EQ.1) WRITE(LU,1090) MASTR0
          IF(LNG.EQ.2) WRITE(LU,2090) MASTR0
!
        ELSE
!
          IF(LNG.EQ.1) WRITE(LU,1100) MASTR2
          IF(LNG.EQ.2) WRITE(LU,2100) MASTR2
          IF(NFRLIQ.GT.0) THEN
            DO IFRLIQ=1,NFRLIQ
              IF(LNG.EQ.1) WRITE(LU,1110) IFRLIQ,-FLT_BOUND(IFRLIQ)
              IF(LNG.EQ.2) WRITE(LU,2110) IFRLIQ,-FLT_BOUND(IFRLIQ)
            ENDDO
          ENDIF
          IF(ABS(MASSOU).GT.1.D-8) THEN
            IF(LNG.EQ.1) WRITE(LU,1113) MASSOU
            IF(LNG.EQ.2) WRITE(LU,2113) MASSOU
          ENDIF
          DENOM = MAX(MASTR2,ABS(FLUXT*DT))
          IF(DENOM.GT.1.D-8) THEN
            ERREUT = ERREUT / DENOM
            IF(LNG.EQ.1) WRITE(LU,1120) ERREUT
            IF(LNG.EQ.2) WRITE(LU,2120) ERREUT
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1130) ERREUT
            IF(LNG.EQ.2) WRITE(LU,2130) ERREUT
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FINAL MASS BALANCE
!
      IF(LT.EQ.NIT) THEN
!
        WRITE(LU,*)
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '                BILAN FINAL DE QUANTITE DE ',
     &    TRIM(NAMETRAC(1:16)),' (UNITE : ',TRIM(NAMETRAC(17:32)),')'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) '                     FINAL BALANCE OF ',
     &    TRIM(NAMETRAC(1:16)),' (UNIT: ',TRIM(NAMETRAC(17:32)),')'
        ENDIF
!
          PERDUE = MASTR0+MASTEN+
     &             MASBOR+MASTOU-MASTR2
          DENOM = MAX(MASTR0,MASTR2,ABS(MASTEN),ABS(MASTOU))
          IF(DENOM.GT.1.D-8) THEN
            RELATI = PERDUE / DENOM
            IF(LNG.EQ.1) WRITE(LU,1140) RELATI
            IF(LNG.EQ.2) WRITE(LU,2140) RELATI
          ELSE
            RELATI = PERDUE
            IF(LNG.EQ.1) WRITE(LU,1150) RELATI
            IF(LNG.EQ.2) WRITE(LU,2150) RELATI
          ENDIF
          IF(LNG.EQ.1) THEN
            WRITE(LU,1160) MASTR0,MASTR2
            IF(ABS(MASTEN).GT.1.D-8) WRITE(LU,1161) MASTEN
            IF(ABS(MASTOU).GT.1.D-8) WRITE(LU,1164) MASTOU
            WRITE(LU,1165) PERDUE
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,2160) MASTR0,MASTR2
            IF(ABS(MASTEN).GT.1.D-8) WRITE(LU,2161) MASTEN
            IF(ABS(MASTOU).GT.1.D-8) WRITE(LU,2164) MASTOU
            WRITE(LU,2165) PERDUE
          ENDIF
!
       ENDIF
!
!  END OF THE PRINTOUTS :
!
!=======================================================================
!
!  FORMATS :
!
1090  FORMAT(5X,'QUANTITE INITIALE DE TRACEUR :',G16.7)
2090  FORMAT(5X,'INITIAL QUANTITY OF TRACER:',G16.7)
1100  FORMAT(/,5X,'QUANTITE DE TRACEUR :',G16.7)
2100  FORMAT(/,5X,'QUANTITY OF TRACER:',G16.7)
1110  FORMAT(5X,'FRONTIERE ',1I3,' FLUX :           ',G16.7,
     &          ' ( >0 : ENTRANT  <0 : SORTANT )')
1113  FORMAT(5X,'QUANTITE CREEE PAR TERME SOURCE :  ' , G16.7 )
2110  FORMAT(5X,'BOUNDARY ',1I3,' FLUX:         ',G16.7,
     &          ' ( >0 : ENTERING  <0 : EXITING )')
2113  FORMAT(5X,'QUANTITY CREATED BY SOURCE TERM:   ' , G16.7 )
1120  FORMAT(5X,'ERREUR RELATIVE : ',G16.7)
2120  FORMAT(5X,'RELATIVE ERROR: ',G16.7)
1130  FORMAT(5X,'ERREUR ABSOLUE : ',G16.7)
2130  FORMAT(5X,'ABSOLUTE ERROR: ',G16.7)
1140  FORMAT(/,5X,'ERREUR RELATIVE CUMULEE : ',G16.7)
2140  FORMAT(/,5X,'RELATIVE ERROR CUMULATED: ',G16.7)
1150  FORMAT(/,5X,'ERREUR ABSOLUE  CUMULEE: ',G16.7)
2150  FORMAT(/,5X,'ABSOLUTE CUMULATED ERROR: ',G16.7)
1160  FORMAT(/,5X,'QUANTITE INITIALE                 : ',G16.7,
     &       /,5X,'QUANTITE FINALE                   : ',G16.7)
1161  FORMAT(  5X,'QUANTITE ENTREE AUX FRONT. LIQUID.: ',G16.7,
     &            '  ( SI <0 QUANTITE SORTIE )')
1164  FORMAT(  5X,'QUANTITE CREEE PAR TERME SOURCE   : ',G16.7)
1165  FORMAT(  5X,'QUANTITE TOTALE PERDUE            : ',G16.7)
2160  FORMAT(/,5X,'INITIAL QUANTITY                  : ',G16.7,
     &       /,5X,'FINAL QUANTITY                    : ',G16.7)
2161  FORMAT(  5X,'QUANTITY ENTERED THROUGH LIQ. BND.: ',G16.7,
     &            '  ( IF <0 EXIT )')
2164  FORMAT(  5X,'QUANTITY CREATED BY SOURCE TERM   : ',G16.7)
2165  FORMAT(  5X,'TOTAL QUANTITY LOST               : ',G16.7)
!
!=======================================================================
!
      RETURN
      END
