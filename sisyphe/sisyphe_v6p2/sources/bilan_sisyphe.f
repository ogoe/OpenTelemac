!                    ************************
                     SUBROUTINE BILAN_SISYPHE
!                    ************************
!
     &(E,ESOMT,MESH,MSK,MASKEL,T1,T2,S,IELMU,VCUMU,DT,NPTFR,
     & INFO,ZFCL_C,ZFCL_S,
     & QSCLXC,QSCLYC,NSICLA,VOLTOT,DZF_GF,MASS_GF,LGRAFED,
     & NUMLIQ,NFRLIQ,FLBCLA,VF,LT,NIT,NPOIN,VOLU2D,CSF_SABLE,MASDEP,
     & MASDEPT,SUSP)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE MASS BALANCE.
!
!note     T2 IS NOT USED
!
!history  CMGDL
!+
!+        V5P9
!+   CHANGED FOR GRADED SEDIMENT
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!history  J-M HERVOUET (EDF-LNHE)
!+        14/02/2012
!+        V6P2
!+  NSICLM and MAXFRO used instead of 10 and 300. New and compatible
!+  computation: flux given as argument, mass computed differently,
!+  and coefficient CSF_SABLE. 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CSF_SABLE      |-->| 1-POROSITY
!| DT             |-->| TIME STEP
!| DZF_GF         |---| A SUPPRIMER
!| E              |-->| BED EVOLUTION AT A GIVEN TIME STEP
!| ESOMT          |-->| CUMULATED BED EVOLUTION
!| FLBCLA         |-->| BLOCK OF FLUXES AT BOUNDARY FOR EACH CLASS
!| IELMU          |-->| NUMER OF ELEMENTS 
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| LGRAFED        |---| A SUPPRIMER
!| LT             |-->| CURRENT TIME STEP
!| MASDEP         |-->| VOLUME DEPOSITED ON THE BOTTOM FOR EACH CLASS
!|                |   | FROM THE BEGINNING
!| MASDEPT        |-->| VOLUME DEPOSITED ON THE BOTTOM FOR EACH CLASS
!|                |   | FOR THIS TIME STEP
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASS_GF        |---|A SUPPRIMER
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| QSCLXC         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYC         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| S              |-->| VOID STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| VCUMU          |<->| VOLUME OF SEDIMENT ENTERING THE DOMAIN
!| VF             |-->| IF YES : FINITE VOLUMES IF NO : FINITE ELEMENTS
!| VOLTOT         |-->| VOLUME TOTAL PER CLASS OF SEDIMENT 
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS (NOT ASSEMBLED IN //)
!| ZFCL_C         |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!| ZFCL_S         |<->| SUSPENDED LOAD EVOLUTION FOR EACH SEDIMENT CLASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NSICLM,MAXFRO
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPTFR,NFRLIQ,IELMU,NSICLA,LT,NIT
      INTEGER, INTENT(IN)          :: NPOIN,NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: DT
      LOGICAL, INTENT(IN)          :: MSK,INFO,VF,SUSP
!
      LOGICAL, INTENT(IN)          :: LGRAFED
      DOUBLE PRECISION, INTENT(INOUT) :: MASS_GF,VCUMU
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,VOLTOT(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASDEPT(NSICLA)
!
!-----------------------------------------------------------------------
!
!     VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,S,ZFCL_C,QSCLXC,QSCLYC
      TYPE(BIEF_OBJ), INTENT(IN)    :: E,ESOMT,DZF_GF,VOLU2D,ZFCL_S
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,FLBCLA
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURES
!
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IFRLIQ,IPTFR,ICLA
      DOUBLE PRECISION RMASSE,RCUMU,RMASCLA(NSICLM)
      DOUBLE PRECISION VCUMUCLA(NSICLM),MASST,FLUXT,FLUXTCLA,VOLDEP
      DOUBLE PRECISION FLT_BOUND(MAXFRO),VOLDEPC
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE EVOLUTION (E)
!
      RMASSE=0.D0
      DO I=1,NPOIN
        RMASSE=RMASSE+E%R(I)*VOLU2D%R(I)
      ENDDO
      IF(NCSIZE.GT.1) RMASSE = P_DSUM(RMASSE)
!
!=======================================================================
!
!     COMPUTES THE INTEGRAL OF EVOLUTION AT THE END (ESOMT)
!
      IF(LT.EQ.NIT) THEN
        RCUMU=0.D0
        DO I=1,NPOIN
          RCUMU=RCUMU+ESOMT%R(I)*VOLU2D%R(I)
        ENDDO
        IF(NCSIZE.GT.1) RCUMU = P_DSUM(RCUMU)
      ENDIF
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE BOUNDARIES
!
      CALL OS('X=Y     ',X=T1,Y=FLBCLA%ADR(1)%P)
      IF(NSICLA.GT.1) THEN
        DO I=2,NSICLA
          CALL OS('X=X+Y   ',X=T1,Y=FLBCLA%ADR(I)%P)
        ENDDO
      ENDIF
!
      FLUXT=0.D0
!
      IF(NFRLIQ.GT.0) THEN
        DO IFRLIQ=1,NFRLIQ
          FLT_BOUND(IFRLIQ)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO IPTFR=1,NPTFR
            IFRLIQ=NUMLIQ(IPTFR)
            IF(IFRLIQ.GT.0) THEN
              FLT_BOUND(IFRLIQ)=FLT_BOUND(IFRLIQ)+T1%R(IPTFR)
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
      VCUMU = VCUMU - FLUXT*DT/CSF_SABLE
!
!     BALANCE IN EXTENDED GRANULOMETRY
!
      IF(NSICLA.GT.1) THEN
!
        DO ICLA=1,NSICLA
!
!       COMPUTES THE EVOLUTION PER CLASS
!
        RMASCLA(ICLA)=0.D0
        IF(SUSP) THEN
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +( ZFCL_C%ADR(ICLA)%P%R(I)
     &                     +ZFCL_S%ADR(ICLA)%P%R(I) )*VOLU2D%R(I)
          ENDDO
        ELSE
          DO I=1,NPOIN
            RMASCLA(ICLA)=RMASCLA(ICLA)
     &                   +ZFCL_C%ADR(ICLA)%P%R(I)*VOLU2D%R(I)
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) RMASCLA(ICLA) = P_DSUM(RMASCLA(ICLA))
!
!       COMPUTES THE FREE FLUXES BY CLASS
!
        FLUXTCLA=0.D0
        IF(NFRLIQ.GT.0) THEN
          IF(NPTFR.GT.0) THEN
            DO IPTFR=1,NPTFR
              IFRLIQ=NUMLIQ(IPTFR)
              IF(IFRLIQ.GT.0) THEN
                FLUXTCLA=FLUXTCLA+FLBCLA%ADR(ICLA)%P%R(IPTFR)
              ENDIF
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) FLUXTCLA=P_DSUM(FLUXTCLA)
        ENDIF
!
        VCUMUCLA(ICLA) = - FLUXTCLA*DT/CSF_SABLE
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
!     GRAIN-FEEDING
!     IF (LGRAFED) THEN
!        CALL VECTOR(T1,'=','MASVEC          ',IELMU,
!    &               1.D0,DZF_GF,S,S,S,S,S,MESH,MSK,MASKEL)
!        MASST = BIEF_SUM(T1)
!        IF(NCSIZE.GT.1) MASST = P_DSUM(MASST)
!        MASS_GF = MASS_GF + MASST
!     ENDIF
!     IF(DREDGESIM) ...   ?????
!
      VOLDEPC=0.D0
      IF(SUSP) THEN
        DO I=1,NSICLA
          VOLDEPC=VOLDEPC+MASDEPT(I)
        ENDDO
        VOLDEPC=VOLDEPC/CSF_SABLE
      ENDIF
!
!     WRITES OUT THE BALANCE
!
      IF(INFO) THEN
!
!       GLOBAL BALANCE
!
        WRITE(LU,*)
        IF(LNG.EQ.1) THEN
          WRITE(LU,1000)
          WRITE(LU,1010) RMASSE
          IF(NFRLIQ.GT.0) THEN
            DO IFRLIQ=1,NFRLIQ
              WRITE(LU,1110) IFRLIQ,-FLT_BOUND(IFRLIQ)
            ENDDO
            WRITE(LU,1111) -FLUXT/CSF_SABLE
            IF(SUSP) WRITE(LU,1112) VOLDEPC
          ENDIF
          WRITE(LU,1033) RMASSE+DT*FLUXT/CSF_SABLE-VOLDEPC
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,2000)
          WRITE(LU,2010) RMASSE
          IF(NFRLIQ.GT.0) THEN
            DO IFRLIQ=1,NFRLIQ
              WRITE(LU,2110) IFRLIQ,-FLT_BOUND(IFRLIQ)
            ENDDO
            WRITE(LU,2111) -FLUXT/CSF_SABLE
            IF(SUSP) WRITE(LU,2112) VOLDEPC
          ENDIF
          WRITE(LU,2033) RMASSE+DT*FLUXT/CSF_SABLE-VOLDEPC
        ENDIF
!
!       BALANCE PER CLASS
!
        IF(NSICLA.GT.1) THEN
          DO I=1,NSICLA
            WRITE(LU,*)
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'BILAN POUR LA CLASSE DE SEDIMENT :',I
              WRITE(LU,*) 'VOLUME TOTAL DE LA CLASSE :',VOLTOT(I)
              WRITE(LU,3011) RMASCLA(I)
              WRITE(LU,3032) VCUMUCLA(I)
              IF(SUSP) THEN
                WRITE(LU,3033) MASDEPT(I)/CSF_SABLE
                WRITE(LU,1033) RMASCLA(I)-VCUMUCLA(I)
     &                                   -MASDEPT(I)/CSF_SABLE
              ELSE
                WRITE(LU,1033) RMASCLA(I)-VCUMUCLA(I)
              ENDIF
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*) 'MASS BALANCE FOR SEDIMENT CLASS :',I
              WRITE(LU,*) 'TOTAL VOLUME:',VOLTOT(I)
              WRITE(LU,3010) RMASCLA(I)
              WRITE(LU,3031) VCUMUCLA(I)
              IF(SUSP) THEN
                WRITE(LU,3034) MASDEPT(I)/CSF_SABLE
                WRITE(LU,1033) RMASCLA(I)-VCUMUCLA(I)
     &                                   -MASDEPT(I)/CSF_SABLE
              ELSE
                WRITE(LU,2033) RMASCLA(I)-VCUMUCLA(I)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!
!       FINAL GLOBAL BALANCE
!
        IF(LT.EQ.NIT) THEN
          VOLDEP=0.D0
          DO I=1,NSICLA
            VOLDEP=VOLDEP+MASDEP(I)
          ENDDO
          VOLDEP=VOLDEP/CSF_SABLE
          WRITE(LU,*)
          IF(LNG.EQ.1) THEN
            WRITE(LU,1030) RCUMU
            WRITE(LU,1031) VCUMU
            IF(SUSP) WRITE(LU,1032) VOLDEP
            WRITE(LU,1033) RCUMU-VCUMU-VOLDEP
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,2030) RCUMU
            WRITE(LU,2031) VCUMU
            IF(SUSP) WRITE(LU,2032) VOLDEP
            WRITE(LU,2033) RCUMU-VCUMU-VOLDEP
          ENDIF
        ENDIF
!
!       IF(LGRAFED) THEN
!         IF(LNG.EQ.1) THEN
!           WRITE(LU, 4000) MASST
!           WRITE(LU, 4010) MASS_GF
!         ENDIF
!         IF(LNG.EQ.2) THEN
!           WRITE(LU, 4001) MASST
!           WRITE(LU, 4011) MASS_GF
!         ENDIF
!       ENDIF
!
      ENDIF
!
1000  FORMAT(1X,'BILAN DE MASSE (EN VOLUME, VIDES INCLUS) : ')
1010  FORMAT(1X,'SOMME DES EVOLUTIONS : ',G16.7,' M3')
1020  FORMAT(1X,'FLUX IMPOSE          : ', G16.7,' M3/S'
     &         ,'  ( M3/S  >0 = ENTRANT )')
1021  FORMAT(1X,'FLUX LIBRE           : ', G16.7,' M3/S'
     &         ,'  ( M3/S  >0 = ENTRANT )')
1030  FORMAT(1X,'SOMME DES EVOLUTIONS CUMULEES : ',G16.7)
1031  FORMAT(1X,'VOLUME ENTRE AUX FRONTIERES   : ',G16.7,' M3'
     &         ,'  ( SI <0 VOLUME SORTI )')
1032  FORMAT(1X,'VOLUME DEPOSE SUR LE FOND     : ',G16.7,' M3'
     &         ,'  ( SI <0 VOLUME ERODE )')
1033  FORMAT(1X,'VOLUME PERDU                  : ',G16.7,' M3'
     &         ,'  ( SI <0 VOLUME SORTI )')
1110  FORMAT(1X,'FRONTIERE ',1I3,' FLUX EN CHARRIAGE = ',G16.7,
     &          ' ( M3/S  >0 = ENTRANT )')
1111  FORMAT(1X,'TOTAL         FLUX EN CHARRIAGE = ',G16.7,
     &          ' ( M3/S  >0 = ENTRANT )')
1112  FORMAT(1X,'DEPOT DE SUSPENSION SUR LE FOND = ',G16.7,
     &          ' ( M3/S )')
2000  FORMAT(1X,'MASS-BALANCE (IN VOLUME, INCLUDING VOID): ')
2010  FORMAT(1X,'SUM OF THE EVOLUTIONS : ',G16.7,' M3')
2020  FORMAT(1X,'PRESCRIBED FLOW       : ',G16.7,' M3/S'
     &         ,'  ( M3/S  >0 = ENTERING )')
2021  FORMAT(1X,'FREE FLOW             : ',G16.7,' M3/S'
     &         ,'  ( M3/S  >0 = ENTERING )')
2030  FORMAT(1X,'SUM OF THE CUMULATED EVOLUTIONS : ',G16.7)
2031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN  : ',G16.7,' M3'
     &         ,'  ( IF <0 EXIT )')
2032  FORMAT(1X,'VOLUME DEPOSITED ON THE BOTTOM  : ',G16.7,' M3'
     &         ,'  ( IF <0 ERODED )')
2033  FORMAT(1X,'LOST VOLUME                     : ',G16.7,' M3'
     &         ,'  ( IF <0 EXIT )')
2110  FORMAT(1X,'BOUNDARY ',1I3,' BEDLOAD FLUX = ',G16.7,
     &          '  ( M3/S  >0 = ENTERING )')
2111  FORMAT(1X,'TOTAL        BEDLOAD FLUX = ',G16.7,
     &          '  ( M3/S  >0 = ENTERING )')
2112  FORMAT(1X,'DEPOSIT ON BOTTOM         = ',G16.7,
     &          '  ( M3/S )')
3010  FORMAT(1X,'SUM OF THE EVOLUTIONS FOR THIS CLASS: ',G16.7)
3031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN FOR THIS CLASS: '
     &       ,G16.7,' M3')
3034  FORMAT(1X,'VOLUME DEPOSITED ON BOTTOM FOR THIS CLASS:     '
     &       ,G16.7,' M3')
3011  FORMAT(1X,'SOMME DES EVOLUTIONS POUR CETTE CLASSE : ',G16.7)
3032  FORMAT(1X,'VOLUME ENTRE DANS LE DOMAINE POUR CETTE CLASSE : '
     &       ,G16.7,' M3')
3033  FORMAT(1X,'VOLUME DEPOSE SUR LE FOND POUR CETTE CLASSE    : '
     &       ,G16.7,' M3')
4000  FORMAT(1X,'GRAIN-FEEDING A CET INSTANT       : ',G16.7)
4010  FORMAT(1X,'GRAIN-FEEDING JUSQU''A MAINTENANT : ',G16.7)
4001  FORMAT(1X,'GRAIN-FEEDING THIS MOMENT : ',G16.7)
4011  FORMAT(1X,'GRAIN-FEEDING UNTIL NOW   : ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
