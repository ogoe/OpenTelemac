!                    ************************
                     SUBROUTINE BILAN_SISYPHE
!                    ************************
!
     &( E      , ESOMT  , QSX    , QSY    , MESH   , MSK    , MASKEL ,
     &  T1     , T2     , S      , IELMU  , VCUMU  , DT     , NPTFR  ,
     &  INFO   , ZFCL_C , QSCLXC , QSCLYC , NSICLA ,
     &  VOLTOT , DZF_GF , MASS_GF, LGRAFED, NUMLIQ , NFRLIQ)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| DZF_GF         |---| A SUPPRIMER
!| E              |-->| BED EVOLUTION AT A GIVEN TIME STEP
!| ESOMT          |-->| CUMULATED BED EVOLUTION
!| IELMU          |-->| NUMER OF ELEMENTS 
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| LGRAFED        |---| A SUPPRIMER
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASS_GF        |---|A SUPPRIMER
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPTFR          |-->| NUMBER OF BOUNDARY NODES
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| QSCLXC         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYC         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSX            |<->| BEDLOAD TRANSPORT RATE X-DIRECTION
!| QSY            |<->| BEDLOAD TRANSPORT RATE Y-DIRECTION
!| S              |-->| VOID STRUCTURE
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| VCUMU          |<->| VOLUME OF SEDIMENT ENTERING THE DOMAIN
!| VOLTOT         |-->| VOLUME TOTAL PER CLASS OF SEDIMENT 
!| ZFCL_C         |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NPTFR,NFRLIQ,IELMU,NSICLA
      INTEGER, INTENT(IN)          :: NUMLIQ(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: DT
      LOGICAL, INTENT(IN)          :: MSK, INFO
!
      LOGICAL,          INTENT(IN)    :: LGRAFED
      DOUBLE PRECISION, INTENT(INOUT) :: MASS_GF,VCUMU
      DOUBLE PRECISION, INTENT(IN)    :: VOLTOT(10)
!
!-----------------------------------------------------------------------
!
!     VECTOR STRUCTURES
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,S,ZFCL_C,QSCLXC,QSCLYC
      TYPE(BIEF_OBJ), INTENT(IN)    :: E,ESOMT,QSX,QSY,DZF_GF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2
!
!-----------------------------------------------------------------------
!
!     MESH STRUCTURES
!
      TYPE(BIEF_MESH) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IFRLIQ,IPTFR
      DOUBLE PRECISION RMASSE,RCUMU,RMASCLA(10)
      DOUBLE PRECISION VCUMUCLA(10),MASST,FLUXT
!     300 STANDS FOR MAXFRO, THE MAXIMUM NUMBER OF LIQUID BOUNDARIES
      DOUBLE PRECISION FLT_BOUND(300)
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE EVOLUTION (E)
!
      CALL VECTOR(T1,'=','MASVEC          ',IELMU,
     &            1.D0,E,S,S,S,S,S,MESH,MSK,MASKEL)
      RMASSE = BIEF_SUM(T1)
      IF(NCSIZE.GT.1) RMASSE = P_DSUM(RMASSE)
!
!=======================================================================
!
!     COMPUTES THE INTEGRAL OF EVOLUTION (ESOMT)
!
      CALL VECTOR(T1,'=','MASVEC          ',IELMU,
     &            1.D0,ESOMT,S,S,S,S,S,MESH,MSK,MASKEL)
      RCUMU = BIEF_SUM(T1)
      IF(NCSIZE.GT.1) RCUMU = P_DSUM(RCUMU)
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE BOUNDARIES
!
      CALL VECTOR(T1,'=','FLUBOR          ',IELBOR(IELMU,1),
     &            1.D0,S,S,S,QSX,QSY,S,MESH,MSK,MASKEL)
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
      VCUMU = VCUMU - FLUXT*DT
!
!     BALANCE IN EXTENDED GRANULOMETRY
!
      IF(NSICLA.GT.1) THEN
!
        DO I=1,NSICLA
!
!       COMPUTES THE EVOLUTION PER CLASS
!
        CALL VECTOR(T1,'=','MASVEC          ',IELMU,
     &              1.D0,ZFCL_C%ADR(I)%P,S,S,S,S,S,
     &              MESH,MSK,MASKEL)
        RMASCLA(I) = BIEF_SUM(T1)
        IF(NCSIZE.GT.1) RMASCLA(I) = P_DSUM(RMASCLA(I))
!
!       COMPUTES THE FREE FLUXES BY CLASS
!
        CALL VECTOR(T1,'=','FLUBOR          ',IELBOR(IELMU,1),
     &              1.D0,S,S,S,QSCLXC%ADR(I)%P,QSCLYC%ADR(I)%P,
     &              S,MESH,MSK,MASKEL)
!
        FLUXT=0.D0
        IF(NFRLIQ.GT.0) THEN
          IF(NPTFR.GT.0) THEN
            DO IPTFR=1,NPTFR
              IFRLIQ=NUMLIQ(IPTFR)
              IF(IFRLIQ.GT.0) THEN
                FLUXT=FLUXT+T1%R(IPTFR)
              ENDIF
            ENDDO
          ENDIF
          IF(NCSIZE.GT.1) FLUXT=P_DSUM(FLUXT)
        ENDIF
!
        VCUMUCLA(I) = - FLUXT*DT
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
!  WRITES OUT THE BALANCE
!
      IF (INFO) THEN
!
          WRITE(LU,*)
          IF(LNG.EQ.1) THEN
            WRITE(LU,1000)
            WRITE(LU,1010) RMASSE
            IF(NFRLIQ.GT.0) THEN
              DO IFRLIQ=1,NFRLIQ
                WRITE(LU,1110) IFRLIQ,-FLT_BOUND(IFRLIQ)
              ENDDO
            ENDIF
            WRITE(LU,1030) RCUMU
            WRITE(LU,1031) VCUMU
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,2000)
            WRITE(LU,2010) RMASSE
            IF(NFRLIQ.GT.0) THEN
              DO IFRLIQ=1,NFRLIQ
                WRITE(LU,2110) IFRLIQ,-FLT_BOUND(IFRLIQ)
              ENDDO
            ENDIF
            WRITE(LU,2030) RCUMU
            WRITE(LU,2031) VCUMU
          ENDIF
         IF (NSICLA>1) THEN
          DO I=1,NSICLA
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) '     BILAN POUR LA CLASSE DE SEDIMENT :',I
             WRITE(LU,*) '     VOLUME TOTAL DE LA CLASSE :',VOLTOT(I)
             WRITE(LU,3011) RMASCLA(I)
             WRITE(LU,3032) VCUMUCLA(I)
           ELSEIF(LNG.EQ.2) THEN
             WRITE(LU,*) '     MASS BALANCE FOR SEDIMENT CLASS :',I
             WRITE(LU,*) '     TOTAL VOLUME:',VOLTOT(I)
             WRITE(LU,3010) RMASCLA(I)
             WRITE(LU,3031) VCUMUCLA(I)
           ENDIF
          ENDDO
         ENDIF
         IF (LGRAFED) THEN
            IF (LNG.EQ.1) THEN
               WRITE(LU, 4000) MASST
               WRITE(LU, 4010) MASS_GF
            ENDIF
            IF (LNG.EQ.2) THEN
               WRITE(LU, 4001) MASST
               WRITE(LU, 4011) MASS_GF
            ENDIF
         ENDIF
      ENDIF
!
1000  FORMAT(1X,'BILAN DE MASSE : ')
1010  FORMAT(1X,'SOMME DES EVOLUTIONS : ',G16.7)
1020  FORMAT(1X,'FLUX IMPOSE          : ', G16.7,' M3/S'
     &         ,'  ( >0 : ENTRANT  <0 : SORTANT )')
1021  FORMAT(1X,'FLUX LIBRE           : ', G16.7,' M3/S'
     &         ,'  ( >0 : ENTRANT  <0 : SORTANT )')
1030  FORMAT(1X,'SOMME DES EVOLUTIONS CUMULEES : ',G16.7)
1031  FORMAT(1X,'VOLUME ENTRE AUX FRONTIERES   : ',G16.7,' M3'
     &         ,'  ( SI <0 VOLUME SORTI )')
1110  FORMAT(1X,'FRONTIERE ',1I3,' FLUX EN CHARRIAGE = ',G16.7,
     &          ' ( >0 : ENTRANT  <0 : SORTANT )')
2000  FORMAT(1X,'MASS-BALANCE          : ')
2010  FORMAT(1X,'SUM OF THE EVOLUTIONS : ',G16.7)
2020  FORMAT(1X,'PRESCRIBED FLOW       : ',G16.7,' M3/S'
     &         ,'  ( >0 : ENTERING  <0 : EXITING )')
2021  FORMAT(1X,'FREE FLOW             : ',G16.7,' M3/S'
     &         ,'  ( >0 : ENTERING  <0 : EXITING )')
2030  FORMAT(1X,'SUM OF THE CUMULATED EVOLUTIONS : ',G16.7)
2031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN  : ',G16.7,' M3'
     &         ,'  ( IF <0 EXIT )')
2110  FORMAT(1X,'BOUNDARY ',1I3,' BEDLOAD FLUX = ',G16.7,
     &          ' ( >0 : ENTERING  <0 : EXITING )')
3010  FORMAT(1X,'SUM OF THE EVOLUTIONS FOR THIS CLASS: ',G16.7)
3031  FORMAT(1X,'VOLUME THAT ENTERED THE DOMAIN FOR THIS CLASS: '
     &       ,G16.7,' M3')
3011  FORMAT(1X,'SOMME DES EVOLUTIONS POUR CETTE CLASSE : ',G16.7)
3032  FORMAT(1X,'VOLUME ENTRE DANS LE DOMAINE POUR CETTE CLASSE : '
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
