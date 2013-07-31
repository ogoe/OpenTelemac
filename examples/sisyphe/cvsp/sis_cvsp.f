! global variable for recirculation per class 
        MODULE RECIRCMODUL 
!              use declarations_sisyphe :: nsicla 
 
        IMPLICIT NONE 
!              DOUBLE PRECISION, SAVE  :: Q_outCLA(NSICLA) 
              DOUBLE PRECISION, SAVE  :: Q_outCLA(10)=0.0000001 
!
              integer       :: outputcounter = 0
!
        END MODULE RECIRCMODUL
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
c
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-0.3D0,NPOIN)

        DO i=1,NPOIN
        IF (X(i) < 100.0D0) THEN
           ! zr(i)=zf(i)
        END IF
        END DO
C
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
C                         ********************* 
                          SUBROUTINE INIT_COMPO 
C                         ********************* 
C 
     *(NCOUCHES) 
C 
C*********************************************************************** 
C SISYPHE VERSION 6.2 
C
C           Version for the FLUME CASES OF ASTRID BLOM (2003)
C              @ DELFT / DELTARES 
C 
C           prepared by uwe.merkel@uwe-merkel.com
C
C           Choose:  BLOMCASE = 'B1' or A1 / A2 / T5 / T10 
C 
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
C |                |    | AVAIL(NPOIN,10,NSICLA) 
C |    ES          |<-- | THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10) 
C |    NCOUCHES    |--> | NUMBER OF LAYER FOR EACH POINT 
C |    NSICLA      |--> | NUMBER OF SIZE-CLASSES OF BED MATERIAL 
C |                |    | (LESS THAN 10) 
C |    NPOIN       |--> | NUMBER OF NODES 
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
      INTEGER LNG,LU,kk
      CHARACTER*2 BLOMCASE
      COMMON/INFO/LNG,LU 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
C                                       NPOIN 
      INTEGER, INTENT (INOUT)::NCOUCHES(*) 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
      INTEGER I, J, K 
C 
C----------------------------------------------------------------------- 
C 
      DO J=1,NPOIN 
 
 
            NCOUCHES(J) = NOMBLAY
            NLAYER%I(J) = NOMBLAY

            ES(J,1) = 0.03D0 

            ES(J,2) = 0.01D0
            ES(J,3) = 0.01D0
            ES(J,4) = 0.01D0
            ES(J,5) = 0.01D0
            ES(J,6) = 0.01D0
            ES(J,7) = 0.01D0
            ES(J,8) = 0.01D0

            ES(J,9) = (ZF%R(J) -ZR%R(J)-(NOMBLAY-2)*0.01D0 - ES(J,1))

          BLOMCASE = 'B1'

 
          DO I = 1, NSICLA 
          DO K = 1, NCOUCHES(J) 
C        Nur für BLOOM B CASES 
            IF (BLOMCASE.EQ.'A1') THEN 
               AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'A2') THEN 
               AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'T5') THEN 
               AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'T10') THEN 
               AVAIL(J,K,I) = AVA0(I) 
            ELSEIF ((K.EQ.1)) THEN 
               AVAIL(J,K,I) = AVA0(I) 
            ELSE 
               AVAIL(J,K,1) = 0.998D0 
               AVAIL(J,K,2) = 0.001D0 
               AVAIL(J,K,3) = 0.001D0 
            ENDIF 
 
          ENDDO 
        ENDDO 
 
 
      ENDDO 
C 
C----------------------------------------------------------------------- 
C  !CONVERT TO CVSM in CASE VSMTYPE = 1
C  No user edit necessary
C-----------------------------------------------------------------------



      IF (VSMTYPE.eq.1) THEN

        !Folder for Debugging Outputs
!        CALL system('mkdir ./ERR')
!        CALL system('cp ../../vsp2svg.exe ./ERR')
!        CALL system('cp ../../*.css ./ERR')
!        CALL system('cp ../../*.svg ./ERR')


        !Folder for Hirano Profile Outputs
!        CALL system('mkdir ./LAY')
!        CALL system('cp ../../vsp2svg.exe ./LAY')
!        CALL system('cp ../../*.css ./LAY')
!        CALL system('cp ../../*.svg ./LAY')


         do kk = 1,100
         if (CVSMOUTPUT(kk).gt.0) THEN
                call LAYERS_P('./LAY/VSP_',  CVSMOUTPUT(kk))
         endif
         enddo

C

        !Folder for CVSP Profile Outputs
!        CALL system('mkdir ./VSP')
!        CALL system('cp ../../vsp2svg.exe ./VSP')
!       CALL system('cp ../../*.css ./VSP')
!        CALL system('cp ../../*.svg ./VSP')

        !Copy TECPLOT Layouts to the Targetfolder
!        CALL system('cp ../*.lay .')


        !call CVSP_INIT()
         call CVSP_INIT_from_LAYERS()

        !output to Selafin file
         if (CVSM_OUT_FULL) call CVSP_OUTPUT_INIT()
            if (CVSM_OUT_FULL) call CVSP_WRITE_PROFILE()

         do kk = 1,100
         if (CVSMOUTPUT(kk).gt.0) THEN
                call CVSP_P('./','V_', CVSMOUTPUT(kk))
         endif
         enddo

      ENDIF ! END CVSM

 
      RETURN 
      END SUBROUTINE INIT_COMPO
!                    *****************
                     SUBROUTINE CONLIT_UWE
!                    *****************
!
     &(NBOR,AT)
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!+               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!+                INPUT FILE).
!+
!+
!+            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!+                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!+                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  C. MACHET
!+        07/06/2002
!+
!+
!
!history  CV
!+        19/06/2008
!+        V5P9
!+   TAKES INTO ACCOUNT CBOR_VASE AND CBOR_SABLE
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
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!| AT             |-->| TEMPS (s)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
      USE RECIRCMODUL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NBOR(NPTFR)
! CV: 12/06...
      DOUBLE PRECISION, INTENT(IN) :: AT
      DOUBLE PRECISION, EXTERNAL:: CGL
!...CV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,KK,IFRLIQ,IRANK, NSIC
      DOUBLE PRECISION Inflowwidth
!
!-----------------------------------------------------------------------
!


!     CALCULATING THE WITH OF THE INFLOW, OUR SEDIMENT INPUT HAS TO BE DISTRIBUTED OVER IT
        Inflowwidth = 0.D0
      DO K=1,NPTFR
              KK = MESH%KP1BOR%I(K)
          if (k.ne.kk) then
              IF((NUMLIQ%I(K).EQ.2).and.(NUMLIQ%I(KK).EQ.2)) THEN ! FLUX for one class
                IF(LIEBOR%I(K).EQ.4.AND.LIHBOR%I(K).EQ.4) THEN 
                Inflowwidth = Inflowwidth + MESH%LGSEG%R(k) 
!                print*,'Inflowwidth',k,mesh%lgseg%r(k),Inflowwidth
!     &           ,MESH%X%R(MESH%NBOR%I(K)),MESH%X%R(MESH%NBOR%I(KK)) 
                ENDIF 
              Endif ! FLUX for one class
          endif
      ENDDO

                !print*,'Inflowwidth: ',Inflowwidth



      DO  K=1,NPTFR
!
        I = NBOR(K)
!
!       HERE KADH (WALL WITH NO SLIP CONDITION) IS CHANGED INTO KLOG (WALL)
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
!
!       DIRICHLET CONDITIONS
!       EITHER ON EVOLUTION OR ON SOLID DISCHARGE
!
!       EXAMPLE 1: IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
!
!       QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!       NODE K , CLASS OF SEDIMENT J, EXCLUDING VOIDS
!
!
       ! RECIRC INPUT ....
       If (NUMLIQ%I(K).eq.2) then
            LIEBOR%I(K)=KSORT
            LIQBOR%I(K)=KENT
         Print *, 'QBOR%ADR(NSIC)%P%R(K), Q_outCLA(NSIC),Inflowwidth,LT'
       Do NSIC = 1,NSICLA
            QBOR%ADR(NSIC)%P%R(K)= (Q_outCLA(NSIC) / Inflowwidth)
         Print *, QBOR%ADR(NSIC)%P%R(K), Q_outCLA(NSIC), Inflowwidth, LT
       ENDDO
       Endif
!
!       EXAMPLE 2: IMPOSED BED EVOLUTON
!
!       LIEBOR%I(K)=KENT
!       (LIQBOR%I(K)=KSORT IS DONE IN SISYPHE.F)
!       IF(LIEBOR%I(K).EQ.KENT) THEN
!         EBOR%ADR(1)%P%R(K)=1.D-4
!         EBOR%ADR(2)%P%R(K)=1.D-4.....
!       ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
!-----------------------------------------------------------------------
!
      IF(SUSP) THEN
!
        DO K=1,NPTFR
!
!         SO FAR LICBOR=LIEBOR (WITH KADH CHANGED INTO KLOG, SEE ABOVE,
!                               BUT CAN BE CHANGED)
!
          LICBOR%I(K) = LIEBOR%I(K)
!
!         ENTRANCE : IMPOSED CONCENTRATION
!         -------------------------------
!
!         NOTE JMH: KSORT MUST BE TREATED ALSO BECAUSE SUBROUTINE DIFFIN
!                   MAY CHANGE A KSORT INTO KENT, DEPENDING OF FLOW
!
          IFRLIQ=NUMLIQ%I(K)
          IF(LIEBOR%I(K).EQ.KENT.OR.LIEBOR%I(K).EQ.KSORT) THEN
            DO I=1,NSICLA
               IRANK=I+(IFRLIQ-1)*NSICLA
               CBOR%ADR(I)%P%R(K) = CBOR_CLASSE(IRANK)
            ENDDO
          ENDIF
!
! CV 12/06 READING BOUNDARY CONDITION FILE
!
          IF(LICBOR%I(K).EQ.KENT.AND.
     *               SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
!
             IF(IFRLIQ.GT.0) THEN
               DO I=1,NSICLA
                  CBOR%ADR(I)%P%R(K) = CGL(IFRLIQ,AT)/XMVS
               ENDDO
             ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
