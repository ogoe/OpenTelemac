! Changement de l'appel a init_compo_coh: le nombre de couches est fixe 
! Test pour assurer que la somme des couches = ZF-ZR
! Calcul du bilan de masse initial
!                    *********************
                     SUBROUTINE INIT_MIXTE
!                    *********************
!
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY,NOMBLAY,CONC_VASE,
     & MS_SABLE,MS_VASE,ZF,ZR,AVA0,CONC,NLAYER,DEBU)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief
!
!history
!+
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+ 
!history  C.VILLARET (EDF-LNHE)
!+        22/08/2012
!+        V6P2
!+  Changing the calling to init_compo_coh: the number of layers is fixed   
!+  Testing SUM(layers) = ZF-ZR
!+  Compute the initial mass balance  
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA0           |-->| VOLUME PERCENT 
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NOMBLAY        |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| XMVS           |-->| WATER DENSITY 
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!| CONC           |<->| CONC OF EACH BED LAYER
!| NLAYER         |<->| NUMBER OB LAYERS 
!| DEBU           |-->| FLAG, FOR PREVIOUS SEDIMENTOLOGICAL FILE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_MIXTE=> INIT_MIXTE
      USE DECLARATIONS_SISYPHE, ONLY :MASVT,MASV0,T1,BILMA,VOLU2D
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPOIN,NSICLA,NOMBLAY
      TYPE (BIEF_OBJ),  INTENT(INOUT)  :: NLAYER
      DOUBLE PRECISION, INTENT(IN)     :: XMVS
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)     :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
      LOGICAL, INTENT (IN)             :: DEBU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER I,J,K
      DOUBLE PRECISION HAUTSED,TEST1
! Ajout CV
      INTEGER NK
      DOUBLE PRECISION DIFF,EST
      DOUBLE PRECISION, EXTERNAL :: P_DSUM
!
!-----------------------------------------------------------------------
!
!*******INITIAL SEDIMENT COMPOSITION IS IDENTICAL AT EACH NODE
! DEFAULT INITIALISATION: ALL LAYERS ARE EMPTY EXCEPT BED LAYER
! OTHERWISE SET THICKNESS OF THE MUD LAYERS IN EPAI_VASE(I= 1, NCOUCH_TASS-1)
! 
!CV V6P2 ..

!  INITIALISATION OF ES : THICKNESS OF EACH LAYERS
!  INIT_COMPO_COH : composition of the sediment bed : thickness of layers 
!                  and concentrations The number of sediment bed layers is fixed
 
       	IF(.NOT.DEBU) THEN
!    
          CALL INIT_COMPO_COH(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,
     *       NSICLA,AVAIL,AVA0)
!
! Recalcul des epaisseurs pour satisfaire : Sum (ES)=ZF-ZR
! 
         DO J=1,NPOIN 
!
           ELAY(J)=ZF(J)-ZR(J)
!
!
!       THE HEIGHT OF SEDIMENT (SUM OF ES) MUST BE EQUAL TO ZF-ZR
!       IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
!       IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
!
            HAUTSED = 0.D0
!
            DO K=1,NOMBLAY
!	    
              IF(HAUTSED + ES(J,K) .GE. ELAY(J)) THEN
                ES(J,K) = ELAY(J) -  HAUTSED
                NK=K
                HAUTSED = HAUTSED + ES(J,K)
                GOTO 144
              ENDIF
              HAUTSED = HAUTSED + ES(J,K)
!	      
           ENDDO
!	   
144        CONTINUE
!
!!       FOR CLEAN OUTPUTS
!
           IF(NK.LT.NOMBLAY) THEN
             DO K=NK+1,NOMBLAY
               ES(J,K) = 0.D0
             ENDDO
           ENDIF
!
!        THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
!        THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
!
           IF(HAUTSED.LT.ELAY(J)) THEN
             ES(J,NOMBLAY)=ES(J,NOMBLAY)+ELAY(J)-HAUTSED
           ENDIF
!
         ENDDO

       ELSE
!       Check that sum of layers (simple precision) is equal to ZF-ZR 
!
         DO I=1,NPOIN
!
           ELAY(I)=0.D0
!
           IF(NOMBLAY.GT.1) THEN	   
             DO J=1,NOMBLAY-1
               ELAY(I)=ELAY(I)+ES(I,J)
             ENDDO
           ELSE
             ELAY(I)=ES(I,1)
           ENDIF	   
!
           DIFF= (ZF(I)-ZR(I)) - ELAY(I)
!
           IF(ABS(DIFF-ES(I,NOMBLAY)).GE.1.D-4) THEN
             WRITE(LU,*) 'ERROR IN INIT-MIXTE:'
             WRITE(LU,*) 'THE SUM OF THICKNESS OF BED LAYERS  
     *      IS DIFFERENT FROM ERODIBLE BED THICKNESS'
             CALL PLANTE(1)
             STOP
           ELSE 
             ES(I,NOMBLAY) = DIFF
             ELAY(I) = ZF(I)-ZR(I)
           ENDIF
!
         ENDDO        
!       
       ENDIF
!
! END LOOP  (initialization of layers)
!
! Check sum ELAY = ZF-ZR
!                = SUM (ES)
       DO I = 1, NPOIN
         EST=0.D0
         ELAY(I)= ZF(I)-ZR(I)
         DO J= 1, NOMBLAY
           EST=EST+ES(I,J)
         ENDDO
         DIFF=ABS(EST-ELAY(I))
         IF(DIFF.GT.1.D-08) THEN
            WRITE(LU,*) 'ERREUR POINT I'   
     *       , I, 'ELAY=',ELAY(I), 'EST=', EST
            CALL PLANTE(1)
            STOP
         ENDIF    
       ENDDO          
!
!  COMPUTING THE INITIAL MASSES OF MUD AND SAND
!
       DO I=1,NPOIN
        T1%R(I)=0.D0
        DO J=1,NOMBLAY
           IF(NSICLA.EQ.1) THEN
!           MS_VASE(I,J) = ES(I,J)*CONC(I,J)
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)
!
           ELSE 
! FOR MIXTE SEDIMENTS : (MUD, second class )
!....         FILLING VOIDS BETWEEN SAND GRAINS ....(XKV=1)
!
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)*AVAIL(I,J,2)
            MS_SABLE(I,J)= ES(I,J)*XMVS*AVAIL(I,J,1)
          ENDIF
          T1%R(I)= T1%R(I)+MS_VASE(I,J)
        ENDDO
      ENDDO
!      
! FOR MASS BALANCE
!
      IF(BILMA) THEN
          MASV0=DOTS(T1,VOLU2D)      
          IF(NCSIZE.GT.1) MASV0=P_DSUM(MASV0)
          MASVT=MASV0
          IF(LNG.EQ.1) WRITE(LU,1) MASV0
          IF(LNG.EQ.2) WRITE(LU,2) MASV0
       ENDIF
!
      !----------------------------------------------------------------!
001   FORMAT(1X,'MASSE INITIALE DU LIT DE VASE  : ', G20.11, ' KG')
      !----------------------------------------------------------------!
002   FORMAT(1X,'INITIAL MASS OF THE MUD BED: ', G20.11, ' KG')
      !----------------------------------------------------------------!
!
!-----------------------------------------------------------------------
!
1800  FORMAT(1X,'IL Y A PLUS DE ',1I6,' COUCHES DANS LA STRATIFICATION')
1815  FORMAT(1X,'THERE ARE MORE THAN ',1I6,' LAYERS IN STRATIFICATION')
!
!-----------------------------------------------------------------------

      RETURN
      END
