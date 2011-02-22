C                       ****************                               
                        SUBROUTINE MAJZZ                               
C                       ****************                               
C                                                                       
     *(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DDMIN,KFROT,SMH,
     * HN,QU,QV)                   
C                                                                       
C***********************************************************************
C                                         N. GOUTAL 24/11/97
C-----------------------------------------------------------------------
C                                                                       
C  FONCTION  : . INTEGRATION EN TEMPS                                   
C  
C
C  NOTE JMH : DDMIN NON UTILISE
C                                                                     
C-----------------------------------------------------------------------
C                             ARGUMENTS                                 
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C ! . FLUX         ! -->! FLUX DE ROE                                  !
C ! . AIRS         ! -->! TABLEAU DES AIRES DES CELLULES               !
C ! . H            !<-->! ENTHALPIE                                    !
C !                !    !                                              !
C ! . DT           ! -->! PAS DE TEMPS.                                !
C ! . 3            ! -->! DIMENSION DU SYSTEME                         !
C !________________!____!______________________________________________!
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)   
C        -- (TABLEAU DE TRAVAIL)                                        
C-----------------------------------------------------------------------
C     - SOUS PROGRAMME(S) APPELANT : RESOLU                             
C     - SOUS PROGRAMME(S) APPELE   : AUCUN                              
C     - PORTABILITE: CRAY                                               
C***********************************************************************
C                                                                       
      IMPLICIT NONE
C                                                     
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU                                                
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS,DDMIN
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN) 
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),QU(NPOIN),QV(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C 
      INTEGER I 
C 
      DOUBLE PRECISION FACT
      DOUBLE PRECISION, PARAMETER :: G = 9.81D0
C
C     UPDATING
C
      DO 10 I = 1 , NPOIN 
         FACT=DT/AIRS(I) 
         W(1,I) = W(1,I) + FACT *( FLUX (I,1)+SMH(I)) 
         W(2,I) = W(2,I) + FACT *  FLUX (I,2)
         W(3,I) = W(3,I) + FACT *  FLUX (I,3) 
10    CONTINUE 
C
C     FRICTION
C     
      IF (KFROT.NE.0) CALL FRICTION(NPOIN,G,DT,W,HN,QU,QV,CF)
C
C-----------------------------------------------------------------------
C  
      RETURN                                                            
      END
