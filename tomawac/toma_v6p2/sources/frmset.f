!                       ***************** 
                        SUBROUTINE FRMSET 
!                       ***************** 
     &( X     , Y     , NEIGB , NB_CLOSE, NPOIN2,MAXNSP, NRD, NELEM2, 
     &  IKLE  , RK    , RX    , RY    , RXX   , RYY ) 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    DIFFRACTION 
!+ 
!+         SETTING THE DOMAINS FOR THE FREE-MESH METHOD 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
!+ 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+   Modification for V6P2 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!| IKLE           |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING 
!|                |   | OF THE 2D MESH 
!| MAXNSP         |-->| CONSTANT FOR MESHFREE TECHNIQUE 
!| NB_CLOSE       |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| NEIGB          |<->| NEIGHBOUR POINTS FOR MESHFREE METHOD 
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH 
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH 
!| NRD            |-->| CONSTANT FOR MESHFREE TECHNIQUE 
!| RK             |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RX             |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RXX            |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RY             |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| RYY            |<->| ARRAY USED IN THE MESHFREE TECHNIQUE 
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH 
!| Y              |-->| ORDINATES OF POINTS IN THE MESH 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IMPLICIT NONE 
! 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
 
!.....VARIABLES IN ARGUMENT 
!     """""""""""""""""""" 
      INTEGER NPOIN2, MAXNSP, NRD, NELEM2 
      INTEGER NB_CLOSE(NPOIN2), NEIGB(NPOIN2,MAXNSP) 
      INTEGER IKLE(NELEM2,3) 
      DOUBLE PRECISION X(NPOIN2), Y(NPOIN2) 
      DOUBLE PRECISION RK(MAXNSP,NPOIN2) 
      DOUBLE PRECISION RX(MAXNSP,NPOIN2), RY(MAXNSP,NPOIN2)   
      DOUBLE PRECISION RXX(MAXNSP,NPOIN2),RYY(MAXNSP,NPOIN2) 
! 
!.....LOCAL VARIABLES 
!     """"""""""""""" 
      INTEGER IP, IPOIN, IP2, I 
      INTEGER ICLM, J, IELEM, ICLM2, ICLM3, ILM 
      INTEGER ILM_POIN(NPOIN2,8), CLM(NPOIN2), KACC(NPOIN2) 
      INTEGER M, ICST, ICST2, NCST, IP_S, ILP 
      INTEGER NB_C(NPOIN2), SUR_P(NPOIN2,8), L(2) 
      INTEGER STACK(NPOIN2), STACK2(NPOIN2) 
      LOGICAL ALREADY_POM(NPOIN2) 
 
      DOUBLE PRECISION MINDIST(NPOIN2)  
      DOUBLE PRECISION AC,QUO,RAD1 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
! ILM_POIN array with the elements to which a point belongs 
! CLM(IP) array with the number of elements for each point 
!  for IP belong to the elements  ILM_POIN(CLM(IP-1)+1:CLM(IP)) 
      ICLM2=0 
      DO IPOIN =1, NPOIN2 
        ICLM=0 
        DO  IELEM=1,NELEM2 
          IF (IPOIN.EQ.IKLE(IELEM,1)) THEN 
            ICLM=ICLM+1 
            ILM_POIN(IPOIN,ICLM)=IELEM 
          ELSEIF (IPOIN.EQ.IKLE(IELEM,2)) THEN 
            ICLM=ICLM+1 
            ILM_POIN(IPOIN,ICLM)=IELEM 
          ELSEIF (IPOIN.EQ.IKLE(IELEM,3)) THEN 
            ICLM=ICLM+1 
            ILM_POIN(IPOIN,ICLM)=IELEM 
          ENDIF 
        ENDDO 
        CLM(IPOIN)=ICLM   
        IF(CLM(IPOIN).GT.8) write(6,*) '**** OUPS ', IPOIN,CLM(IPOIN) 
      ENDDO 
! 
! searching for the points which are around the point IPOIN  
! and add the to a look up array SUR_P(IPOIN,NB_C(IPOIN)) 
! 
!    Initialize all the arrays  and logics for the new subdomain   
       DO IP=1,NPOIN2 
          ALREADY_POM(IP) =.FALSE. 
       ENDDO 
! 
       DO IPOIN=1,NPOIN2 
        NB_C(IPOIN)=0 
        MINDIST(IPOIN)=1.E+6 
        DO ILM=1,CLM(IPOIN) 
          IELEM=ILM_POIN(IPOIN,ILM) 
!      loop over 3 nodes of each triangle 
          DO J=1,3 
!           test if the selected node belongs to the triangle 
            IF (IKLE(IELEM,J).EQ.IPOIN) THEN 
             IF (J.EQ.1) THEN 
              L(1)=IKLE(IELEM,2) 
              L(2)=IKLE(IELEM,3) 
             ENDIF 
             IF (J.EQ.2) THEN 
              L(1)=IKLE(IELEM,1) 
              L(2)=IKLE(IELEM,3) 
             ENDIF 
             IF (J.EQ.3) THEN 
              L(1)=IKLE(IELEM,1) 
              L(2)=IKLE(IELEM,2) 
             ENDIF 
            ENDIF 
          ENDDO 
!           
          DO M=1,2 
           IF (.NOT.ALREADY_POM(L(M))) THEN 
            NB_C(IPOIN)=NB_C(IPOIN)+1 
            SUR_P(IPOIN,NB_C(IPOIN)) =L(M)  
            ALREADY_POM(L(M)) =.TRUE. 
	   ENDIF 
          ENDDO 
!    
        ENDDO 
! 
!   CALCULATE DISTANCE of EVERY POINT TO THE NEIGHBOUR POINTS 
         DO J=1,NB_C(IPOIN) 
          IP=SUR_P(IPOIN,J) 
          RAD1=SQRT((X(IP)-X(IPOIN))**2+(Y(IP)-Y(IPOIN))**2) 
          IF(RAD1.LE.MINDIST(IPOIN)) MINDIST(IPOIN)=RAD1 
          ALREADY_POM(IP) =.FALSE. 
        ENDDO 
 
      ENDDO 
! 
! make the subdomain search over the nearest point of each point and  
!  add them in the NEIGB(IPOIN,MAXNSP) aray 
      DO IPOIN=1,NPOIN2   
        NB_CLOSE(IPOIN)=1 
        NEIGB(IPOIN,1) =IPOIN 
        ALREADY_POM(IPOIN) =.TRUE. 
        NCST=1 
        STACK(NCST)=IPOIN 
!        WRITE(6,*) (SUR_P(STACK(NCST),j),j=1,7)         
! 
! ipoin is the main point of domain ipoin 
! around the ipoin do a search in the elements it belongs 
! loop around the point of Stack and do what you did before 
! 
! while loop 
        DO   
          ICST2=0 
          DO ICST =1,NCST 
            IP= STACK(ICST) 
 
            DO ILP=1,NB_C(IP) 
              IP_S=SUR_P(IP,ILP) 
              IF (.NOT.ALREADY_POM(IP_S)) THEN 
                NB_CLOSE(IPOIN)=NB_CLOSE(IPOIN)+1 
                NEIGB(IPOIN,NB_CLOSE(IPOIN)) =IP_S 
                ALREADY_POM(IP_S) =.TRUE. 
                IF(NB_CLOSE(IPOIN).GE.NRD) GOTO 222 
                ICST2=ICST2+1     
                STACK2(ICST2)=IP_S 
              ENDIF 
            ENDDO ! ILP                
          ENDDO ! ICST   
          NCST=ICST2 
          STACK=STACK2 
        ENDDO             
222   CONTINUE 
! end of while loop 
! 
!subdomain (Ipoin) finish after initializing 
! logic goto to the next subdomain 
        DO J=1,NB_CLOSE(IPOIN)! initialize already for points logic 
           IP2=NEIGB(IPOIN,J) 
           ALREADY_POM(IP2) =.FALSE. ! initialize already for points logic 
        ENDDO 
      ENDDO  !1,NPOIN2 
! 
! CALCULATE THE RADIAL FUNCTION OF RPI 
! AND INVERSE MATRICES OF EACH SUB DOMAIN 
        QUO = 1.03 
        AC = 8. 
        DO I=1,NPOIN2 
         CALL RPI_INVR(X, Y, NEIGB, NB_CLOSE, 
     &      RK(1,I), RX(1,I), RY(1,I), RXX(1,I), RYY(1,I), 
     &      NPOIN2, I, QUO, AC, MAXNSP, MINDIST) 
!      
       ENDDO 
! 
      RETURN 
      END                   
