!                    *********************
                     SUBROUTINE FLUSEC_T2D
!                    *********************
!
     &(GLOSEG,DIMGLO,NSEG,NPOIN,DT,MESH,UNSV2D,FLODEL,FLULIM,H,DOPLOT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief  COMPUTES FLUXES OVER LINES (FLUXLINES/CONTROL SECTIONS) VIA 
!+      FLODEL/FLULIM
!+        
!+      THE FLUXES OF THE SEGMENTS ARE ALLREADY COMPUTED IN THE POSITIVE
!+      DEPTHS ROUTINE (BIEF)      
!+            
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS 
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE OF THE
!+      FLUXLINE. 
!+
!+      DURING LATER CALLS WE SUM UP THE FLUXES FOR EACH SEGMENT AND USE
!+      FLUXPR_TELEMAC2D TO WRITE OUT THE FLUXES
!
!history  L. STADLER (BAW)
!+        17/03/2016
!+        V7P2
!+   New way of computing discharges through control sections.
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| FLULIM         |<--| LIMITATION OF FLUXES
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| H              |<--| NEW WATER DEPTH
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFLX
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEG,NPOIN
      INTEGER, INTENT(IN)          :: DIMGLO
      INTEGER, INTENT(IN)          :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH)              :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL,UNSV2D,H 
      DOUBLE PRECISION, INTENT(IN) :: FLULIM(NSEG)
      LOGICAL, INTENT(IN)          :: DOPLOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX: CUMULATED VOLUME THROUGH SECTIONS
!     FLX: FLUX THROUGH CONTROL SECTIONS
!
      INTEGER, PARAMETER :: MAXEDGES=500
!
      TYPE :: FLUXLINE
        INTEGER, ALLOCATABLE :: SECTIONIDS(:)
        INTEGER, ALLOCATABLE :: DIRECTION(:)
        INTEGER              :: NOFSECTIONS
      END TYPE FLUXLINE
!
      INTEGER I,INP,ISEC,MYPOS,NUMBEROFLINES,IERR
!
      DOUBLE PRECISION, DIMENSION (2) :: SEG1,SEG2
      DOUBLE PRECISION :: SEGMENTFLUX,TIME,SIGN1,SIGN2
      DOUBLE PRECISION, ALLOCATABLE :: FLX(:,:),VOLFLUX(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: POTFLX(:,:),POTVOLFLUX(:,:)
!
      TYPE(FLUXLINE), ALLOCATABLE :: FLUXLINEDATA(:)       
!
      DOUBLE PRECISION :: SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX
      DOUBLE PRECISION,ALLOCATABLE :: FLUXLINES(:,:)  
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!     PARALLEL DATA NOT NEEDED, USING P_DSUM
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
      DOUBLE PRECISION :: SUMFLX, SUMVOLFLUX
!
      SAVE FLUXLINEDATA,DEJA,VOLFLUX,FLX,NUMBEROFLINES,TIME
      SAVE POTFLX,POTVOLFLUX
!
!----------------------------------------------------------------------
!
!     PART I
!     
!     SEARCH AND SAVE SEGMENTS (FIRST RUN ONLY)
!
!----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
!   
        INP=T2D_FILES(T2DFLX)%LU
        TIME = 0.D0       
!       READ FLUXLINE FILE
        READ(INP,*) NUMBEROFLINES 
!    
!       ALLOCATE THE FLUXLINES
        IF(.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES,9), STAT=IERR)
          IF(IERR.NE.0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*)'FLUXLINE : ERREUR D''ALLOCATION',IERR
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*)'FLUXLINE: ERROR REALLOCATING FLUXLINES:',IERR
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF    
!       READ NODES INTO FLUXLINE
        DO I = 1,NUMBEROFLINES
          READ(INP,*) FLUXLINES(I,1:9)
        ENDDO 
!    
        WRITE(LU,*) 'FLUXLINES FOUND ',NUMBEROFLINES
!
!------- DYNAMIC ALLOCATION OF FLX, VOLFLUX,...
!
        ALLOCATE(FLX(NUMBEROFLINES,1),STAT=IERR)
        ALLOCATE(VOLFLUX(NUMBEROFLINES,1),STAT=IERR)
        ALLOCATE(POTFLX(NUMBEROFLINES,1),STAT=IERR)
        ALLOCATE(POTVOLFLUX(NUMBEROFLINES,1),STAT=IERR)
        ALLOCATE(FLUXLINEDATA(NUMBEROFLINES),STAT=IERR)
        DO I = 1,NUMBEROFLINES
          ALLOCATE(FLUXLINEDATA(I)%SECTIONIDS(MAXEDGES),STAT=IERR)
          ALLOCATE(FLUXLINEDATA(I)%DIRECTION(MAXEDGES),STAT=IERR)
        ENDDO 
!
        IF(IERR.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,100) IERR
          IF(LNG.EQ.2) WRITE(LU,200) IERR
100       FORMAT(1X,'FLUSEC_T2D : ERREUR A L''ALLOCATION DE MEMOIRE',/,
     &               1X,'CODE D''ERREUR : ',1I6)
200       FORMAT(1X,'FLUSEC_T2D: ERROR DURING ALLOCATION OF MEMORY',/,
     &               1X,'ERROR CODE: ',1I6)
        ENDIF
!     
!------ CLEANUP
!
        DO ISEC =1,NUMBEROFLINES
          VOLFLUX(ISEC,1) = 0.D0
          FLUXLINEDATA(ISEC)%NOFSECTIONS = 0
        ENDDO
!
!-------LOOP OVER ALL MESH SEGMENTS TO STORE THEM FOR EACH FLUXLINE
!
        DO I = 1,MESH%NSEG
!
          SEG1(1) = MESH%X%R(GLOSEG(I,1))
          SEG1(2) = MESH%Y%R(GLOSEG(I,1))
          SEG2(1) = MESH%X%R(GLOSEG(I,2))
          SEG2(2) = MESH%Y%R(GLOSEG(I,2))
!        
!         LOOP OVER ALL FLUXLINES
          DO ISEC =1,NUMBEROFLINES
!            
!----------------------------------------------------------
!
! SIGN IS USED TO LOOK ON WHICH SIDE OF THE LINE A NODE IS
!
!  - SIGN IS NEGATIVE IF WE ARE ON THE RIGHT SIDE
!  - SIGN IS POSITIVE IF WE ARE ON THE LEFT SIDE
!  - SIGN IS ZERO IF WE ARE ON A POINT
!
!---------------------------------------------------------
!
            SIGN1 = (SEG1(1) - FLUXLINES(ISEC,3))* 
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG1(2) - FLUXLINES(ISEC,4)) * 
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))

            SIGN2 = (SEG2(1) - FLUXLINES(ISEC,3))* 
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG2(2) - FLUXLINES(ISEC,4)) * 
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
!
!---------------------------------------------------------
!
! THE FLUXLINE SHOULD NEVER CROSS A NODE (BE ZERO) 
! IF THIS HAPPENS WE SHIFT THE NODE (RIGHT AND UPWARDS)
!
!---------------------------------------------------------
!
            IF(SIGN1.EQ.0.D0) THEN
              SIGN1 = (SEG1(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG1(2)+0.001D0 - FLUXLINES(ISEC,4)) * 
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
            IF(SIGN2.EQ.0.D0) THEN
              SIGN2 = (SEG2(1)+0.001D0 - FLUXLINES(ISEC,3)) * 
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG2(2)+0.001D0 - FLUXLINES(ISEC,4)) * 
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!
!           ADD THE SEGMENT ID TO THE NODES
!
            IF(SIGN1*SIGN2.LT.0.D0) THEN
!                
              SEGXMIN = MIN(SEG1(1),SEG2(1))
              SEGXMAX = MAX(SEG1(1),SEG2(1))
              SEGYMIN = MIN(SEG1(2),SEG2(2))
              SEGYMAX = MAX(SEG1(2),SEG2(2))
!
              IF(SEGXMIN > FLUXLINES(ISEC,5).AND.
     &           SEGXMAX < FLUXLINES(ISEC,7).AND.
     &           SEGYMIN > FLUXLINES(ISEC,6).AND.
     &           SEGYMAX < FLUXLINES(ISEC,8)) THEN
!       
                MYPOS = FLUXLINEDATA(ISEC)%NOFSECTIONS + 1
                IF(MYPOS.EQ.MAXEDGES) THEN
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'FLUSEC_T2D :' 
                    WRITE(LU,*) 'TROP DE SEGMENTS DANS UNE SECTION'
                    WRITE(LU,*) 'AUGMENTER MAXEDGES'
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'FLUSEC_T2D:'
                    WRITE(LU,*) 'TOO MANY SEGMENTS IN A SECTION'
                    WRITE(LU,*) 'INCREASE MAXEDGES'
                  ENDIF
                  CALL PLANTE(1)
                  STOP
                ENDIF
                FLUXLINEDATA(ISEC)%SECTIONIDS(MYPOS) = I
                IF(SIGN1.GT.0.D0) THEN
                  FLUXLINEDATA(ISEC)%DIRECTION(MYPOS) = 1
                ELSE 
                  FLUXLINEDATA(ISEC)%DIRECTION(MYPOS) = -1
                ENDIF
                FLUXLINEDATA(ISEC)%NOFSECTIONS = MYPOS
!
!               FOR DEBUGGING
!
!               WRITE(LU,*)'ADDED SEGMENTS ',
!    &                      I,GLOSEG(I,1),GLOSEG(I,2)
!               WRITE(LU,*)'AT COORDINATES ',SEG1,SEG2
!               WRITE(LU,*)'SECTIONS FOUND ',
!    &                      FLUXLINEDATA(ISEC)%NOFSECTIONS
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!     END SEARCH SEGEMENT (DEJA)
      DEJA = .TRUE.
!
!----------------------------------------------------------------------
!
!     PART II
!
!     ADD THE FLUXES (FLODEL FROM POSITIVE DEPTHS) FOR SEGMENTS
!
!     TODO WE SHOULD THINK ABOUT HOW WE CAN HANDLE THIS IN THE PARALLEL
!          CASE! IF A SEGMENT IS SHARED WE NEED THE HALF FLUX?
!
!----------------------------------------------------------------------
!
      TIME = TIME + DT
!     LOOP OVER ALL FLUXLINES
      DO ISEC =1,NUMBEROFLINES
        FLX(ISEC,1) = 0.D0
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA(ISEC)%NOFSECTIONS
          SEGMENTFLUX =  FLUXLINEDATA(ISEC)%DIRECTION(I) *  
     &                   FLODEL%R(FLUXLINEDATA(ISEC)%SECTIONIDS(I))  
          FLX(ISEC,1) = FLX(ISEC,1) + SEGMENTFLUX            
          VOLFLUX(ISEC,1) = VOLFLUX(ISEC,1) + (SEGMENTFLUX * DT)
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!     PART III
!
!----------------------------------------------------------------------
!
      IF(DOPLOT) THEN
!       PARALLEL CASE
        IF(NCSIZE.GT.1) THEN
!         PREPARE SINGLE DATA FOR SENDING
          DO I=1,NUMBEROFLINES
            SUMFLX = P_DSUM(FLX(I,1))
            SUMVOLFLUX = P_DSUM(VOLFLUX(I,1))            
            WRITE(LU,FMT=1000) 'SECTION ',I,' WATER ',
     &      SUMFLX,SUMVOLFLUX,TIME,DT
          ENDDO
!       SERIAL CASE
        ELSE
          DO I=1,NUMBEROFLINES
            WRITE(LU,FMT=1000) 'SECTION ',I,' WATER ',
     &      FLX(I,1),VOLFLUX(I,1),TIME,DT
          ENDDO
        ENDIF
      ENDIF
!
1000  FORMAT (A9,I2,A7,ES22.14,ES22.14,ES22.14,ES22.14)
!
!----------------------------------------------------------------------
!
      RETURN
      END

