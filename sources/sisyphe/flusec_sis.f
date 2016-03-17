!                    *********************
                     SUBROUTINE FLUSEC_SIS
!                    *********************
!
     &(GLOSEG,DIMGLO,NSEG,NPOIN,DT,MESH,UNSV2D,FLODEL,FLULIM,HZ,
     & ICLA,DOPLOT)
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
!+      DURING LATER CALLS WE SUMM UP THE FLUXES FOR EACH SEGMENT AND USE
!+      FLUXPR_TELEMAC2D TO WRITE OUT THE FLUXES
!+
!
!history  L. STADLER (BAW)
!+        15/03/2016
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
!| ICLA           |-->| SEDIMENT CLASS
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| HZ             |<--| NEW AVAILABLE LAYER OF SEDIMENT
!| MESH           |-->| MESH STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY: SIS_FILES, SISFLX
!
      IMPLICIT NONE
      INTEGER LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NSEG,ICLA,NPOIN
      INTEGER, INTENT(IN)    :: DIMGLO
      INTEGER, INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL,UNSV2D, HZ
      DOUBLE PRECISION, INTENT(IN) :: FLULIM(NSEG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX: CUMULATED VOLUME THROUGH SECTIONS
!     FLUX: FLUX THROUGH CONTROL SECTIONS
!
      INTEGER :: MAXEDGES 
        TYPE :: FLUXLINE
        INTEGER, DIMENSION(1000) :: SECTIONIDS
        INTEGER, DIMENSION(1000) :: DIRECTION 
        INTEGER                 :: NOFSECTIONS
      END TYPE FLUXLINE
!
      INTEGER IERR
      LOGICAL DOPLOT
!
      INTEGER IELEM,IEL,IELMH,I,INP,J,MYCLA
      INTEGER ISEG,ISEC
      INTEGER MYPOS,NUMBEROFLINES
      INTEGER MAXNUMBEROFCLASSES
      

      DOUBLE PRECISION, DIMENSION (2)   :: SEG1,SEG2,POINT
      DOUBLE PRECISION :: SEGMENTFLUX, MASS, MASSTOTAL, TIME
      DOUBLE PRECISION                  :: SIGN1,SIGN2
      DOUBLE PRECISION, ALLOCATABLE :: FLUX(:,:),VOLFLUX(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: POTFLUX(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: POTVOLFLUX(:,:)

      TYPE (FLUXLINE), ALLOCATABLE :: FLUXLINEDATA(:)       
!
      DOUBLE PRECISION :: BOXX1,BOXX2,BOXY1,BOXY2,SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX,DELTA
      DOUBLE PRECISION,ALLOCATABLE  :: FLUXLINES (:,:)  
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!     PARALLEL DATA NOT NEEDED, USING P_DSUM
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
      DOUBLE PRECISION :: SUMFLUX, SUMVOLFLUX
      DOUBLE PRECISION :: SUMBOX, SUMGLOBAL

!
      SAVE FLUXLINEDATA,DEJA,VOLFLUX,FLUX,NUMBEROFLINES,TIME
      SAVE POTFLUX, POTVOLFLUX
!
!
!-----------------------------------------------------------------------
!
!
!----------------------------------------------------------------------
!
!     PART I
!     
!     SEARCH AND SAVE SEGMENTS (FIRST RUN ONLY)
!
!----------------------------------------------------------------------
!
      MAXEDGES = 1000

      IF(.NOT.DEJA) THEN
!   
        INP=SIS_FILES(SISFLX)%LU
        MAXNUMBEROFCLASSES = 20
        TIME = 0.0D0   
!
!------- OPEN FLUXLINE FILE
!
        READ(INP,*) NUMBEROFLINES    
!       ALLOCATE THE FLUXLINES
        IF (.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES,9), STAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(LU,*)'FLUSEC_SIS: ERROR OF ALLOCATION:',IERR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!       READ NODES INTO FLUXLINE
        DO I = 1,NUMBEROFLINES
          READ(INP,*) FLUXLINES(I,1:9)
        ENDDO 
!    
        WRITE(LU,*) "FLUXLINES FOUND ",NUMBEROFLINES,"CLASSES",ICLA
!
!------- DYNAMIC ALLOCATION OF FLUX, VOLFLUX,...
!
        ALLOCATE(FLUX(NUMBEROFLINES,MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(VOLFLUX(NUMBEROFLINES,MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(POTFLUX(NUMBEROFLINES,MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(POTVOLFLUX(NUMBEROFLINES,MAXNUMBEROFCLASSES),STAT=IERR)
        ALLOCATE(FLUXLINEDATA(NUMBEROFLINES),STAT=IERR)
!
        IF(IERR.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,100) IERR
          IF(LNG.EQ.2) WRITE(LU,200) IERR
100       FORMAT(1X,'FLUSEC_SIS : ERREUR A L''ALLOCATION 
     &               DE MEMOIRE : ',/,1X,'CODE D''ERREUR : ',1I6)
200       FORMAT(1X,'FLUSEC_SIS: ERROR DURING ALLOCATION 
     &              OF MEMORY: ',/,1X,'ERROR CODE: ',1I6)
        ENDIF
!     
!------ CLEANUP
!
        DO ISEC =1,NUMBEROFLINES
          DO I = 1,MAXNUMBEROFCLASSES
            VOLFLUX(ISEC,I) = 0.0D0
            POTVOLFLUX(ISEC,I) = 0.0D0
          ENDDO
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
!
            IF(SIGN2.EQ.0.D0) THEN
              SIGN2 = (SEG2(1)+0.001D0 - FLUXLINES(ISEC,3)) * 
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG2(2)+0.001D0 - FLUXLINES(ISEC,4)) * 
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!           ADD THE SEGMENT ID TO THE NODES
            IF(SIGN1*SIGN2.LT.0.D0) THEN
                
              SEGXMIN = MIN(SEG1(1),SEG2(1))
              SEGXMAX = MAX(SEG1(1),SEG2(1))
              SEGYMIN = MIN(SEG1(2),SEG2(2))
              SEGYMAX = MAX(SEG1(2),SEG2(2))
!
              IF((SEGXMIN > FLUXLINES(ISEC,5).AND.(SEGXMAX < 
     &            FLUXLINES(ISEC,7))).AND.
     &           (SEGYMIN > FLUXLINES(ISEC,6)).AND.(SEGYMAX < 
     &              FLUXLINES(ISEC,8))) THEN
!       
                    MYPOS = FLUXLINEDATA(ISEC)%NOFSECTIONS + 1                    
                    IF(MYPOS.EQ.MAXEDGES) THEN
                      IF(LNG.EQ.1) WRITE(LU,52)
                      IF(LNG.EQ.2) WRITE(LU,53)
52                    FORMAT(/,1X,' STOP :',/
     &                ,1X,' LIMITE MAXIMUM ATTEINT DES BORDS')
53                    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &                ,1X,' REACHED MAXIMUM LIMIT OF EDGES')
                      CALL PLANTE(1)
                      STOP      
                    ENDIF
!                    
                    FLUXLINEDATA(ISEC)%SECTIONIDS(MYPOS) = I
                    IF(SIGN1.GT.0.D0) THEN
                      FLUXLINEDATA(ISEC)%DIRECTION(MYPOS) = 1
                    ELSE 
                      FLUXLINEDATA(ISEC)%DIRECTION(MYPOS) = -1
                    ENDIF
                    FLUXLINEDATA(ISEC)%NOFSECTIONS = MYPOS
!
!                   FOR DEBUGGING
!
!                   WRITE(LU,*) 'ADDED SEGMENTS ',
!    &                          I,GLOSEG(I,1),GLOSEG(I,2)
!                   WRITE(LU,*) 'AT COORDINATES ',
!    &                          SEG1(1),SEG1(2),SEG2(1),SEG2(2)
!                   WRITE(LU,*) 'SECTIONS FOUND ',
!    &                          FLUXLINEDATA(ISEC)%NOFSECTIONS
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
!     ONLY INCREASE THE TIME SORRY, THIS INCLUDES THE MORPHOLOGIC FACTOR!
      IF(ICLA.EQ.1)THEN
        TIME = TIME + DT
      ENDIF 
!     LOOP OVER ALL FLUXLINES
      DO ISEC =1,NUMBEROFLINES
!       ICLA ARE THE CLASSES
        FLUX(ISEC,ICLA) = 0.0D0
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA(ISEC)%NOFSECTIONS
          SEGMENTFLUX = FLUXLINEDATA(ISEC)%DIRECTION(I) *  
     &                  FLODEL%R(FLUXLINEDATA(ISEC)%SECTIONIDS(I))  
          FLUX(ISEC,ICLA) = FLUX(ISEC,ICLA) + SEGMENTFLUX   
          VOLFLUX(ISEC,ICLA) = VOLFLUX(ISEC,ICLA) + (SEGMENTFLUX*DT)            
        ENDDO
      ENDDO
!       
!----------------------------------------------------------------------
!
!     PART IIb
!
!     SCRIPTING AREA ;-)
!     ADD A MASSBOX IF YOU LIKE (ADVANCED USERS ONLY) !
!     WARNING THIS PART IS NOT PARALLEL  
!----------------------------------------------------------------------
!      IF (DOPLOT) THEN 
!        MASS = 0.0D0
!        MASSTOTAL = 0.0D0
!        DO I=1,NPOIN
!            POINT(1) = MESH%X%R(I)
!            POINT(2) = MESH%Y%R(I)
!            IF (POINT(2).GE.31.2) THEN
!               MASS = MASS + HZ%R(I) * (1.0D0/ UNSV2D%R(I))
!            ENDIF
!            MASSTOTAL = MASSTOTAL + HZ%R(I) * (1.0D0/ UNSV2D%R(I))
!        ENDDO
!        
!        IF(NCSIZE.GT.1) THEN
!            SUMBOX = P_DSUM(MASS)
!            SUMGLOBAL = P_DSUM(MASSTOTAL)
!            WRITE(6,FMT=1001) "FLUXLINE_MASSTOTAL",SUMGLOBAL,TIME,DT
!            WRITE(6,FMT=1001) "FLUXLINE_MASSBOX  ",SUMBOX,TIME,DT
!        ELSE
!            WRITE(6,FMT=1001) "FLUXLINE_MASSTOTAL",MASSTOTAL,TIME,DT
!            WRITE(6,FMT=1001) "FLUXLINE_MASSBOX  ",MASS,TIME,DT
!        ENDIF
!
!1001  FORMAT (A18,ES22.14,ES22.14,ES22.14)
!      ENDIF
!----------------------------------------------------------------------
!
!     PART III
!
!     SEND THE RESULTS TO FLUXPR_TELEMAC2D
!
!----------------------------------------------------------------------
!
      IF(DOPLOT) THEN
        IF(NCSIZE.GT.1) THEN
!         PARALLEL CASE       
!         PREPARE SINGLE DATA FOR SENDING
          DO I=1,NUMBEROFLINES
            SUMFLUX = P_DSUM(FLUX(I,ICLA))
            SUMVOLFLUX = P_DSUM(VOLFLUX(I,ICLA))
            WRITE(LU,FMT=1000) 'FLUXLINE',I,'MYCLASS',ICLA,
     &      SUMFLUX,SUMVOLFLUX,TIME,DT
          ENDDO           
        ELSE
!         SERIAL CASE
          DO I=1,NUMBEROFLINES
            WRITE(LU,FMT=1000) 'FLUXLINE',I,'MYCLASS',ICLA,
     &      FLUX(I,ICLA),VOLFLUX(I,ICLA),TIME,DT
          ENDDO
        ENDIF      
      ENDIF
!
1000  FORMAT (A8,' ',I2,' ',A7,' ',I2,' ',ES22.14,ES22.14,ES22.14,
     &        ES22.14)
!
!----------------------------------------------------------------------
!
      RETURN
      END
