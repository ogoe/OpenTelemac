!                    *********************
                     SUBROUTINE FLUSEC_T2D
!                    *********************
!
     &(GLOSEG,DIMGLO,NSEG,NPOIN,DT,MESH,UNSV2D,FLODEL,H,DOPLOT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief  COMPUTES FLUXES OVER LINES (FLUXLINES/CONTROL SECTIONS) VIA
!+      FLODEL
!+
!+      THE FLUXES OF THE SEGMENTS ARE ALLREADY COMPUTED IN THE POSITIVE
!+      DEPTHS ROUTINE (BIEF)
!+
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE OF THE
!+      FLUXLINE.
!+
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE
!+      OF THE FLUXLINE.
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
!| DT             |-->| TIME_FLUSECT2D STEP
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| H              |<--| NEW WATER DEPTH
!| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFLX,
     &                            FLUXLINEDATA_FLUSECT2D,
     &                            DEJA_FLUSECT2D,VOLFLUX_FLUSECT2D,
     &                            FLX_FLUSECT2D,
     &                            NUMBEROFLINES_FLUSECT2D,
     &                            TIME_FLUSECT2D
!
!##> JR @ RWTH: ALLOW COMPILERS TO CHECK PARALLEL INTERFACE
      USE INTERFACE_PARALLEL, ONLY : P_DSUM
!##< JR @ RWTH
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEG,NPOIN
      INTEGER, INTENT(IN)          :: DIMGLO
      INTEGER, INTENT(IN)          :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH)              :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL,UNSV2D,H
      LOGICAL, INTENT(IN)          :: DOPLOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX_FLUSECT2D: CUMULATED VOLUME THROUGH SECTIONS
!     FLX_FLUSECT2D: FLUX THROUGH CONTROL SECTIONS
!
      INTEGER, PARAMETER :: MAXEDGES=500
!
      INTEGER I,INP,ISEC,MYPOS,IERR
!
      DOUBLE PRECISION, DIMENSION (2) :: SEG1,SEG2
      DOUBLE PRECISION :: SEGMENTFLUX,SIGN1,SIGN2
!
!
      DOUBLE PRECISION :: SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX
      DOUBLE PRECISION,ALLOCATABLE :: FLUXLINES(:,:)
!
!     PARALLEL DATA NOT NEEDED, USING P_DSUM
!##> JR @ RWTH: INTERFACE CHECKED SO NO NEED FOR EXTERNALS
!      DOUBLE PRECISION P_DSUM
!      EXTERNAL         P_DSUM
!##< JR @ RWTH
      DOUBLE PRECISION :: SUMFLX, SUMVOLFLUX
!
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
      IF(.NOT.DEJA_FLUSECT2D) THEN
!
        INP=T2D_FILES(T2DFLX)%LU
        TIME_FLUSECT2D = 0.D0
!       READ FLUXLINE FILE
        READ(INP,*) NUMBEROFLINES_FLUSECT2D
!
!       ALLOCATE THE FLUXLINES
        IF(.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES_FLUSECT2D,9), STAT=IERR)
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
        DO I = 1,NUMBEROFLINES_FLUSECT2D
!##> JR @ RWTH: AVOID PART-REF WITH NON-ZERO RANK IN MODE T1V
          READ(INP,*) ( FLUXLINES(I,ISEC), ISEC=1, 9 )
!##< JR @ RWTH
        ENDDO
!
        WRITE(LU,*) 'FLUXLINES FOUND ',NUMBEROFLINES_FLUSECT2D
!
!------- DYNAMIC ALLOCATION OF FLX_FLUSECT2D, VOLFLUX_FLUSECT2D,...
!
        ALLOCATE(FLX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
        ALLOCATE(VOLFLUX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
        ALLOCATE(FLUXLINEDATA_FLUSECT2D(NUMBEROFLINES_FLUSECT2D),
     &           STAT=IERR)
        DO I = 1,NUMBEROFLINES_FLUSECT2D
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%SECTIONIDS(MAXEDGES),
     &           STAT=IERR)
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%DIRECTION(MAXEDGES),
     &           STAT=IERR)
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
        DO ISEC =1,NUMBEROFLINES_FLUSECT2D
          VOLFLUX_FLUSECT2D(ISEC,1) = 0.D0
          FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = 0
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
          DO ISEC =1,NUMBEROFLINES_FLUSECT2D
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
                MYPOS = FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS + 1
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
                FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(MYPOS) = I
                IF(SIGN1.GT.0.D0) THEN
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = 1
                ELSE
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = -1
                ENDIF
                FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = MYPOS
!
!               FOR DEBUGGING
!
!               WRITE(LU,*)'ADDED SEGMENTS ',
!    &                      I,GLOSEG(I,1),GLOSEG(I,2)
!               WRITE(LU,*)'AT COORDINATES ',SEG1,SEG2
!               WRITE(LU,*)'SECTIONS FOUND ',
!    &                      FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!     END SEARCH SEGEMENT (DEJA_FLUSECT2D)
      DEJA_FLUSECT2D = .TRUE.
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
      TIME_FLUSECT2D = TIME_FLUSECT2D + DT
!     LOOP OVER ALL FLUXLINES
      DO ISEC =1,NUMBEROFLINES_FLUSECT2D
        FLX_FLUSECT2D(ISEC,1) = 0.D0
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
          SEGMENTFLUX =  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(I) *
     &             FLODEL%R(FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(I))
          FLX_FLUSECT2D(ISEC,1) = FLX_FLUSECT2D(ISEC,1) + SEGMENTFLUX
          VOLFLUX_FLUSECT2D(ISEC,1) = VOLFLUX_FLUSECT2D(ISEC,1) +
     &                                (SEGMENTFLUX * DT)
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
          DO I=1,NUMBEROFLINES_FLUSECT2D
            SUMFLX = P_DSUM(FLX_FLUSECT2D(I,1))
            SUMVOLFLUX = P_DSUM(VOLFLUX_FLUSECT2D(I,1))
            WRITE(LU,FMT=1000) 'SECTION ',I,' WATER ',
     &      SUMFLX,SUMVOLFLUX,TIME_FLUSECT2D,DT
          ENDDO
!       SERIAL CASE
        ELSE
          DO I=1,NUMBEROFLINES_FLUSECT2D
            WRITE(LU,FMT=1000) 'SECTION ',I,' WATER ',
     &      FLX_FLUSECT2D(I,1),VOLFLUX_FLUSECT2D(I,1),TIME_FLUSECT2D,DT
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
