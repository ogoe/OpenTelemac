!                    ******************************
                     SUBROUTINE CORRECTION_DEPTH_3D
!                    ******************************
!
     &(W2D,W3D,GLOSEG,DIMGLO)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.
!
!history  J.M. HERVOUET (LNHE)
!+        28/07/2009
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
!history  J.M. HERVOUET (LNHE)
!+        26/08/2011
!+        V6P2
!+   Call to FLUX_EF_VF_3D changed
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| W2D            |<->| WORK ARRAY IN 2D
!| W3D            |<->| WORK ARRAY IN 3D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D,
     &                     EX_CORRECTION_DEPTH_3D => CORRECTION_DEPTH_3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER IOPT, ISEG, ISEG3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: W2D(NELEM2,3),W3D(NELEM3,6)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) :: FORMUL
      INTEGER I
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1) THEN
!
        IF(OPT_HNEG.EQ.2) THEN
!
!         FOR THE TIME BEING, FLODEL IS A WORKING ARRAY, HENCE
!         YAFLODEL=.FALSE.; TO USE FLODEL IN TEL4DEL WOULD REQUIRE
!         TRANSFORMING IT INTO 3D.
!
          FORMUL = 'VGRADP 2     HOR'
          SAVEZ     =>MESH3D%Z%R
          MESH3D%Z%R=>ZPROP%R
!
!         HERE T3_01 IS NOT REALLY USED, WE USE THE NON ASSEMBLED
!         FORM MESH3D%W, WHICH IS HERE ALSO W3D, HENCE LEGO=.FALSE.
!         
          CALL VECTOR(T3_01,'=',FORMUL,IELM3,-1.D0,DM1,GRAZCO,GRAZCO,
     *                UCONV,VCONV,VCONV,MESH3D,MSK,MASKEL,LEGO=.FALSE.)
!
          MESH3D%Z%R=>SAVEZ
!
!         CALCULATES 3D FLODEL (NOTE JMH: THIS WILL BE REDONE IN FLUX3D
!                                         OPTIMISATION MAYBE POSSIBLE  )
          IOPT=2
          CALL FLUX_EF_VF_3D(FLODEL%R,MESH2D%W%R,MESH3D%W%R,
     &                       MESH2D%NSEG,MESH3D%NSEG,MESH2D%NELEM,
     &                       MESH3D%NELEM,MESH2D,.TRUE.,IOPT,1,
     &                       IELM3,NPLAN,MESH3D%IKLE%I,MESH3D%NELMAX,
     &                       MESH3D%KNOLG%I)
!
!         CALCULATES 2D FLODEL (PUT IN FIRST PLANE OF 3D FLODEL)
!      
!         IT IS DIFFERENT FROM THE METHOD CONSISTING OF SUMMING THE
!         FLUXES ON THE VERTICAL FIRST AND THEN CALLING FLUX_EF_VF
!
          IF(IELM3.EQ.41) THEN
!         
            DO ISEG = 1,MESH2D%NSEG
              DO I = 2,NPLAN
                ISEG3D = ISEG + (I-1)*MESH2D%NSEG
                FLODEL%R(ISEG) = FLODEL%R(ISEG) + FLODEL%R(ISEG3D)
              ENDDO
            ENDDO
!
          ELSEIF(IELM3.EQ.51) THEN
!
!           NOTHING TO DO, THIS WAS DONE IN FLUX_EF_VF_3D
!
          ELSE
           WRITE(LU,*) 'CORRECTION_DEPTH_3D: UNKNOWN ELEMENT:',IELM3
           CALL PLANTE(1)
           STOP
          ENDIF
!
          CALL POSITIVE_DEPTHS(T2_01,T2_02,T2_03,T2_04,H,HN,
     &                         MESH2D,FLODEL,.FALSE.,
     &                         FLBOR,DT,UNSV2D,NPOIN2,
     &                         GLOSEG(1:DIMGLO,1),
     &                         GLOSEG(1:DIMGLO,2),
     &                         MESH2D%NBOR%I,NPTFR2,.FALSE.,
!                                                    YAFLODEL
     &                         SMH,.TRUE.,2,
!                                   YASMH OPTSOU
!                              SMH IN PROJECTED FORM IN T3D
     &                         FLULIM%R,LIHBOR%I,HBOR%R,KENT,INFOGR,
     &                         MESH2D%W%R,NAMECODE,2,MAXADV)
!                                                  2 HARDCODED OPTION
!                                                  FOR POSITIVE DEPTH ALGORITHM
!                                                  INDEPENDENT OF SEGMENT
!                                                  NUMBERING
!
        ELSEIF(OPT_HNEG.EQ.1) THEN
!
!         CONSERVATIVE SMOOTHING OF NEGATIVE DEPTHS
!
!         1) PUTS NEGATIVE VALUES IN T2_01 AND REMOVES THEM FROM H
!
          CALL OS( 'X=-(Y,C)' , X=T2_01 , Y=H     , C=0.D0 )
          CALL OS( 'X=X-Y   ' , X=H     , Y=T2_01 )
!
!         2) SMOOTHES NEGATIVE VALUES (TWO LOOPS HERE)
!            AND MASKS TO NOT AFFECT THE TIDAL FLATS
!
          IF(OPTBAN.EQ.1) THEN
            CALL FILTER(T2_01,.TRUE.,T2_02,T2_03,
     &                  MAT2D%ADR(1)%P,'MATMAS          ',
     &                  1.D0,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                  MESH2D,MSK,MASKEL,2)
          ENDIF
!
!         3) PUTS BACK IN H THE SMOOTHED NEGATIVE VALUES
!
          CALL OS( 'X=X+Y   ' , X=H , Y=T2_01 )
!
        ENDIF
!
      ENDIF
!
! CLIPS H AND COMPUTES Z
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP(H,HMIN,.TRUE., 1.D6, .FALSE., 0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
