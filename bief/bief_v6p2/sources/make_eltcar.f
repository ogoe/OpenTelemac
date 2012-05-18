!                    **********************
                     SUBROUTINE MAKE_ELTCAR
!                    **********************
!
     &(ELTCAR,IKLE,NPOIN2,NELEM2,NELMAX,KNOLG,SCORE,ISCORE,MESH,NPLAN,
     & IELM)
!
!***********************************************************************
! BIEF   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    For every point in the mesh, gives an element that contains
!+        this point. This element must be the same in scalar and in
!+        parallel mode, ELTCAR(I)=0 means that the element is in another 
!+        sub-domain.
!
!note     In every element a point is followed by another one:
!+        2 follows 1, 3 follows 2, 1 follows 3
!+        For every point we choose the element where the next point 
!+        has the higher rank.
!+        With quadratic interpolation, the next linear point is taken
!+        2 follows 4, 3 follows 5, 1 follows 6
!+        The case of quasi-bubble is obvious and not treated here: the
!+        point is in the middle of an element, so no problem of choice
!
!history  C. DENIS (SINETICS, EDF R&D), J-M HERVOUET (LNHE, EDF R&D)
!+        27/04/2012
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTCAR         |<--| ELEMENT CHOSEN FOR EVERY POINT
!| IELM           |-->| TYPE OF ELEMENT (11: TRIANGLE, 41: PRISM...)
!| IKLE           |-->| CONNECTIVITY TABLE
!| ISCORE         |<->| INTEGER WORK ARRAY
!| KNOLG          |-->| GLOBAL NUMBER OF POINTS IN ORIGINAL MESH
!| MESH           |-->| MESH STRUCTURE
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPLAN          |-->| NUMBER OF PLANES (CASE OF A 3D MESH, OR 1 IN 2D)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SCORE          |<->| DOUBLE PRECISION WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    
      USE BIEF_DEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NELEM2,NELMAX,NPLAN,IELM
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*),KNOLG(NPOIN2)
      INTEGER, INTENT(INOUT)          :: ELTCAR(*)
      INTEGER, INTENT(INOUT)          :: ISCORE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: SCORE(*)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IELEM,N1,N2,N3,N4,N5,N6,IPLAN,NP
!
      IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41) THEN
        NP=NPOIN2
      ELSEIF(IELM.EQ.13) THEN
        NP=NPOIN2+MESH%NSEG
      ELSE
        WRITE(LU,*) 'MAKE_ELTCAR NOT PROGRAMMED FOR TETRAHEDRA'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO I=1,NP
        ISCORE(I)=0
      ENDDO
!        
      IF(NCSIZE.LE.1) THEN
!
!       SIMPLE CASE: SCALAR MODE
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41) THEN
! 
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).LT.N2) THEN 
              ISCORE(N1)=N2
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.N3) THEN 
              ISCORE(N2)=N3
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.N1) THEN 
              ISCORE(N3)=N1
              ELTCAR(N3)=IELEM
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!        
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).LT.N2) THEN 
              ISCORE(N1)=N2
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.N3) THEN 
              ISCORE(N2)=N3
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.N1) THEN 
              ISCORE(N3)=N1
              ELTCAR(N3)=IELEM
            ENDIF
            IF(ISCORE(N4).LT.N2) THEN 
              ISCORE(N4)=N2
              ELTCAR(N4)=IELEM
            ENDIF 
            IF(ISCORE(N5).LT.N3) THEN 
              ISCORE(N5)=N3
              ELTCAR(N5)=IELEM
            ENDIF
            IF(ISCORE(N6).LT.N1) THEN 
              ISCORE(N6)=N1
              ELTCAR(N6)=IELEM
            ENDIF             
          ENDDO
!
        ENDIF
!
      ELSE
!
!       NOW IN PARALLEL, FIRST LIKE IN SCALAR BUT WITH GLOBAL NUMBERS
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41) THEN
! 
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).LT.KNOLG(N2)) THEN 
              ISCORE(N1)=KNOLG(N2)
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.KNOLG(N3)) THEN 
              ISCORE(N2)=KNOLG(N3)
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.KNOLG(N1)) THEN 
              ISCORE(N3)=KNOLG(N1)
              ELTCAR(N3)=IELEM
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).LT.KNOLG(N2)) THEN 
              ISCORE(N1)=KNOLG(N2)
              ELTCAR(N1)=IELEM
            ENDIF
            IF(ISCORE(N2).LT.KNOLG(N3)) THEN 
              ISCORE(N2)=KNOLG(N3)
              ELTCAR(N2)=IELEM
            ENDIF
            IF(ISCORE(N3).LT.KNOLG(N1)) THEN 
              ISCORE(N3)=KNOLG(N1)
              ELTCAR(N3)=IELEM
            ENDIF
            IF(ISCORE(N4).LT.KNOLG(N2)) THEN 
              ISCORE(N4)=KNOLG(N2)
              ELTCAR(N4)=IELEM
            ENDIF 
            IF(ISCORE(N5).LT.KNOLG(N3)) THEN 
              ISCORE(N5)=KNOLG(N3)
              ELTCAR(N5)=IELEM
            ENDIF
            IF(ISCORE(N6).LT.KNOLG(N1)) THEN 
              ISCORE(N6)=KNOLG(N1)
              ELTCAR(N6)=IELEM
            ENDIF             
          ENDDO
!
        ENDIF
!        
!       DOUBLE PRECISION FOR CALLING PARCOM2
        DO I=1,NP
          SCORE(I)=ISCORE(I)
        ENDDO
!       LARGEST VALUE BETWEEN NEIGHBOURING SUB-DOMAINS TAKEN
        CALL PARCOM2(SCORE,SCORE,SCORE,NPOIN2,1,1,1,MESH)
        IF(IELM.EQ.13) THEN
          CALL PARCOM2_SEG(SCORE(NPOIN2+1:NP),
     &                     SCORE(NPOIN2+1:NP),
     &                     SCORE(NPOIN2+1:NP),
     &                     MESH%NSEG,1,1,1,MESH,1,11)
        ENDIF
!       BACK TO INTEGERS
        DO I=1,NP
          ISCORE(I)=NINT(SCORE(I))
        ENDDO
!
        IF(IELM.EQ.11.OR.IELM.EQ.12.OR.IELM.EQ.41) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            IF(ISCORE(N1).EQ.KNOLG(N2)) THEN 
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N1)=0
            ENDIF          
            IF(ISCORE(N2).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N2)=0
            ENDIF          
            IF(ISCORE(N3).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N3)=0
            ENDIF
          ENDDO
!
        ELSEIF(IELM.EQ.13) THEN
!
          DO IELEM = 1,NELEM2
            N1=IKLE(IELEM,1)
            N2=IKLE(IELEM,2)
            N3=IKLE(IELEM,3)
            N4=IKLE(IELEM,4)
            N5=IKLE(IELEM,5)
            N6=IKLE(IELEM,6)
            IF(ISCORE(N1).EQ.KNOLG(N2)) THEN 
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N1)=0
            ENDIF          
            IF(ISCORE(N2).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N2)=0
            ENDIF          
            IF(ISCORE(N3).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N3)=0
            ENDIF
            IF(ISCORE(N4).EQ.KNOLG(N2)) THEN 
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N4)=0
            ENDIF          
            IF(ISCORE(N5).EQ.KNOLG(N3)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N5)=0
            ENDIF          
            IF(ISCORE(N6).EQ.KNOLG(N1)) THEN
!             THERE IS NO BETTER ELEMENT IN ANOTHER SUB-DOMAIN
              ISCORE(N6)=0
            ENDIF
          ENDDO
!
        ENDIF
!
!       IF A POINT HAS A BETTER ELEMENT IN ANOTHER SUB-DOMAIN
        DO I=1,NP
          IF(ISCORE(I).NE.0) THEN
            ELTCAR(I)=0
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPLETING FOR 3D PRISMS
!
!     NOTE: THIS PART IS NOT USED SO FAR BY SUBROUTINE GTSH41
!
      IF(NPLAN.GT.1) THEN
        DO IPLAN=2,NPLAN
          DO I=1,NPOIN2
!           ACCORDING TO POINT AND ELEMENT NUMBERING IN PRISMS
            ELTCAR(I+(IPLAN-1)*NPOIN2)=ELTCAR(I)+(IPLAN-1)*NELEM2
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
