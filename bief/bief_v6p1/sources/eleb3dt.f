!                    ******************
                     SUBROUTINE ELEB3DT
!                    ******************
!
     &(IKLE3,NBOR,KP1BOR,NELBOR,IKLBOR,NULONE,
     & NELEM2,NELMAX2,NPOIN2,NPLAN,NETAGE,NPTFR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CASE OF PRISMS SPLIT IN TETRAHEDRONS.
!+                BUILDS THE 3D MESH.
!+
!+            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!+                       TO ELEBD.
!+
!+            OUTPUT: ARRAYS COMPLETE IN 3D.
!
!history  J-M HERVOUET(LNH)
!+        23/08/99
!+        V5P3
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBOR         |<--| CONNECTIVITY TABLE FOR BOUNDARY ELEMENTS
!| IKLE3          |<--| CONNECTIVITY TABLE IN 3D
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS IN 2D
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX2        |-->| MAXIMUM NUMBER OF ELEMENTS IN 2D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE 
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEB3DT => ELEB3DT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR,NELMAX2
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,3,NETAGE,4)
      INTEGER, INTENT(INOUT) :: IKLBOR(NPTFR,2,NETAGE,3)
      INTEGER, INTENT(INOUT) :: NULONE(NPTFR,2,NETAGE,3)
      INTEGER, INTENT(INOUT) :: NELBOR(NPTFR,2,NETAGE),NBOR(NPTFR*NPLAN)
      INTEGER, INTENT(INOUT) :: KP1BOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL OK(2)
!
      INTEGER IELEM,IPOIN,T(3)
      INTEGER IETAGE,IPTFR,IL1,IL2,IL3,IL4,IG(2,2,3),IL(2,2,3)
      INTEGER IG1,IG2,IG3,IG4
      INTEGER NUM1(12),NUM2(12),NUM3(12),K,L,M,N
!
      DATA NUM1 / 1 , 2 , 4 , 1 , 3 , 2 , 2 , 3 , 4 , 3 , 1 , 4 /
      DATA NUM2 / 2 , 4 , 1 , 3 , 2 , 1 , 3 , 4 , 2 , 1 , 4 , 3 /
      DATA NUM3 / 4 , 1 , 2 , 2 , 1 , 3 , 4 , 2 , 3 , 4 , 3 , 1 /
!
!***********************************************************************
!
! CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR3 ,
! CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS AND 3D LOCAL NUMBERS --> NULONE
!
! COMPUTES NELBO3
!
! LATERAL BOUNDARIES :
! FOR EACH RECTANGULAR FACE SPLIT IN TWO TRIANGLES
! THE LOWER TRIANGLE IS NUMBER 1, THE HIGHER IS NUMBER 2
!
      DO IPTFR = 1,NPTFR
!
!        NUMBER OF THE TRIANGLE TOUCHING THE BOUNDARY IN 2D
         IELEM = NELBOR(IPTFR,1,1)
!
!        GLOBAL NUMBER OF BOTTOM-LEFT POINT OF THE RECTANGULAR FACE
!        IN THE FIRST LAYER (THE PATTERN REPEATS ITSELF THEREAFTER)
         IPOIN = NBOR(IPTFR)
!
         DO IETAGE = 1,NETAGE
!
!           3D BOUNDARY NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
            IL1 =       IPTFR   + (IETAGE-1)*NPTFR
            IL2 = KP1BOR(IPTFR) + (IETAGE-1)*NPTFR
            IL3 = IL2 + NPTFR
            IL4 = IL1 + NPTFR
!
!           3D GLOBAL NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
            IG1 =        NBOR(IPTFR)  + (IETAGE-1)*NPOIN2
            IG2 = NBOR(KP1BOR(IPTFR)) + (IETAGE-1)*NPOIN2
            IG3 = IG2 + NPOIN2
            IG4 = IG1 + NPOIN2
!
! NUMBERS OF THE 3 TETRAHEDRONS POSSIBLY TOUCHING THE FACE
!
            T(1) = (IETAGE-1)*3*NELEM2+IELEM
            T(2) = T(1) + NELEM2
            T(3) = T(2) + NELEM2
!
! LOOKS FOR THE LOWER TRIANGLE (CAN BE 1-2-4 OR 1-2-3)
!
!           2 POSSIBLE FORMS OF THE LOWER TRIANGLE (GLOBAL AND BOUNDARY)
            IG(1,1,1)=IG1
            IG(1,1,2)=IG2
            IG(1,1,3)=IG4
            IG(1,2,1)=IG1
            IG(1,2,2)=IG2
            IG(1,2,3)=IG3
            IL(1,1,1)=IL1
            IL(1,1,2)=IL2
            IL(1,1,3)=IL4
            IL(1,2,1)=IL1
            IL(1,2,2)=IL2
            IL(1,2,3)=IL3
!           2 POSSIBLE FORMS OF THE HIGHER TRIANGLE (GLOBAL AND BOUNDARY)
            IG(2,1,1)=IG1
            IG(2,1,2)=IG3
            IG(2,1,3)=IG4
            IG(2,2,1)=IG2
            IG(2,2,2)=IG3
            IG(2,2,3)=IG4
            IL(2,1,1)=IL1
            IL(2,1,2)=IL3
            IL(2,1,3)=IL4
            IL(2,2,1)=IL2
            IL(2,2,2)=IL3
            IL(2,2,3)=IL4
!
            OK(1)=.FALSE.
            OK(2)=.FALSE.
!
!           K=1 LOWER TRIANGLE   K=2 HIGHER TRIANGLE
            DO K=1,2
!           2 POSSIBLE SPLITS
            DO L=1,2
!           12 WAYS FOR A TETRAHEDRON OF PRESENTING ITS FACES
            DO M=1,12
!           3 POSSIBLE TETRAHEDRONS
            DO N=1,3
              IF(IG(K,L,1).EQ.IKLE3(IELEM,N,IETAGE,NUM1(M)).AND.
     &           IG(K,L,2).EQ.IKLE3(IELEM,N,IETAGE,NUM2(M)).AND.
     &           IG(K,L,3).EQ.IKLE3(IELEM,N,IETAGE,NUM3(M))) THEN
!
                  IKLBOR(IPTFR,K,IETAGE,1) = IL(K,L,1)
                  IKLBOR(IPTFR,K,IETAGE,2) = IL(K,L,2)
                  IKLBOR(IPTFR,K,IETAGE,3) = IL(K,L,3)
                  NELBOR(IPTFR,K,IETAGE)   = T(N)
                  NULONE(IPTFR,K,IETAGE,1) = NUM1(M)
                  NULONE(IPTFR,K,IETAGE,2) = NUM2(M)
                  NULONE(IPTFR,K,IETAGE,3) = NUM3(M)
!
                  OK(K) = .TRUE.
!
              ENDIF
            ENDDO
            ENDDO
            ENDDO
            ENDDO
            IF(.NOT.OK(1).OR..NOT.OK(2)) THEN
            WRITE(LU,*) 'PB IN ELEB3DT IELEM=',IELEM,' IPTFR=',IPTFR
            CALL PLANTE(1)
            STOP
            ENDIF
!
!           GLOBAL NUMBERS OF THE LATERAL BOUNDARY POINTS
!           ALL THE PLANES EXCEPT SURFACE
            NBOR(IPTFR +(IETAGE-1)*NPTFR)=IPOIN+(IETAGE-1)*NPOIN2
!
         ENDDO
      ENDDO
!
! GLOBAL NUMBERS OF THE LATERAL BOUNDARY POINTS: ON THE SURFACE
!
      DO IPTFR = 1,NPTFR
         NBOR(IPTFR+(NPLAN-1)*NPTFR)=NBOR(IPTFR)+(NPLAN-1)*NPOIN2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
