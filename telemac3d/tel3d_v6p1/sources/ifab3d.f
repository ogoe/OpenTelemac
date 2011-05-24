!                    *****************
                     SUBROUTINE IFAB3D
!                    *****************
!
     &(IFABOR,LIUBOF,LIUBOL,LIUBOS,KP1BOR,NELBOR,NULONE,
     & IKLE2,NELEM2,NPOIN2,NPTFR,NPLAN,NPLINT,NETAGE,KLOG,TRANSF)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FILLS 'IFABOR' OF MESH3D.
!
!warning  WITH TIDAL FLATS, WILL BE MODIFIED BY MASK3D
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN 95 VERSION
!
!history  J-M HERVOUET (LNHE)     ; J-M JANIN (LNH)    ; F LEPEINTRE (LNH)
!+        22/07/2008
!+        V5P9
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
!| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!| IKLE2          |-->| CORRESPONDANCE LOCALE - GLOGALE EN 2D
!| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
!| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!| LIUBOL         |---|
!| LIUBOS         |---|
!| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
!| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
!| NETAGE         |-->| NPLAN - 1
!| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
!| NPLINT         |-->| NUMERO DU PLAN INTERMEDIAIRE
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
!| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!|                |   | NUMEROTATION LOCALE 3D
!| TRANSF         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NELEM2,NPOIN2,NETAGE,NPLAN,TRANSF
      INTEGER, INTENT(IN)          :: NPTFR,NPLINT,KLOG
      INTEGER, INTENT(INOUT)       :: IFABOR(NELEM2,5,NETAGE)
      INTEGER, INTENT(IN)          :: LIUBOF(NPOIN2),LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)          :: LIUBOL(NPTFR,NPLAN)
      INTEGER, INTENT(IN)          :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN)          :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(IN)          :: KP1BOR(NPTFR), NELBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2, IETAGE, IPTFR1, IPTFR2, IELBR, IFACE,LOC
      LOGICAL FLAG
!
!=======================================================================
!  IFABOR ARRAYS
!=======================================================================
!
!     SEE CHAR41 FOR THE MEANING OF LOC
!     1 IS : DO NOT RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
!     2 IS : RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
!
!     IF(TRANSF.EQ.1.OR.TRANSF.EQ.2.OR.TRANSF.EQ.3) THEN
!
!     JMH ON 18/02/2010: WITH A FIXED PLANE
!                        THE VELOCITY MUST BE RECOMPUTED
      IF(TRANSF.EQ.1.OR.TRANSF.EQ.2) THEN
        LOC = 1
      ELSEIF(TRANSF.EQ.0.OR.TRANSF.EQ.3.OR.TRANSF.EQ.5) THEN
        LOC = 2
      ELSE
        WRITE(LU,*) 'WRONG VALUE OF TRANSF IN IFAB3D: ',TRANSF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  IFABOR ON THE VERTICAL FACES OF THE ELEMENTS OF THE FIRST LAYER
!
      DO 40 IELEM2 = 1,NELEM2
!        ALREADY DONE BY 2D
!        IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
!        IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
!        IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
         IFABOR(IELEM2,4,1) = LOC
         IFABOR(IELEM2,5,1) = LOC
40    CONTINUE
      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
            IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
            IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
            IFABOR(IELEM2,4,IETAGE) = LOC
            IFABOR(IELEM2,5,IETAGE) = LOC
          ENDDO
        ENDDO
      ENDIF
!
!=======================================================================
!
! TYPES OF BOUNDARY CONDITIONS IMPOSED ON IFABOR
!
!=======================================================================
!
!  IFABOR ON THE LATERAL WALLS
!
      DO IPTFR1 = 1,NPTFR
!
         IPTFR2 = KP1BOR(IPTFR1)
!        IF NEXT POINT IN THE SUBDOMAIN
         IF(IPTFR2.NE.IPTFR1) THEN
           IELBR = NELBOR(IPTFR1)
           IFACE = NULONE(IPTFR1,1,1)
           IF(IFABOR(IELBR,IFACE,1).NE.-2) THEN
             FLAG = .FALSE.
             IF(LIUBOL(IPTFR1,1).NE.KLOG .AND.
     &          LIUBOL(IPTFR2,1).NE.KLOG) FLAG = .TRUE.
             DO IETAGE = 1,NETAGE
               IFABOR(IELBR,IFACE,IETAGE) = -1
               IF(LIUBOL(IPTFR1,IETAGE+1).NE.KLOG .AND.
     &            LIUBOL(IPTFR2,IETAGE+1).NE.KLOG) THEN
                 IF(FLAG) IFABOR(IELBR,IFACE,IETAGE) = 0
                 FLAG = .TRUE.
               ELSE
                 FLAG = .FALSE.
               ENDIF
             ENDDO
           ELSE
             IF(NETAGE.GT.1) THEN
               DO IETAGE = 2,NETAGE
                 IFABOR(IELBR,IFACE,IETAGE) = -2
               ENDDO
             ENDIF
           ENDIF
         ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!  IFABOR ON THE BOTTOM, SURFACE AND EITHER SIDE OF THE INTERMEDIATE PLANE
!
      DO IELEM2 = 1,NELEM2
!
        IFABOR(IELEM2,4,1) = -1
        IF(LIUBOF(IKLE2(IELEM2,1)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,2)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(IELEM2,4,1)=0
!
        IFABOR(IELEM2,5,NETAGE) = -1
        IF(LIUBOS(IKLE2(IELEM2,1)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,2)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(IELEM2,5,NETAGE)=0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
