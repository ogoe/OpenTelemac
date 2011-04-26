!                    ****************
                     SUBROUTINE ELEBD
!                    ****************
!
     &(NELBOR,NULONE,KP1BOR,IFABOR,NBOR,IKLE,SIZIKL,IKLBOR,NELEM,NELMAX,
     & NPOIN,NPTFR,IELM,LIHBOR,KLOG,IFANUM,OPTASS,ISEG,T1,T2,T3)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDING DATA STRUCTURES TO NAVIGATE IN A 2D MESH.
!+
!+            1) ARRAYS NELBOR AND NULONE,
!+
!+            2) ARRAY KP1BOR,
!+
!+            3) DISTINGUISHES IN THE ARRAY IFABOR FOR
!+                   SOLID BOUNDARY FACES OR LIQUID FACES,
!+
!+            4) IKLBOR, CONNECTIVITY TABLE OF BOUNDARY ELEMENTS
!
!history  J-M HERVOUET (LNHE)
!+        23/06/2008
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
!| IELM           |-->| TYPE D'ELEMENT.
!|                |   | 11 : TRIANGLES.
!|                |   | 21 : QUADRILATERES.
!| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
!| IFANUM         |---|
!| IKLBOR         |---|
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| ISEG           |---|
!| KLOG           |-->| CONVENTION POUR LA CONDITION LIMITE DE PAROI
!| KP1BOR         |<--| NUMERO DU POINT SUIVANT LE POINT DE BORD K.
!| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
!| NBOR           |-->| NUMERO GLOBAL DU POINT DE BORD K.
!| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |---|
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
!| OPTASS         |---|
!| SIZIKL         |---|
!| T2             |---|
!| T3             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ELEBD => ELEBD
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: KLOG,NELMAX,NELEM,SIZIKL
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM,OPTASS
      INTEGER, INTENT(OUT)   :: NELBOR(NPTFR),NULONE(NPTFR,2)
      INTEGER, INTENT(OUT)   :: KP1BOR(NPTFR,2)
      INTEGER, INTENT(INOUT) :: NBOR(*)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*)
      INTEGER, INTENT(IN)    :: IKLE(SIZIKL,*)
      INTEGER, INTENT(IN)    :: LIHBOR(NPTFR)
      INTEGER, INTENT(OUT)   :: IKLBOR(NPTFR,2)
      INTEGER, INTENT(INOUT) :: IFANUM(NELMAX,*)
      INTEGER, INTENT(IN)    :: ISEG(NPTFR)
      INTEGER, INTENT(OUT)   :: T1(NPOIN),T2(NPOIN),T3(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,NFACE,NPT,KEL,IPOIN
      INTEGER K,IFACE,I1,I2,N1,N2,IPT,IEL,I,K1,K2
!
      INTEGER SOMFAC(2,4,2)
!
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!       TRIANGLES
        NFACE = 3
        NPT = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,900) IELM
        IF(LNG.EQ.2) WRITE(LU,901) IELM
900     FORMAT(1X,'ELEBD : IELM=',1I6,' TYPE D''ELEMENT INCONNU')
901     FORMAT(1X,'ELEBD: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  INITIALISES T1,2,3 TO 0
!
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
        T2(IPOIN) = 0
        T3(IPOIN) = 0
      ENDDO
!
!  STORES K IN TRAV(*,3), ADDRESS NBOR(K)
!  GIVES CORRESPONDENCE GLOBAL --> BOUNDARY NUMBER
!
      DO K = 1, NPTFR
        T3(NBOR(K)) = K
      ENDDO
!
!  LOOP ON ALL THE FACES OF ALL THE ELEMENTS:
!
      DO 20 IFACE = 1 , NFACE
      DO 10 IELEM = 1 , NELEM
!
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
!      THIS IS A TRUE BOUNDARY FACE (INTERNAL FACES ARE MARKED WITH -2
!                                    IN PARALLELE MODE)
!      GLOBAL NUMBERS OF THE FACE POINTS :
!
       I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
       I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
!      STORES IN T1 AND T2 (ADDRESS I1) : I2 AND IELEM
!
       T1(I1) = I2
       T2(I1) = IELEM
!
!      A LIQUID FACE IS RECOGNIZED BY THE BOUNDARY CONDITION ON H
!
       IF(NPTFR.GT.0) THEN
       IF(LIHBOR(T3(I1)).NE.KLOG.AND.LIHBOR(T3(I2)).NE.KLOG) THEN
!        LIQUID FACE : IFABOR=0  SOLID FACE : IFABOR=-1
         IFABOR(IELEM,IFACE)=0
       ENDIF
       ENDIF
!
      ENDIF
!
10    CONTINUE
20    CONTINUE
!
!     LOOP ON ALL THE POINTS:
!
      IF(NPTFR.GT.0) THEN
        DO I = 1 , NPOIN
          IF(T1(I).NE.0) THEN
!           FOLLOWING POINT
            KP1BOR(T3(I),1)=T3(T1(I))
!           PRECEDING POINT
            KP1BOR(T3(T1(I)),2)=T3(I)
            NELBOR(T3(I))=T2(I)
          ENDIF
        ENDDO
      ENDIF
!
!     DUMMY VALUES IN KP1BOR WHEN THE FOLLOWING POINT IS IN ANOTHER SUB-DOMAIN
!     NELBOR AND NULONE SET TO 0
!
      IF(NCSIZE.GT.1) THEN
        DO 49 K1=1,NPTFR
          IF(ISEG(K1).GT.0) THEN
            KP1BOR(K1,1)=K1
            NELBOR(K1)=0
            NULONE(K1,1)=0
            NULONE(K1,2)=0
          ELSEIF(ISEG(K1).EQ.-9999) THEN
            KP1BOR(K1,1)=K1
            KP1BOR(K1,2)=K1
            NELBOR(K1)  =0
            NULONE(K1,1)=0
            NULONE(K1,2)=0
          ELSEIF(ISEG(K1).LT.0) THEN
            KP1BOR(K1,2)=K1
          ENDIF
49      CONTINUE
      ENDIF
!
! COMPUTES ARRAY NULONE
!
      DO 50 K1=1,NPTFR
!
      IF(NCSIZE.GT.1) THEN
        IF(ISEG(K1).GT.0.OR.ISEG(K1).EQ.-9999) GO TO 50
      ENDIF
!
      K2=KP1BOR(K1,1)
      IEL = NELBOR(K1)
      N1  = NBOR(K1)
      N2  = NBOR(K2)
!
      I1 = 0
      I2 = 0
!
      DO IPT=1,NPT
        IF(IKLE(IEL,IPT).EQ.N1) THEN
          NULONE(K1,1) = IPT
          I1 = 1
        ENDIF
        IF(IKLE(IEL,IPT).EQ.N2) THEN
          NULONE(K1,2) = IPT
          I2 = 1
        ENDIF
      ENDDO
!
      IF(I1.EQ.0.OR.I2.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,810) IEL
        IF(LNG.EQ.2) WRITE(LU,811) IEL
810     FORMAT(1X,'ELEBD: ERREUR DE NUMEROTATION DE L''ELEMENT:' ,I10,/,
     &         1X,'       CAUSE POSSIBLE :                       '   ,/,
     &         1X,'       LE FICHIER DES CONDITIONS AUX LIMITES NE'  ,/,
     &         1X,'       CORRESPOND PAS AU FICHIER DE GEOMETRIE  ')
811     FORMAT(1X,'ELEBD: ERROR OF NUMBERING IN THE ELEMENT:',    I10,/,
     &         1X,'       POSSIBLE REASON:                       '   ,/,
     &         1X,'       THE BOUNDARY CONDITION FILE IS NOT      '  ,/,
     &         1X,'       RELEVANT TO THE GEOMETRY FILE           ')
        CALL PLANTE(1)
        STOP
      ENDIF
!
50    CONTINUE
!
!  COMPUTES IKLBOR : LIKE IKLE FOR BOUNDARY POINTS, WITH BOUNDARY
!                    POINTS NUMBERING
!
      DO K=1,NPTFR
        IKLBOR(K,1) = K
        IKLBOR(K,2) = KP1BOR(K,1)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
