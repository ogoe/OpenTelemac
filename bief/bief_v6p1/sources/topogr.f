C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINELY ANALYSES THE TOPOGRAPHY AND BUILDS ZFE.
!><br>            THE ARRAY OF BOTTOM ELEVATIONS BY ELEMENTS: ZFE
!>                WILL ENSURE IN THE FUTURE THAT THERE WILL NOT BE
!>                LIQUID DOMAINS CONNECTED BY A SINGLE NODE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IFABOR, IKLE, ITRA02, ITRA03, ITRA05, MXPTVS, NBOR, NELBOR, NELEM, NPOIN, NPTFR, NULONE, ZF, ZFE, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSILO, ERR, FLAG, I, I1, I2, I3, IELEM, IFAN, IMAX, IPOIN, IPREV, IPTFR, ITRA01, ITRA04, N1, N2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 17/08/94
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMERO DES ELEMENTS VOISINS
!>    </td></tr>
!>          <tr><td>IFAN
!></td><td>---</td><td>NUMERO LOCAL DE LA FACE DANS L'ELEMENT VOISIN
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>ITRA01
!></td><td>---</td><td>TABLEAUX DE TRAVAIL D'ENTIERS
!>    </td></tr>
!>          <tr><td>ITRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITRA05
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS VOISINS D'UN POINT
!>                  (A VERIFIER, LE NOMBRE D'ELEMENTS SUFFIT
!>                  VRAISEMBLABLEMENT)
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAL DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>NUMERO LOCAL DES NOEUDS AU BORD
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND AUX NOEUDS
!>    </td></tr>
!>          <tr><td>ZFE
!></td><td><--</td><td>COTE DU FOND PAR ELEMENT
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>CORRECTIF DE ZFE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TOPOGR
     &(ZF,ZREF,ZFE,IKLE,IFABOR,NBOR,NELBOR,NULONE,
     & ITRA05,ITRA02,ITRA03,NELEM,NPTFR,NPOIN,MXPTVS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IFABOR         |-->| NUMERO DES ELEMENTS VOISINS
C| IFAN           |---| NUMERO LOCAL DE LA FACE DANS L'ELEMENT VOISIN
C| IKLE           |-->| TABLE DE CONNECTIVITE
C| ITRA01         |---| TABLEAUX DE TRAVAIL D'ENTIERS
C| ITRA02         |---| 
C| ITRA03         |---| 
C| ITRA05         |---| 
C| MXPTVS         |-->| NOMBRE MAXIMUM DE POINTS VOISINS D'UN POINT
C|                |   | (A VERIFIER, LE NOMBRE D'ELEMENTS SUFFIT
C|                |   | VRAISEMBLABLEMENT)
C| NBOR           |-->| NUMERO GLOBAL DES POINTS DE BORD
C| NELBOR         |-->| NUMERO DES ELEMENTS DE BORD
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE POINTS
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NULONE         |-->| NUMERO LOCAL DES NOEUDS AU BORD
C| ZF             |-->| COTE DU FOND AUX NOEUDS
C| ZFE            |<--| COTE DU FOND PAR ELEMENT
C| ZREF           |---| CORRECTIF DE ZFE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM,NPTFR,NPOIN,MXPTVS
      INTEGER, INTENT(IN)    :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(IN)    :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT) :: ITRA05(NPOIN),ITRA02(NPOIN)
      INTEGER, INTENT(INOUT) :: ITRA03(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFE(NELEM),ZREF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IPTFR,IPOIN,I,I1,I2,I3,N1,N2,ERR,IPREV(3),IMAX
      DOUBLE PRECISION EPSILO
      LOGICAL FLAG
C
C     DYNAMICALLY ALLOCATES INTEGERS
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRA01,ITRA04,IFAN
C
      DATA IPREV  / 3 , 1 , 2 /
      DATA EPSILO / 1.D-6 /
C
C-----------------------------------------------------------------------
C
      ALLOCATE(IFAN(NELEM,3)          ,STAT=ERR)
      ALLOCATE(ITRA01(NPOIN,MXPTVS+1) ,STAT=ERR)
      ALLOCATE(ITRA04(NPOIN,6)        ,STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'TOPOGR : MAUVAISE ALLOCATION DE W'
        IF(LNG.EQ.2) WRITE(LU,*) 'TOPOGR: WRONG ALLOCATION OF W'
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C    FILLS IN IFAN
C
C   IFAN(IELEM,IFACE) GIVES THE LOCAL NUMBER IN THE ELEMENT DEFINED
C   BY IFABOR(IELEM,IFACE) OF THE NODE WITH LOCAL NUMBER IFACE IN
C   ELEMENT IELEM.
C
C    FIRST GO AT FILLING ZFE
C
C    STARTS TO FILL IN ITRA01 AND ITRA02
C
C   ITRA01 AND ITRA02 PERFORM THE REVERSE OPERATION FROM IKLE.
C   IN THIS LOOP ITRA01(IPOIN,1) IS FILLED IN. IT GIVES THE BIGGEST
C   ELEMENT NUMBER CONTAINING IPOIN. ITRA02(IPOIN) GIVES THE LOCAL
C   NUMBER OF IPOIN IN THIS ELEMENT.
C
C-----------------------------------------------------------------------
C
C>>>>
C  LOOPS 5 AND 6 ADDED BY JMH 22/5/95
C  INITIALISES ITRA01 TO DETECT HOLES IN THE MESH
C  (POINTS NOT LINKED TO AN ELEMENT, CASE OF CURVILINEAR MESH)
C
      DO 6 I1    = 1 , MXPTVS+1
      DO 5 IPOIN = 1 , NPOIN
         ITRA01(IPOIN,I1) = 0
5     CONTINUE
6     CONTINUE
C
C<<<<
C
      DO 10 IELEM = 1,NELEM
C
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
C
         IFAN(IELEM,1) = 0
         N1 = IFABOR(IELEM,1)
         IF (N1.GT.0) THEN
            IF(IKLE(N1,1).EQ.I1) IFAN(IELEM,1) = 3
            IF(IKLE(N1,2).EQ.I1) IFAN(IELEM,1) = 1
            IF(IKLE(N1,3).EQ.I1) IFAN(IELEM,1) = 2
         ENDIF
C
         IFAN(IELEM,2) = 0
         N1 = IFABOR(IELEM,2)
         IF (N1.GT.0) THEN
            IF(IKLE(N1,1).EQ.I2) IFAN(IELEM,2) = 3
            IF(IKLE(N1,2).EQ.I2) IFAN(IELEM,2) = 1
            IF(IKLE(N1,3).EQ.I2) IFAN(IELEM,2) = 2
         ENDIF
C
         IFAN(IELEM,3) = 0
         N1 = IFABOR(IELEM,3)
         IF (N1.GT.0) THEN
            IF(IKLE(N1,1).EQ.I3) IFAN(IELEM,3) = 3
            IF(IKLE(N1,2).EQ.I3) IFAN(IELEM,3) = 1
            IF(IKLE(N1,3).EQ.I3) IFAN(IELEM,3) = 2
         ENDIF
C
         ZFE(IELEM) = MAX(ZF(I1),ZF(I2),ZF(I3))
         ITRA01(I1,1) = IELEM
         ITRA02(I1)   = 1
         ITRA01(I2,1) = IELEM
         ITRA02(I2)   = 2
         ITRA01(I3,1) = IELEM
         ITRA02(I3)   = 3
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C    STARTS TO FILL IN ITRA01 AND ITRA02 FOR BOUNDARY NODES
C
C   FOR THESE POINTS ITRA01(IPOIN,1) IS NOT ANY ELEMENT: IT'S THE
C   ELEMENT CONTAINING A BOUNDARY SIDE BETWEEN NODE IPOIN AND THE
C   FOLLOWING NODE IN THE LOCAL NUMBERING OF THE ELEMENT.
C
C-----------------------------------------------------------------------
C
      DO 20 IPTFR = 1,NPTFR
         ITRA01(NBOR(IPTFR),1) = NELBOR(IPTFR)
         ITRA02(NBOR(IPTFR))   = NULONE(IPTFR)
20    CONTINUE
C
C-----------------------------------------------------------------------
C
C    RESUMES TO FILL IN ITRA01
C
C   ITRA01(IPOIN,I+1) IS THE NUMBER OF THE ELEMENT ADJACENT TO ELEMENT
C   ITRA01(IPOIN,1) WHEN TURNING ANTI-CLOCKWISE AROUND POINT IPOIN.
C   ITRA02 IS A UNIDIMENSIONAL ARRAY BECAUSE THE LOCAL NUMBER OF IPOIN
C   IN ANY ELEMENT ITRA01(IPOIN,*) WILL NOT NEEDED IN THE FUTURE.
C
C
C    FILLS IN ITRA03
C
C   ITRA03(IPOIN) CORRESPONDS TO THE NUMBER OF ELEMENTS CONTAINING IPOIN
C   (WITH A NEGATIVE SIGN IF IPOIN IS A BOUNDARY NODE).
C
C    BEWARE |||
C
C   ONE POINT CAN ONLY BE PART OF A MAXIMUM OF 10 ELEMENTS
C
C-----------------------------------------------------------------------
C
      DO 30 IPOIN = 1,NPOIN
         ITRA03(IPOIN) = 0
30    CONTINUE
C
      IMAX = 0
C
40    CONTINUE
      FLAG = .FALSE.
      IMAX = IMAX + 1
      IF (IMAX.GT.MXPTVS+1) THEN
        IF(LNG.EQ.1) WRITE(LU,23) MXPTVS
        IF(LNG.EQ.2) WRITE(LU,24) MXPTVS
23      FORMAT(1X,'TOPOGR : LE NOMBRE DE POINTS VOISINS MAXIMUM'/,1X,
     &            '         TROUVE DANS TOPOGR EST SUPERIEUR A ',/,1X,
     &            '         LA VALEUR ANNONCEE MXPTVS :',1I6)
24      FORMAT(1X,'TOPOGR : THE MAXIMUM NUMBER OF NEIGHBOURS TO'/,1X,
     &            '         A POINT IS GREATER THAN THE VALUE  ',/,1X,
     &            '         GIVEN BY MXPTVS :',1I6)
        CALL PLANTE(0)
        STOP
      ENDIF
C
      DO 50 IPOIN = 1,NPOIN
C
         IF (ITRA03(IPOIN).EQ.0) THEN
            N1 = ITRA01(IPOIN,IMAX)
            IF(N1.NE.0) THEN
              FLAG = .TRUE.
              N2 = IFABOR(N1,IPREV(ITRA02(IPOIN)))
C                          HERE IMAX IS NEVER AT ITS MAXIMUM
              ITRA01(IPOIN,IMAX+1) = N2
              ITRA02(IPOIN) = IFAN(N1,IPREV(ITRA02(IPOIN)))
              IF (N2.LE.0)               ITRA03(IPOIN) = -IMAX
              IF (N2.EQ.ITRA01(IPOIN,1)) ITRA03(IPOIN) =  IMAX
            ENDIF
         ENDIF
C
50    CONTINUE
C
      IF (FLAG) GOTO 40
C
60    CONTINUE
C
C-----------------------------------------------------------------------
C
C    DETERMINES LOCAL EXTREMA FOR ZFE BY TURNING AROUND A NODE
C
C   ITRA04(IPOIN,I) CORRESPONDS TO THE IEME EXTREMUM FOUND, WITH THE
C   ASSOCIATED ELEMENT GIVEN BY ITRA01(IPOIN,ITRA04(IPOIN,I)).
C   ITRA02(IPOIN) GIVES THE TOTAL NUMBER OF INCREASE AND DECREASE STAGES
C   (WITH A NEGATIVE SIGN IF THE LAST STAGE FOUND IS A DECREASE).
C
C
C-----------------------------------------------------------------------
C
      DO 70 IPOIN = 1,NPOIN
         ITRA02(IPOIN) = 0
         ITRA05(IPOIN) = 0
70    CONTINUE
C
      DO 80 I = 1,IMAX-1
C
         DO 90 IPOIN = 1,NPOIN
C
            IF (ITRA03(IPOIN).GE.I.OR.ITRA03(IPOIN).LT.-I) THEN
C
               N1 = ITRA01(IPOIN,I)
               N2 = ITRA01(IPOIN,I+1)
C
               IF (ZFE(N2).GT.ZFE(N1)+EPSILO) THEN
                  IF (ITRA02(IPOIN).LT.0) ITRA04(IPOIN,-ITRA02(IPOIN))=I
                  IF (ITRA02(IPOIN).LE.0) ITRA02(IPOIN)=-ITRA02(IPOIN)+1
               ELSEIF (ZFE(N2).LT.ZFE(N1)-EPSILO) THEN
                  IF (ITRA02(IPOIN).GT.0) ITRA04(IPOIN, ITRA02(IPOIN))=I
                  IF (ITRA02(IPOIN).GE.0) ITRA02(IPOIN)=-ITRA02(IPOIN)-1
               ENDIF
C
            ENDIF
C
90       CONTINUE
C
80    CONTINUE
C
      DO 95 IPOIN = 1,NPOIN
         IF((ITRA03(IPOIN).LT.0.AND.(ITRA02(IPOIN).LE.-4.OR.
     &       ITRA02(IPOIN).GE.5)).OR.ABS(ITRA02(IPOIN)).GE.6) THEN
           IF (LNG.EQ.1) THEN
            WRITE(LU,*) 'LE MAILLAGE AUTOUR DU POINT ',IPOIN,' EST TROP'
            WRITE(LU,*) 'GROSSIER PAR RAPPORT A LA BATHYMETRIE'
           ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*) 'THE MESH AROUND THE NODE ',IPOIN,' HAS TO'
            WRITE(LU,*) 'BE REFINED BECAUSE OF THE BATHYMETRY'
           ENDIF
           STOP
         ENDIF
95    CONTINUE
C
C-----------------------------------------------------------------------
C
C    CORRECTS ZFE DEPENDING ON ITRA02
C
C-----------------------------------------------------------------------
C
      FLAG = .FALSE.
C
      DO 100 IPOIN = 1,NPOIN
C
         I1 = ITRA03(IPOIN)
C
         IF (I1.LT.0) THEN
C
C-----------------------------------------------------------------------
C
C    CORRECTS ZFE FOR BOUNDARY NODES
C
C   IF ITRA02(IPOIN) EQUALS 1, -1, 2 : NO CORRECTION
C   IF ITRA02(IPOIN) EQUALS -2, 3, -3, 4 : CORRECTION
C
C-----------------------------------------------------------------------
C
            IF (ITRA02(IPOIN).EQ.-2) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,-I1)).GT.ZFE(ITRA01(IPOIN,1))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
                  ITRA05(IPOIN) = -I1
               ELSE
                  ITRA02(IPOIN) = 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
               ENDIF
               ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))
C
            ELSEIF (ITRA02(IPOIN).EQ.3) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))).GT.
     &             ZFE(ITRA01(IPOIN,1))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
                  ITRA05(IPOIN) = -I1
               ELSE
                  ITRA02(IPOIN) = 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
               ENDIF
               ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))
C
            ELSEIF (ITRA02(IPOIN).EQ.-3) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,-I1)).GT.
     &             ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
                  ITRA05(IPOIN) = -I1
               ELSE
                  ITRA02(IPOIN) = 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
               ENDIF
               ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))
C
            ELSEIF (ITRA02(IPOIN).EQ.4) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &             ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
                  ITRA05(IPOIN) = -I1
               ELSE
                  ITRA02(IPOIN) = 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
               ENDIF
               ZREF(IPOIN) = ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))
C
            ENDIF
C
         ELSE
C
C-----------------------------------------------------------------------
C
C    CORRECTS ZFE FOR INTERIOR NODES
C
C   IF ITRA02(IPOIN) EQUALS 1, -1, 2, -2, 3, -3 : NO CORRECTION
C   IF ITRA02(IPOIN) EQUALS 4, -4, 5, -5 : CORRECTION
C
C-----------------------------------------------------------------------
C
            IF (ITRA02(IPOIN).EQ.4) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &             ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
                  ITRA05(IPOIN) = I1
               ELSE
                  ITRA02(IPOIN) = 2
                  ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
               ENDIF
               ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))),
     &                           ZFE(ITRA01(IPOIN,1)))
C
            ELSEIF (ITRA02(IPOIN).EQ.-4) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))).GT.
     &             ZFE(ITRA01(IPOIN,1))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,3) - 1
               ELSE
                  ITRA02(IPOIN) = MOD(ITRA04(IPOIN,3),I1) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
               ENDIF
               ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1))),
     &                           ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))))
C
            ELSEIF (ITRA02(IPOIN).EQ.5) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,4))).GT.
     &             ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2)))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,3) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,1) - 1
               ELSE
                  ITRA02(IPOIN) = ITRA04(IPOIN,1) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,3) - 1
               ENDIF
               ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1))),
     &                           ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))))
C
            ELSEIF (ITRA02(IPOIN).EQ.-5) THEN
C
               FLAG = .TRUE.
               IF (ZFE(ITRA01(IPOIN,ITRA04(IPOIN,3))).GT.
     &             ZFE(ITRA01(IPOIN,ITRA04(IPOIN,1)))) THEN
                  ITRA02(IPOIN) = ITRA04(IPOIN,2) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,4) - 1
               ELSE
                  ITRA02(IPOIN) = MOD(ITRA04(IPOIN,4),I1) + 1
                  ITRA05(IPOIN) = ITRA04(IPOIN,2) - 1
               ENDIF
               ZREF(IPOIN) = MIN(ZFE(ITRA01(IPOIN,ITRA04(IPOIN,2))),
     &                           ZFE(ITRA01(IPOIN,ITRA04(IPOIN,4))))
C
            ENDIF
C
         ENDIF
C
100   CONTINUE
C
      IF (FLAG) THEN
C
         DO 110 IPOIN = 1,NPOIN
C
            IF (ITRA05(IPOIN).NE.0) THEN
C
               IF (ITRA05(IPOIN).LT.ITRA02(IPOIN)) THEN
                  DO 120 I = ITRA02(IPOIN),ITRA03(IPOIN)
                     ZFE(ITRA01(IPOIN,I)) = MAX(ZFE(ITRA01(IPOIN,I)),
     &                                          ZREF(IPOIN))
120               CONTINUE
                  ITRA02(IPOIN) = 1
               ENDIF
C
               DO 130 I = ITRA02(IPOIN),ITRA05(IPOIN)
                  ZFE(ITRA01(IPOIN,I)) = MAX(ZFE(ITRA01(IPOIN,I)),
     &                                       ZREF(IPOIN))
130            CONTINUE
C
            ENDIF
C
110      CONTINUE
C
         GOTO 60
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(IFAN)
      DEALLOCATE(ITRA01)
      DEALLOCATE(ITRA04)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C