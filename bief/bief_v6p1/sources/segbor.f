C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DETERMINES THE NUMBER OF BOUNDARY SEGMENTS OF THE MESH
!>               (INCLUDES INTERNAL BOUNDARIES IN PARALLEL MODE).
!><br>            BASED UPON THE PRINCIPLE OF VOISIN,
!>                WHICH WILL BE CALLED LATER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLES, NELEM, NELMAX, NPOIN, NSEGBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR, I, I1, I2, IADR, IDIMAT, IELEM, IELEM2, IFABOR, IFACE, IFACE2, IMAX, IV, KEL, M1, M2, MAT1, MAT2, MAT3, NDP, NFACE, NVOIS, SOMFAC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SEGBOR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 19/06/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>IKLES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>NSEGBOR
!></td><td><--</td><td>NOMBRE DE SEGMENTS DE BORD
!>    </td></tr>
!>          <tr><td>T1,2,3
!></td><td>--></td><td>TABLEAUX DE TRAVAIL ENTIERS.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES ELEMENTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SEGBOR
     &(NSEGBOR,IKLES,NELEM,NELMAX,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| IKLES          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE POINTS
C| NSEGBOR        |<--| NOMBRE DE SEGMENTS DE BORD
C| T1,2,3         |-->| TABLEAUX DE TRAVAIL ENTIERS.
C| X,Y            |-->| COORDONNEES DES ELEMENTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SEGBOR => SEGBOR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN,NELMAX,NELEM
      INTEGER, INTENT(OUT)           :: NSEGBOR
      INTEGER, INTENT(IN)            :: IKLES(3,NELEM)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,ERR,I1,I2,IDIMAT
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C     DYNAMICALLY ALLOCATES WORKING ARRAYS
C
      INTEGER, ALLOCATABLE :: IFABOR(:,:),MAT1(:),MAT2(:),MAT3(:)
      INTEGER, ALLOCATABLE :: NVOIS(:),IADR(:)
C
C-----------------------------------------------------------------------
C
      NFACE = 3
C     NUMBER OF POINTS PER ELEMENT
      NDP = 3
C     ADDRESS IN SOMFAC
      KEL = 1
C
C     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS FOR
C     ALL THE POINTS (NEIGHBOUR = LINKED BY A SEGMENT)
C
      IDIMAT = NDP*2*NELEM
C
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
      ALLOCATE(IFABOR(NELEM,3),STAT=ERR)
      ALLOCATE(NVOIS(NPOIN),STAT=ERR)
      ALLOCATE(IADR(NPOIN),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'SEGBOR : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'SEGBOR: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  COMPUTES THE ARRAY NVOIS FOR EACH POINT
C  BEWARE : NVOIS IS BIGGER THAN THE NUMBER OF NEIGHBOURS
C           THE SUM OF NVOIS FOR ALL THE POINTS WILL GIVE IDIMAT
C
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
C
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLES( SOMFAC(1,IFACE,KEL) , IELEM )
          I2 = IKLES( SOMFAC(2,IFACE,KEL) , IELEM )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C  COMPUTES THE ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE
C  COMPACT MATRIX
C
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
C
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        IF(LNG.EQ.1) WRITE(LU,51) IDIMAT,IMAX
        IF(LNG.EQ.2) WRITE(LU,52) IDIMAT,IMAX
51      FORMAT(1X,'SEGBOR: TAILLE DE MAT1,2,3 (',1I9,') INSUFFISANTE',/,
     &         1X,'IL FAUT AU MOINS : ',1I9)
52      FORMAT(1X,'SEGBOR: SIZE OF MAT1,2,3 (',1I9,') TOO SHORT',/,
     &         1X,'MINIMUM SIZE: ',1I9)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISES THE COMPACT MATRIX TO 0
C
      DO I=1,IMAX
        MAT1(I) = 0
      ENDDO
C
C-----------------------------------------------------------------------
C
C  LOOP ON THE SIDES OF EACH ELEMENT:
C
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
C
         IFABOR(IELEM,IFACE) = 0
C
C        GLOBAL NUMBERS OF THE POINTS OF THE SIDE:
C
         I1 = IKLES( SOMFAC(1,IFACE,KEL) , IELEM )
         I2 = IKLES( SOMFAC(2,IFACE,KEL) , IELEM )
C
C        ORDERED GLOBAL NUMBERS :
C
         M1 = MIN(I1,I2)
         M2 = MAX(I1,I2)
C
         DO 80 IV = 1,NVOIS(M1)
C
           IF(MAT1(IADR(M1)+IV-1).EQ.0) THEN
              MAT1(IADR(M1)+IV-1)=M2
              MAT2(IADR(M1)+IV-1)=IELEM
              MAT3(IADR(M1)+IV-1)=IFACE
              GO TO 81
           ELSEIF(MAT1(IADR(M1)+IV-1).EQ.M2) THEN
              IELEM2 = MAT2(IADR(M1)+IV-1)
              IFACE2 = MAT3(IADR(M1)+IV-1)
              IFABOR(IELEM,IFACE) = IELEM2
              IFABOR(IELEM2,IFACE2) = IELEM
              GO TO 81
           ENDIF
C
80       CONTINUE
C
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'SEGBOR : ERREUR DANS LE MAILLAGE       ',/,1X,
     &             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'SEGBOR : ERROR IN THE MESH             ',/,1X,
     &             '         MAYBE SUPERIMPOSED POINTS     ')
         CALL PLANTE(1)
         STOP
C
81       CONTINUE
C
70    CONTINUE
60    CONTINUE
C
      NSEGBOR = 0
      DO IFACE=1,NFACE
        DO IELEM=1,NELEM
          IF(IFABOR(IELEM,IFACE).EQ.0) NSEGBOR=NSEGBOR+1
        ENDDO
      ENDDO
C
      IF (LNG.EQ.1) WRITE(LU,500) NSEGBOR
      IF (LNG.EQ.2) WRITE(LU,501) NSEGBOR
500   FORMAT(1X,'SEGBOR (BIEF) : NOMBRE DE SEGMENTS DE BORD = ',1I6,/,
     &       1X,'EN COMPTANT CEUX DUS A LA DECOMPOSITION DE DOMAINE')
501   FORMAT(1X,'SEGBOR (BIEF) : NUMBER OF BOUNDARY SEGMENTS = ',1I6,/,
     &       1X,'INCLUDING THOSE DUE TO DOMAIN DECOMPOSITION')
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
      DEALLOCATE(IFABOR)
      DEALLOCATE(NVOIS)
      DEALLOCATE(IADR)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C