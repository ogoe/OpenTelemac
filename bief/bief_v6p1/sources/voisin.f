C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE ARRAY IFABOR, WHERE IFABOR(IELEM, IFACE) IS
!>                THE GLOBAL NUMBER OF THE NEIGHBOUR OF SIDE IFACE OF
!>                ELEMENT IELEM (IF THIS NEIGHBOUR EXISTS) AND 0 IF THE
!>                SIDE IS ON THE DOMAIN BOUNDARY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IADR, IELM, IFABOR, IKLE, NACHB, NBOR, NELEM, NELMAX, NPOIN, NPTFR, NVOIS, SIZIKL
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR, I, I1, I2, IDIMAT, IELEM, IELEM2, IFACE, IFACE2, IMAX, IR1, IR2, IR3, IR4, IV, J, KEL, M1, M2, MAT1, MAT2, MAT3, NDP, NFACE, SOMFAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), GREDELSEG(), INBIEF()

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
!> </td><td> 16/06/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> MODIFICATION TO THE MAX NUMBER OF NEIGHBOURS
!>           IN PARALLEL MODE WHERE THE SUB-DOMAIN IS NOT A
!>           PROPER FINITE ELEMENT MESH (TRIANGLES LINKED
!>           BY ONE VERTEX FOR EXAMPLE)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/02/08
!> </td><td> OLIVIER BOITEAU (SINETICS)
!> </td><td> SIZE OF NACHB
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IADR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>11: TRIANGLES
!>                  21: QUADRILATERES
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td><--</td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
!>    </td></tr>
!>          <tr><td>NACHB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVOIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZIKL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VOISIN
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,
     & NPOIN,NACHB,NBOR,NPTFR,IADR,NVOIS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IADR           |---| 
C| IELM           |-->| 11: TRIANGLES
C|                |   | 21: QUADRILATERES
C| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
C| NACHB          |---| 
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE
C| NPTFR          |---| 
C| NVOIS          |---| 
C| SIZIKL         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VOISIN => VOISIN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPTFR,SIZIKL,NELEM,NELMAX,IELM,NPOIN
      INTEGER, INTENT(IN)    :: NBOR(NPTFR),NACHB(NBMAXNSHARE,NPTIR)
      INTEGER, INTENT(IN)    :: IKLE(SIZIKL,*)
      INTEGER, INTENT(INOUT) :: IFABOR(NELMAX,*)
      INTEGER, INTENT(INOUT) :: NVOIS(NPOIN),IADR(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,J,ERR,IR1,IR2,IR3,IR4,I1,I2,IDIMAT
C
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C     DYNAMICALLY ALLOCATES THE WORKING ARRAYS
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.21) THEN
C       QUADRILATERALS
        NFACE = 4
C       NUMBER OF POINTS PER ELEMENT
        NDP = 4
C       ADDRESS IN SOMFAC
        KEL = 2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
C       TRIANGLES
        NFACE = 3
C       NUMBER OF POINTS PER ELEMENT
        NDP = 3
C       ADDRESS IN SOMFAC
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS OF
C     ALL THE POINTS (NEIGHBOUR = CONNECTED BY A SEGMENT)
C
      IDIMAT = NDP*2*NELEM
C
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'VOISIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  ARRAY NVOIS FOR EACH POINT
C  BEWARE : NVOIS IS BIGGER THAN THE ACTUAL NUMBER OF NEIGHBOURS
C           THE SUM OF NVOIS WILL GIVE IDIMAT
C
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
C
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C  ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE COMPACT MATRIX
C
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
51      FORMAT(1X,'VOISIN: TAILLE DE MAT1,2,3 (',1I9,') INSUFFISANTE',/,
     &         1X,'IL FAUT AU MOINS : ',1I9)
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I9,') TOO SHORT',/,
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
      IFABOR(IELEM,IFACE) = -1
C
C        GLOBAL NODE NUMBERS FOR THE SIDE:
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C        ORDERED GLOBAL NUMBERS:
C
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
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
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     &             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     &             '         MAYBE SUPERIMPOSED POINTS     ')
         CALL PLANTE(1)
         STOP
C
81       CONTINUE
C
70    CONTINUE
60    CONTINUE
C
C  COULD TRY SOMETHING A BIT LIGHTER
C  USING INDPU FOR EXAMPLE
C
      IF(NCSIZE.GT.1) THEN
C
      DO 61 IFACE=1,NFACE
      DO 71 IELEM=1,NELEM
C
C  SOME BOUNDARY SIDES ARE INTERFACES BETWEEN SUB-DOMAINS IN
C  ACTUAL FACT: THEY ARE ASSIGNED A VALUE -2 INSTEAD OF -1
C
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
         IR1=0
         IR2=0
C
         IF(NPTIR.GT.0) THEN
           DO 44 J=1,NPTIR
             IF(I1.EQ.NACHB(1,J)) IR1=1
             IF(I2.EQ.NACHB(1,J)) IR2=1
44         CONTINUE
         ENDIF
C
         IF(IR1.EQ.1.AND.IR2.EQ.1) THEN
C          INTERFACE SEGMENT DETECTED, CHECKS WHETHER IT IS NOT
C          ALSO A TRUE BOUNDARY SIDE
           IR3=0
           IR4=0
           DO 55 J=1,NPTFR
             IF(I1.EQ.NBOR(J)) IR3=1
             IF(I2.EQ.NBOR(J)) IR4=1
55         CONTINUE
C          PRIORITY TO THE TRUE BOUNDARY SIDES
           IF(IR3.EQ.0.OR.IR4.EQ.0) THEN
             IFABOR(IELEM,IFACE)=-2
           ENDIF
         ENDIF
C
      ENDIF
C
71    CONTINUE
61    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C