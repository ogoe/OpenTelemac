C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ARRAYS GIVING ADRESSES FOR
!>                FRONTAL MATRIX-VECTOR PRODUCT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GLOSEG, IELM, IKLE, IKLEM1, LIMVOI, MXPTVS, NBOR, NELEM, NELMAX, NPMAX, NPOIN, NPTFR, NSEG, OPTASS, PRODUC, SIZGLO, T1
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I1, I2, IELEM, IPOIN, IPTFR, ISG, K, NBVOIS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FROPRO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 20/03/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>                  11 : TRIANGLES.
!>                  21 : QUADRILATERES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>IKLEM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMVOI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>--></td><td>NOMBRE MAXIMUM DE VOISINS D'UN POINT
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAL DU POINT DE BORD K.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRODUC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAUX DE TRAVAIL ENTIERS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FROPRO
     &(NBOR,IKLE,NELEM,NELMAX,NPOIN,NPMAX,NPTFR,IELM,
     & IKLEM1,LIMVOI,OPTASS,PRODUC,MXPTVS,T1,
     & GLOSEG,SIZGLO,NSEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GLOSEG         |---| 
C| IELM           |-->| TYPE D'ELEMENT.
C|                |   | 11 : TRIANGLES.
C|                |   | 21 : QUADRILATERES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| IKLEM1         |---| 
C| LIMVOI         |---| 
C| MXPTVS         |-->| NOMBRE MAXIMUM DE VOISINS D'UN POINT
C| NBOR           |-->| NUMERO GLOBAL DU POINT DE BORD K.
C| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |---| 
C| NPMAX          |---| 
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NSEG           |---| 
C| OPTASS         |---| 
C| PRODUC         |---| 
C| SIZGLO         |---| 
C| T1             |-->| TABLEAUX DE TRAVAIL ENTIERS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FROPRO => FROPRO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELMAX,NPMAX,MXPTVS,NELEM
      INTEGER, INTENT(IN)    :: NPOIN,NPTFR,IELM,OPTASS,PRODUC
      INTEGER, INTENT(IN)    :: NSEG,SIZGLO,NBOR(*)
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,*),GLOSEG(SIZGLO,2)
      INTEGER, INTENT(OUT)   :: IKLEM1(NPMAX,MXPTVS,4,2)
      INTEGER, INTENT(OUT)   :: LIMVOI(MXPTVS,2)
      INTEGER, INTENT(OUT)   :: T1(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IPTFR,IPOIN,ISG,K,I,I1,I2,NBVOIS
C
C-----------------------------------------------------------------------
C
      IF(IELM.NE.11) THEN
        IF(LNG.EQ.1) WRITE(LU,900) IELM
        IF(LNG.EQ.2) WRITE(LU,901) IELM
900     FORMAT(1X,'FROPRO : IELM=',1I6,' TYPE D''ELEMENT INCONNU')
901     FORMAT(1X,'FROPRO: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        STOP
      ENDIF
C
      IF(PRODUC.EQ.2) THEN
C
C=======================================================================
C COMPUTES THE NUMBER OF NEIGHBOURING POINTS AND ELEMENTS
C=======================================================================
C
         DO 100 IPOIN = 1,NPOIN
            T1(IPOIN) = 0
100      CONTINUE
C
C        NUMBER OF ELEMENTS NEIGHBOURING A POINT
C
         DO 110 IELEM = 1,NELEM
            T1(IKLE(IELEM,1)) = T1(IKLE(IELEM,1)) + 1
            T1(IKLE(IELEM,2)) = T1(IKLE(IELEM,2)) + 1
            T1(IKLE(IELEM,3)) = T1(IKLE(IELEM,3)) + 1
110      CONTINUE
C
C        NUMBER OF POINTS NEIGHBOURING A POINT
C     =  NUMBER OF ELEMENTS NEIGHBOURING A POINT
C     +  1 ON BOUNDARIES
C
         DO 112 IPTFR = 1,NPTFR
            T1(NBOR(IPTFR)) = T1(NBOR(IPTFR)) + 1
112      CONTINUE
C
C=======================================================================
C CHECKS THAT THE RENUMBERING WAS MADE CORRECTLY IN STBTEL
C FILLS IN LIMVOI AND IKLEM1
C=======================================================================
C
         IF(T1(1).EQ.0) THEN
           IF(LNG.EQ.1) WRITE(LU,96)
           IF(LNG.EQ.2) WRITE(LU,97)
96         FORMAT(1X,'FROPRO : LE POINT 1 N''A PAS DE VOISIN')
97         FORMAT(1X,'FROPRO: POINT NUMBER 1 HAS NO NEIGHBOUR')
           STOP
         ENDIF
C
         DO 120 IPOIN = 2,NPOIN
          IF(T1(IPOIN).LT.T1(IPOIN-1)) THEN
            IF(LNG.EQ.1) WRITE(LU,98)
            IF(LNG.EQ.2) WRITE(LU,99)
98          FORMAT(1X,'FROPRO : PRODUIT FRONTAL, IL FAUT UNE',/,1X,
     &                'RENUMEROTATION DES POINTS AVEC STBTEL')
99          FORMAT(1X,'FROPRO: FRONTAL PRODUCT REQUIRES A',/,1X,
     &                'RENUMBERING OF POINTS WITH STBTEL')
            STOP
          ELSEIF(T1(IPOIN).GT.MXPTVS) THEN
            IF(LNG.EQ.1) WRITE(LU,94) IPOIN
            IF(LNG.EQ.2) WRITE(LU,95) IPOIN
94          FORMAT(1X,'FROPRO : LE POINT ',1I6,' A TROP DE VOISINS')
95          FORMAT(1X,'FROPRO: POINT ',1I6,' HAS TOO MANY NEIGHBOURS')
            STOP
          ENDIF
120      CONTINUE
C
C  BUILDS ARRAY LIMVOI
C  LIMVOI(K,1) : BEGINNING OF SERIES WITH K NEIGHBOURS
C  LIMVOI(K,2) : END       OF SERIES WITH K NEIGHBOURS
C
         DO K=1,MXPTVS
           LIMVOI(K,1) = 0
           LIMVOI(K,2) = 0
         ENDDO
C        POINT 1 IS THE BEGINNING OF SERIES WITH T1(1) NEIGHBOURS
         NBVOIS = T1(1)
         LIMVOI(NBVOIS,1) = 1
         DO I=2,NPOIN
           IF(T1(I).NE.NBVOIS) THEN
C          PREVIOUS POINT WAS AN END OF A SERIES
           LIMVOI(NBVOIS,2) = I-1
C          CURRENT POINT IS THE BEGINNING OF A SERIES
           NBVOIS = T1(I)
           LIMVOI(NBVOIS,1) = I
           ENDIF
         ENDDO
C        POINT NPOIN IS THE END OF ITS SERIES
         LIMVOI(NBVOIS,2) = NPOIN
C
C   ARRAYS FOR FRONTAL MATRIX-VECTOR PRODUCT :
C
         DO 130 IPOIN = 1,NPOIN
           T1(IPOIN) = 1
130      CONTINUE
C
         IF(OPTASS.EQ.3) THEN
C
C        IY DOES NOT DEPEND HERE ON THE DIRECT OR TRANSPOSE CHARACTER
         DO ISG = 1,NSEG
            I1 = GLOSEG(ISG,1)
            I2 = GLOSEG(ISG,2)
C
C           ANY MATRIX
C           IXM IN DIRECT PRODUCT
            IKLEM1(I1,T1(I1),1,1) = ISG
            IKLEM1(I2,T1(I2),1,1) = ISG + NSEG
C           IY IN DIRECT PRODUCT
            IKLEM1(I1,T1(I1),2,1) = I2
            IKLEM1(I2,T1(I2),2,1) = I1
C           IXM IN TRANSPOSE PRODUCT
            IKLEM1(I1,T1(I1),3,1) = ISG + NSEG
            IKLEM1(I2,T1(I2),3,1) = ISG
C           IY IN TRANSPOSE PRODUCT
            IKLEM1(I1,T1(I1),4,1) = I2
            IKLEM1(I2,T1(I2),4,1) = I1
C
C           SYMMETRICAL MATRIX
C           IXM IN DIRECT PRODUCT
            IKLEM1(I1,T1(I1),1,2) = ISG
            IKLEM1(I2,T1(I2),1,2) = ISG
C           IY IN DIRECT PRODUCT
            IKLEM1(I1,T1(I1),2,2) = I2
            IKLEM1(I2,T1(I2),2,2) = I1
C           IXM IN TRANSPOSE PRODUCT
            IKLEM1(I1,T1(I1),3,2) = ISG
            IKLEM1(I2,T1(I2),3,2) = ISG
C           IY IN TRANSPOSE PRODUCT
            IKLEM1(I1,T1(I1),4,2) = I2
            IKLEM1(I2,T1(I2),4,2) = I1
C
C           UPDATES THE NUMBER OF NEIGHBOURS
            T1(I1) = T1(I1) + 1
            T1(I2) = T1(I2) + 1
         ENDDO
C
         ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'STOCKAGE INCONNU DANS FROPRO :',OPTASS
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'UNKNOWN STORAGE IN FROPRO :',OPTASS
            ENDIF
            CALL PLANTE(1)
            STOP
         ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C