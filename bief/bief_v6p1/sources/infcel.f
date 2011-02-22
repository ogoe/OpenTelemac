C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES NUBO, VNOIN, AIRS, JMI, CMI, AIRST.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRST, CMI, IKLE, JMI, NELEM, NELMAX, NPOIN, NSEG, NUBO, NVMAX, VNOIN, XX, YY
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ENTOPB, EPS, ERR, I1, I2, I3, IEL, IS, IS1, IS2, ISEG, J1, JARET, JVOIS, KV, KV1, KV2, NB1, NB2, NEX, NU, NUB1, RNORM, UNSIX, X1, X2, X3, XG, XG1, XI1, XI2, XI3, XS1, XS2, Y1, Y2, Y3, YG, YG1, YI1, YI2, YI3, YS1, YS2
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
!>      <td><center> 5.4                                       </center>
!> </td><td> 18/06/03
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRS
!></td><td><--</td><td>AIRES DES CELLULES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>AIRST
!></td><td><--</td><td>AIRES DES SOUS-TRIANGLES D'UNE CELLULE
!>    </td></tr>
!>          <tr><td>CMI
!></td><td><--</td><td>COORDONNEES DES POINTS MILIEUX D'INTERFACE
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE.
!>    </td></tr>
!>          <tr><td>JMI
!></td><td><--</td><td>NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!>                  POINT MILIEU D'UNE INTERFACE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE SOMMETS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td><--</td><td>NUMEROS DES DEUX SOMMETS D'UNE ARETE
!>    </td></tr>
!>          <tr><td>NVMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE VOISINS D'UN POINT
!>                  (MXPTVS DANS TELEMAC)
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td><--</td><td>NORMALE A L'INTERFACE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>XX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XX YY
!></td><td>--></td><td>COORDONNEES DES SOMMETS.
!>    </td></tr>
!>          <tr><td>YY
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INFCEL
     & ( XX , YY , IKLE , NUBO , VNOIN , NPOIN ,
     &   NVMAX , NELEM , NELMAX , NSEG ,CMI ,JMI ,AIRST)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |<--| AIRES DES CELLULES DU MAILLAGE.
C| AIRST          |<--| AIRES DES SOUS-TRIANGLES D'UNE CELLULE
C| CMI            |<--| COORDONNEES DES POINTS MILIEUX D'INTERFACE
C| IKLE           |-->| TABLE DE CONNECTIVITE.
C| JMI            |<--| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU D'UNE INTERFACE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |---| 
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE.
C| NSEG           |-->| NOMBRE DE SEGMENTS DU MAILLAGE.
C| NUBO           |<--| NUMEROS DES DEUX SOMMETS D'UNE ARETE
C| NVMAX          |-->| NOMBRE MAXIMUM DE VOISINS D'UN POINT
C|                |   | (MXPTVS DANS TELEMAC)
C| VNOIN          |<--| NORMALE A L'INTERFACE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| XX             |---| 
C| XX YY          |-->| COORDONNEES DES SOMMETS.
C| YY             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT  NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEG,NELMAX,NPOIN,NVMAX,NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      INTEGER, INTENT(INOUT)          :: JMI(*)
      INTEGER, INTENT(INOUT)          :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: XX(NPOIN),YY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: VNOIN(3,NSEG),CMI(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: AIRST(2,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NB1,NB2,ISEG,IS,KV
      INTEGER I1,I2,I3,IS1,IS2,KV1,KV2,J1
      INTEGER IEL,JARET,ERR
C
      INTEGER NU(3),NEX(3),NUB1
C
      DOUBLE PRECISION EPS
      DOUBLE PRECISION UNSIX
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,RNORM,ENTOPB(2,3)
      DOUBLE PRECISION XI1,YI1,XI2,YI2,XI3,YI3
      DOUBLE PRECISION XG,YG
      DOUBLE PRECISION XS1,YS1,XS2,YS2,XG1,YG1
C
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JVOIS
C
      INTRINSIC ABS,SQRT
C
C-----------------------------------------------------------------------
C
C  JVOIS: ARRAY OF THE VERTICES (1) AND SEGMENTS (2) NEIGHBOURING A VERTEX
C
      ALLOCATE(JVOIS(NPOIN,NVMAX,2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'INFCEL: MAUVAISE ALLOCATION DE JVOIS'
        IF(LNG.EQ.2) WRITE(LU,*) 'INFCEL: WRONG ALLOCATION OF JVOIS'
        STOP
      ENDIF
C
      EPS=1.D-5
C
C  NEX: POINTER OF THE SUCCESSORS OF THE VERTICES IN AN ELEMENT
C
C------
C 1. INITIALISES
C------
C
      DO ISEG = 1 , NSEG
        VNOIN(1,ISEG) = 0.D0
        VNOIN(2,ISEG) = 0.D0
        VNOIN(3,ISEG) = 0.D0
        CMI(1,ISEG)   = 0.D0
        CMI(2,ISEG)   = 0.D0
        JMI(ISEG)     = 0
      ENDDO
C
      DO IS = 1 , NPOIN
        DO KV = 1 , NVMAX
          JVOIS(IS,KV,1) = 0
          JVOIS(IS,KV,2) = 0
        ENDDO
      ENDDO
C
      ISEG = 0
      UNSIX = 1.D0/6.D0
C
C------
C 2. BUILDS ARRAYS : JVOIS , NUBO
C------
C
C  -->  ORIENTATION
C
      NEX (1) = 2
      NEX (2) = 3
      NEX (3) = 1
C
C     LOOP ON THE ELEMENTS
C     ***********************
C
      DO 30  IEL = 1 , NELEM
C
        I1 = IKLE(IEL,1)
        I2 = IKLE(IEL,2)
        I3 = IKLE(IEL,3)
C
        X1 = XX( I1 )
        Y1 = YY( I1 )
C
        X2 = XX( I2 )
        Y2 = YY( I2 )
C
        X3 = XX( I3 )
        Y3 = YY( I3 )
C
         XG= (X1+X2+X3)/3.D0
         YG= (Y1+Y2+Y3)/3.D0
C
        ENTOPB(1,1) = Y2 - Y3
        ENTOPB(2,1) = X3 - X2
C
        ENTOPB(1,2) = Y3 - Y1
        ENTOPB(2,2) = X1 - X3
C
        ENTOPB(1,3) = Y1 - Y2
        ENTOPB(2,3) = X2 - X1
C
        NU(1) = I1
        NU(2) = I2
        NU(3) = I3
C
C       LOOP ON THE 3 VERTICES OF ELEMENT IEL
C ----  *******************************************
C
        DO 40  IS = 1 , 3
          IS1 = NU(IS)
          IS2 = NU(NEX(IS))
C
C         IS IS2 NEIGHBOURING IS1? ---> LOOP ON IS1 NEIGHBOURS
C
          DO 41  KV1 = 1 , NVMAX
            IF (JVOIS (IS1, KV1, 1) .EQ. 0) GO TO 43
            IF (JVOIS (IS1, KV1, 1) .EQ. IS2) GO TO 44
41        CONTINUE
C
 43       CONTINUE
          JVOIS (IS1, KV1, 1) = IS2
C
C         IS IS1 NEIGHBOURING IS2? ---> LOOP ON IS2 NEIGHBOURS
C
          DO 45  KV2 = 1 , NVMAX
            IF (JVOIS (IS2, KV2, 1) .EQ. 0) GO TO 46
 45       CONTINUE
C
 46       CONTINUE
C
C  -->    BUILDS THE SEGMENTS AND NEIGHBOURS
C
          ISEG = ISEG + 1
C
          JVOIS(IS2,KV2,1) = IS1
          JVOIS(IS1,KV1,2) = ISEG
          JVOIS(IS2,KV2,2) = ISEG
C
          JARET = ISEG
          NUBO (1, ISEG) = -IS1
          NUBO (2, ISEG) = IS2
C
          UNSIX = ABS(UNSIX)
          JMI(ISEG)=IEL
          GO TO 48
C
 44       CONTINUE
C         THE SEGMENT HAS ALREADY BEEN TREATED
C
          JARET = JVOIS (IS1, KV1, 2)
          NUBO(1,JARET) = - NUBO(1,JARET)
          NB1 = NUBO(1,JARET)
          NB2 = NUBO(2,JARET)
          IF(NB1.EQ.IS1.AND.NB2.EQ.IS2) THEN
            UNSIX = ABS(UNSIX)
          ELSE
            IF(NB2.EQ.IS1.AND.NB1.EQ.IS2) THEN
              UNSIX = - ABS(UNSIX)
            ENDIF
          ENDIF
C
C         END OF LOOP ON THE VERTICES
C ----    *********************************
C
C       BUILDS THE CONTRIBUTION TO VECTOR NIJ
C
 48      VNOIN(1,JARET) = VNOIN(1,JARET) +
     &   UNSIX * ( ENTOPB(1,NEX(IS)) - ENTOPB(1,IS))
C
         VNOIN(2,JARET) = VNOIN(2,JARET) +
     &   UNSIX * ( ENTOPB(2,NEX(IS)) - ENTOPB(2,IS))
C
C   COORDINATES OF POINT M, MIDDLE OF SEGMENT GG1 BELONGING TO
C   THE CELL BOUNDARY
C
C
         CMI(1,JARET)= CMI(1,JARET)+0.5D0*XG
         CMI(2,JARET)= CMI(2,JARET)+0.5D0*YG
C
         IF(JMI(JARET).NE.IEL) THEN
C
C  COMPUTES SURFACES I1GG1 AND I2GG1
C
C
C  BEWARE IS1 # NUBO(1,JARET) (TRAVELS ON THE EDGE FOR THE 2ND TIME
C   IN THE OTHER DIRECTION)
C
        NB1= NUBO(1,JARET)
        NB2= NUBO(2,JARET)
C
         XS1 = XX(NB1)
         YS1 = YY(NB1)
         XS2 = XX(NB2)
         YS2 = YY(NB2)
C
         J1=JMI(JARET)
C
         XG1=(XX(IKLE(J1,1))+XX(IKLE(J1,2))+XX(IKLE(J1,3)))/3.D0
         YG1=(YY(IKLE(J1,1))+YY(IKLE(J1,2))+YY(IKLE(J1,3)))/3.D0
C
C     AIRST : SURFACE OF THE SUB-TRIANGLES I1GG1 AND I2GG1
C
           AIRST(1,JARET)=  0.5D0*ABS((XG1-XS1) * (YG-YS1)
     &                              - (YG1-YS1) * (XG-XS1))
           AIRST(2,JARET)=  0.5D0*ABS((XG1-XS2) * (YG-YS2)
     &                              - (YG1-YS2) * (XG-XS2))
C
C IF M BELONGS TO TRIANGLE IEL, JMI(JARET) =IEL
C
           XI1=X1-CMI(1,JARET)
           XI2=X2-CMI(1,JARET)
           XI3=X3-CMI(1,JARET)
           YI1=Y1-CMI(2,JARET)
           YI2=Y2-CMI(2,JARET)
           YI3=Y3-CMI(2,JARET)
           IF((XI1*YI2-XI2*YI1).LT.EPS) GOTO 40
           IF((XI2*YI3-XI3*YI2).LT.EPS) GOTO 40
           IF((XI3*YI1-XI1*YI3).LT.EPS) GOTO 40
           JMI(JARET) =IEL
C
         ENDIF
C
 40      CONTINUE
 30   CONTINUE
C
C   BUILDS THE BOUNDARY SEGMENTS
C
      DO 50  ISEG =1, NSEG
        NUB1 = NUBO(1,ISEG)
        IF(NUB1.LT.0) THEN
C
C           THIS IS A BOUNDARY EDGE
C
          NUBO(1,ISEG) = - NUBO(1,ISEG)
          IS1=NUBO(1,ISEG)
          IS2=NUBO(2,ISEG)
C
         XS1 = XX(IS1)
         YS1 = YY(IS1)
         XS2 = XX(IS2)
         YS2 = YY(IS2)
         CMI(1,ISEG) = 0.5D0* (XS1 +XS2)
         CMI(2,ISEG) = 0.5D0* (YS1 +YS2)
C
C  COMPUTES SURFACES I1GM AND I2GM
C
         J1=JMI(ISEG)
         XG= (XX(IKLE(J1,1))+XX(IKLE(J1,2))
     &                     + XX(IKLE(J1,3)))/3.D0
         YG= (YY(IKLE(J1,1))+YY(IKLE(J1,2))
     &                     + YY(IKLE(J1,3)))/3.D0
C
C     AIRST : SURFCAE OF THE SUB-TRIANGLES I1GM AND I2GM
C
          AIRST(1,ISEG)= 0.5D0*ABS((CMI(1,ISEG)-XS1) * (YG-YS1)
     &                           - (CMI(2,ISEG)-YS1) * (XG-XS1))
          AIRST(2,ISEG)= 0.5D0*ABS((CMI(1,ISEG)-XS2) * (YG-YS2)
     &                           - (CMI(2,ISEG)-YS2) * (XG-XS2))
C
        ENDIF
50    CONTINUE
C
C   BUILDS THE NORMALS
C
      DO 31 ISEG=1,NSEG
        RNORM = SQRT(VNOIN(1,ISEG)**2 + VNOIN(2,ISEG)**2)
        VNOIN(3,ISEG) = RNORM
        VNOIN(1,ISEG) = VNOIN(1,ISEG) / RNORM
        VNOIN(2,ISEG) = VNOIN(2,ISEG) / RNORM
31    CONTINUE
C
C---------------------------------------------------------------------
C
      DEALLOCATE(JVOIS)
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C