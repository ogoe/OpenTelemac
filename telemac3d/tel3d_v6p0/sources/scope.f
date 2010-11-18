C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTRACTS SPECIFIC CROSS-SECTIONS, WHICH WILL BE
!>                WRITTEN IN THE SCOPE FORMAT (READ BY RUBENS).
!><br>            COORDINATES OF THE CROSS-SECTION :
!>                FROM (X1,Y1,Z1) TO (X2,Y2,Z2).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE RESULT OF THE CUT IS FINT, WHICH IS A FUNCTION OF SCURV.
!>         4 TYPES OF X-SECTIONS ARE POSSIBLE : THE NUMBER OF POINTS
!>         FOR EACH OF THEM IS A PARAMETER.

!>  @warning  REMOVE THE FIRST TWO LINES FOLLOWING THE DECLARATIONS
!>            TO ACTIVATE THIS SUBROUTINE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DT, HN, IFABOR, IKLE3, LT, NELEM2, NELEM3, NETAGE, NIT, NPLAN, NPOIN2, NPOIN3, NSCO, PRIVE, SURFAC, TRA01, TRA02, TRA03, U, V, W, X, Y, Z, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BULLER, C, FINT1, FINT2, IADR, IPLAN, IPOIN, NPO1, NPO2, NPO3, NPO4, SCURV1, SCURV2, X1, X2, Y1, Y2, Z1, Z2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SCOPE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COUPE(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 25/11/97
!> </td><td> F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>TABLE DE CONNECTIVITE 3D
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE D'ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NOMBRE TOTAL DE PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
!>                  RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>NSCO
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER POUR SCOPE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAUX RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>TRA01,
!></td><td>--></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>VITESSE 3D
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SCOPE
     & (U,V,W,HN,ZF,X,Y,Z,TRA01,TRA02,TRA03,SURFAC,IKLE3,IFABOR,NELEM3,
     &  NELEM2,NPOIN2,NPOIN3,NETAGE,NPLAN,LT,AT,DT,NIT,NSCO,PRIVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU PAS DE TEMPS
C| DT             |-->| PAS DE TEMPS
C| HN             |-->| HAUTEUR D'EAU
C| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
C| IKLE3          |-->| TABLE DE CONNECTIVITE 3D
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NETAGE         |-->| NOMBRE D'ETAGES
C| NIT            |-->| NOMBRE TOTAL DE PAS DE TEMPS
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPRIV          |-->| NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
C|                |   | RESERVES A L'UTILISATEUR
C| NSCO           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER POUR SCOPE
C| PRIVE          |-->| TABLEAUX RESERVES A L'UTILISATEUR
C| SURFAC         |-->| SURFACE DES ELEMENTS DU MAILLAGE 2D
C| TRA01,         |-->| TABLEAUX DE TRAVAIL
C| TRA02          |---| 
C| TRA03          |---| 
C| U,V,W          |-->| VITESSE 3D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_SCOPE => SCOPE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NSCO,NPOIN2,NELEM2,NPOIN3,NETAGE
      INTEGER, INTENT(IN) ::  NIT,LT,NELEM3,NPLAN
!
      INTEGER, INTENT(IN) :: IKLE3(NELEM3,6), IFABOR(NELEM2,3)
!
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN3),V(NPOIN3),W(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3),Y(NPOIN3),Z(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3), TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN2),ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PRIVE
!
      DOUBLE PRECISION, INTENT(IN) :: DT,AT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN,IADR,IPOIN
      INTEGER NPO1,NPO2,NPO3,NPO4
      PARAMETER (NPO1=200,NPO2=200,NPO3=200,NPO4=200)
!
      DOUBLE PRECISION SCURV1(NPO1),SCURV2(NPO2)
      DOUBLE PRECISION FINT1(NPO1),FINT2(NPO2)
!
      DOUBLE PRECISION C,X1,Y1,Z1,X2,Y2,Z2
!
      LOGICAL BULLER
!
!***********************************************************************
!
C     DELETE THESE LINES TO ACTIVATE THE SUBROUTINE
!
      BULLER = .TRUE.
      IF (BULLER) RETURN
!
!-----------------------------------------------------------------------
!
C     EXAMPLE FOR A 2D VARIABLE (HERE HN) --> CROSS-SECTION 1
!
C     DUPLICATES THE 2D VARIABLE IN A 3D ARRAY
!
      DO IPLAN=1,NPLAN
         IADR = 1 + (IPLAN-1)*NPOIN2
         CALL OV( 'X=Y     ' , TRA01(IADR) , HN , Z , C , NPOIN2 )
      END DO
!
C     COORDINATES OF ENDS OF CROSS-SECTION
!
      X1 = X(1)
      Y1 = Y(1)
      Z1 = Z(1)
      X2 = X(2)
      Y2 = Y(2)
      Z2 = Z(2)
!
      CALL COUPE(TRA01,FINT1,SCURV1,NPO1,IKLE3,IFABOR,X,Y,Z,SURFAC,
     &           NELEM2,NPOIN3,NETAGE,X1,Y1,Z1,X2,Y2,Z2)
!
!-----------------------------------------------------------------------
!
C     EXAMPLE OF A 3D VARIABLE (HERE U) --> CROSS-SECTION 2
!
C     COORDINATES OF ENDS OF CROSS-SECTION
!
      X1 = X(1)
      Y1 = Y(1)
      Z1 = Z(1)
      X2 = X(2)
      Y2 = Y(2)
      Z2 = Z(2)
!
      CALL COUPE(U,FINT2,SCURV2,NPO2,IKLE3,IFABOR,X,Y,Z,SURFAC,
     &           NELEM2,NPOIN3,NETAGE,X1,Y1,Z1,X2,Y2,Z2)
!
!-----------------------------------------------------------------------
!
C     WRITES OUT IN SCOPE FORMAT
!
      REWIND NSCO
      WRITE (NSCO,'(A9)') '''NOMXXXX'''
      WRITE (NSCO,'(1H'',A8,1H'')') '02/10/92'
!
C     WRITES THE TITLE ON 60 CHARACTERS FOR SCOPE
      WRITE (NSCO,'(A)') '''TELEMAC-3D : THE BEST'''
!
      WRITE (NSCO,'(A)') '''ABSCISSES'''
      WRITE (NSCO,'(A)') '''VARIABLE 1'''
      WRITE (NSCO,'(A)') '''VARIABLE 2'''
C     NO CROSS-SECTIONS 3 AND 4 IN THE EXAMPLE
CCX   WRITE (NSCO,'(A)') '''VARIABLE 3'''
CCX   WRITE (NSCO,'(A)') '''VARIABLE 4'''
!
      DO IPOIN=1,NPO1
         IF (FINT1(IPOIN).LT.1.D+50 .OR. SCURV1(IPOIN).LT.1.D+50) THEN
            WRITE (NSCO,'(2(1X,G14.6),7X,A1,14X,A1,14X,A1)')
     &      SCURV1(IPOIN),FINT1(IPOIN),'X','X','X'
         ENDIF
      END DO
!
      DO IPOIN=1,NPO2
         IF (FINT2(IPOIN).LT.1.D+50 .OR. SCURV2(IPOIN).LT.1.D+50) THEN
            WRITE
     &      (NSCO,'(1X,G14.6,7X,A1,7X,1X,G14.6,7X,A1,14X,A1)')
     &      SCURV2(IPOIN),'X',FINT2(IPOIN),'X','X'
         ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE SCOPE
C
C#######################################################################
C