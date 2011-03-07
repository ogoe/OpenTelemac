!                    ****************
                     SUBROUTINE SCOPE
!                    ****************
!
     & (U,V,W,HN,ZF,X,Y,Z,TRA01,TRA02,TRA03,SURFAC,IKLE3,IFABOR,NELEM3,
     &  NELEM2,NPOIN2,NPOIN3,NETAGE,NPLAN,LT,AT,DT,NIT,NSCO,PRIVE)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    EXTRACTS SPECIFIC CROSS-SECTIONS, WHICH WILL BE
!+                WRITTEN IN THE SCOPE FORMAT (READ BY RUBENS).
!+
!+            COORDINATES OF THE CROSS-SECTION :
!+                FROM (X1,Y1,Z1) TO (X2,Y2,Z2).
!
!note     THE RESULT OF THE CUT IS FINT, WHICH IS A FUNCTION OF SCURV.
!+         4 TYPES OF X-SECTIONS ARE POSSIBLE : THE NUMBER OF POINTS
!+         FOR EACH OF THEM IS A PARAMETER.
!
!warning  REMOVE THE FIRST TWO LINES FOLLOWING THE DECLARATIONS
!+            TO ACTIVATE THIS SUBROUTINE
!
!history  F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P1
!+   
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        
!+   FORTRAN95 VERSION 
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
!| AT             |-->| TEMPS DU PAS DE TEMPS
!| DT             |-->| PAS DE TEMPS
!| HN             |-->| HAUTEUR D'EAU
!| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!| IKLE3          |-->| TABLE DE CONNECTIVITE 3D
!| LT             |-->| NUMERO DU PAS DE TEMPS
!| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
!| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
!| NETAGE         |-->| NOMBRE D'ETAGES
!| NIT            |-->| NOMBRE TOTAL DE PAS DE TEMPS
!| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| NPOIN3         |-->| NOMBRE DE POINTS 3D
!| NSCO           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER POUR SCOPE
!| PRIVE          |-->| TABLEAUX RESERVES A L'UTILISATEUR
!| SURFAC         |-->| SURFACE DES ELEMENTS DU MAILLAGE 2D
!| TRA02          |---| 
!| TRA03          |---| 
!| U,V,W          |-->| VITESSE 3D
!| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
!| ZF             |-->| COTE DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_SCOPE => SCOPE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!     DELETE THESE LINES TO ACTIVATE THE SUBROUTINE
!
      BULLER = .TRUE.
      IF (BULLER) RETURN
!
!-----------------------------------------------------------------------
!
!     EXAMPLE FOR A 2D VARIABLE (HERE HN) --> CROSS-SECTION 1
!
!     DUPLICATES THE 2D VARIABLE IN A 3D ARRAY
!
      DO IPLAN=1,NPLAN
         IADR = 1 + (IPLAN-1)*NPOIN2
         CALL OV( 'X=Y     ' , TRA01(IADR) , HN , Z , C , NPOIN2 )
      END DO
!
!     COORDINATES OF ENDS OF CROSS-SECTION
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
!     EXAMPLE OF A 3D VARIABLE (HERE U) --> CROSS-SECTION 2
!
!     COORDINATES OF ENDS OF CROSS-SECTION
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
!     WRITES OUT IN SCOPE FORMAT
!
      REWIND NSCO
      WRITE (NSCO,'(A9)') '''NOMXXXX'''
      WRITE (NSCO,'(1H'',A8,1H'')') '02/10/92'
!
!     WRITES THE TITLE ON 60 CHARACTERS FOR SCOPE
      WRITE (NSCO,'(A)') '''TELEMAC-3D : THE BEST'''
!
      WRITE (NSCO,'(A)') '''ABSCISSES'''
      WRITE (NSCO,'(A)') '''VARIABLE 1'''
      WRITE (NSCO,'(A)') '''VARIABLE 2'''
!     NO CROSS-SECTIONS 3 AND 4 IN THE EXAMPLE
!CX   WRITE (NSCO,'(A)') '''VARIABLE 3'''
!CX   WRITE (NSCO,'(A)') '''VARIABLE 4'''
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