!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     &(PHIR,PHII,C,CG,K,X,Y,ZF,H,
     & HHO,U0,V0,PHAS,S,TRA01,TRA02,TRA03,TRA04,INCI,
     & GRAV,PER,OMEGA,IKLE,NBOR,KP1BOR,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
!
!***********************************************************************
! ARTEMIS   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ALMOST ALL THE COMPUTATION VARIABLES ARE AVAILABLE
!+             HERE TO WRITE OUT SPECIFIC OUTPUT, COMPUTE ANALYTICAL
!+             SOLUTIONS...
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!code
!+     EXAMPLE : U1 AND V1
!+              (HORIZONTAL VELOCITIES AT T/4)
!+               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
!+------------------------------------------------------------------
!+
!+      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
!+     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+
!+      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
!+     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+     *            MESH , XMESH ,
!+
!+      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
!+     *            1.D0 , BID , BID , BID , BID , BID , BID ,
!+     *            MESH , MESH , MSK , MASKEL )
!+     *            MESH , XMESH ,
!+
!+      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
!+      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
!+
!+      DO 25 I = 1,NPOIN
!+         PRIVE%ADR(1)%P%R(1) = TRA02(I)
!+         PRIVE%ADR(1)%P%R(2) = TRA03(I)
!+ 25   CONTINUE
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        04/06/1999
!+        V5P1
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
!| C,CG           |-->| VITESSE DE PHASE ET DE GROUPE
!| GRAV           |-->| GRAVITE
!| H              |-->| HAUTEUR D'EAU AU REPOS
!| HHO            |-->| HAUTEUR DE LA HOULE
!| IELM           |-->| TYPE D'ELEMENT
!| IELMB          |-->| TYPE D'ELEMENT DE BORD
!| IKLE           |-->| TABLE DE CONNECTIVITE
!| INCI           |-->| INCIDENCE DE LA HOULE
!| K              |-->| NOMBRE D'ONDE
!| KP1BOR         |-->| NUMERO DE BORD DU POINT SUIVANT
!| NBOR           |-->| ADRESSES DES POINTS DE BORD
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELMAX         |---|
!| NPOIN          |-->| NOMBRE DE POINTS
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| OMEGA          |-->| PULSATION DE LA HOULE
!| PER            |-->| PERIODE DE LA HOULE
!| PHAS           |-->| PHASE DE LA HOULE
!| PHIR,PHII      |-->| COMPOSANTES DU POTENTIEL
!| PRIVE          |<->| TABLEAUX RESERVE A L'UTILISATEUR
!| S              |-->| COTE DE LA SURFACE LIBRE
!| TRA02          |---|
!| TRA03          |---|
!| TRA04          |---|
!| U0,V0          |-->| VITESSES EN SURFACE (A T=0)
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
!| ZF             |-->| COTE DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_ARTEMIS, EX_UTIMP=> UTIMP
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN
      INTEGER IKLE(NELMAX,3),NBOR(NPTFR),KP1BOR(NPTFR)
!
      DOUBLE PRECISION PHIR(NPOIN),PHII(NPOIN)
      DOUBLE PRECISION C(NPOIN),CG(NPOIN),K(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),H(NPOIN),HHO(NPOIN),U0(NPOIN),V0(NPOIN)
      DOUBLE PRECISION INCI(NPOIN)
      DOUBLE PRECISION PHAS(NPOIN),S(NPOIN)
      DOUBLE PRECISION TRA01(NPOIN),TRA02(NPOIN)
      DOUBLE PRECISION TRA03(NPOIN),TRA04(NPOIN)
!
      TYPE(BIEF_OBJ) :: PRIVE
!
      DOUBLE PRECISION GRAV,PER,OMEGA
!
!------------------------------------------------------------------
!     EXAMPLE : U1 AND V1
!              (HORIZONTAL VELOCITIES AT T/4)
!               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
!------------------------------------------------------------------
!
!      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
!     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!
!      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
!     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!     *            MESH , XMESH ,
!
!      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
!     *            1.D0 , BID , BID , BID , BID , BID , BID ,
!     *            MESH , MESH , MSK , MASKEL )
!     *            MESH , XMESH ,
!
!      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
!      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
!
!      DO 25 I = 1,NPOIN
!         PRIVE%ADR(1)%P%R(1) = TRA02(I)
!         PRIVE%ADR(1)%P%R(2) = TRA03(I)
! 25   CONTINUE
!
      RETURN
      END