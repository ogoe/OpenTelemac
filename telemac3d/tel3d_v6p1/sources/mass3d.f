
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MASSES.

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
!> </td><td> 09/04/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE MASS3D
     &(INFO,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AMESH2         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D
C| AMESH3         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
C| H              |-->| HAUTEUR D'EAU AU PAS DE TEMPS PRESENT
C| IELM2          |-->| TYPE DE DISCRETISATION 2D
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IMESH2         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D
C| IMESH3         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
C| INFO           |-->| LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
C| LT             |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C| MASSE          |<--| MASSE AU PAS EN COURS
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| NVBIL          |-->| NOMBRE DE VARIABLES TRAITEES DANS LE BILAN
C| SEDI           |-->| SI OUI, SEDIMENT
C| SVIDE          |-->| STRUCTURE VIDE
C| T2_01          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| T3_01          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
C| TA             |-->| TRACEURS ACTIFS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT
      LOGICAL, INTENT(IN) :: INFO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC,I
!
!***********************************************************************
!
C FUNCTIONS
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL P_DSUM
!
!***********************************************************************
!
C   WATER MASS
!   ==========
!
      CALL VECTOR
     &  (T2_01,'=','MASVEC          ',IELM2H,1.D0,H,SVIDE,SVIDE,
     &  SVIDE, SVIDE, SVIDE, MESH2D, MSK, MASKEL)
      MASSE_WATER = BIEF_SUM(T2_01)
!
      IF(NCSIZE.GT.1) MASSE_WATER = P_DSUM(MASSE_WATER)
!
      IF(INFO.AND.LT.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,101) MASSE_WATER
        IF(LNG.EQ.2) WRITE(LU,102) MASSE_WATER
      ENDIF
!
C   TRACERS MASS
!   ============
!
      IF(NTRAC.GT.0) THEN
!
         DO ITRAC=1,NTRAC
!
C           UP TO RELEASE 5.4
!
C           CALL VECTOR
C    &      (T3_01, '=', 'MASVEC          ', IELM3, 1.D0,
C    &       TA%ADR(ITRAC)%P,
C    &       SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
C           MASSE%R(5+ITRAC) = SUM(T3_01)
!
C           FROM RELEASE 5.5 ON
!
C           THE 2 VERSIONS ARE NOT EQUIVALENT WHEN VOLU IS COMPUTED
C           WITH FORMULA MASBAS2 WHICH GIVES A COMPATIBILITY WITH 2D
C           WHEN THERE IS A MASS-LUMPING
!
!           TRACERS IN MASSE COME AFTER U,V,W,K AND EPSILON (HENCE THE 5)
!
            MASSE%R(5+ITRAC) = 0.D0
            DO I=1,NPOIN3
              MASSE%R(5+ITRAC)=MASSE%R(5+ITRAC)+TA%ADR(ITRAC)%P%R(I)*
     &                                          VOLU%R(I)
            ENDDO
!
C           END OF MODIFICATION BETWEEN 5.4 AND 5.5
!
            IF(NCSIZE.GT.1) MASSE%R(5+ITRAC) = P_DSUM(MASSE%R(5+ITRAC))
!
         ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
101   FORMAT(' MASSE D''EAU INITIALE DANS LE DOMAINE : ',20X, G16.7)
102   FORMAT(' INITIAL MASS OF WATER IN THE DOMAIN :',20X,G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
