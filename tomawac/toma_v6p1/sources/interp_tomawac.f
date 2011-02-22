C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC, TOMAWAC_MPI
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, ELT, ETA, ETAP1, F, IKLE2, NELEM2, NPLAN, NPOIN2, SHP1, SHP2, SHP3, SHZ, TRA01
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> TOMAWAC_MPI :<br>
!> @link TOMAWAC_MPI::F_RECV F_RECV@endlink, 
!> @link TOMAWAC_MPI::F_SEND F_SEND@endlink, 
!> @link TOMAWAC_MPI::IFREQ IFREQ@endlink, 
!> @link TOMAWAC_MPI::RECVCHAR RECVCHAR@endlink, 
!> @link TOMAWAC_MPI::RECVCOUNTS RECVCOUNTS@endlink, 
!> @link TOMAWAC_MPI::SENDCOUNTS SENDCOUNTS@endlink, 
!> @link TOMAWAC_MPI::SH_LOC SH_LOC@endlink<hr>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ELT2, ETA2, ETAG, ETAGP1, I, I3D, IP, IP2, IP3, IP4, IPLAN, IPOIN, IPOIN1, IPOIN2, IPOIN3, NNRECV, NNSEND, TYPE, UMSHZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GLOB_FONCTION_COMM(), PARCOM2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PROPA()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>--></td><td>FACTEUR DE PROPORTIONNALITE
!>    </td></tr>
!>          <tr><td>ELT
!></td><td>--></td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>--></td><td>NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>ETAP1
!></td><td>--></td><td>TABLEAU DES ETAGES SUPERIEURS
!>    </td></tr>
!>          <tr><td>F
!></td><td><-></td><td>DENSITE SPECTRALE D'ACTION D'ONDE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>SHP1
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
!>                  LEURS ELEMENTS 2D "ELT" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INTERP_TOMAWAC
     & ( F , B , SHP1 , SHP2 , SHP3 , SHZ , ELT , ETA , IKLE2,
     &   ETAP1, NPOIN2 , NELEM2 , NPLAN , TRA01)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |-->| FACTEUR DE PROPORTIONNALITE
C| ELT            |-->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |-->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| ETAP1          |-->| TABLEAU DES ETAGES SUPERIEURS
C| F             |<->| DENSITE SPECTRALE D'ACTION D'ONDE
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| SHP1           |-->| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
C|                |   | LEURS ELEMENTS 2D "ELT" ASSOCIES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHZ            |-->| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| TRA01          |<->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE TOMAWAC_MPI
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER ETAG,ETAGP1,IPOIN1,IPOIN2,IPOIN3,IPLAN
      INTEGER NELEM2,NPOIN2,NPLAN,IP
      INTEGER I,I3D,TYPE
      INTEGER IPOIN
      DOUBLE PRECISION F(NPOIN2,NPLAN),TRA01(NPOIN2,NPLAN)
      DOUBLE PRECISION SHP3(NPOIN2,NPLAN),B(NPOIN2)
      DOUBLE PRECISION SHP1(NPOIN2,NPLAN),SHP2(NPOIN2,NPLAN)
      DOUBLE PRECISION SHZ(NPOIN2,NPLAN),UMSHZ
C
      INTEGER IKLE2(NELEM2,3),ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
      INTEGER ETAP1(NPLAN)
      INTEGER ELT2,ETA2
      INTEGER IP2,IP3,IP4,NNSEND,NNRECV
C
C-----------------------------------------------------------------------
C
C
      DO 2 IPLAN=1,NPLAN
        DO 3 IP=1,NPOIN2
      	TRA01(IP,IPLAN)=F(IP,IPLAN)*B(IP)
3       CONTINUE
2     CONTINUE
C
       IF (NCSIZE.GT.1) THEN
C
       IF (.NOT.ALLOCATED(F_SEND)) ALLOCATE(F_SEND(SUM(
     &                                      RECVCOUNTS(:,IFREQ))))
       IF (.NOT.ALLOCATED(F_RECV)) ALLOCATE(F_RECV(SUM(
     &                                      SENDCOUNTS(:,IFREQ))))
        NNSEND = SUM(SENDCOUNTS(:,IFREQ))
        NNRECV = SUM(RECVCOUNTS(:,IFREQ))
C
        DO IP2 = 1,SUM(RECVCOUNTS(:,IFREQ))
           ELT2 = SH_LOC(IFREQ)%ELT(IP2)
           ETA2 = SH_LOC(IFREQ)%ETA(IP2)
            F_SEND(IP2)%F(1) = TRA01(IKLE2(ELT2,1),ETA2)
            F_SEND(IP2)%F(2) = TRA01(IKLE2(ELT2,2),ETA2)
            F_SEND(IP2)%F(3) = TRA01(IKLE2(ELT2,3),ETA2)
            F_SEND(IP2)%F(4) = TRA01(IKLE2(ELT2,1),ETAP1(ETA2))
            F_SEND(IP2)%F(5) = TRA01(IKLE2(ELT2,2),ETAP1(ETA2))
            F_SEND(IP2)%F(6) = TRA01(IKLE2(ELT2,3),ETAP1(ETA2))
            F_SEND(IP2)%IOR   = RECVCHAR(IP2,IFREQ)%IOR
            F_SEND(IP2)%MYPID = RECVCHAR(IP2,IFREQ)%MYPID
            F_SEND(IP2)%NEPID = RECVCHAR(IP2,IFREQ)%NEPID
            F_SEND(IP2)%SHP1  = SH_LOC(IFREQ)%SHP1(IP2)
            F_SEND(IP2)%SHP2  = SH_LOC(IFREQ)%SHP2(IP2)
            F_SEND(IP2)%SHP3  = SH_LOC(IFREQ)%SHP3(IP2)
            F_SEND(IP2)%SHZ   = SH_LOC(IFREQ)%SHZ(IP2)
            F_SEND(IP2)%BP    = (F_SEND(IP2)%F(1)*F_SEND(IP2)%SHP1 +
     &                  F_SEND(IP2)%F(2)*F_SEND(IP2)%SHP2 +
     &    F_SEND(IP2)%F(3)*F_SEND(IP2)%SHP3)*(1.D0-F_SEND(IP2)%SHZ) +
     &                  (F_SEND(IP2)%F(4)*F_SEND(IP2)%SHP1 +
     &                  F_SEND(IP2)%F(5)*F_SEND(IP2)%SHP2 +
     &    F_SEND(IP2)%F(6)*F_SEND(IP2)%SHP3)*F_SEND(IP2)%SHZ
       ENDDO

       CALL GLOB_FONCTION_COMM ()

       ENDIF
C
      DO 10 IPLAN = 1 , NPLAN
         DO 20 IP = 1 , NPOIN2
         ETAG=ETA(IP,IPLAN)
          ETAGP1=ETAP1(ETAG)
         IPOIN1=IKLE2(ELT(IP,IPLAN),1)
         IPOIN2=IKLE2(ELT(IP,IPLAN),2)
         IPOIN3=IKLE2(ELT(IP,IPLAN),3)
         UMSHZ=1.D0-SHZ(IP,IPLAN)
           F(IP,IPLAN)=
     &      ((TRA01(IPOIN1,ETAG) * SHP1(IP,IPLAN)
     &      + TRA01(IPOIN2,ETAG) * SHP2(IP,IPLAN)
     &      + TRA01(IPOIN3,ETAG) * SHP3(IP,IPLAN)) * UMSHZ
     &     +( TRA01(IPOIN1,ETAGP1) * SHP1(IP,IPLAN)
     &      + TRA01(IPOIN2,ETAGP1) * SHP2(IP,IPLAN)
     &      + TRA01(IPOIN3,ETAGP1) * SHP3(IP,IPLAN))
     &       * SHZ(IP,IPLAN) ) /B(IP)
         IF (F(IP,IPLAN).LT.0.D0) THEN
            F(IP,IPLAN)=0.D0
         ENDIF

20      CONTINUE
10    CONTINUE

      IF (NCSIZE.GT.1) THEN

        DO 110 IP = 1 ,NNSEND

           UMSHZ=1.D0-F_RECV(IP)%SHZ
           IPLAN = F_RECV(IP)%IOR/NPOIN2+1
           IP2 = F_RECV(IP)%IOR-(IPLAN-1)*NPOIN2
           IF (IP2==0) IPLAN = IPLAN-1
           IF (IP2==0) IP2 = NPOIN2
           F(IP2,IPLAN) =
     &         F_RECV(IP)%BP/B(IP2)
           IF (F(IP2,IPLAN).LT.0.D0) THEN
              F(IP2,IPLAN) = 0.D0
           ENDIF
C
110     CONTINUE
       DEALLOCATE(F_SEND,F_RECV)

         DO IPLAN=1,NPLAN
           CALL PARCOM2
     &     ( F(:,IPLAN) ,
     &       F(:,IPLAN) ,
     &       F(:,IPLAN) ,
     &       NPOIN2 , 1 , 3 , 1 , MESH )
         ENDDO

       ENDIF
C
      RETURN
      END
C
C#######################################################################
C