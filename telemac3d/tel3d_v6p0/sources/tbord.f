C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES U STAR AND AUBOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AUBORL, DISBOR, DNUVIH, DNUVIV, IKLE, KADH, KARMAN, KENT, KENTU, KFROTL, KLOG, KSORT, LISRUL, LIUBOL, MESH2D, NBOR, NELBOR, NELEM2, NONHYD, NPLAN, NPOIN2, NPTFR, NULONE, RUGOL, U, UETCAL, UTANG, V, W
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIST, I3, IELEM, IPLAN, IPOIN2, IPTFR, ITER, MAXITEREICH, PROPNU, TEST, TESTREICH, UETREICH, UETUTA, VNORM, YPLUS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM_BORD(), PLANTE()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 26/06/2008
!> </td><td> V. BOYER UMIST
!> </td><td> PARALLELISM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AUBORL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DISBOR
!></td><td>--></td><td>DISTANCE AU BORD DES POINTS VOISINS DU BORD
!>    </td></tr>
!>          <tr><td>DNUVIH
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION HORIZONTALE
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION VERTICALE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>CONVENTION POUR UNE PAROI AVEC ADHERENCE
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR IMPOSEE
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KFROTL
!></td><td>--></td><td>LOI DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR UNE PAROI LOGARITHMIQUE
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>CONVENTION POUR UN POINT A VALEUR LIBRE
!>    </td></tr>
!>          <tr><td>LISRUL
!></td><td>--></td><td>REGIME DE TURBULENCE 1: LISSE
!>                  DES BORDS            2: RUGUEUX
!>    </td></tr>
!>          <tr><td>LIUBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES POUR U
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NONHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS  DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RUGOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>COMPOSANTES X DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>UETCAL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UTANG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>COMPOSANTES Y DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DES POINTS DU MAILLAGE 3D REEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TBORD
     & (AUBORL,LIUBOL,
     &  RUGOL,DISBOR,NELBOR,NULONE,IKLE,NELEM2,
     &  U,V,W,NBOR,NPOIN2,NPLAN,NPTFR,
     &  DNUVIH,DNUVIV,KARMAN,LISRUL,KFROTL,
     &  KENT,KENTU,KSORT,KADH,KLOG,UETCAL,NONHYD,UTANG,MESH2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AUBORL         |---| 
C| DISBOR         |-->| DISTANCE AU BORD DES POINTS VOISINS DU BORD
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
C| H             |-->| HAUTEUR D'EAU AU TEMPS N
C| IKLE           |---| 
C| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
C| KARMAN         |-->| CONSTANTE DE KARMAN
C| KENT           |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
C| KENTU          |---| 
C| KFROTL         |-->| LOI DE FROTTEMENT
C| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
C| KSORT          |-->| CONVENTION POUR UN POINT A VALEUR LIBRE
C| LISRUL         |-->| REGIME DE TURBULENCE 1: LISSE
C|                |   | DES BORDS            2: RUGUEUX
C| LIUBOL         |---| 
C| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES POUR U
C| MESH2D         |---| 
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NELBOR         |---| 
C| NELEM2         |---| 
C| NONHYD         |---| 
C| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
C| NULONE         |---| 
C| RUGOL          |---| 
C| U             |-->| COMPOSANTES X DE LA VITESSE AU TEMPS N
C| UETCAL         |---| 
C| UTANG          |---| 
C| V             |-->| COMPOSANTES Y DE LA VITESSE AU TEMPS N
C| W             |---| 
C| Z             |-->| COTES DES POINTS DU MAILLAGE 3D REEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,NPLAN,NPOIN2,KENTU,NELEM2
      INTEGER, INTENT(IN) :: LISRUL,KENT,KSORT,KADH,KLOG,KFROTL
!
      INTEGER, INTENT(INOUT) :: LIUBOL(NPTFR,NPLAN)
!
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELEM2,3)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: W(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: UETCAL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(INOUT) :: AUBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: UTANG(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN) :: DISBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: RUGOL(NPTFR,NPLAN)
!
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV, KARMAN
!
      LOGICAL, INTENT(IN) :: NONHYD
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN2,ITER,I3,IELEM
!
      DOUBLE PRECISION UETUTA,DIST,PROPNU,VNORM
      DOUBLE PRECISION YPLUS
      DOUBLE PRECISION, PARAMETER :: TESTREICH = 1.D-4
      DOUBLE PRECISION UETREICH, TEST
      INTEGER, PARAMETER :: MAXITEREICH = 30
!
      INTRINSIC SQRT,MAX,LOG
!
!-----------------------------------------------------------------------
!
      PROPNU = (2*DNUVIH + DNUVIV) /3.D0
!
!=======================================================================
!
C                /* LOOP ON LATERAL BOUNDARIES */
!
C  COMPUTES THE FRICTION VELOCITY ON THE WALL, COMPUTES AUBOR
!
!=======================================================================
!
!
C        COMPUTES UETOIL FOR THE SOLID BOUNDARIES
!        ----------------------------------------
!
!
C        SMOOTH FRICTION TURBULENCE REGIME
!
!
!        *********************
         IF(LISRUL.EQ.1) THEN
!        *********************
!
C                  3
C                  **         IT IS ASSUMED HERE THAT POINT 3 IS IN
C                 *  *        THE LOGARITHMIC LAYER. DISBOR IS THE DISTANCE
C                *    *       BETWEEN 3 AND SEGMENT 1-2
!               *      *
!              *        *
!             *          *
C          1 *            * 2
!  ************************************
!
!
         DO IPLAN=1,NPLAN
           DO IPTFR=1,NPTFR
C            TANGENTIAL VELOCITY AT POINT 3 (IT IS ASSUMED THAT THE POINT IS
C            CLOSE ENOUGH TO THE BOUNDARY SO THAT THE VELOCITY IS TANGENTIAL)
             DIST = DISBOR(IPTFR)
             IELEM=NELBOR(IPTFR)
             IF(IELEM.GT.0) THEN
C              UTANG IS THE TANGENTIAL VELOCITY INSIDE THE DOMAIN,
C              ASSUMED TO BE CLOSE ENOUGH TO THE BOUNDARY THAT IT CAN BE
C              CONSIDERED TANGENTIAL
               I3 = IKLE(IELEM,MOD(NULONE(IPTFR)+2,3)+1)
               UTANG(IPTFR)=SQRT( U(I3,IPLAN)**2
     &                           +V(I3,IPLAN)**2
     &                           +W(I3,IPLAN)**2 )
             ELSE
               UTANG(IPTFR)=0.D0
             ENDIF
           ENDDO
C          FINDS THE TRUE VALUE OF UTANG IF IN ANOTHER
C          SUBDOMAIN
           IF(NCSIZE.GT.1) THEN
             CALL PARCOM_BORD(UTANG,3,MESH2D)
           ENDIF
!
           DO IPTFR=1,NPTFR
C            INITIAL GUESS
             UETUTA= 6.D-2
             UETREICH= 6.D-2
             DO ITER=1,MAXITEREICH
               YPLUS = DIST*UETUTA*UTANG(IPTFR)/PROPNU
               UETUTA = 1.D0/(LOG(1.D0+KARMAN*YPLUS)/KARMAN + 7.8D0*
     &                    (1.D0-EXP(-YPLUS/11.D0) - YPLUS/11.D0
     &                                            *EXP(-0.33D0* YPLUS)))
               TEST = ABS(UETUTA-UETREICH)/UETREICH
               IF(TEST.LT.TESTREICH) THEN
                 GOTO 44
               ELSE
                 UETREICH = UETUTA
               ENDIF
             ENDDO
44           CONTINUE
             UETCAL(IPTFR,IPLAN) = (UETUTA*UTANG(IPTFR))**2
           ENDDO
!
         ENDDO
!
C        ROUGH FRICTION TURBULENCE REGIME
!
!        ************************
         ELSEIF(LISRUL.EQ.2) THEN
!        ************************
!
           IF(KFROTL.EQ.0) THEN
!
C            NO FRICTION NO STRESS
!
             DO IPTFR=1,NPTFR
               DO IPLAN=1,NPLAN
                 UETCAL(IPTFR,IPLAN) = 0.D0
               ENDDO
             ENDDO
!
           ELSEIF(KFROTL.EQ.5) THEN
!
           DO IPLAN=1,NPLAN
             DO IPTFR=1,NPTFR
               DIST = DISBOR(IPTFR)
               IELEM=NELBOR(IPTFR)
               IF(IELEM.GT.0) THEN
                 I3 = IKLE(IELEM,MOD(NULONE(IPTFR)+2,3)+1)
                 UTANG(IPTFR)=SQRT( U(I3,IPLAN)**2
     &                          +V(I3,IPLAN)**2
     &                          +W(I3,IPLAN)**2 )
               ELSE
                 UTANG(IPTFR)=0.D0
               ENDIF
             ENDDO
             IF(NCSIZE.GT.1) THEN
               CALL PARCOM_BORD(UTANG,3,MESH2D)
             ENDIF
             DO IPTFR=1,NPTFR
C              NIKURADSE LAW
               UETUTA=1.D0/(8.5D0+LOG(DIST/RUGOL(IPTFR,IPLAN))/KARMAN)
               UETCAL(IPTFR,IPLAN) = (UETUTA*UTANG(IPTFR))**2
             ENDDO
           ENDDO
!
           ELSE
!
             IF(LNG.EQ.1) WRITE(LU,201) KFROTL
             IF(LNG.EQ.2) WRITE(LU,202) KFROTL
201          FORMAT('TBORD : FROTTEMENT SUR LES PAROIS INCONNU : ',I6)
202          FORMAT('TBORD: UNKNOWN FRICTION ON BOUNDARIES : ',I6)
             CALL PLANTE(1)
             STOP
!
           ENDIF
!
!        ****
         ELSE
!        ****
!
            IF (LNG.EQ.1) WRITE(LU,101) LISRUL
            IF (LNG.EQ.2) WRITE(LU,102) LISRUL
101         FORMAT('TBORD : REGIME DE TURBULENCE INCONNU : ',I6)
102         FORMAT('TBORD: UNKNOWN TURBULENCE MODEL : ',I6)
            CALL PLANTE(1)
            STOP
!
!        *****
         ENDIF
!        *****
!
!-----------------------------------------------------------------------
!
C     COMPUTES AUBORL, AVBORL AND AWBORL
!
C     USING       : NUT * DU/DN = UETOIL**2 = -AUBOR*U(N+1)
C     TURNED INTO : NUT * DU/DN = UETOIL**2  *  U(N+1) / U(N)
C                               = UETOIL * (UETOIL/UTANG) * U(N+1)
C                               = UETOIL *  UETUTA        * U(N+1)
!
!
      IF(KFROTL.NE.0) THEN
!
      IF(NONHYD) THEN
!
        DO IPTFR=1,NPTFR
          IPOIN2 = NBOR(IPTFR)
          DO IPLAN=1,NPLAN
            VNORM=SQRT(  U(IPOIN2,IPLAN)**2
     &                  +V(IPOIN2,IPLAN)**2
     &                  +W(IPOIN2,IPLAN)**2 )
            AUBORL(IPTFR,IPLAN) = - UETCAL(IPTFR,IPLAN)/MAX(1.D-4,VNORM)
          ENDDO
        ENDDO
!
      ELSE
!
        DO IPTFR=1,NPTFR
          IPOIN2 = NBOR(IPTFR)
          DO IPLAN=1,NPLAN
            VNORM=SQRT( U(IPOIN2,IPLAN)**2+V(IPOIN2,IPLAN)**2)
            AUBORL(IPTFR,IPLAN) = - UETCAL(IPTFR,IPLAN)/MAX(1.D-4,VNORM)
          ENDDO
        ENDDO
!
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C