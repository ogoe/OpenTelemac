C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, TOMAWAC_MPI, TOMAWAC_MPI_TOOLS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CT, CX, CY, DT, ELT, ETA, ETAP1, GOODELT, IFABOR, IKLE2, ITR01, JF, MESH, NELEM2, NF, NPLAN, NPOIN2, NPOIN3, NRK, SHP1, SHP2, SHP3, SHZ, TETA, TRA01, X, Y
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> TOMAWAC_MPI :<br>
!> @link TOMAWAC_MPI::IFREQ IFREQ@endlink, 
!> @link TOMAWAC_MPI::ISPDONE ISPDONE@endlink, 
!> @link TOMAWAC_MPI::NARRV NARRV@endlink, 
!> @link TOMAWAC_MPI::NCHARA NCHARA@endlink, 
!> @link TOMAWAC_MPI::NCHDIM NCHDIM@endlink, 
!> @link TOMAWAC_MPI::NLOSTCHAR NLOSTCHAR@endlink, 
!> @link TOMAWAC_MPI::NSEND NSEND@endlink, 
!> @link TOMAWAC_MPI::RECVAGAIN RECVAGAIN@endlink, 
!> @link TOMAWAC_MPI::RECVCHAR RECVCHAR@endlink, 
!> @link TOMAWAC_MPI::SH_AGAIN SH_AGAIN@endlink, 
!> @link TOMAWAC_MPI::SH_LOC SH_LOC@endlink, 
!> @link TOMAWAC_MPI::TEST TEST@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IER, IP, IPLAN, IPOIN, ITE, LAST_NOMB, NARRSUM, NBB, NLOSTAGAIN, NRECV, NUMBER, NUMBERLOST, PROMIN, SHF, SURDET, TEST2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOC_AGAIN(), ALLOC_LOCAL(), CORRECT_GOODELT(), ENVOI_AGAIN(), FINAL_ORGA_RECV(), GLOB_CHAR_COMM(), INCREM_ENVOI_RECV(), INIT_TOMAWAC(), ORGANIZE_SENDAGAIN(), PIEDS_TOMAWAC(), PIEDS_TOMAWAC_MPI(), PREP_INITIAL_SEND(), RESET_COUNT(), SUPP_ENVOI_AGAIN(), WIPE_HEAPED_CHAR()
!>   </td></tr>
!>     </table>

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
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ETAP1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GOODELT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITR01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NRK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE PIED_MPI(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,
     &                    TRA01,SHP1,SHP2,SHP3,SHZ,JF,ELT,ETA,ITR01,
     &                    GOODELT,NPLAN,NPOIN2,NPOIN3,NF,NELEM2,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CT             |---| 
C| CX             |---| 
C| CY             |---| 
C| DT             |---| 
C| ELT            |---| 
C| ETA            |---| 
C| ETAP1          |---| 
C| GOODELT        |---| 
C| IFABOR         |---| 
C| IKLE2          |---| 
C| ITR01          |---| 
C| JF             |---| 
C| MESH           |---| 
C| NELEM2         |---| 
C| NF             |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NPOIN3         |---| 
C| NRK            |---| 
C| SHP1           |---| 
C| SHP2           |---| 
C| SHP3           |---| 
C| SHZ            |---| 
C| TETA           |---| 
C| TRA01          |---| 
C| X             |---| 
C| Y             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      USE TOMAWAC_MPI_TOOLS
      USE TOMAWAC_MPI, ONLY : SH_AGAIN,RECVAGAIN,SH_LOC,RECVCHAR,
     &                        NARRV,NCHARA,NLOSTCHAR,NSEND,TEST,
     &                        NCHDIM,NFREQ,IFREQ,ISPDONE,INIT_TOMAWAC,
     &                        PIEDS_TOMAWAC,PIEDS_TOMAWAC_MPI,
     &                        WIPE_HEAPED_CHAR,PREP_INITIAL_SEND,
     &                        GLOB_CHAR_COMM
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IMPLICIT NONE

      DOUBLE PRECISION CX(NPOIN3,NF) , CY(NPOIN3,NF)
      DOUBLE PRECISION CT(NPOIN3,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION TETA(NPLAN)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DT,TRA01(NPOIN3,8),PROMIN
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAP1(NPLAN)
      INTEGER ITR01(NPOIN3,3),JF
      INTEGER NPLAN,NPOIN2,NPOIN3,NF,NELEM2,NRK
      INTEGER LAST_NOMB,NLOSTAGAIN,NUMBER,IER,NRECV,NUMBERLOST
      INTEGER ITE,IP,IPLAN,NBB,IPOIN,GOODELT(NPOIN2,NPLAN)
      INTEGER NARRSUM
      INTEGER P_ISUM,P_IMAX
      EXTERNAL P_ISUM,P_IMAX
      DOUBLE PRECISION :: TEST2(NPOIN3,NF)
C      DOUBLE PRECISION :: TES(NPOIN2,NPLAN)
      TYPE(BIEF_MESH)  ::  MESH


         CALL CORRECT_GOODELT(GOODELT,NPOIN2,NPLAN,MESH)
C
         IF (.NOT.ALLOCATED(NCHARA)) ALLOCATE(NCHARA(NF),NLOSTCHAR(NF),
     &                                        NSEND(NF))
         CALL INIT_TOMAWAC(NCHARA(JF),NCHDIM,1,
     &                                       NPOIN3,LAST_NOMB)

C
         IF(.NOT.ALLOCATED(TEST)) ALLOCATE(TEST(NPOIN3,NF))
         IFREQ=JF

           CALL PIEDS_TOMAWAC
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SHP1(1,JF),
     &  SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),ELT(1,JF),ETA(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2),MESH%IFAPAR%I,TEST(1,JF),
     &  NCHDIM,NCHARA(JF),MESH,GOODELT)
C CHECKS WHETHER A CHARACTERISTICS CLOSE TO THE BOUNDARY EXITS AND NOT
C THE OTHER ONE. IN THIS CASE ONLY THE MAXIMUM CONTRIBUTION (FROM BOTH)
C IS CONSIDERED AND THE EXIT CHARACTERISTICS IS NOT TREATED
C          DO IP = 1,NPOIN2
C              DO IPLAN = 1,NPLAN
C                 TES(IP,IPLAN)  =TEST(IP+NPOIN2*(IPLAN-1),JF)
C              ENDDO
C          ENDDO
         WHERE (TEST(:,JF).LT.0.5D0)
             SHP1(:,JF)=0.D0
             SHP2(:,JF)=0.D0
             SHP3(:,JF)=0.D0
             SHZ(:,JF) = 0.D0
         END WHERE
C          DO IPLAN = 1,NPLAN
C          CALL PARCOM2
C      * ( TES(1,IPLAN) ,
C      *   TES(1,IPLAN) ,
C      *   TES(1,IPLAN) ,
C      *   NPOIN2 , 1 , 2 , 1 , MESH )
C          ENDDO
C          DO IP = 1,NPOIN2
C             DO IPLAN = 1,NPLAN
C                TEST(IP+NPOIN2*(IPLAN-1),JF)=TES(IP,IPLAN)
C             ENDDO
C          ENDDO
C          WHERE (TEST(:,JF).GT.1.5D0)
C             SHP1(:,JF)=SHP1(:,JF)/TEST(:,JF)
C             SHP2(:,JF)=SHP2(:,JF)/TEST(:,JF)
C             SHP3(:,JF)=SHP3(:,JF)/TEST(:,JF)
C          END WHERE
C HEAPCHAR(NCHARA,NFREQ) AND HEAPCOUNT(NCSIZE,NFREQ)
C HEAPCOUNT=> NUMBER OF CHARACTERISTICS ON EACH PROCESSOR
         CALL WIPE_HEAPED_CHAR(TEST(1,JF),NPOIN3,.TRUE.,NSEND(JF),
     &                        NLOSTCHAR(JF),NCHDIM,
     &                        NCHARA(JF))

C IS NOT NECESSARILY USEFUL, CHECKS IF TEST==1, IN WHICH CASE IT IS DELETED
C FROM THE LIST OF CHARACTERISTICS BY ASSIGNING HEAPCAHR%NEPID==-1
C        DO WHILE(P_IMAX(NLOSTCHAR(JF))>0)! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA)

C CREATES THE ARRAY 'SDISP' AND ORDERS THE DATA (ASCENDING)
          CALL GLOB_CHAR_COMM ()
C SENDS SENDCHAR AND WRITES TO RECVCHAR


!
         IF(.NOT.ALLOCATED(ISPDONE)) ALLOCATE(ISPDONE(NPOIN3,NF))
         IF(.NOT.ALLOCATED(NARRV)) ALLOCATE(NARRV(NF))
         CALL ALLOC_LOCAL(NARRV(IFREQ),IFREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)

       TEST2(:,JF) = 1.D0
         IF (NUMBERLOST>0) THEN
       CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,
     &  TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_LOC(JF)%SHP1,
     &  SH_LOC(JF)%SHP2,SH_LOC(JF)%SHP3,SH_LOC(JF)%SHZ,
     &  SH_LOC(JF)%ELT,SH_LOC(JF)%ETA,
     &  NARRV(JF),NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVCHAR(1,JF))

        CALL ALLOC_AGAIN(NARRV(IFREQ),IFREQ,NLOSTAGAIN,NUMBERLOST,
     &                   NUMBER)
        CALL ORGANIZE_SENDAGAIN()

        CALL SUPP_ENVOI_AGAIN(IFREQ,NUMBER)

!
           ITE = 0
          DO WHILE((NUMBERLOST>0).AND.(ITE.LE.20))
           ITE= ITE + 1
          CALL ORGANIZE_SENDAGAIN()
          CALL ENVOI_AGAIN(NRECV)
          TEST2(:,JF)=1.D0
          CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_AGAIN%SHP1,
     &  SH_AGAIN%SHP2,SH_AGAIN%SHP3,SH_AGAIN%SHZ,
     &  SH_AGAIN%ELT,SH_AGAIN%ETA,
     &  NRECV,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVAGAIN)
        CALL INCREM_ENVOI_RECV(IFREQ,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                         NRECV)
        ENDDO ! END OF THE DOWHILE LOOP
         CALL FINAL_ORGA_RECV(NARRV(IFREQ),IFREQ)
          ELSE
           CALL RESET_COUNT(IFREQ)
          ENDIF
      END SUBROUTINE PIED_MPI



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SELECTS THE COMPUTATION NODES CLOSEST TO THE
!>                REQUIRED OUTPUT LOCATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, ISLEO, NELEM2, NLEO, NOLEO, NPOIN2, SURDET, X, XLEO, Y, YLEO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> ECRSPE_MPI : SPE_SEND
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIST, DIST2, I, IELEM, ILEO, N1G, N2G, N3G, NOELEM, SHP1, SHP2, SHP3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITE NOEUD ELEMENTS
!>    </td></tr>
!>          <tr><td>ISLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NLEO
!></td><td>--></td><td>NOMBRE DE POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>NOLEO
!></td><td><-></td><td>TABLEAU DES NUMERO DES POINTS CHOISIS
!>    </td></tr>
!>          <tr><td>NOPID
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>1/SUPERFICIE ELEMENTS
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DES POINTS
!>    </td></tr>
!>          <tr><td>XLEO
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>ORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>YLEO
!></td><td>--></td><td>TABLEAU DES ORDONNEES DES POINTS DE SORTIE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE PRELEO_MPI
     &(XLEO,YLEO,NLEO,X,Y,IKLE,SURDET,NPOIN2,NELEM2,NOLEO,ISLEO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| CONNECTIVITE NOEUD ELEMENTS
C| ISLEO          |---| 
C| NCSIZE         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NLEO           |-->| NOMBRE DE POINTS DE SORTIE
C| NOLEO          |<->| TABLEAU DES NUMERO DES POINTS CHOISIS
C| NOPID          |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| SURDET         |-->| 1/SUPERFICIE ELEMENTS
C| X             |-->| ABSCISSES DES POINTS
C| XLEO           |-->| TABLEAU DES ABSCISSES DES POINTS DE SORTIE
C| Y             |-->| ORDONNEES DES POINTS
C| YLEO           |-->| TABLEAU DES ORDONNEES DES POINTS DE SORTIE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE

      COMMON/ECRSPE_MPI/SPE_SEND
      INTEGER SPE_SEND
C
      INTEGER I,ILEO,NLEO,NPOIN2,NELEM2,IELEM,NOELEM
C
      DOUBLE PRECISION X(NPOIN2)  , Y(NPOIN2)
      DOUBLE PRECISION XLEO(NLEO)  , YLEO(NLEO)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DIST,DIST2,SHP1,SHP2,SHP3
C
      INTEGER NOLEO(NLEO),N1G,N2G,N3G
      INTEGER IKLE(NELEM2,3)
      LOGICAL ISLEO(NLEO)
C
C-----------------------------------------------------------------------
C
C       DO 10 ILEO=1,NLEO
C         DIST=1.D99
C         DO 20 I=1,NPOIN2
C          DIST2=(XLEO(ILEO)-X(I))**2+(YLEO(ILEO)-Y(I))**2
C          IF (DIST2.LT.DIST) THEN
C              DIST=DIST2
C              NOLEO(ILEO)=I
C          ENDIF
C 20      CONTINUE
C 10    CONTINUE
       SPE_SEND = 0
       ISLEO = .FALSE.
       NOLEO = 1
       DO ILEO = 1,NLEO
          NOELEM = 0
          DO 20 IELEM = 1,NELEM2
             N1G=IKLE(IELEM,1)
             N2G=IKLE(IELEM,2)
             N3G=IKLE(IELEM,3)
               SHP1 = ((X(N3G)-X(N2G))*(YLEO(ILEO)-Y(N2G))
     &               -(Y(N3G)-Y(N2G))*(XLEO(ILEO)-X(N2G)))*SURDET(IELEM)
               SHP2 = ((X(N1G)-X(N3G))*(YLEO(ILEO)-Y(N3G))
     &               -(Y(N1G)-Y(N3G))*(XLEO(ILEO)-X(N3G)))*SURDET(IELEM)
               SHP3 = ((X(N2G)-X(N1G))*(YLEO(ILEO)-Y(N1G))
     &               -(Y(N2G)-Y(N1G))*(XLEO(ILEO)-X(N1G)))*SURDET(IELEM)
             IF ((SHP1.GE.0.D0).AND.(SHP2.GE.0.D0)
     &                                        .AND.(SHP3.GE.0.D0)) THEN
               ISLEO(ILEO) = .TRUE.
               NOELEM = IELEM
               IF (SHP2>SHP1) THEN
                  NOLEO(ILEO) = N2G
                  IF (SHP3>SHP2) NOLEO(ILEO) = N3G
               ELSE
                  NOLEO(ILEO) = N1G
                  IF (SHP3>SHP1) NOLEO(ILEO) = N3G
               ENDIF
               SPE_SEND=SPE_SEND+1
               GOTO 30
              ENDIF
20         CONTINUE
30       CONTINUE
       ENDDO

C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C