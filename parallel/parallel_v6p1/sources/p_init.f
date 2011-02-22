C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES.
!>                REGISTERS PROGRAM WITH PARASTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHAINE, IPID, NCAR, NCSIZE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EXE, I, IER, MYNAM, MYTID, NPREAD, PNUMBER, YAPARA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> MPI_BARRIER, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_INIT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_INIT()

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
!>  <tr>
!>    <td><center> 5.9                                    </center></td>
!>    <td> 16/05/2008                                              </td>
!>    <td> P. VEZOLLE (IBM)                                        </td>
!>    <td> MODIFIED (SIZE OF EXTENSION)                            </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 28/12/1999                                              </td>
!>    <td> J.A. JANKOWSKI (BAW KARLSRUHE)                          </td>
!>    <td> RELEASE 5.0 MODIFIED                                    </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> **/10/1999                                              </td>
!>    <td> RAINER JOHANNI (SGI MUNICH)                             </td>
!>    <td> ADAPTED FOR MPI                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/12/1996                                              </td>
!>    <td> J-M HERVOUET (LNH)                                      </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/06/1996                                              </td>
!>    <td> REINHARD HINKELMANN (HANOVER)                           </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> **/06/1996                                              </td>
!>    <td> HANS HERRMANN (HANOVER)                                 </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHAINE
!></td><td>---</td><td>REPERTOIRE DE TRAVAIL
!>    </td></tr>
!>          <tr><td>IPID
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_INIT
     &(CHAINE,NCAR,IPID,NCSIZE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHAINE         |---| REPERTOIRE DE TRAVAIL
C| IPID           |---| 
C| NCAR           |---| 
C| NCSIZE         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON /INFO/ LNG,LU
C
      INCLUDE 'mpif.h'
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(OUT)            :: NCAR,IPID,NCSIZE
      CHARACTER(LEN=250), INTENT(OUT) :: CHAINE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER MYTID,IER,I,NPREAD
C
      CHARACTER*255 EXE
C
      LOGICAL YAPARA
      CHARACTER*11 PNUMBER
      CHARACTER*13 MYNAM
C
C-----------------------------------------------------------------------
C MPI IS SILENT WHEN EVERYTHING IS GOING ON PROPERLY
C DEFAULT LANGUAGE 2 AND STANDARD OUTPUT 6
C IN ORDER TO SEE ERROR MESSAGES (TO THE MASTER!)
C THE SUBROUTINE CALLED NEXT IS READ_CONFIG !
C THIS IS NOT PRETTY...
C
      LNG=2
      LU=6
C
C     ALL WRITE STATEMENTS BEFORE OPENING A FILE ON LU=6 SEEM TO RAISE
C     A PROBLEM ON WINDOWS COMPAQ COMPILER
C
C     FROM SOGREAH (BUT DENYNONE IS NOT STANDARD)
C     OPEN(UNIT=LU,FILE="PARALLEL.LOG",FORM='FORMATTED',ACTION='WRITE',
C    *     SHARE='DENYNONE')
C
C$$$      IF(LNG.EQ.1) WRITE(LU,*) 'ENTREE DANS P_INIT'
C$$$      IF(LNG.EQ.2) WRITE(LU,*) 'ENTERING P_INIT'
C$$$      IF(LNG.EQ.1) WRITE(LU,*)
C$$$     *    'LOGGING OUTPUT DIRECTED INTO FILES FOR EACH PROCESSOR'
C$$$      IF(LNG.EQ.2) WRITE(LU,*)
C$$$     *    'LOGGING OUTPUT DIRECTED INTO FILES FOR EACH PROCESSOR'
C
C INITIALISES MPI
C
      CALL MPI_INIT(IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_INIT'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_INIT'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
C
C OBTAINS MYTID, IT IS VIRTUALLY THE PROCESSOR NUMBER (RANK)
C
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IPID,IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_COMM_RANK'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_RANK'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
C
C OBTAINS NCSIZE
C
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NCSIZE,IER)
      IF(IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_COMM_SIZE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR IN MPI_COMM_SIZE'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ENDIF
C
C$$$      IF(LNG.EQ.1) WRITE(LU,*)
C$$$     &    'MPI: CALCUL AVEC ',NCSIZE,' PROCESSEURS'
C$$$      IF(LNG.EQ.2) WRITE(LU,*)
C$$$     &    'MPI: COMPUTATION WITH ',NCSIZE,' PROCESSORS'
C
C MANIPULATES MASTER'S AND SLAVES' STANDART OUTPUT TO FILES PE#LOG.TXT
C SLAVES WRITE TO CHANNEL 95 (?)
C WORKS FOR PE# 0-999 (WE HAVE A DECENT NUMBER OF PROCESSORS!)
C
      IF(NCSIZE.GT.1) THEN
        PNUMBER = '00000-00000'
C
        IF((NCSIZE-1).LT.10) THEN
          WRITE(PNUMBER(05:05),'(I1)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.100) THEN
          WRITE(PNUMBER(04:05),'(I2)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.1000) THEN
          WRITE(PNUMBER(03:05),'(I3)') NCSIZE-1
        ELSEIF((NCSIZE-1).LT.10000) THEN
          WRITE(PNUMBER(02:05),'(I4)') NCSIZE-1
        ELSE
          WRITE(PNUMBER(01:05),'(I5)') NCSIZE-1
        ENDIF
C
        IF(IPID.LT.10) THEN
          WRITE(PNUMBER(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(PNUMBER(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(PNUMBER(9:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(PNUMBER(8:11),'(I4)') IPID
        ELSE
          WRITE(PNUMBER(7:11),'(I5)') IPID
        ENDIF
        WRITE(MYNAM,'("PE", A11)') PNUMBER
C
      ENDIF
C
      IF(IPID.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'MAITRE PROCESSEUR NUMERO ',
     &                  IPID,' OF THE GROUP OF ',NCSIZE
        IF(LNG.EQ.2) WRITE(LU,*) 'MASTER PROCESSOR NUMBER ',
     &                  IPID,' OF THE GROUP OF ',NCSIZE
      ELSE
        OPEN(UNIT=LU,FILE=MYNAM//'.LOG', FORM='FORMATTED',
     &         STATUS='UNKNOWN')
        IF(LNG.EQ.1) WRITE(LU,*) 'ESCLAVE PROCESSEUR NUMERO ',
     &                           IPID,' IN THE GROUP OF ',NCSIZE
        IF(LNG.EQ.2) WRITE(LU,*) 'SLAVE  PROCESSOR NUMBER ',
     &                           IPID,' IN THE GROUP OF ',NCSIZE
      ENDIF
C
C READS THE NUMBER OF PROCESSORS AND NAME OF THE EXECUTABLE
C
      NCAR=0
      NPREAD=1
      YAPARA=.FALSE.
      INQUIRE(FILE='./PARAL',EXIST=YAPARA)

      IF(YAPARA) THEN
        OPEN(40,FILE='PARAL',FORM='FORMATTED',ACTION='READ')
        READ(40,*) NPREAD
        IF(NPREAD.NE.NCSIZE) THEN
          WRITE (LU,*)
     &      'P_INIT: FILE PARAL IS INCONSISTENT WITH MPI PARAMETERS'
          WRITE (LU,*) 'MPI NCSIZE   = ',NCSIZE
          WRITE (LU,*) 'PARAL NCSIZE = ',NPREAD
        ENDIF
CC        IF(LNG.EQ.1) WRITE(LU,*)'CALCUL AVEC ',NPREAD,' PROCESSEURS'
CC        IF(LNG.EQ.2) WRITE(LU,*)'COMPUTATION WITH ',NPREAD,' PROCESSORS'
        READ(40,*) NCAR
        READ(40,100) CHAINE
100     FORMAT(A250)
        EXE(1:NCAR+5)=CHAINE(1:NCAR) // 'A.EXE'
        IF(LNG.EQ.1) WRITE(LU,*) 'FICHIER EXECUTABLE : ',EXE(1:NCAR+5)
        IF(LNG.EQ.2) WRITE(LU,*) 'EXECUTABLE FILE: ',EXE(1:NCAR+5)
        CLOSE(40)
      ELSEIF(IPID.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: FICHIER PARAL NON TROUVE'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: FILE PARAL NOT FOUND'
        STOP
      ENDIF
C
C THE BARRIER USUALLY COMES UNEXPECTED
C
      CALL MPI_BARRIER(MPI_COMM_WORLD,IER)
      IF (IER.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'P_INIT: ERREUR DANS MPI_BARRIER'
        IF(LNG.EQ.2) WRITE(LU,*) 'P_INIT: ERROR MPI_BARRIER'
        WRITE(LU,*) 'MPI ERROR ',IER
        STOP
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'BARRIERE PASSEE'
        IF(LNG.EQ.2) WRITE(LU,*) 'BARRIER PASSED'
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END









C
C#######################################################################
C