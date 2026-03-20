       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-08-RAPPORT-CONGES.
      * US-08 : Rapport des conges
      * Feature 3 : Gestion des conges
      * PI-2 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-CONGES
               ASSIGN TO '../conges.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-RAPPORT
               ASSIGN TO '../rapport-conges.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-CONGES.
           COPY '../../../copybooks/conge.cpy'.

        FD FICHIER-RAPPORT.
        01 LIGNE-RAPPORT      PIC X(80).

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X     VALUE 'N'.
        01 WS-TOTAL-CP        PIC 9(3)  VALUE 0.
        01 WS-TOTAL-RTT       PIC 9(3)  VALUE 0.
        01 WS-TOTAL-MAL       PIC 9(3)  VALUE 0.
        01 WS-TOTAL-JOURS     PIC 9(3)  VALUE 0.
        01 WS-AFF-NB          PIC ZZ9.
        01 WS-LIGNE           PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.

           OPEN INPUT  FICHIER-CONGES
           OPEN OUTPUT FICHIER-RAPPORT

           PERFORM LIRE-CONGES
           PERFORM ECRIRE-RAPPORT

           CLOSE FICHIER-CONGES
           CLOSE FICHIER-RAPPORT
           DISPLAY 'Rapport genere : rapport-conges.txt'
           STOP RUN.

       LIRE-CONGES.
           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-CONGES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD CONGE-NB-JOURS TO WS-TOTAL-JOURS
                       EVALUATE CONGE-TYPE
                           WHEN 'CP        '
                               ADD CONGE-NB-JOURS TO WS-TOTAL-CP
                           WHEN 'RTT       '
                               ADD CONGE-NB-JOURS TO WS-TOTAL-RTT
                           WHEN 'Maladie   '
                               ADD CONGE-NB-JOURS TO WS-TOTAL-MAL
                       END-EVALUATE
               END-READ
           END-PERFORM.

       ECRIRE-RAPPORT.
           MOVE '======================================' 
              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE 'RAPPORT DES CONGES PI-2' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE '======================================' 
              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-CP TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total CP      : ' WS-AFF-NB ' jours'
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-RTT TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total RTT     : ' WS-AFF-NB ' jours'
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-MAL TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total Maladie : ' WS-AFF-NB ' jours'
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '--------------------------------------' 
              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-JOURS TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total general : ' WS-AFF-NB ' jours'
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '======================================' 
              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT.